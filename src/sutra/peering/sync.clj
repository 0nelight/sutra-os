(ns sutra.peering.sync
  (:require [sutra.catalog.lookup :as lookup]
            [sutra.kit.refusal :as refusal]
            [sutra.peering.objects :as objects]
            [sutra.peering.rows :as rows]
            [sutra.peering.wire :as wire]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.streams :as streams]
            [sutra.store.world :as world]
            [sutra.types.authority :as policy]
            [sutra.types.peering :as peering]))

(set! *warn-on-reflection* true)

(def page-max 5000)

(def peer-path "/api/peering/peer")

(defn- dataset-of [schema ds]
  (when-let [rid (lookup/rid-of schema :datasets (keyword (name ds)))]
    [rid (get-in schema [:datasets rid])]))

(defn share!
  [store actor id ds]
  (let [c (rows/by-name-or-id store id)]
    (when-not (dataset-of (objects/schema store) ds)
      (refusal/refuse! :refusal.peering/no-dataset {:dataset ds}))
    (rows/share! store {:connection-id (:connection-id c) :dataset (name ds)
                        :created-by actor})))

(defn unshare! [store id ds]
  (let [c (rows/by-name-or-id store id)]
    (rows/unshare! store (:connection-id c) ds)
    {:unshared (name ds) :connection-id (:connection-id c)}))

(defn- wire-markings [schema c rid row-markings]
  (peering/wire-markings (into (objects/type-markings schema rid) (map keyword) row-markings)
                         (policy/classification-of schema) (:marking-map c)))

(def ^:private sql-type
  {:string "TEXT" :long "BIGINT" :integer "BIGINT" :int "BIGINT" :short "BIGINT"
   :double "DOUBLE PRECISION" :float "DOUBLE PRECISION" :decimal "NUMERIC"
   :boolean "BOOLEAN" :date "DATE" :timestamp "TIMESTAMP"})

(defn- columns-of [entry]
  (vec (for [{:keys [attribute-name attribute-type]} (:columns entry)
             :let [t (some-> attribute-type (#(if (vector? %) (first %) %)) name keyword)]]
         {:name (name attribute-name) :type (get sql-type t "TEXT")
          :ontology-type (or t :string)})))

(defn- read-refusal
  [store schema c ds]
  (let [[rid _] (dataset-of schema ds)]
    (if-not rid
      [:refusal.peering/no-dataset {}]
      (when-let [why (peering/share-refusal c (rows/shared? store (:connection-id c) ds)
                                            (name ds) (wire-markings schema c rid []))]
        [:refusal.peering/dataset-not-readable {:why why}]))))

(defn- current-rows
  [store schema ds rid entry]
  (if (:stream entry)
    (mapv (fn [r] {:values r :markings []})
          (:rows (streams/view store schema (keyword (name ds)) Integer/MAX_VALUE)))
    (dsets/marked-rows store rid)))

(defn served
  [store c {:keys [op dataset offset]}]
  (let [store  (world/at store objects/branch)
        schema (objects/schema store)]
    (case (keyword (name op))
      :shared
      {:datasets (vec (for [{ds :dataset} (rows/shares store (:connection-id c))
                            :let [[_ entry] (dataset-of schema ds)]
                            :when (and entry (nil? (read-refusal store schema c ds)))]
                        {:name ds :stream? (boolean (:stream entry))
                         :columns (columns-of entry)}))}
      :rows
      (if-let [[code params] (read-refusal store schema c dataset)]
        (refusal/refuse! code {:dataset dataset} params)
        (let [[rid entry] (dataset-of schema dataset)
              all   (current-rows store schema dataset rid entry)
              ok    (filterv #(peering/admitted? (:ceiling c)
                                                 (wire-markings schema c rid (:markings %)))
                             all)
              start (long (or offset 0))
              page  (subvec ok (min start (count ok)) (min (+ start page-max) (count ok)))]
          {:columns (columns-of entry)
           :rows    (mapv #(update-keys (:values %) name) page)
           :held    (- (count all) (count ok))
           :next    (when (< (+ start page-max) (count ok)) (+ start page-max))})))))

(defn- ask! [{:keys [store c]} msg]
  (wire/deliver! store (:trust c) peer-path (assoc msg :connection-id (:connection-id c))))

(defn- open [spec _ctx]
  (let [store (ctx/system-store)
        c     (rows/by-name-or-id store (:connection spec))]
    (when-not (peering/imports? (get-in c [:data-types :datasets]))
      (refusal/refuse! :refusal.peering/datasets-not-taken
                       {:connection (:connection spec)}
                       {:name (:name c)}))
    {:store store :c c}))

(defn- shared [h] (:datasets (ask! h {:op :shared})))

(defn- shared-one [h table]
  (or (some #(when (= (str table) (:name %)) %) (shared h))
      (refusal/refuse! :refusal.peering/remote-dataset-missing {:table table})))

(defn- scan
  [h table _pushed]
  (loop [offset 0 acc (transient [])]
    (let [{:keys [rows next]} (ask! h {:op :rows :dataset (str table) :offset offset})
          acc (reduce conj! acc rows)]
      (if next (recur (long next) acc) (persistent! acc)))))

(def connector
  {:profile {:capabilities #{:catalog :scan} :route #{:direct} :needs [:connection]
             :scan {:preds {} :project? false :page? false :compound? false}}
   :config  [:map [:connection :string]]
   :form    {:label "Sutra-Instanz (Peering)"
             :fields [{:key :connection :label "Peer-Verbindung" :kind :string
                       :required true}]}
   :open    open
   :close   (fn [_] nil)
   :probe   (fn [h] (mapv :name (shared h)))
   :tables  (fn [h] (mapv :name (shared h)))
   :columns (fn [h table] (:columns (shared-one h table)))
   :scan    scan})
