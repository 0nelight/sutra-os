(ns sutra.media.http.sets
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.noted :as noted]
            [sutra.store.control :as ctl]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.substrate :as sub]
            [sutra.media.sets :as msets]
            [sutra.types.value-types :as vt]
            [sutra.types.media :as mt]
            [sutra.blobs.api :as blobs]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.seat :as seat]
            [sutra.media.ports :as ports]
            [sutra.store.access.dataset-rows :as dsrows]
            [sutra.catalog.grants :as grants]
            [sutra.http.responses :refer [ok]]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.foundation.media-type :as media-type]))

(set! *warn-on-reflection* true)

(def ^:private media-column?
  vt/media?)

(defn- blob-catalog-dataset [schema]
  (when-let [rid (lookup/rid-of schema :object-types :blob)]
    (get-in schema [:object-types rid :dataset])))

(defn media-attribute-types
  [schema]
  (into {}
        (for [[rid ot] (:object-types schema)
              :let [attrs (into [] (comp (filter #(media-column? (:attribute-type %)))
                                         (map :attribute-name))
                                (:attribute-list ot))]
              :when (seq attrs)]
          [rid attrs])))

(defn media-datasets [schema]
  (into (if-let [b (blob-catalog-dataset schema)] #{b} #{})
        (for [[rid ds] (:datasets schema)
              :when (some (comp media-column? :attribute-type) (:columns ds))]
          rid)))

(def ^:private hash-pattern #"[0-9a-f]{64}")

(defn- row-hashes [row]
  (into #{}
        (comp (filter some?) (mapcat #(re-seq hash-pattern (str %))))
        (vals (:values row))))

(defn- branch-names [store]
  (mapv :name (world/branches store)))

(defn- branch-refs [store branch]
  (let [schema (try (seat/rebuild-schema-from-storage (ctx/at store branch))
                    (catch InterruptedException e (throw e))
                    (catch Exception e
                      {::error (.getMessage e)}))]
    (if-let [msg (::error schema)]
      {:hashes #{} :errors [{:branch branch :error msg}]}
      (reduce
       (fn [acc ds-rid]
         (try
           (let [branch-store (ctx/at store branch)]
             (update acc :hashes into
                     (concat (mapcat row-hashes (dsets/rows branch-store ds-rid))
                             (mapcat #(row-hashes {:values %})
                                     (dsets/past-rows branch-store ds-rid)))))
           (catch InterruptedException e (throw e))
           (catch Exception e
             (update acc :errors conj {:branch  branch
                                       :dataset (str ds-rid)
                                       :error   (.getMessage e)}))))
       (reduce
        (fn [acc [ot-rid attrs]]
          (try
            (update acc :hashes into
                    (mapcat row-hashes (sub/read-type (ctx/at store branch) ot-rid)))
            (catch InterruptedException e (throw e))
            (catch Exception e
              (update acc :errors conj {:branch      branch
                                        :object-type (str ot-rid)
                                        :attributes  (mapv str attrs)
                                        :error       (.getMessage e)}))))
        (try
          {:hashes (msets/addresses (ctx/at store branch)) :errors []}
          (catch InterruptedException e (throw e))
          (catch Exception e
            {:hashes #{} :errors [{:branch branch :media-sets "unreadable"
                                   :error (.getMessage e)}]}))
        (media-attribute-types schema))
       (media-datasets schema)))))

(defn- platform-refs
  [store]
  (noted/quietly "platform-held blob roots"
    (into #{} (mapcat #(% store)) (vals (ports/blobs-roots store)))))

(def max-land-bytes
  mt/max-upload-bytes)

(def default-grace-days 7)

(defn- retire-expired!
  [store branch now apply?]
  (let [branch-store (ctx/at store branch)]
    (try
      {:retired (mapv #(assoc % :branch branch)
                      (msets/expire! branch-store (seat/rebuild-schema-from-storage branch-store)
                                     now apply?))
       :errors  []}
      (catch InterruptedException e (throw e))
      (catch Exception e
        {:retired []
         :errors  [{:branch branch :retention "unreadable" :error (.getMessage e)}]}))))

(defn sweep!
  [store {:keys [apply? grace-days]}]
  (let [blob-store    (ctx/blobs)
        grace    (long (or grace-days default-grace-days))
        now      (ctl/millis store)
        cutoff   (- now (* 86400000 grace))
        branches (branch-names store)
        retires  (mapv #(retire-expired! store % now apply?) branches)
        retired  (into [] (mapcat :retired) retires)
        scans    (mapv #(branch-refs store %) branches)
        git      (platform-refs store)
        refs     (into (or git #{}) (mapcat :hashes) scans)
        errors   (into (into (if (nil? git)
                               [{:branch "*" :error "platform-held hashes unreadable"}]
                               [])
                             (mapcat :errors) retires)
                       (mapcat :errors) scans)
        held     (vec (blobs/hashes blob-store))
        sizes    (into {} (map (fn [h] [h (or (blobs/size-of blob-store h) 0)])) held)
        unref    (remove refs held)
        young?   (fn [h] (> (or (blobs/stored-at blob-store h) 0) cutoff))
        fresh    (filterv young? unref)
        stale    (into [] (remove young?) unref)
        bytes-of (fn [hs] (reduce + 0 (map sizes hs)))
        blocked  (cond
                   (seq errors)                   :incomplete-scan
                   (and (empty? refs) (seq held)) :no-references-found
                   :else                          nil)
        deleted  (when (and apply? (nil? blocked))
                   (filterv #(blobs/delete! blob-store % cutoff) stale))]
    {:branches     (vec branches)
     :retired      {:count (count retired) :items retired}
     :held         {:count (count held) :bytes (bytes-of held)}
     :referenced   (count refs)
     :unreferenced {:count (count stale) :bytes (bytes-of stale)}
     :within-grace {:count (count fresh) :grace-days grace}
     :deleted      {:count (count (or deleted [])) :bytes (bytes-of (or deleted []))}
     :applied?     (boolean deleted)
     :blocked      blocked
     :errors       errors}))

(defn media-set-of-column
  [schema dataset media-column]
  (when-let [rid (lookup/rid-of schema :datasets (keyword dataset))]
    (some (fn [c] (when (= (keyword media-column) (:attribute-name c)) (mt/set-of c)))
          (get-in schema [:datasets rid :columns]))))

(defn- write-row!
  [store schema branch dataset row {:keys [key-field]}]
  (let [nm      (name dataset)
        rid     (lookup/rid-of schema :datasets (keyword nm))
        ds      (get-in schema [:datasets rid])
        rows    [{:values row}]
        key-col (some-> key-field ->kw)]
    (if (nil? ds)
      (refusal/refused :refusal.http/unknown-dataset {:name nm})
      (do
        (grants/authorize! (grants/read-rights store) {:resource rid} nm)
        (if-let [{:keys [message data]} (dsrows/write-problem rid ds nm rows :append key-col)]
          (refusal/refused :refusal.media/passed-message data {:message message})
          (let [store (ctx/at store branch)]
            (dsets/ensure-datasets! store {rid (:columns ds)})
            (dsets/write-rows! store rid :append (dsrows/records nm key-col rows))
            (ok {:dataset nm :written 1 :mode :append :key key-col :branch branch})))))))

(defn land!
  [store schema branch {:keys [bytes dataset media-column values values-fn key-field
                               filename mime]}]
  (let [{:keys [hash size]} (blobs/put! (ctx/blobs) bytes)
        mset  (media-set-of-column schema dataset media-column)
        row   (-> (merge (or values {}) (when values-fn (values-fn hash)))
                  (assoc (keyword media-column) hash))
        {write :result triggered :triggered-by}
        ((ports/around-write store) store schema branch dataset
                              #(write-row! store schema branch dataset row {:key-field key-field}))]
    (when (and mset (= 200 (:status write)))
      (msets/record! (ctx/at store branch)
                     {:media-set mset :path (or filename hash) :address hash
                      :size size :mime (media-type/checked mime bytes)
                      :filename filename}))
    {:hash hash :size size :write write :media-set mset
     :triggered-by (when (= 200 (:status write)) triggered)}))
