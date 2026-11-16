(ns sutra.apps.http.standard-view
  (:require [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.store.api :as api]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.lifecycle :as lifecycle]
            [sutra.types.link-side :as ls]))

(set! *warn-on-reflection* true)

(defn app-type-rid
  [schema t]
  (or (lookup/->type-rid schema :object-types t) (lookup/->type-rid schema :interfaces t)))

(defn on-rid
  [schema act]
  (some->> (:on act) (#(if (keyword? %) (subs (str %) 1) %)) (app-type-rid schema)))

(defn app-row
  [rid r]
  (-> r
      (update :object-type-ref #(or % rid))
      (assoc :_version (api/object-version r))))

(defn- humanize-api
  [k]
  (let [s (str/replace (name k) "-" " ")]
    (if (str/blank? s) s (str (str/upper-case (subs s 0 1)) (subs s 1)))))

(defn- many-side?
  [lt dir]
  (let [card (->kw (:cardinality lt))]
    (case dir
      :to   (contains? #{:one-to-many :many-to-many} card)
      :from (contains? #{:many-to-one :many-to-many} card))))

(defn- side-label
  [lt dir]
  (let [many?   (many-side? lt dir)
        [nm pl] (if (= :to dir)
                  [(:forward-name lt) (:forward-plural lt)]
                  [(:reverse-name lt) (:reverse-plural lt)])]
    (or (not-empty (if many? (or (not-empty pl) nm) nm))
        (not-empty pl))))

(defn- far-type-label
  [schema lt dir far]
  (let [ot   (get-in schema [:object-types far])
        nm   (or (not-empty (:display-name ot)) (humanize-api (:object-type-name ot)))]
    (if (many-side? lt dir) (or (not-empty (:plural ot)) nm) nm)))

(defn- side-labels
  [schema sides]
  (let [guess (mapv (fn [{:keys [lt dir far]}] (far-type-label schema lt dir far)) sides)
        taken (frequencies guess)]
    (mapv (fn [{:keys [api lt dir]} g]
            (or (side-label lt dir)
                (if (= 1 (taken g)) g (humanize-api api))))
          sides guess)))

(defn standard-sides
  [auth schema scope rid]
  (let [hidden (grants/hidden-object-types auth)]
    (for [[k lt] (sort-by (comp str key) (:link-types schema))
          [dir far] (cond-> []
                      (= (:from-object-type lt) rid) (conj [:to (:to-object-type lt)])
                      (= (:to-object-type lt) rid)   (conj [:from (:from-object-type lt)]))
          :when (and far
                     (contains? (:type scope) far)
                     (not (contains? hidden far))
                     (get-in schema [:object-types far]))]
      {:api (->kw (ls/api-name k lt dir)) :lt lt :dir dir :far far})))

(defn- heuristic-cols
  [ot]
  (let [attrs (:attribute-list ot)
        vis   #(some-> % :opts :visibility ->kw)
        prom  (keep #(when (= :prominent (vis %)) (:attribute-name %)) attrs)
        first-visible (some #(when-not (= :hidden (vis %)) (:attribute-name %)) attrs)]
    (->> (cons (or (:title-key ot) first-visible) prom)
         (remove nil?)
         (map ->kw)
         distinct
         (take 3)
         vec)))

(defn- standard-cols
  [ot]
  (if (seq (:row ot))
    (mapv ->kw (:row ot))
    (heuristic-cols ot)))

(defn- standard-actions
  [schema rid]
  (let [ot  (get-in schema [:object-types rid])
        lc  (:lifecycle ot)
        on? (let [ifaces (set (:implements ot))]
              (fn [act] (when-let [r (on-rid schema act)]
                          (or (= rid r) (contains? ifaces r)))))
        trs (into {} (for [tr (:transitions lc)]
                       [(->kw (:action tr)) (mapv lifecycle/state-value (:from tr))]))]
    (vec (for [[an act] (sort-by (comp str key) (lookup/actions-by-name schema))
               :when (on? act)]
           (cond-> {:action an
                    :label  (or (not-empty (:display-name act)) (humanize-api an))}
             (and lc (contains? trs an))
             (assoc :property (->kw (:property lc)) :from (get trs an)))))))

(defn standard-view-info
  [auth schema scope rid]
  (when-let [ot (get-in schema [:object-types rid])]
    {:type     (:object-type-name ot)
     :links    (let [sides (vec (standard-sides auth schema scope rid))]
                 (mapv (fn [{:keys [api far]} label]
                         {:api   api
                          :label label
                          :cols  (standard-cols (get-in schema [:object-types far]))})
                       sides (side-labels schema sides)))
     :symbol   (:symbol ot)
     :display-name (not-empty (:display-name ot))
     :plural   (not-empty (:plural ot))
     :subtitle (some-> (:subtitle ot) ->kw)
     :actions  (standard-actions schema rid)}))

(defn app-standard-views
  ([auth schema scope] (app-standard-views auth schema scope (:type scope)))
  ([auth schema scope rids]
   (into {}
         (mapcat (fn [rid]
                   (when-let [info (standard-view-info auth schema scope rid)]
                     [[rid info]])))
         rids)))

(defn standard-link-groups
  [query rights schema scope root-rid id only]
  (let [root-name (get-in schema [:object-types root-rid :object-type-name])]
    (into {}
          (for [{:keys [api far]} (standard-sides rights schema scope root-rid)
                :when (or (nil? only) (contains? only api))
                :let [rows (:rows (query {:find root-name :ids [(str id)] :around [api]}))]]
            [api (mapv #(app-row far %) rows)]))))

(defn app-standard-around
  [query rights schema scope root-rid id]
  (let [root-name (get-in schema [:object-types root-rid :object-type-name])
        derived   (into #{} (keep #(when (get-in % [:opts :derived]) (->kw (:attribute-name %))))
                        (get-in schema [:object-types root-rid :attribute-list]))
        obj
        (first (:rows (query {:find root-name :ids [(str id)] :want derived})))]
    {:object (some->> obj (app-row root-rid))
     :links  (standard-link-groups query rights schema scope root-rid id nil)}))
