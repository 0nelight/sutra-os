(ns sutra.media.input-entries
  (:require [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [sutra.blobs.api :as blobs]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.row-shape :as row-shape]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.catalog.lookup :as lookup]
            [sutra.media.sets :as sets]
            [sutra.types.column :as column]
            [sutra.types.naming :as naming]))

(set! *warn-on-reflection* true)

(defn media-ref-columns
  [schema dataset-rid]
  (->> (get-in schema [:datasets dataset-rid :columns])
       (filter (fn [{:keys [attribute-type]}]
                 (or (#{:media-reference :attachment} attribute-type)
                     (and (vector? attribute-type)
                          (= :array (first attribute-type))
                          (#{:media-reference :attachment} (second attribute-type))))))
       (map (comp column/sql-name :attribute-name))
       (map keyword)))

(defn media-refs
  [rows ref-cols]
  (into (sorted-set)
        (for [row  rows
              col  ref-cols
              :let [v (get row col)]
              r    (cond (string? v) [v] (sequential? v) v :else nil)
              :when (and (string? r) (blobs/valid-address? r))]
          r)))

(defn media-set-entry
  [schema ds-rid rows]
  (let [refs (media-refs rows (media-ref-columns schema ds-rid))]
    (cond-> {:kind :media-set :rows rows}
      (seq refs) (assoc :media-refs refs))))

(defn whole-media-set
  [store schema in]
  (let [ds-rid (lookup/rid-of schema :datasets (:dataset in))
        t      (naming/dataset-table ds-rid)]
    (media-set-entry schema ds-rid
                     (dsets/row-values store t (row-shape/array-keys store schema t)))))

(defn set-items-entry
  [store schema ref]
  (let [nm (sets/set-name schema ref)]
    (when-not (sets/readable? store schema nm)
      (refusal/refuse! :refusal.media/set-not-readable {:media-set nm} {:name (name nm)}))
    (let [rows (mapv (fn [{:keys [address path filename mime size source-modified]}]
                       {:media_ref address :path path :filename filename :mime mime :size size
                        :modified source-modified})
                     (sets/items store nm))]
      (cond-> {:kind :media-set :rows rows}
        (seq rows) (assoc :media-refs (into (sorted-set) (map :media_ref) rows))))))

(defn media-item-entry
  [store schema in address]
  (let [ds-rid (lookup/rid-of schema :datasets (:dataset in))
        t      (some-> ds-rid naming/dataset-table)
        cols   (when ds-rid (media-ref-columns schema ds-rid))
        ds     (name (or (some->> ds-rid (lookup/name-of schema :datasets)) (:dataset in)))
        refuse #(refusal/refuse! % {:dataset ds :address address})]
    (cond
      (not (and (string? address) (blobs/valid-address? address)))
      (refuse :refusal.media/input-not-address)
      (empty? cols)
      (refuse :refusal.media/input-no-media-refs)
      (not (contains? (media-refs (dsets/row-values store t (row-shape/array-keys store schema t)
                                                    {:columns (mapv name cols)})
                                  cols)
                      address))
      (refuse :refusal.media/input-unreferenced)
      :else
      {:kind :media :address address :media-refs #{address}})))

(defn language-model-entry
  [schema in]
  (let [in (llm-models/with-source schema in)]
    (cond-> {:kind :language-model :source (:source in) :model (:model in)}
      (:base-url in) (assoc :base-url (:base-url in)))))

(defn set-item-entry
  [store schema media-set address]
  (when (and (string? address) (blobs/valid-address? address)
             (sets/readable? store schema media-set) (sets/holds? store media-set address))
    {:kind :media :address address :media-refs #{address}}))

(defn media-sources
  [store schema admin?]
  (concat
   (for [rid (sort (keys (:media-sets schema)))
         :let [nm (name (lookup/name-of schema :media-sets rid))]
         :when (sets/readable? store schema nm)]
     {:kind "media-set" :name nm})
   (when admin?
     (for [rid (sort (keys (:datasets schema)))
           :when (seq (media-ref-columns schema rid))]
       {:kind "dataset" :name (name (lookup/name-of schema :datasets rid))}))))

(def ^:private label-columns [:filename :file_name :dateiname :name :title :betreff :path])

(defn row-items
  [cols label-cols rows]
  (let [label (fn [row r] (or (some #(let [x (get row %)] (when (and (string? x)
                                                                    (not (str/blank? x)))
                                                           x))
                                    label-cols)
                              r))]
    (->> (for [row  rows
               col  cols
               :let [v (get row col)]
               r    (cond (string? v) [v] (sequential? v) v :else nil)
               :when (and (string? r) (blobs/valid-address? r))]
           {:address r :label (label row r)})
         (reduce (fn [m {:keys [address] :as it}] (update m address #(or % it))) {})
         vals
         (sort-by :label)
         vec)))

(defn source-items
  [store schema {:keys [kind name]}]
  (case (str kind)
    "media-set" (vec (for [{:keys [address path filename mime]} (sets/items store name)]
                       (cond-> {:address address :label (or filename path address)}
                         mime (assoc :mime mime))))
    "dataset"   (let [ds-rid (lookup/rid-of schema :datasets (keyword name))
                      cols   (when ds-rid (media-ref-columns schema ds-rid))
                      t      (some-> ds-rid naming/dataset-table)]
                  (row-items cols label-columns
                             (when (seq cols)
                               (dsets/row-values store t
                                                 (row-shape/array-keys store schema t)))))
    (refusal/refuse! :refusal.media/source-kind {:kind kind})))
