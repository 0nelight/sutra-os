(ns sutra.parser.adopt
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn- col-name [c] (keyword (str/lower-case (name (:name c)))))

(defn dataset-name
  [table]
  (keyword (str/replace (str/lower-case (str table)) #"[^a-z0-9]+" "_")))

(defn- default-key [cols]
  (let [names (set (map col-name cols))]
    (some #(when (names %) [%]) [:key :id])))

(defn plan
  [{:keys [source table columns key dataset as project existing] bind :into}]
  (let [cols  (vec columns)
        names (set (map col-name cols))
        key   (some->> (seq key) (mapv #(keyword (str/lower-case (name %)))))
        key   (or key (default-key cols))
        ds    (keyword (name (or bind dataset (dataset-name table))))
        has?  (fn [section n]
                (contains? (set (map #(keyword (name %)) (get existing section)))
                           (keyword (name n))))]
    (cond
      (empty? cols)
      {:problem :no-columns :message (str "the source names no columns for table " (pr-str table))}

      (nil? key)
      {:problem :no-key
       :message (str "name the key that carries the whole grain of " table " (--key col[,col]); "
                     "its columns: " (str/join ", " (map name (sort names))))}

      (not-every? names key)
      {:problem :unknown-key :key key
       :message (str "the key names columns " table " does not have: "
                     (str/join ", " (map name (remove names key))))}

      (and bind (not (has? :datasets bind)))
      {:problem :unknown-dataset
       :message (str "no dataset " (name bind) " to bind; leave out --into to create one")}

      (and (not bind) (has? :datasets ds))
      {:problem :dataset-exists
       :message (str "a dataset " (name ds) " exists already — bind it with --into " (name ds)
                     " or name another with --dataset")}

      (and as (has? :objects as))
      {:problem :type-exists
       :message (str "an object type " (name as) " exists already: adopting would shadow it. "
                     "Back it by this dataset, or add its fields by a supplement transform")}

      (and as (< 1 (count key)))
      {:problem :composite-key
       :message "an object type needs a single-column key; adopt the dataset alone, then shape a key in a transform"}

      :else
      (let [columns (mapv (fn [c]
                            {:attribute-name (col-name c)
                             :attribute-type (or (:ontology-type c) :string)}) cols)
            dset    (cond-> {:kind    :virtual
                             :virtual {:source (keyword (name source)) :table (str table)
                                       :primary-key key}
                             :columns columns}
                      (and project (not bind)) (assoc :project (keyword (name project))))
            otype   (when as
                      (cond-> {:match-keys [(first key)]
                               :attributes (mapv (juxt :attribute-name :attribute-type) columns)
                               :backing    {:dataset ds :primary-key (first key)}}
                        project (assoc :project (keyword (name project)))))]
        (cond-> {:schema  {:datasets {ds dset}}
                 :dataset ds
                 :key     key}
          as (-> (assoc-in [:schema :objects (keyword (name as))] otype)
                 (assoc :type (keyword (name as)))))))))
