(ns sutra.catalog.signature
  (:require [sutra.catalog.outputs :as outputs]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.types.nested :as nested]
            [sutra.world.view :as view]
            [sutra.types.naming :as naming]
            [sutra.types.kinds :as kinds]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(def ^:private family-columns
  {:link    (fn [_ _]
              [{:attribute-name :from-id :attribute-type :string}
               {:attribute-name :to-id :attribute-type :string}])
   :object  (fn [W out]
              (or (seq (get-in W [:object-types (:object-type out) :attribute-list]))
                  (vec (for [[a _] (:column-map out)]
                         {:attribute-name a :attribute-type :string}))))
   :dataset (fn [W out] (get-in W [:datasets (:dataset out) :columns]))})

(defn- output-columns [W out]
  (when-let [f (family-columns (kinds/ddl-family (:kind out)))] (f W out)))

(defn- output-decls
  [W t-kw t]
  (for [[ds cols] (if (:output-columns t)
                    [[(rid/dataset-rid-for t-kw) (:output-columns t)]]
                    (for [out (outputs/outputs-of t)]
                      [(or (:dataset out) (rid/dataset-rid-for t-kw)) (output-columns W out)]))
        :when (some? cols)]
    [ds (vec cols)]))

(def ^:private system (set (keys sysds/system-datasets)))

(defn- user-decl
  [ds spec]
  (when (and (not (contains? system (str ds)))
             (not (#{:object-backing :virtual} (keyword (:kind spec)))))
    [ds (vec (:columns spec))]))

(defn datasets-of
  [W]
  (let [object-locators (into #{} (map rid/rid-locator) (keys (:object-types W)))
        own?    (fn [ds] (contains? object-locators (rid/rid-locator ds)))
        outputs (for [[t-kw t] (sort-by (comp str key) (:transforms W))
                      d (output-decls W t-kw t)]
                  d)
        user    (keep (fn [[ds spec]] (user-decl ds spec)) (sort-by (comp str key) (:datasets W)))]
    (into {}
          (comp (remove (comp own? first)) (map (fn [[ds cols]] [(naming/dataset-table ds) cols])))
          (concat user outputs))))

(def ^:private filed-of
  {:transforms   (fn [W k e]
                   (concat (for [[ds _] (output-decls W k e)
                                 :let [t (naming/dataset-table ds)]
                                 f [[[:by-table t] [:t k]]
                                    [[:loc-tables (rid/rid-locator ds)] [t [:t k]]]]]
                             f)
                           (for [out (outputs/outputs-of e)
                                 f (cond-> []
                                     (:object-type out) (conj [[:by-type (:object-type out)] k])
                                     (:dataset out)     (conj [[:by-dataset (:dataset out)] k]))]
                             f)))
   :datasets     (fn [_ k e]
                   (when (user-decl k e)
                     (let [t (naming/dataset-table k)]
                       [[[:by-table t] [:d k]]
                        [[:loc-tables (rid/rid-locator k)] [t [:d k]]]])))
   :object-types (fn [_ k _] [[[:ot-locs (rid/rid-locator k)] k]])})

(def ^:private read-sections (keys filed-of))

(def ^:private readers-in
  {:object-types :by-type :datasets :by-dataset})

(defn- filed [W sec k e] ((filed-of sec) W k e))

(defn- filed-at [W [s k]] (when-some [e (get-in W [s k])] (filed W s k e)))

(defn- aux-of [W]
  (nested/refile-in {} nil (for [sec read-sections [k e] (get W sec) p (filed W sec k e)] p)))

(defn- table-decl
  [W aux table]
  (let [cs     (get-in aux [:by-table table])
        ds-of  (fn [[kind k]]
                 (if (= :d kind)
                   (some-> (user-decl k (get-in W [:datasets k])) vector)
                   (output-decls W k (get-in W [:transforms k]))))
        own?   (fn [[ds]] (seq (get-in aux [:ot-locs (rid/rid-locator ds)])))
        of     (fn [kind]
                 (mapcat ds-of (sort-by (comp str second) (filter (comp #{kind} first) cs))))
        decls  (filter #(and (= table (naming/dataset-table (first %))) (not (own? %)))
                       (concat (of :d) (of :t)))]
    (second (last decls))))

(defn decls-init
  [W]
  {:decls (datasets-of W) :aux (aux-of W)})

(defn decls-step
  [{:keys [decls aux]} W0 W nodes]
  (let [nodes  (distinct (filter (fn [[s]] (contains? filed-of s)) nodes))
        readers (for [[s k] nodes
                      :let [a (readers-in s)]
                      :when a
                      t (get-in aux [a k])]
                  [:transforms t])
        nodes  (distinct (concat nodes readers))
        aux1   (reduce (fn [a n] (nested/refile-in a (filed-at W0 n) (filed-at W n))) aux nodes)
        tables (into #{} (concat
                          (for [n nodes Wx [W0 W] [[a t]] (filed-at Wx n) :when (= :by-table a)]
                            t)
                          (for [[s k] nodes
                                :when (= :object-types s)
                                ax [aux aux1]
                                [t] (get-in ax [:loc-tables (rid/rid-locator k)])]
                            t)))]
    (with-meta
      {:aux   aux1
       :decls (view/recompute decls tables #(table-decl W aux1 %))}
      {:touched tables})))

(defn installing
  [opts world]
  (cond-> opts
    world (assoc :world world :steps [[:drop-unclaimed]]
                 :drop-unclaimed? true :dataset-decls datasets-of
                 :dataset-view {:init decls-init :step decls-step})))
