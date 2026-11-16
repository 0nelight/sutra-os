(ns sutra.types.set-query
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]))

(def page-keys [:limit :order-by :sort-dir])

(defn- refused [msg] {::problem msg})

(defn problem [x] (::problem x))

(defn- shaped? [q] (some #(contains? q %) page-keys))

(declare set-of)

(defn- what-it-is
  [specs nm advice]
  (let [spec (get specs nm)]
    (cond
      (:function spec) (str "the result of function " (name (->kw (:function spec)))
                            " — rows it computes, not objects the store holds; " advice)
      (:join spec)     "a :join — filter its sides before it"
      (:union spec)    "a :union — filter its parts before it"
      (:pluck spec)    "a :pluck — filter the set it plucks from before it"
      :else            "no object set the store answers — filter before it")))

(defn- base-of
  [specs nm seen]
  (cond
    (seen nm)                       (refused (str (name nm) " reads itself"))
    (not (contains? specs nm))      {:root nm :clauses []}
    :else                           (set-of specs nm seen)))

(defn set-of
  ([specs nm] (set-of specs nm #{}))
  ([specs nm seen]
   (let [nm   (->kw nm)
         spec (get specs nm)]
     (cond
       (nil? spec) {:root nm :clauses []}

       (or (:join spec) (:union spec) (:pluck spec) (:agg spec) (:default-agg spec)
           (:object-property spec) (:expr spec) (:coalesce spec)
           (not (or (:source spec) (:from spec) (:linked spec))))
       nil

       :else
       (let [from (some-> (or (:source spec) (:linked spec) (:from spec)) ->kw)
             base (cond
                    (:source spec) {:root from :clauses [] :source? true}
                    (:linked spec) {:root from :clauses []}
                    :else          (base-of specs from (conj seen nm)))]
         (cond
           (problem base) base

           (nil? base)
           (refused (str (name nm) " filters " (name from) ", which is "
                         (what-it-is specs from
                                     "pass the filter to the function as an argument")))

           (and (shaped? base) (seq (:where spec)))
           (refused (str (name nm) " filters " (name from) " after its :limit/:order-by —"
                         " a page is not filtered again; filter first, then page"))

           :else
           (cond-> (update base :clauses into (:where spec))
             (shaped? spec) (merge (select-keys spec page-keys)))))))))

(defn agg-of
  [specs nm]
  (let [spec (get specs (->kw nm))
        [f prop of] (or (some-> (:default-agg spec) vec)
                        (when (:agg spec) [(first (:agg spec)) (second (:agg spec))
                                           (:of spec)]))]
    (when f
      (let [base (set-of specs (->kw of))]
        (cond
          (problem base) base
          (nil? base)
          (refused (str (name nm) " aggregates " (name of) ", which is "
                        (what-it-is specs (->kw of)
                                    "let the function compute the figure itself")
                        " (the store aggregates, the browser does not)"))
          (shaped? base)
          (refused (str (name nm) " aggregates the page of " (name of) " (:limit/:order-by)"
                        " — aggregate the set, not a page of it"))
          :else
          (-> base
              (update :clauses into (:where spec))
              (assoc :agg [(->kw f) (some-> prop ->kw)])))))))

(defn query-var?
  [specs nm]
  (let [q (or (agg-of specs nm) (set-of specs nm))]
    (and (some? q) (not (problem q)) (contains? specs (->kw nm)))))

(defn vars-problem
  [specs]
  (some (fn [nm] (some-> (or (agg-of specs nm) (set-of specs nm)) problem))
        (sort (keys specs))))

(defn- ref-name
  [x]
  (when (and (map? x) (or (contains? x :val) (contains? x :var)))
    (->kw (or (:val x) (:var x)))))

(defn holes
  [{:keys [clauses]}]
  (into #{}
        (mapcat (fn [[_ op ref]]
                  (if-let [nm (ref-name ref)]
                    [nm]
                    (when (= :filter (->kw op))
                      (keep ref-name (tree-seq coll? #(if (map? %) (vals %) (seq %)) ref))))))
        clauses))

(defn parts
  [specs nm]
  (if-let [u (:union (get specs (->kw nm)))]
    (into [] (keep #(some-> (or (:val %) (:from %)) ->kw)) u)
    [(->kw nm)]))

(def ^:private sorting-controls #{:select-input :combo-input})

(defn- control-order
  [{:keys [item-value sort] sort-prop :sort-by}]
  (let [p (or sort-prop (when sort item-value))]
    (when (or (keyword? p) (string? p))
      {:order-by (->kw p) :sort-dir (if (= :desc (some-> sort ->kw)) :desc :asc)})))

(defn items-orders
  [specs tree]
  (into {}
        (for [n     (tree/nodes tree)
              :when (sorting-controls (some-> (:widget n) ->kw))
              :let  [order (control-order n)
                     nm    (when (map? (:items n)) (:val (:items n)))]
              :when (and order nm)
              part  (parts specs nm)]
          [part order])))

(defn with-items-order
  [order q]
  (cond-> q
    (and order (not (problem q)) (not (:agg q)) (not (shaped? q))) (assoc :items-order order)))

(defn layout-specs [layout]
  (reduce (fn [acc n] (merge acc (:vars n))) {} (tree/nodes layout)))

(defn queries [specs]
  (for [nm (keys specs)
        :let [q (or (agg-of specs nm) (set-of specs nm))]
        :when (and q (not (problem q)))]
    q))
