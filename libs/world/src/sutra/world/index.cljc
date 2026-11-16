(ns sutra.world.index
  (:require [clojure.string :as str]
            [sutra.types.entities :as ents]
            [sutra.types.nested :refer [conj-in disj-in dissoc-in refile-in seqable]]
            [sutra.types.refs :as refs]))

(defn nm
  [x]
  (cond (or (keyword? x) (symbol? x) (string? x)) (name x)
        (nil? x)                                  nil
        :else                                     (str x)))

(def ^:private rid-re #"ri\.sutra\.main\.([a-z-]+)\.([0-9A-Za-z_-]+)")

(defn loc
  [r]
  (some-> (re-matches rid-re (str r)) (nth 2)))

(defn kw [x] (cond (keyword? x) x (nil? x) nil (symbol? x) (keyword x) :else (keyword (str x))))

(def name-field (into {} (map (juxt identity ents/name-field)) ents/rid-sections))

(defn ref-values
  [sec e]
  (cond-> (refs/ref-values (get ents/refs sec) e)
    (= :actions sec) (into (refs/ref-values ents/action-refs e))))

(defn inputs-of [t]
  (cond (map? (:inputs t)) (vals (:inputs t))
        (seq (:inputs t))  (:inputs t)
        (:input t)         [(:input t)]))

(defn outputs-of [t] (or (seq (:outputs t)) (some-> (:output t) vector)))

(def ^:private edges
  {:object-types      (fn [e] (for [i (seqable (:implements e))] [:implements i]))
   :interfaces        (fn [e] (concat (for [p (seqable (:extends e))] [:extends p])
                                      (for [c (seqable (:link-constraints e)) :when (map? c)]
                                        [:constraint (:link-name c)])))
   :link-types        (fn [e]
                        (concat (for [end (distinct [(:from-object-type e)
                                                     (:to-object-type e)])]
                                  [:link-end end])
                                (for [s (seqable (:satisfies e))
                                      :when (map? s)
                                      f [[:satisfies (:interface s) (:constraint s)]
                                         [:satisfies-kw (:interface s) (kw (:constraint s))]]]
                                  f)))
   :transforms        (fn [e]
                        (concat (for [in (inputs-of e) :when (map? in)
                                      :let [k (:kind in)]]
                                  (if (= :language-model k)
                                    [:model (str (:model in))]
                                    [:input k (get in k)]))
                                (for [o (outputs-of e) :when (map? o)]
                                  [:output (:kind o) (get o (:kind o))])))
   :shared-properties (fn [e] (when-some [v (:value-type e)] [[:value-type (kw v)]]))})

(def ^:private table-kinds
  {:transforms :bulk :datasets :bulk :object-types :ot :link-types :link})

(defn table-kind [sec] (get table-kinds sec))

(defn table-name
  [k]
  (some-> (loc k) (str/replace "-" "_")))

(def rows
  [{:id    :by-name
    :facts (fn [sec _k e] (when-let [nf (name-field sec)]
                         (when-some [n (get e nf)] [[sec n]])))}
   {:id    :by-name-str
    :facts (fn [sec _k e] (when-let [nf (name-field sec)]
                         (when-some [n (get e nf)] [[sec (nm n)]])))}
   {:id    :refs-in
    :facts (fn [sec _k e] (keep (fn [[_ v]] (when (some? v) v)) (ref-values sec e)))}
   {:id    :members
    :facts (fn [sec _k e]
             (cond-> []
               (some? (:project e))                       (conj [:project (nm (:project e))])
               (and (= :projects sec) (some? (:space e))) (conj [:space (nm (:space e))])))}
   {:id    :edges
    :facts (fn [sec _k e] (when-let [f (edges sec)] (f e)))}
   {:id    :instance
    :facts (fn [sec _k e] (cond (ents/instance-section? sec)  [[:held]]
                                (ents/instance-entry? sec e) (cons [:held]
                                                                   (ents/instance-via sec e))))}
   {:id    :tables
    :facts (fn [sec k _e] (when-let [kind (table-kinds sec)]
                            (when-let [t (table-name k)] [[kind t]])))}])

(defn- facts [row [sec k] e] (when (map? e) (distinct ((:facts row) sec k e))))

(defn changed
  [a b]
  (let [a (set a) b (set b)]
    (into (into #{} (remove b) a) (remove a) b)))

(defn retract
  [idx x fs]
  (reduce (fn [idx f] (disj-in idx [f] x)) idx fs))

(defn add
  [idx x fs]
  (reduce (fn [idx f] (conj-in idx [f] x)) idx fs))

(defn refile
  [idx x fs0 fs1]
  (if (= fs0 fs1) idx (-> idx (retract x fs0) (add x fs1))))

(defn- filed [by x r] (for [[p f] by fact (f r)] [(conj p fact) x]))

(defn refile-record
  [m at by x r1]
  (let [path (conj at x)]
    (cond-> (refile-in m (filed by x (get-in m path)) (filed by x r1))
      (some? r1) (assoc-in path r1)
      (nil? r1)  (dissoc-in path))))

(defn- init-row [row W]
  (reduce-kv (fn [idx sec es]
               (if (map? es)
                 (reduce-kv (fn [idx k e] (add idx [sec k] (facts row [sec k] e))) idx es)
                 idx))
             {} W))

(defn- step-row [row idx W0 W1 nodes]
  (reduce (fn [idx node]
            (refile idx node (facts row node (get-in W0 node)) (facts row node (get-in W1 node))))
          idx nodes))

(defn moved
  [W0 W1 nodes]
  (into #{} (for [r rows node nodes
                  f (changed (facts r node (get-in W0 node)) (facts r node (get-in W1 node)))]
              [(:id r) f])))

(defn init
  [W]
  (into {} (map (fn [r] [(:id r) (init-row r W)])) rows))

(defn step
  [idx W0 W1 nodes]
  (into {} (map (fn [r] [(:id r) (step-row r (get idx (:id r)) W0 W1 nodes)])) rows))
