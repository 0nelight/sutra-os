(ns sutra.store.plan
  (:require [sutra.ref.objectset :as ros]
            [sutra.types.aggregate :as agg]))

(set! *warn-on-reflection* true)

(defn fk-join
  [sig L]
  (when-let [{:keys [holder target] :as fk} (get-in sig [:links L :fk])]
    {:op :fk-join :link L :holder holder :target target :fk fk}))

(defn- children [{:keys [in]}]
  (cond (map? in) [in] (sequential? in) (vec in) :else []))

(defn- rewrite [f n]
  (f (cond-> n
       (map? (:in n))        (update :in (partial rewrite f))
       (sequential? (:in n)) (update :in #(mapv (partial rewrite f) %)))))

(defn fk-joins
  [sig plan]
  (let [edges (fn [m] (if-let [j (and (:link m) (fk-join sig (:link m)))] (assoc m :edges j) m))]
    (rewrite (fn [{:keys [op] :as n}]
               (case op
                 :search-around (edges n)
                 :with          (cond-> (edges n)
                                  (:far-in n) (update :far-in #(fk-joins sig %)))
                 :filter        (cond-> n (seq (:include n)) (update :include (partial mapv edges)))
                 :aggregate     (cond-> n (seq (:sides n)) (update :sides update-vals edges))
                 n))
             plan)))

(defn columns
  [plan]
  (loop [n plan acc {}]
    (case (:op n)
      (:with :compute) (recur (:in n)
                              (cond-> acc (not (contains? acc (:name n))) (assoc (:name n) n)))
      :filter          (recur (:in n) acc)
      acc)))

(defn column-order
  [plan]
  (let [named (columns plan)]
    (loop [n plan seen #{} acc ()]
      (case (:op n)
        (:with :compute) (let [nm (:name n)]
                           (recur (:in n) (conj seen nm) (cond-> acc (not (seen nm)) (conj nm))))
        :filter          (recur (:in n) seen acc)
        (filterv named acc)))))

(defn merge-chains
  [chains]
  (let [leaf (last (take-while :op (iterate :in (first chains))))]
    (reduce (fn [acc chain]
              (let [named (columns chain)]
                (reduce (fn [acc nm]
                          (if (contains? (columns acc) nm)
                            acc
                            (assoc (named nm) :in acc)))
                        acc
                        (column-order chain))))
            leaf
            chains)))

(defn- pred-ops [p]
  (when (vector? p)
    (let [[op] p]
      (if (#{:and :or :not} op) (cons op (mapcat pred-ops (rest p))) [op]))))

(defn- attr-type [sig T a]
  (some #(when (= a (:name %)) (:type %))
        (or (get-in sig [:types T :attrs]) (get-in sig [:interfaces T :props]))))

(defn- metric-ok? [aggs sig T {f :fn a :attr}]
  (let [allowed (get aggs f)]
    (or (= :all allowed)
        (and (set? allowed) (or (nil? a) (contains? allowed (attr-type sig T a)))))))

(defn- metrics-ok? [{:keys [aggs]} sig {T :type :as n}]
  (every? #(metric-ok? aggs sig T %) (:aggs (agg/normalize n))))

(def ^:private default-folds
  #{:attribute})

(declare claimable-node?)

(defn- with-ok?
  [{:keys [aggs folds] :as p} sig {:keys [far attr expr far-in] f :fn}]
  (let [folds (or folds default-folds)
        cols  (if far-in (set (keys (columns far-in))) #{})]
    (and (contains? folds (if expr :expr :attribute))
         (or (nil? far-in)
             (and (contains? folds :nested)
                  (every? #(claimable-node? p sig %)
                          (take-while :op (iterate :in far-in)))))
         (let [allowed (get aggs f)]
           (or (= :all allowed)
               (and (set? allowed)
                    (every? #(or (cols %) (contains? allowed (attr-type sig far %)))
                            (if expr (ros/compute-refs expr) (remove nil? [attr])))))))))

(defn claimable-node?
  [{:keys [ops preds] :as p} sig {:keys [op where include sides] :as n}]
  (and (contains? ops op)
       (every? #(= :exact (get preds %)) (remove #{:and :or :not} (pred-ops where)))
       (or (not-any? :edges (concat [n] include (vals sides))) (contains? ops :fk-join))
       (or (not= :aggregate op) (metrics-ok? p sig n))
       (or (not= :with op) (with-ok? p sig n))))

(defn claims
  [p sig plan]
  (letfn [(whole? [n] (and (claimable-node? p sig n) (every? whole? (children n))))
          (shaping? [n] (#{:order :page :project :aggregate} (:op n)))
          (holds-with? [n] (or (#{:with :compute} (:op n)) (some holds-with? (children n))))
          (below [path n]
            (if (and (whole? n) (not (shaping? n)) (not= :static (:op n)) (not (holds-with? n)))
              [path]
              (into [] (mapcat (fn [[i c]] (below (conj path i) c)))
                    (map-indexed vector (children n)))))]
    (cond (empty? (:ops p)) []
          (whole? plan)     [[]]
          :else             (below [] plan))))

(defn- at [plan path]
  (reduce (fn [n i] (let [in (:in n)] (if (map? in) in (nth in i)))) plan path))

(defn- put [plan path x]
  (if (empty? path)
    x
    (let [[i & more] path
          in (:in plan)]
      (if (map? in)
        (assoc plan :in (put in more x))
        (assoc plan :in (assoc (vec in) i (put (nth in i) more x)))))))

(defn split
  ([plan claims denied] (split plan claims denied nil))
  ([plan claims denied twin]
   (reduce (fn [{:keys [residual] :as acc} [k path]]
             (let [sub (at plan path)]
               (-> acc
                   (update :fragments conj (cond-> {:ref k :plan sub :denied denied}
                                             twin (assoc :ir (at twin path))))
                   (assoc :residual (put residual path {:op :claimed :ref k :type (:type sub)}))
                   (update :twin #(when % (put % path {:op :claimed :ref k :type (:type sub)}))))))
           {:fragments [] :residual plan :twin twin}
           (map-indexed vector claims))))

(defn assemble
  [residual answers]
  (rewrite (fn [n]
             (if (= :claimed (:op n))
               {:op :static :ids (mapv (comp str :id) (get answers (:ref n))) :root (:type n)}
               n))
           residual))

(defn whole?
  [claims]
  (= [[]] claims))

(defn ordered?
  [plan]
  (contains? #{:order :page :aggregate} (:op plan)))

(defn eligible?
  [p {:keys [commit? binding?]}]
  (case (or (:consistency p) :transactional)
    :transactional true
    :snapshot      (and (not commit?) (boolean binding?))))

(defn executor-for
  [candidates q can?]
  (->> candidates
       (filter (fn [[_ p]] (and (eligible? p q) (can? p))))
       (sort-by (fn [[_ p]] (get-in p [:latency :startup-ms] 0)))
       first))

(defn snapshot-binding
  [{:keys [rev root-rev]} storage-snapshot]
  (cond-> {:rev rev :storage-snapshot storage-snapshot} root-rev (assoc :root-rev root-rev)))
