(ns sutra.world.delta
  (:require [sutra.ref.err :refer [fail!]]))

(def absent :sutra.kern.delta/absent)

(def empty-entry :sutra.kern.delta/empty)

(defn field [e f]
  (if (= empty-entry f)
    (if (and (map? e) (zero? (count e))) true absent)
    (get e f absent)))

(defn entry-delta
  [node old new]
  (let [[_ _ a b :as ec] [node empty-entry (field old empty-entry) (field new empty-entry)]
        fs (into []
                 (keep (fn [f]
                         (let [a (field old f) b (field new f)]
                           (when (not= a b) [node f a b]))))
                 (distinct (concat (keys old) (keys new))))]
    (cond (= a b)     fs
          (= true a)  (into [ec] fs)
          :else       (conj fs ec))))

(defn nodes [changes] (into #{} (map first) changes))

(defn frame-node [s] [:frame s])

(defn frame-node? [node] (= :frame (first node)))

(defn frame-value
  [W s]
  (if (contains? W s) (let [v (get W s)] (if (map? v) {} v)) absent))

(defn frame-delta
  [W0 W1]
  (into []
        (keep (fn [s]
                (let [a (frame-value W0 s) b (frame-value W1 s)]
                  (when (not= a b) [(frame-node s) :value a b]))))
        (distinct (concat (keys W0) (keys W1)))))

(defn around
  [fcs cs]
  (let [opens? (fn [[_ _ _ b]] (= {} b))]
    (-> (filterv opens? fcs) (into cs) (into (remove opens? fcs)))))

(defn completed
  [W changes]
  (let [opened (into #{} (keep (fn [[n _ _ b]] (when (and (frame-node? n) (= {} b))
                                                 (second n))))
                     changes)
        opens  (into [] (comp (remove (comp frame-node? first))
                              (keep (fn [[[sec] _ _ b]]
                                      (when (and (not= absent b) (not (contains? W sec))
                                                 (not (contains? opened sec)))
                                        sec)))
                              (distinct)
                              (map (fn [sec] [(frame-node sec) :value absent {}])))
                     changes)]
    (if (seq opens) (into opens changes) changes)))

(defn- put-field [e f v]
  (cond (= empty-entry f)   (when (= true v) {})
        (= absent v)  (dissoc e f)
        :else         (assoc (or e {}) f v)))

(defn- put-entry [W [sec k] e f]
  (cond (or (seq e) (and (= empty-entry f) (some? e)))  (assoc-in W [sec k] e)
        (contains? W sec)                         (update W sec dissoc k)
        :else                                     W))

(defn- put-frame [W s v]
  (cond (and (not (map? v)) (seq (let [x (get W s)] (when (map? x) x))))
        (fail! :stale-change "the frame closes a section that still holds entries"
               {:section s :holds (count (get W s))})
        (= absent v)        (dissoc W s)
        (map? v)            (if (map? (get W s)) W (assoc W s {}))
        :else               (assoc W s v)))

(defn held
  [W [_ k :as node] f]
  (if (frame-node? node) (frame-value W k) (field (get-in W node) f)))

(defn apply-to
  [W changes]
  (reduce (fn [W [node f old new :as c]]
            (let [held (held W node f)]
              (when (not= old held)
                (fail! :stale-change "the change was made against another value"
                       {:change c :holds held}))
              (if (frame-node? node)
                (put-frame W (second node) new)
                (put-entry W node (put-field (get-in W node) f new) f))))
          W changes))

(defn invert [changes] (into [] (map (fn [[n f a b]] [n f b a])) (rseq (vec changes))))

(defn squash
  [changes]
  (let [[order m] (reduce (fn [[order m] [n f a b]]
                            (let [el [n f]]
                              (if-let [[_ _ a0] (get m el)]
                                [order (assoc m el [n f a0 b])]
                                [(conj order el) (assoc m el [n f a b])])))
                          [[] {}] changes)]
    (into [] (comp (map m) (remove (fn [[_ _ a b]] (= a b)))) order)))

(defn merge3
  ([ours theirs] (merge3 ours theirs nil))
  ([ours theirs prefer]
   (let [mine (into {} (map (fn [[n f :as c]] [[n f] c])) (squash ours))]
     (reduce (fn [acc [n f a b :as c]]
               (if-let [[_ _ _ b'] (get mine [n f])]
                 (cond
                   (= b b')            acc
                   (= :theirs prefer)  (update acc :changes conj [n f b' b])
                   :else               (update acc :conflicts conj
                                               {:node n :field f :base a :ours b' :theirs b}))
                 (update acc :changes conj c)))
             {:changes [] :conflicts []}
             (squash theirs)))))
