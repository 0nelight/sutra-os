(ns sutra.types.refs)

(def rule-keys
  [:rules :then :else :steps])

(defn matches?
  [{:keys [eq has only-keys]} x]
  (and (map? x)
       (every? (fn [[k vs]] (contains? vs (get x k))) eq)
       (or (nil? has) (let [v (get x has)] (if (coll? v) (boolean (seq v)) (some? v))))
       (or (nil? only-keys) (every? only-keys (keys x)))))

(defn- map-acc
  [f acc xs]
  (reduce (fn [[acc out] x] (let [[acc x] (f acc x)] [acc (conj out x)])) [acc []] xs))

(defn walk-steps
  [rebuild? g acc x]
  (cond
    (map? x)
    (let [[acc s] (g acc x)]
      (reduce (fn [[acc s] k]
                (let [v (get s k)]
                  (if (or (map? v) (sequential? v))
                    (let [[acc v] (walk-steps rebuild? g acc v)]
                      [acc (if rebuild? (assoc s k v) s)])
                    [acc s])))
              [acc s]
              rule-keys))

    (sequential? x)
    (let [[acc xs] (map-acc #(walk-steps rebuild? g %1 %2) acc x)]
      [acc (if rebuild? xs x)])

    :else [acc x]))

(defn steps
  [x]
  (first (walk-steps false (fn [acc s] [(conj acc s) s]) [] x)))

(defn update-steps
  [f x]
  (second (walk-steps true (fn [acc s] [acc (f s)]) nil x)))

(declare walk-all)

(defn- walk
  [rebuild? f acc m {:keys [at many each each-val each-key steps targets] :as slot}]
  (let [at  (or at [])
        c   (when (or (map? m) (sequential? m)) (if (seq at) (get-in m at) m))
        put (fn [[acc c']] [acc (cond (not rebuild?) m
                                      (seq at)       (assoc-in m at c')
                                      :else          c')])]
    (cond
      (and (:if slot) (not (matches? (:if slot) m))) [acc m]

      steps    (if (or (map? c) (sequential? c))
                 (put (walk-steps rebuild? #(walk-all rebuild? f %1 %2 steps) acc c))
                 [acc m])
      each     (cond (sequential? c) (put (map-acc #(walk-all rebuild? f %1 %2 each) acc c))
                     (map? c)        (put (walk-all rebuild? f acc c each))
                     :else           [acc m])
      each-val (if (map? c)
                 (put (reduce (fn [[acc out] [k x]]
                                (let [[acc x] (walk-all rebuild? f acc x each-val)]
                                  [acc (assoc out k x)]))
                              [acc (empty c)] c))
                 [acc m])
      each-key (if (map? c)
                 (put (reduce (fn [[acc out] [k x]]
                                (if (or (nil? (:if each-key)) (matches? (:if each-key) x))
                                  (let [[acc k] (f acc (:targets each-key) k)]
                                    [acc (assoc out k x)])
                                  [acc (assoc out k x)]))
                              [acc (empty c)] c))
                 [acc m])
      many     (if (sequential? c)
                 (put (map-acc #(f %1 targets %2) acc c))
                 [acc m])
      :else    (if (some? c)
                 (put (f acc targets c))
                 [acc m]))))

(defn- walk-all
  [rebuild? f acc m spec]
  (reduce (fn [[acc m] r] (walk rebuild? f acc m r)) [acc m] spec))

(defn fold-refs
  [spec m rf init]
  (first (walk-all false (fn [acc t v] [(rf acc t v) v]) init m spec)))

(defn ref-values
  [spec m]
  (fold-refs spec m (fn [acc t v] (conj acc [t v])) []))

(defn rewrite-refs
  [spec m resolve]
  (second (walk-all true (fn [acc t v] [acc (resolve t v)]) nil m spec)))

(defn rename
  [spec m section names]
  (if (empty? names)
    m
    (rewrite-refs spec m
                  (fn [targets v]
                    (if-let [to (when (and (contains? targets section)
                                           (or (keyword? v) (string? v)))
                                  (get names (name v)))]
                      (if (string? v) (name to) (keyword (name to)))
                      v)))))
