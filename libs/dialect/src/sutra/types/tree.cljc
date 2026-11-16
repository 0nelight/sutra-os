(ns sutra.types.tree
  (:require [sutra.types.rid :as rid]))

(def whole
  :sutra.parser.checkout/whole)

(def ^:private listed ::listed)

(defn- head [m e] (if (map? m) (key e) (first e)))

(defn nodes
  [body]
  (for [[section m] body
        :when (coll? m)
        e m
        :let [k (head m e)]]
    [[section (rid/->name k)]
     {:key k :value (if (map? m) (val e) e) :vector? (not (map? m))}]))

(defn index
  [body]
  (into {} (nodes body)))

(defn body
  [entries]
  (let [put  (fn [m {:keys [key value vector? all]}]
               (if vector?
                 (into (or m []) (or all [value]))
                 (assoc (or m (sorted-map)) key value)))
        body (reduce (fn [b [[section] e]] (update b section put e)) {} entries)]
    (update-vals body #(if (vector? %) (vec (sort-by first %)) %))))

(defn select
  [body keep?]
  (into {}
        (for [[section m] body
              :when (coll? m)
              :let [kept (into (if (map? m) (empty m) [])
                               (filter #(keep? [section (rid/->name (head m %))]))
                               m)]
              :when (seq kept)]
          [section kept])))

(defn changes
  [before after]
  (let [b (index before)
        a (index after)]
    {:added   (into {} (remove (fn [[n]] (contains? b n))) a)
     :removed (into {} (remove (fn [[n]] (contains? a n))) b)
     :changed (into {}
                    (filter (fn [[n e]] (and (contains? b n) (not= (:value (b n)) (:value e)))))
                    a)}))

(def element-keys
  [:attribute-name :parameter-name :name :id])

(defn element-key
  [xs]
  (when (and (sequential? xs) (seq xs) (every? map? xs))
    (some (fn [k] (when (every? #(some? (get % k)) xs) k)) element-keys)))

(defn named
  [xs]
  (when (sequential? xs)
    (let [k  (element-key xs)
          nm (fn [e] (cond k               (rid/->name (get e k))
                           (map? e)        nil
                           (sequential? e) (some-> (first e) rid/->name)
                           :else           (rid/->name e)))
          ns (mapv nm xs)]
      (when (and (not-any? nil? ns) (= (count ns) (count (distinct ns))))
        (mapv vector ns xs)))))

(def cuts
  {:by-name named})

(defn declared
  [elements]
  (fn [section field v]
    (when-let [c (some-> (get-in elements [section field]) cuts)]
      (c v))))

(defn lists-of-maps
  [_ _ v]
  (when (and (sequential? v) (every? map? v))
    (named v)))

(defn elements
  [cut section v]
  (if (map? v)
    (into {}
          (mapcat (fn [[f x]]
                    (if-let [parts (cut section f x)]
                      (cons [[f] listed] (for [[n p] parts] [[f n] p]))
                      [[f x]])))
          v)
    {whole v}))

(defn label
  [e]
  (cond
    (or (nil? e) (= whole e)) nil
    (vector? e)               (cond-> (rid/->name (first e))
                                (second e) (str "/" (second e)))
    :else                     (rid/->name e)))

(defn- field-of [e] (if (vector? e) (first e) e))

(defn- tokens-of
  [token fold section els]
  (if fold
    (into {}
          (keep (fn [[e v]]
                  (if (or (vector? e) (= whole e))
                    [e (token v)]
                    (let [v (fold section e v)] (when (some? v) [e (token v)])))))
          els)
    (update-vals els token)))

(defn base
  ([body cut token] (base body cut token nil))
  ([body cut token fold]
   (into {}
         (for [[[section :as n] {:keys [value]}] (nodes body)]
           [n (tokens-of token fold section (elements cut section value))]))))

(defn- side
  [cut token fold section entry]
  (when entry
    (let [els (delay (elements cut section (:value entry)))]
      (assoc entry :elements els :tokens (delay (tokens-of token fold section @els))))))

(defn- same?
  [x y]
  (or (and (contains? x :value) (contains? y :value) (= (:value x) (:value y)))
      (= (some-> x :tokens force) (some-> y :tokens force))))

(defn- merge-field
  [{:keys [cut section first-side]} b m t r f es]
  (let [bt     (or (some-> b :tokens force) {})
        mt     @(:tokens m)
        tt     @(:tokens t)
        from   #(if (= :mine %) @(:elements m) @(:elements t))
        decide (fn [e]
                 (let [bf (get bt e) mf (get mt e) tf (get tt e)]
                   (cond (= mf bf) :theirs (= tf bf) :mine (= mf tf) :mine :else r)))
        sides  (map (juxt identity decide) es)
        open   (sort-by pr-str (keep (fn [[e s]] (when-not s e)) sides))
        pick   (fn [sides]
                 (into {} (keep (fn [[e s]] (some-> (find (from s) e) vec))) sides))
        sound? (fn [picked]
                 (if (contains? picked [f])
                   (not (contains? picked f))
                   (not-any? #(and (vector? %) (second %)) (keys picked))))
        picked (pick sides)
        picked (cond (seq open)      nil
                     (sound? picked) picked
                     r               (pick (for [e (keys (from r)) :when (= f (field-of e))]
                                             [e r])))]
    (cond
      (seq open)      {:conflicts open}
      (nil? picked)   {:conflicts [f]}
      (contains? picked f) {:value {f (get picked f)}}
      (contains? picked [f])
      (let [parts (into {} (keep (fn [[e v]] (when (second e) [(second e) v])))
                        (dissoc picked [f]))
            [fv ov] (if (= :theirs first-side) [(:value t) (:value m)] [(:value m) (:value t)])
            order (distinct (concat (map first (cut section f (get fv f)))
                                    (map first (cut section f (get ov f)))))]
        {:value {f (vec (keep parts order))}})
      :else           {:value {}})))

(defn- merge-node
  [{:keys [first-side] :as ctx} b m t r]
  (cond
    (same? m b) [t []]
    (same? t b) [m []]
    (same? m t) [(if (= :theirs first-side) t m) []]

    (and m t (map? (:value m)) (map? (:value t)))
    (let [bt (or (some-> b :tokens force) {})
          rs (for [[f es] (group-by field-of (distinct (concat (keys bt) (keys @(:tokens m))
                                                               (keys @(:tokens t)))))]
               (merge-field ctx b m t r f es))
          cs (vec (mapcat :conflicts rs))]
      (if (seq cs)
        [t cs]
        [(assoc (if (= :theirs first-side) t m) :value (into {} (map :value) rs)) []]))

    (= :mine r)   [m []]
    (= :theirs r) [t []]
    :else         [t [(when (and m t) whole)]]))

(defn- assemble
  [first-body other-body result]
  (let [order    (distinct (concat (map first (nodes first-body)) (map first (nodes other-body))))
        shell    (into {} (for [s (distinct (concat (keys first-body) (keys other-body)))
                                :let [m (get first-body s (get other-body s))]
                                :when (coll? m)]
                            [s (if (map? m) {} [])]))
        scalars  (into {} (remove (comp coll? val)) first-body)]
    (reduce (fn [acc [section :as n]]
              (if-let [{:keys [key value vector?]} (get result n)]
                (if vector?
                  (update acc section conj value)
                  (update acc section assoc key value))
                acc))
            (merge scalars shell)
            order)))

(defn- merge-sides
  [bases mine theirs {:keys [cut token fold resolve] first-side :first :or {first-side :mine}}]
  (let [cut     (or cut (constantly nil))
        token   (or token identity)
        resolve (cond (nil? resolve) (constantly nil)
                      (keyword? resolve) (constantly resolve)
                      :else resolve)
        mi      (index mine)
        ti      (index theirs)
        ctx     {:cut cut :first-side first-side}
        {:keys [result conflicts]}
        (reduce (fn [acc [section :as n]]
                  (let [m      (get mi n)
                        t      (get ti n)
                        [e cs] (if (= m t)
                                 [(side cut token fold section t) []]
                                 (merge-node (assoc ctx :section section)
                                             (get bases n)
                                             (side cut token fold section m)
                                             (side cut token fold section t)
                                             (#{:mine :theirs} (resolve n))))]
                    (cond-> (if e (assoc-in acc [:result n] e) acc)
                      (seq cs) (update :conflicts conj [n cs]))))
                {:result {} :conflicts []}
                (distinct (concat (keys bases) (keys mi) (keys ti))))
        [fb ob] (if (= :theirs first-side) [theirs mine] [mine theirs])]
    {:body      (assemble fb ob result)
     :conflicts (vec (for [[n cs] (sort-by (comp pr-str first) conflicts)
                           f      cs]
                       {:node n :field f}))}))

(defn merge3
  [base mine theirs {:keys [cut token fold] :as opts}]
  (let [cut   (or cut (constantly nil))
        token (or token identity)]
    (merge-sides (into {} (for [[[section :as n] e] (nodes base)]
                            [n (side cut token fold section e)]))
                 mine theirs opts)))

(defn merge3-over
  [tokens mine theirs opts]
  (merge-sides (update-vals tokens (fn [t] {:tokens t})) mine theirs opts))
