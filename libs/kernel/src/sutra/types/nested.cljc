(ns sutra.types.nested)

(defn seqable
  [x]
  (cond (nil? x) [] (sequential? x) x :else [x]))

(defn dissoc-in
  [m [k & ks]]
  (if-not ks
    (dissoc m k)
    (let [v (dissoc-in (get m k) ks)]
      (if (empty? v) (dissoc m k) (assoc m k v)))))

(defn conj-in
  [m path x]
  (update-in m path (fnil conj #{}) x))

(defn disj-in
  [m path x]
  (let [s (disj (get-in m path) x)]
    (if (empty? s) (dissoc-in m path) (assoc-in m path s))))

(defn refile-in
  [m ps0 ps1]
  (let [s0 (set ps0) s1 (set ps1)]
    (as-> m m
      (reduce (fn [m [p x]] (if (contains? s1 [p x]) m (disj-in m p x))) m s0)
      (reduce (fn [m [p x]] (if (contains? s0 [p x]) m (conj-in m p x))) m s1))))
