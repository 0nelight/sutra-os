(ns sutra.types.table)

(defn rows
  [table pred]
  (into {} (filter (fn [[_ row]] (pred row))) table))

(defn where
  [table pred]
  (into #{} (keep (fn [[k row]] (when (pred row) k))) table))

(defn column
  ([table col] (column table col some?))
  ([table col pred]
   (into {} (keep (fn [[k row]]
                    (let [v (col row)]
                      (when (pred v) [k v]))))
         table)))

(defn known
  [table]
  (into (sorted-set) (keys table)))

(defn unknown
  [problem what table k]
  {:problem problem
   :known   (known table)
   :message (str "unknown " what " " (pr-str k)
                 " — known: " (pr-str (vec (sort (keys table)))))})

(defn holds!
  [invariant bad]
  (when (seq bad)
    (throw (ex-info (str invariant ": " (pr-str bad)) {:invariant invariant :bad bad}))))

(defn per-key
  [what reg m]
  (let [ks  (set (if (map? reg) (keys reg) reg))
        out (reduce-kv (fn [acc k v]
                         (if (set? k) (into acc (map (fn [x] [x v])) k) (assoc acc k v)))
                       {} m)]
    (holds! (str what " names no entry for these keys of its registry")
            (sort-by str (remove #(contains? out %) ks)))
    (holds! (str what " names keys its registry does not hold")
            (sort-by str (remove ks (keys out))))
    out))

(defn on
  [t k & args]
  (let [v (get t k)]
    (if (fn? v) (apply v args) v)))
