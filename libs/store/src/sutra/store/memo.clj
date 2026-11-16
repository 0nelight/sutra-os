(ns sutra.store.memo)

(set! *warn-on-reflection* true)

(defn hit
  [memo t k]
  (some-> (get @memo t) (find k) val vector))

(defn- restarted
  [table {:keys [cap newest]} kvs]
  (let [room (max 0 (- (quot cap 2) (count kvs)))]
    (into (if newest (into {} (take room) (sort-by key #(compare %2 %1) table)) {}) kvs)))

(defn admitted
  [m spec t kvs]
  (let [{:keys [cap clears] :as bound} (get spec t)]
    (if (> (+ (count (get m t)) (count kvs)) cap)
      (-> (reduce #(assoc %1 %2 {}) m clears) (assoc t (restarted (get m t) bound kvs)))
      (update m t (fnil into {}) kvs))))

(defn put!
  [memo spec t kvs]
  (swap! memo admitted spec t kvs))

(defn through
  [memo spec t k f]
  (if-let [[v] (hit memo t k)]
    v
    (let [v (f)] (put! memo spec t [[k v]]) v)))
