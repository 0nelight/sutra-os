(ns sutra.llm.rate
  (:require [sutra.foundation.settings :as settings]))

(set! *warn-on-reflection* true)

(def window-ms 60000)

(def flight-poll-ms
  250)

(defonce ^:private state (atom {}))

(defn limits
  []
  {:rpm         (settings/value :llm-requests-per-minute)
   :tpm         (settings/value :llm-tokens-per-minute)
   :concurrency (settings/value :llm-concurrency)
   :share       (settings/value :llm-batch-share-pct)})

(defn- live [entries now]
  (filterv #(> (long (:t %)) (- now window-ms)) entries))

(defn- free-at
  [entries measure room]
  (loop [es entries, m (reduce + 0 (map measure entries)), gone nil]
    (if (or (empty? es) (<= m room))
      (some-> gone :t (+ window-ms))
      (recur (rest es) (- m (measure (first es))) (first es)))))

(defn- capped
  [cap share batch?]
  (when (and cap (pos? cap))
    (if batch? (max 1 (long (* cap (/ (or share 100) 100.0)))) cap)))

(defn wait-ms
  [entries flying {:keys [rpm tpm concurrency share]} batch? tokens now]
  (let [rpm   (capped rpm share batch?)
        tpm   (capped tpm share batch?)
        cc    (capped concurrency share batch?)
        until (max (or (when (and rpm (>= (count entries) rpm))
                         (free-at entries (constantly 1) (dec rpm)))
                       0)
                   (or (when (and tpm (seq entries)
                                  (> (+ tokens (reduce + 0 (map :tokens entries))) tpm))
                         (free-at entries :tokens (max 0 (- tpm tokens))))
                       0)
                   (if (and cc (>= flying cc)) (+ now flight-poll-ms) 0))]
    (when (pos? until) (max 1 (- until now)))))

(defn- source-of [[source _]] source)

(defn- held [s key lim batch? tokens now]
  (wait-ms (live (get-in s [:windows key]) now) (count (get-in s [:flight (source-of key)]))
           lim batch? tokens now))

(defn admitting
  [s key lim id batch? tokens now]
  (let [s (update-in s [:windows key] live now)]
    (if (held s key lim batch? tokens now)
      s
      (-> s
          (update-in [:windows key] (fnil conj []) {:id id :t now :tokens tokens})
          (update-in [:flight (source-of key)] (fnil conj #{}) id)))))

(defn landing
  [s key id tokens]
  (cond-> (update-in s [:flight (source-of key)] disj id)
    tokens (update-in [:windows key]
                      (fn [es] (mapv #(cond-> % (= id (:id %)) (assoc :tokens tokens)) es)))))

(defn forgetting
  [s key id]
  (-> s
      (update-in [:flight (source-of key)] disj id)
      (update-in [:windows key] (fn [es] (filterv #(not= id (:id %)) es)))))

(defn admit!
  [key id batch? tokens]
  (let [lim     (limits)
        now     (System/currentTimeMillis)
        [old _] (swap-vals! state admitting key lim id batch? tokens now)]
    (held (update-in old [:windows key] live now) key lim batch? tokens now)))

(defn land! [key id tokens] (swap! state landing key id tokens))

(defn forget! [key id] (swap! state forgetting key id))
