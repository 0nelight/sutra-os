(ns sutra.builds.run.claim)

(set! *warn-on-reflection* true)

(defn lapsed?
  [until now]
  (or (nil? until) (< (inst-ms until) (long now))))

(defn free?
  [{:keys [claimed-until]} now]
  (lapsed? claimed-until now))

(defn mine?
  [record claim-id]
  (and (some? claim-id) (= claim-id (:claim-id record))))

(defn claimed
  [record claim-id now ms]
  (assoc record :claim-id claim-id :claimed-until (java.util.Date. (+ (long now) (long ms)))))

(defn renewed
  [record claim-id now ms]
  (when (mine? record claim-id)
    (assoc record :claimed-until (java.util.Date. (+ (long now) (long ms))))))

(defn released
  [record]
  (assoc record :claim-id nil :claimed-until nil))
