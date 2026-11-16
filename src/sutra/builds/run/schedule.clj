(ns sutra.builds.run.schedule)

(set! *warn-on-reflection* true)

(def tidy-every-ms 60000)

(def initial {:running #{} :tidied 0 :stopping? false})

(defn room?
  [{:keys [running stopping?]} {:keys [waiting cap waiting-max]}]
  (let [waiting (count (filter (set waiting) running))]
    (and (not stopping?)
         (< (- (count running) waiting) (long cap))
         (< waiting (long waiting-max)))))

(defn- take-if-room [state event] (when (room? state event) [[:take]]))

(defn step
  [state {:keys [kind now request run-id] :as event}]
  (case kind
    :poll     (if (:stopping? state)
                [state []]
                (let [tidy? (> (- (long now) (long (:tidied state))) tidy-every-ms)]
                  [(cond-> state tidy? (assoc :tidied now))
                   (into (if tidy? [[:tidy]] []) (take-if-room state event))]))
    :taken    (let [s (update state :running conj (:run-id request))]
                [s (into [[:start request]] (take-if-room s event))])
    :none     [state []]
    :finished [(update state :running disj run-id) [[:ring]]]
    :quiesce  (if (:stopping? state)
                [state []]
                [(assoc state :stopping? true)
                 (conj (mapv (fn [id] [:hand-back id]) (sort (:running state))) [:ring])])))
