(ns sutra.schedules.listener-events)

(set! *warn-on-reflection* true)

(defonce ^:private heard (atom {}))

(defn heard!
  [source]
  (swap! heard update (name source) (fnil inc 0))
  nil)

(defn signature
  [source]
  [:listener (name source) (get @heard (name source) 0)])
