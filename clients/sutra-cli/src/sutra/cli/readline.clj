(ns sutra.cli.readline)

(defn make-reader
  [_sess]
  {:backend   :fallback
   :read-line (fn [prompt] (print prompt) (flush) (read-line))
   :close     (fn [] nil)})

(defn read-line!
  [reader prompt]
  ((:read-line reader) prompt))

(defn close!
  [reader]
  ((:close reader)))
