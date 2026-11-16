(ns sutra.types.payloads)

(defn action-call
  [action args]
  {:action action :args args})

