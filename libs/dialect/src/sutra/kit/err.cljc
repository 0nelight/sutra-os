(ns sutra.kit.err)

(defn causes
  [e]
  (take-while some? (iterate ex-cause e)))

(defn cause
  [pred e]
  (some #(when (pred %) %) (causes e)))
