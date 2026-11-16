(ns sutra.kit.coll)

(defn index-by
  [f xs]
  (into {} (map (juxt f identity)) xs))
