(ns sutra.types.upload)

(def placeholders
  {"{{media-reference}}" :hash
   "{{filename}}"        :filename
   "{{dataset}}"         :dataset})

(defn resolve-args
  [args facts]
  (into {} (for [[k v] args]
             [k (if-let [f (and (string? v) (get placeholders v))]
                  (get facts f v)
                  v)])))
