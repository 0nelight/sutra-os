(ns sutra.wire.protocol)

(def version 1)

(def header "X-Sutra-Protocol")

(defn mismatch
  [advertised]
  (let [theirs (some-> advertised str parse-long)]
    (when (and theirs (not= theirs version))
      {:client version :server theirs})))
