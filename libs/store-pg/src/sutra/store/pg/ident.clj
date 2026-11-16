(ns sutra.store.pg.ident
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def ^:const max-ident
  63)

(defn too-long?
  [s]
  (let [s (str s) n (.length s)]
    (cond (<= (* 3 n) max-ident) false
          (> n max-ident)        true
          :else (> (count (.getBytes s "UTF-8")) max-ident))))

(defn fit
  [s]
  (let [s (str s)]
    (if (too-long? s)
      (str (subs s 0 (- max-ident 9)) "_" (format "%08x" (.hashCode s)))
      s)))

(defn refuse-if-too-long!
  [s where]
  (when (too-long? s)
    (throw (ex-info (str where " is " (count (str s)) " bytes and the store keeps "
                         max-ident " — it would be cut to a different name than the"
                         " one that gets looked up. Build it through"
                         " `sutra.store.pg.ident/fit`: " (pr-str s))
                    {:ident (str s) :max max-ident :where where})))
  (str s))

(defn sanitize
  [s]
  (str/replace (str s) #"[^A-Za-z0-9_]" "_"))
