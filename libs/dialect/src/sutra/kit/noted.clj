(ns sutra.kit.noted
  (:require [sutra.kit.attempt :as attempt]))

(set! *warn-on-reflection* true)

(defn note
  [what ^Throwable t]
  (binding [*out* *err*]
    (println (str "[warn] " what ": " (.getSimpleName (class t)) ": " (ex-message t)))))

(defmacro or-else
  [what alt & body]
  `(try ~@body
        (catch Throwable t#
          (attempt/rethrow-fatal! t#)
          (note ~what t#)
          ~alt)))

(defmacro quietly
  [what & body]
  `(or-else ~what nil ~@body))
