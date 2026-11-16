(ns sutra.types.opid
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defn name-id
  [kind nm]
  (str (UUID/nameUUIDFromBytes (.getBytes (str (name kind) "/" nm) "UTF-8"))))
