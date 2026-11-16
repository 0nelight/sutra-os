(ns sutra.foundation.env
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn getenv
  [k]
  (if-let [c (sys/config (sys/process))]
    (get c k)
    (System/getenv k)))

(defn auth-hardening?
  []
  (= "on" (some-> (getenv "SUTRA_AUTH_HARDENING") str .trim .toLowerCase)))

(defn env
  [k default]
  (or (getenv k) default))
