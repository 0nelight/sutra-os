(ns sutra.health.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn result-recorded!
  [store branch check-id]
  (when-let [f (sys/provided (sys/of store) :health/result-recorded)]
    (f store branch check-id)))
