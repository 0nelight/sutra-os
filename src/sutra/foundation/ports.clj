(ns sutra.foundation.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn commit [store] (sys/required (sys/of store) :ops/commit))

(defn dispatch [store] (sys/provided (sys/of store) :ops/dispatch))
