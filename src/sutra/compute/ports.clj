(ns sutra.compute.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn viz-layouts [store] (sys/table (sys/of store) :viz/layouts))
(defn object-reads [store] (sys/required (sys/of store) :objects/reads))
