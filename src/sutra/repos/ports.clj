(ns sutra.repos.ports
  (:require [sutra.foundation.ops :as op]
            [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn riders [store] (sys/table (sys/of store) :repos/riders))

(defn placements [store projection]
  (op/query {:store store} :authoring/placements {:projection projection}))
