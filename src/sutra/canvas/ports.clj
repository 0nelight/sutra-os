(ns sutra.canvas.ports
  (:require [sutra.foundation.ops :as op]))

(set! *warn-on-reflection* true)

(defn lineage-drawing [store fmt drawing]
  (op/query {:store store} :lineage/drawing {:format fmt :drawing drawing}))
