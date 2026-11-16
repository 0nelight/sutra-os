(ns sutra.objects.ports
  (:require [sutra.foundation.ops :as op]
            [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn llm-embed [store] (sys/provided (sys/of store) :llm/relay-embed))

(defn preview-projection [store view]
  (op/query {:store store} :authoring/preview-projection view))
