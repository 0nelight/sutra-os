(ns sutra.evals.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn llm-complete [store] (sys/required (sys/of store) :llm/complete))
