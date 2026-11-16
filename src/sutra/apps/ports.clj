(ns sutra.apps.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn llm-complete [store] (sys/required (sys/of store) :llm/complete))

(defn documents [store] (sys/required (sys/of store) :documents/records))
