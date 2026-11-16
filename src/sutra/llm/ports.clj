(ns sutra.llm.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn llm-catalogues [store] (sys/table (sys/of store) :llm/catalogues))
