(ns sutra.peering.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn direct-writes [] (sys/provided (sys/process) :objects/direct-writes))
