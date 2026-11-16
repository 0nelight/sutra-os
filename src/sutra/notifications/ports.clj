(ns sutra.notifications.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn cluster-broadcast [store] (sys/provided (sys/of store) :cluster/broadcast))
