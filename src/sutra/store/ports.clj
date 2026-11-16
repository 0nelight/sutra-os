(ns sutra.store.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn blobs-drain [store] (sys/provided (sys/of store) :blobs/drain))
