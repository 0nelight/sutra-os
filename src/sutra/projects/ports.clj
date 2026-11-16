(ns sutra.projects.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn movers [store] (sys/table (sys/of store) :kompass/movers))

(defn file-rows [store] (sys/table (sys/of store) :kompass/file-rows))

(defn install-schema!
  [store schema opts]
  ((sys/required (sys/of store) :schema/install) store schema opts))
