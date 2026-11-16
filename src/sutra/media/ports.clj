(ns sutra.media.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn blobs-roots [store] (sys/table (sys/of store) :blobs/roots))
(defn object-reads [store] (sys/required (sys/of store) :objects/reads))
(defn around-write [store] (sys/required (sys/of store) :schedules/around-write))
(defn source-ops [store] (sys/required (sys/of store) :foreign/source-ops))
