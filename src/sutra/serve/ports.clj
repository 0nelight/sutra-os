(ns sutra.serve.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn cluster-broadcast
  ([] (sys/provided (sys/process) :cluster/broadcast))
  ([store] (sys/provided (sys/of store) :cluster/broadcast)))

(defn pg-ds [] (sys/provided (sys/process) :pg/ds))

(defn observe-reports [] (sys/provided (sys/process) :observe/reports))

(defn observe-recordings [] (sys/provided (sys/process) :observe/recordings))

(defn telemetry-samples [] (sys/provided (sys/process) :telemetry/samples))

(defn telemetry-store [] (sys/provided (sys/process) :telemetry/store))
