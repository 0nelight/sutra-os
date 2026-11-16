(ns sutra.builds.ports
  (:require [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn cluster-broadcast [store] (sys/provided (sys/of store) :cluster/broadcast))

(defn stream-wake [] (sys/provided (sys/process) :stream/wake))

(defn foreign-export-rows [store] (sys/required (sys/of store) :foreign/export-rows))

(defn publish-index! [store schema decls group cancel-check]
  ((sys/required (sys/of store) :objects/publish-index) store schema decls group cancel-check))

(defn foreign-datasources [store k]
  (get (sys/required (sys/of store) :foreign/datasources) k))

(defn compute-runner [store k] (get (sys/required (sys/of store) :compute/runner) k))

(defn run-query-function [store schema fn-name args]
  ((sys/required (sys/of store) :functions/run-query) store schema fn-name args))

(defn health-result-recorded! [store branch check-id]
  (when-let [f (sys/provided (sys/of store) :health/result-recorded)]
    (f store branch check-id)))

(defn foreign-owned-ids
  [store obj-type]
  (if-let [f (sys/provided (sys/of store) :objects/foreign-owned)]
    (set (f store obj-type))
    #{}))
