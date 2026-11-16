(ns sutra.lineage.ports
  (:require [sutra.foundation.ops :as op]
            [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn viz-layouts [store] (sys/table (sys/of store) :viz/layouts))

(defn- query-or [ctx o in absent]
  (if (op/bound? o) (op/query ctx o in) absent))

(defn branch-tasks [store branch]
  (query-or {:store store} :branching/tasks {:branch branch} nil))

(defn open-issues [store]
  (query-or {:store store} :issues/open-by-node {} {}))

(defn node-health [store branch]
  (query-or {:store store} :health/by-node {:branch branch} {}))

(defn automations [store branch]
  (query-or {:store store} :automation/list {:branch branch} []))

(defn saved-graph [store schema handle]
  (query-or {:store store :schema schema} :canvas/saved-graph {:handle handle} nil))
