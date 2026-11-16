(ns sutra.scenarios.api
  (:require [sutra.scenarios.apply :as apply]))

(set! *warn-on-reflection* true)

(defn apply-scenario!
  ([store branch target] (apply/apply-scenario! store branch target))
  ([store branch target opts] (apply/apply-scenario! store branch target opts)))

(defn create-scenario-branch! [store opts] (apply/create-scenario-branch! store opts))

(defn duplicate-scenario!
  ([store source parent new-branch] (apply/duplicate-scenario! store source parent new-branch))
  ([store source parent new-branch edit-fn opts]
   (apply/duplicate-scenario! store source parent new-branch edit-fn opts)))

(defn rebuild-scenario!
  ([store branch parent] (apply/rebuild-scenario! store branch parent))
  ([store branch parent opts] (apply/rebuild-scenario! store branch parent opts)))

(defn scenario-definition [store schema] (apply/scenario-definition store schema))

(defn seal-scenario! [store] (apply/seal-scenario! store))
