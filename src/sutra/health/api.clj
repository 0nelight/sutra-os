(ns sutra.health.api
  (:require [sutra.health.checks :as checks]))

(set! *warn-on-reflection* true)

(defn list-all [store branch] (checks/list-all store branch))

(defn upsert! [store branch check] (checks/upsert! store branch check))
