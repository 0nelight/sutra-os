(ns sutra.builds.run.lane
  (:require [sutra.store.access.scope :as ctx]
            [sutra.store.served :as served]
            [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn on-lane
  [store lane]
  (assoc store
         :db       (served/on-lane (:db store) lane)
         :datasets (ctx/datasets (sys/of store) lane)
         :streams  (ctx/streams (sys/of store) lane)))

(defn fenced
  [store fences]
  (update store :db served/fenced fences))

(defn holding
  [store l]
  (fenced store [[:lease (:name l) (:epoch l)]]))
