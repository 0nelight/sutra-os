(ns sutra.store.migrations.index-feed
  (:require [sutra.store.core :as core]
            [sutra.store.storage.instance-config :as ic]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(def ^:private done-key "migrations.index-feed-compacted")

(defn compact!
  [store]
  (when-not (ic/get-cfg store done-key)
    (let [r (core/compact-index-feed! (:db store) (:rev (world/head store)))]
      (ic/set-cfg! store done-key "true")
      r)))
