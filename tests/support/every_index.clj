(ns support.every-index
  (:require [sutra.authoring.install.live :as live]
            [sutra.catalog.world-install :as world-install]
            [sutra.world.db :as kdb]))

(defn index
  [W]
  (let [d     (kdb/db W world-install/views)
        keys* (fn [sec] (keys (get (kdb/world d) sec)))
        nodes (fn [secs] (for [s secs k (keys* s)] [s k]))
        ts    (fn [& _] (keys* :transforms))
        ots   (fn [_] (keys* :object-types))]
    (assoc (live/index-of d)
           :names         (fn [sec _] (keys* sec))
           :producing     ts
           :reading       ts
           :fk-col        ts
           :dataset-out   ts
           :fk-link       ts
           :backed-by     ots
           :derive-handle ots
           :link-end      (fn [_] (keys* :link-types))
           :calling       (fn [_] (nodes [:actions :applications]))
           :referring     (fn [_] (nodes (keys (kdb/world d)))))))
