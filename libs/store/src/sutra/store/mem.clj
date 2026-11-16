(ns sutra.store.mem
  (:require [sutra.store.api :as api]
            [sutra.store.core :as core]
            [sutra.store.kv :as kv]
            [sutra.store.kv-engine :as kv-engine]))

(set! *warn-on-reflection* true)

(def empty-seed
  {:objects {} :links {} :world {}})

(defn engine []
  (kv-engine/engine (kv/mem)))

(defn store
  ([] (store empty-seed {}))
  ([seed] (store seed {}))
  ([{:keys [objects links world]} opts]
   (let [db  (doto (core/db (engine) opts) core/adopt!)
         ops (concat (for [{:keys [id object-type-ref values markings]} (vals objects)]
                       {:op :create-object :type object-type-ref
                        :payload (cond-> {:id id :values (or values {})}
                                   (seq markings) (assoc :markings (vec markings)))})
                     (for [{:keys [id link-type-ref from-object-id to-object-id
                                   markings]} (vals links)]
                       {:op :create-link :type link-type-ref
                        :payload (cond-> {:id id :from-id from-object-id :to-id to-object-id}
                                   (seq markings) (assoc :markings (vec markings)))}))]
     (when (or (seq world) (seq ops))
       (api/transact! db
                      (cond-> {:basis (with-open [s (api/snapshot db {:branch "main"})]
                                        (api/basis s))
                               :actor {:principal "seed"}
                               :ops   (vec ops)}
                        (seq world) (assoc :schema {:authored world :migration []}))))
     db)))
