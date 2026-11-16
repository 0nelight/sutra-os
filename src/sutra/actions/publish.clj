(ns sutra.actions.publish
  (:require [sutra.store.access.run-log :as run-log]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.grants :as grants]
            [sutra.actions.action :as action]
            [sutra.actions.batch :as batch]
            [sutra.actions.request :as request]
            [sutra.store.world :as world]
            [sutra.types.attribute :as attribute]))

(set! *warn-on-reflection* true)

(defn- derived-attrs
  [schema T]
  (into #{} (map first) (:derived (attribute/of (get-in schema [:object-types T])))))

(defn- present-rows
  [store type rows]
  (let [ids  (mapv (comp str :id) rows)
        have (into #{} (map (comp str :id))
                   (sub/read-objects (ctx/as-system store) type ids))]
    (filterv #(have (str (:id %))) rows)))

(defn- elapsed-ms
  [t0]
  (Math/round (double (/ (- (System/nanoTime) t0) 1e6))))

(defn publish!
  [store schema decls group cancel-check]
  (let [req      (request/action-request {:branch    (:branch store)
                                          :authority (grants/request-authority store)
                                          :store store :schema schema})
        store*      (:store req)
        b        (:branch req)
        {:keys [type tx-op authorize-row] :as plan}
        (batch/compile-plan schema group (:authorize (second (action/run-for req nil)))
                            (:authority store*))
        derived  (derived-attrs schema type)
        actor    (grants/principal store*)
        op-of    (fn [r] {:op tx-op :type type
                          :payload (cond-> r
                                     (and (seq derived) (:values r))
                                     (update :values #(apply dissoc % derived)))})
        p        (batch/partition-count (count decls) (.availableProcessors (Runtime/getRuntime)))
        absent   (volatile! 0)
        flush-ms (volatile! 0)
        pid      (str (random-uuid))
        done     (volatile! 0)
        chunk-n  (volatile! 0)
        log-fn   (fn [ops]
                   [(run-log/action-entry nil {:published (vswap! done + (count ops))
                                               :publish-id pid :chunk (vswap! chunk-n inc)}
                                          nil nil)])
        res      (batch/run-planned!
                  plan decls p
                  (fn [rows]
                    (when cancel-check (cancel-check))
                    (when authorize-row (run! authorize-row rows))
                    (let [t0   (System/nanoTime)
                          kept (cond->> rows (= :update-object tx-op) (present-rows store* type))]
                      (vswap! absent + (- (count rows) (count kept)))
                      (world/index! (world/at store* b) (mapv op-of kept)
                                    {:actor actor :log-fn log-fn})
                      (vswap! flush-ms + (elapsed-ms t0)))))
        res      (-> res (update :published - @absent) (update :skipped + @absent))]
    (when (zero? @chunk-n)
      (run-log/append! store* [(run-log/action-entry nil {:published (:published res)} nil nil)]))
    (assoc res :partitions p :interpret-ms @flush-ms)))
