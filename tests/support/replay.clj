(ns support.replay
  (:require [sutra.actions.execution :as x]
            [sutra.actions.action :as action]
            [sutra.store.access.scope :as scope]))

(defn evaluate
  ([ctx f] (evaluate ctx f {}))
  ([ctx f {:keys [timeout-ms snapshot] :or {timeout-ms 60000 snapshot :live}}]
   (let [store (:store ctx)]
     (:result (x/replaying (x/run {:timeout-ms timeout-ms :snapshot snapshot}
                                  (action/performer store nil))
                           #(f (-> ctx (dissoc :store) (assoc :authority (:authority store)))
                               (x/io (assoc (some-> store (action/store-io (scope/code)))
                                            :pass %))))))))
