(ns sutra.types.paths.lineage
  (:require [sutra.types.paths.rules :refer [reads writes m-onto]]))

(defn- b
  [k]
  (keyword "sutra.foundation.contracts.b" (name k)))

(def endpoints
  {:lineage-dependents {:url "/api/lineage/dependents"
                        :methods {:get {:effects reads :requires m-onto
                                        :response (b :lineage-dependents)}}
                        :service :conductor
                        :branch? true
                        :query {:type :str}}
   :lineage-facts {:url "/api/lineage/facts"
                   :methods {:get {:effects reads :requires m-onto
                                   :response (b :lineage-facts)}}
                   :service :conductor
                   :branch? true
                   :query {:node :str, :up :str, :down :str, :to :str, :project :str, :graph :str,
                           :stale :str, :dead :bool, :blocked :bool, :diff-live :bool,
                           :io-only :bool, :column :str, :out-of-date :bool, :modified :bool,
                           :by :str,
                           :schedules :bool}}
   :lineage-graph {:url "/api/lineage/graph"
                   :methods {:get {:effects reads :requires m-onto
                                   :response (b :lineage-graph)}}
                   :service :conductor
                   :branch? true
                   :query {:node :str}}
   :lineage-related {:url "/api/lineage/related"
                     :methods {:get {:effects reads :requires m-onto
                                     :response (b :lineage-related)}}
                     :service :conductor
                     :branch? true
                     :query {:node :str, :kind :str}}
   :lineage-rollback {:url "/api/lineage/rollback"
                      :methods {:post {:effects writes :requires {:op :update}
                                       :request (b :lineage-rollback-in)
                                       :response (b :lineage-rollback)}}
                      :service :conductor
                      :branch? true}
   :lineage-status {:url "/api/lineage/status"
                    :methods {:get {:effects reads :requires m-onto
                                    :response (b :lineage-status)}}
                    :service :conductor
                    :branch? true}
   :schedule-candidates {:url "/api/schedule-candidates"
                         :methods {:get {:effects reads :requires {:op :read}
                                         :response (b :schedule-candidates)}}
                         :service :conductor
                         :branch? true
                         :query {:target :str}}})
