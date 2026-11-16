(ns sutra.types.paths.health
  (:require [sutra.types.paths.rules :refer [reads writes]]))

(defn- b
  [k]
  (keyword "sutra.foundation.contracts.b" (name k)))

(def endpoints
  {:health-check-of {:url "/api/health-checks/:id"
                     :methods {:delete {:effects writes :requires {:op :update}
                                        :response (b :health-deleted)}}
                     :service :conductor
                     :branch? true}
   :health-check-results {:url "/api/health-checks/:id/results"
                          :methods {:get {:effects reads :requires {:op :read}
                                          :response (b :health-results)}}
                          :service :conductor
                          :branch? true}
   :health-check-run {:url "/api/health-checks/:id/run"
                      :methods {:post {:effects writes :requires {:op :update}
                                       :response (b :health-result)}}
                      :service :conductor
                      :branch? true}
   :health-check-watch {:url "/api/health-checks/:id/watch"
                        :methods {:post {:effects writes :requires {:op :read}
                                         :request (b :health-watch-in)
                                         :response (b :health-watch)}}
                        :service :conductor
                        :branch? true}
   :health-check-snooze {:url "/api/health-checks/:id/snooze"
                         :methods {:delete {:effects writes
                                            :requires {:op :update}
                                            :response (b :health-snoozed)}
                                   :post {:effects writes :requires {:op :update}
                                          :request (b :health-snooze-in)
                                          :response (b :health-snoozed)}}
                         :service :conductor
                         :branch? true}
   :health-check-groups {:url "/api/health-check-groups"
                         :methods {:get {:effects reads :requires {:op :read}
                                         :response (b :health-groups)}
                                   :post {:effects writes :requires {:op :update}
                                          :request (b :health-group-in)
                                          :response (b :health-group)}}
                         :service :conductor
                         :branch? true}
   :health-check-group-of {:url "/api/health-check-groups/:id"
                           :methods {:delete {:effects writes
                                              :requires {:op :update}
                                              :response (b :health-deleted)}}
                           :service :conductor
                           :branch? true}
   :health-check-group-subscribe {:url "/api/health-check-groups/:id/subscribe"
                                  :methods {:delete
                                            {:effects writes
                                             :requires {:op :read}
                                             :response (b :health-subscription)}
                                            :post {:effects writes :requires {:op :read}
                                                   :request (b :health-subscribe-in)
                                                   :response (b :health-subscription)}}
                                  :service :conductor
                                  :branch? true}
   :health-checks {:url "/api/health-checks"
                   :methods {:get {:effects reads :requires {:op :read}
                                   :response (b :health-checks)}
                             :post {:effects writes :requires {:op :update}
                                    :request (b :health-check-in)
                                    :response (b :health-check)}}
                   :service :conductor
                   :branch? true}
   :health-overview {:url "/api/health/overview"
                     :methods {:get {:effects reads :requires {:op :read}
                                     :response (b :health-overview)}}
                     :service :conductor
                     :branch? true}
   :health-run-all {:url "/api/health/run-all"
                    :methods {:post {:effects writes :requires {:op :update}
                                     :response (b :health-results)}}
                    :service :conductor
                    :branch? true}})
