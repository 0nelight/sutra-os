(ns sutra.types.paths.automation
  (:require [sutra.types.paths.rules :refer [reads writes]]))

(def endpoints
  {:automation-of {:url "/api/automations/:id"
                   :methods {:delete {:effects writes :requires {:op :update}}}
                   :service :actions
                   :branch? true}
   :automation-preview {:url "/api/automations/:id/preview"
                        :methods {:post {:effects writes :requires {:op :update}
                                         :response :a/automation-preview.post.res
                                         :request :a/automation-preview.post.req}}
                        :service :actions
                        :branch? true}
   :automation-proposal-accept {:url "/api/automations/:id/proposals/:proposal/accept"
                                :methods {:post {:effects writes :requires {:op :update}
                                                 :response :a/automation-proposal-accept.post.res
                                                 :request :a/automation-proposal-accept.post.req}}
                                :service :actions
                                :branch? true}
   :automation-proposal-reject {:url "/api/automations/:id/proposals/:proposal/reject"
                                :methods {:post {:effects writes :requires {:op :update}
                                                 :request :a/automation-proposal-reject.post.req
                                                 :response :a/automation-proposal-reject.post.res}}
                                :service :actions
                                :branch? true}
   :automation-proposals {:url "/api/automations/:id/proposals"
                          :methods {:get {:effects reads :requires {:op :read}
                                          :response :a/automation-proposals.get.res}}
                          :service :actions
                          :branch? true
                          :query {:status :kw, :limit [:clamp 1 1000 200], :offset :long}}
   :automation-events {:url "/api/automations/:id/events"
                       :methods {:get {:effects reads :requires {:op :read}
                                       :response :a/automation-events.get.res}}
                       :service :actions
                       :branch? true
                       :query {:status :kw, :limit [:clamp 1 1000 100], :offset :long}}
   :automation-mute {:url "/api/automations/:id/mute"
                     :methods {:post {:effects writes :requires {:op :update}}}
                     :service :actions
                     :branch? true}
   :automation-subscribe {:url "/api/automations/:id/subscribe"
                          :methods {:delete {:effects writes :requires {:op :read}}
                                    :post {:effects writes :requires {:op :read}}}
                          :service :actions
                          :branch? true}
   :automation-run {:url "/api/automations/:id/run"
                    :methods {:post {:effects writes :requires {:op :update}
                                     :response :a/automation-run.post.res}}
                    :service :actions
                    :branch? true}
   :automation-executions {:url "/api/automations/:id/executions"
                           :methods {:get {:effects reads :requires {:op :read}}}
                           :service :actions
                           :branch? true}
   :automation-execution-cancel {:url "/api/automations/:id/executions/:execution/cancel"
                                 :methods {:post {:effects writes :requires {:op :update}}}
                                 :service :actions
                                 :branch? true}
   :automation-runs {:url "/api/automations/:id/runs"
                     :methods {:get {:effects reads :requires {:op :read}
                                     :response :a/automation-runs.get.res}}
                     :service :actions
                     :branch? true}
   :automations {:url "/api/automations"
                 :methods {:get {:effects reads :requires {:op :read}
                                 :response :a/automations.get.res}
                           :post {:effects writes :requires {:op :update}
                                  :response :a/automations.post.res
                                  :request :a/automations.post.req}}
                 :service :actions
                 :branch? true}})
