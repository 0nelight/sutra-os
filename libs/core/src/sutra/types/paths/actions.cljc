(ns sutra.types.paths.actions
  (:require [sutra.types.paths.rules :refer [reads writes m-own c-action c-writeback]]))

(def endpoints
  {:action-params {:url "/api/actions/params"
                   :methods {:post {:effects writes :requires c-action
                                    :response :a/action-params.post.res
                                    :request :a/action-params.post.req}}
                   :service :actions
                   :branch? true}
   :actions {:url "/api/actions"
             :methods {:post {:effects writes :requires c-action}}
             :service :actions
             :branch? true
             :query {:dry-run :bool, :publish :bool, :stage :bool, :return-edits :bool}
             :encode :keywordize-action
             :request :sutra.foundation.contracts/action-wire
             :response :sutra.foundation.contracts/action-response}
   :actions-batch {:url "/api/actions/batch"
                   :methods {:post {:effects writes :requires c-action}}
                   :service :actions
                   :branch? true
                   :query {:publish :bool, :stage :bool, :return-edits :bool}
                   :encode :keywordize-action-batch
                   :request :sutra.foundation.contracts/action-batch-wire
                   :response :sutra.foundation.contracts/action-batch-response}
   :staged-by-seq {:url "/api/staging/:seq"
                   :methods {:delete {:effects writes :requires m-own
                                      :response :a/staged-by-seq.delete.res}}
                   :service :actions}
   :staging {:url "/api/staging"
             :methods {:delete {:effects writes :requires m-own
                                :response :a/staging.delete.res}
                       :get {:effects reads :requires m-own
                             :response :a/staging.get.res}
                       :post {:effects writes :requires m-own
                              :response :a/staging.post.res}}
             :service :actions
             :branch? true}
   :staging-preview {:url "/api/staging-preview"
                     :methods {:get {:effects reads :requires m-own
                                     :response :a/staging-preview.get.res}}
                     :service :actions
                     :branch? true}
   :writeback-confirm {:url "/api/writeback/confirm"
                       :methods {:post {:effects writes :requires c-writeback
                                        :request :a/writeback-confirm.post.req
                                        :response :a/writeback-confirm.post.res}}
                       :service :actions
                       :branch? true}
   :writeback-preview {:url "/api/writeback/preview"
                       :methods {:post {:effects reads :requires c-writeback
                                        :request :a/writeback-preview.post.req
                                        :response :a/writeback-preview.post.res}}
                       :service :actions
                       :branch? true}})
