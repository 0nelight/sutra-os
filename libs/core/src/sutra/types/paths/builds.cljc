(ns sutra.types.paths.builds
  (:require [sutra.types.paths.rules :refer [reads writes m-onto m-counts c-run c-read c-build]]))

(def endpoints
  {:build-estimate {:url "/api/build/estimate"
                    :methods {:get {:effects reads :requires {:op :read}
                                    :response :a/build-estimate.get.res}}
                    :service :conductor
                    :branch? true
                    :query {:only :csv-kw, :strategy :kw, :from :csv-kw, :days :long,
                            :calls-per-row :double}}
   :builds-active {:url "/api/import/active"
                   :methods {:get {:effects reads :requires {:op :read}}}
                   :process :conductor
                   :service :conductor}
   :catalog-consumption {:url "/api/catalog/consumption"
                         :methods {:get {:effects reads
                                         :requires {:op :read
                                                    :on [[:transforms :datasets] :transform]}
                                         :response :a/catalog-consumption.get.res}}
                         :service :conductor
                         :branch? true
                         :query {:transform :str}}
   :catalog-delta {:url "/api/catalog/delta"
                   :methods {:get {:effects reads
                                   :requires {:op :read, :on [[:transforms :datasets] :transform],
                                              :also [[:datasets :transforms] :input]}
                                   :response :a/catalog-delta.get.res}}
                   :service :conductor
                   :branch? true
                   :query {:transform :str, :input :str}}
   :catalog-transactions {:url "/api/catalog/transactions"
                          :methods {:get {:effects reads
                                          :requires {:op :read
                                                     :on [[:datasets :transforms :object-types]
                                                          :dataset]}
                                          :response :a/catalog-transactions.get.res}}
                          :service :conductor
                          :branch? true
                          :query {:dataset :str, :limit [:long 50]}}
   :dataset-preview {:url "/api/datasets/:id/preview"
                     :methods {:get {:effects reads :requires {:op :read, :on [:datasets :id]}
                                     :response :a/dataset-preview.get.res}}
                     :service :conductor
                     :branch? true
                     :query {:as-of :str}}
   :dataset-force-snapshot {:url "/api/datasets/:name/force-snapshot"
                            :methods {:post {:effects writes
                                             :requires {:op :update, :on [:datasets :name]}}}
                            :service :conductor
                            :branch? true}
   :dataset-write {:url "/api/datasets/:name/rows"
                   :methods {:post {:effects writes :requires {:op :update, :on [:datasets :name]}
                                    :request :a/dataset-write.post.req
                                    :response :a/dataset-write.post.res}}
                   :service :conductor
                   :branch? true}
   :dataset-export {:url "/api/datasets/:id/export"
                    :methods {:get {:effects reads :requires {:op :read, :on [:datasets :id]}}}
                    :service :conductor
                    :branch? true
                    :query {:as-of :str :format :str}}
   :dataset-import {:url "/api/datasets/:name/file"
                    :methods {:post {:effects writes
                                     :requires {:op :update, :on [:datasets :name]}
                                     :response :a/dataset-import.post.res}}
                    :service :conductor
                    :branch? true
                    :query {:format :str :mode :str :key :str}}
   :import-conns-of {:url "/api/import-plan/links/:type"
                     :methods {:delete {:effects writes :requires {:op :update}}
                               :post {:effects writes :requires {:op :update}}}
                     :service :conductor
                     :branch? true}
   :import-objects-of {:url "/api/import-plan/objects/:type"
                       :methods {:delete {:effects writes :requires {:op :update}}
                                 :post {:effects writes :requires {:op :update}}}
                       :service :conductor
                       :branch? true}
   :import-plan {:url "/api/import-plan"
                 :methods {:get {:effects reads :requires {:op :read}}}
                 :service :conductor
                 :branch? true}
   :import-poll {:url "/api/import/poll"
                 :methods {:post {:effects writes :requires {:op :update}}}
                 :process :conductor
                 :service :conductor
                 :branch? true}
   :import-run {:url "/api/import/run"
                :methods {:post {:effects writes :requires c-build
                                 :response :a/import-run.post.res
                                 :request :a/import-run.post.req}}
                :deadline :unbounded
                :process :conductor
                :service :conductor
                :branch? true}
   :import-run-by {:url "/api/import/runs/:id"
                   :methods {:get {:effects reads :requires c-run}}
                   :deadline :unbounded
                   :service :conductor
                   :branch? true}
   :import-runs {:url "/api/import/runs"
                 :methods {:get {:effects reads :requires c-run
                                 :response :a/import-runs.get.res}}
                 :deadline :unbounded
                 :service :conductor
                 :branch? true
                 :query {:kind :kw, :hours :long, :limit [:clamp 1 2000 50], :brief :bool}}
   :data-freshness {:url "/api/data-freshness"
                    :methods {:post {:effects writes :requires c-read
                                     :request :a/data-freshness.post.req
                                     :response :a/data-freshness.post.res}}
                    :service :conductor
                    :branch? true}
   :import-status {:url "/api/import/status"
                   :methods {:get {:effects reads :requires m-counts
                                   :response :a/import-status.get.res}}
                   :service :conductor
                   :branch? true}
   :ledger-consumption {:url "/api/ledger/consumption"
                        :methods {:get {:effects reads
                                        :requires {:op :read
                                                   :on [[:transforms :datasets] :transform]}
                                        :response :a/ledger-consumption.get.res}}
                        :service :conductor
                        :branch? true
                        :query {:transform :str}}
   :ledger-delta {:url "/api/ledger/delta"
                  :methods {:get {:effects reads
                                  :requires {:op :read, :on [[:transforms :datasets] :transform],
                                             :also [[:datasets :transforms] :input]}}}
                  :service :conductor
                  :branch? true
                  :query {:transform :str, :input :str}}
   :ledger-transactions {:url "/api/ledger/transactions"
                         :methods {:get {:effects reads
                                         :requires {:op :read
                                                    :on [[:datasets :transforms :object-types]
                                                         :dataset]}
                                         :response :a/ledger-transactions.get.res}}
                         :service :conductor
                         :branch? true
                         :query {:dataset :str, :limit [:long 50]}}
   :live {:url "/api/live"
          :methods {:get {:effects reads :requires {:op :read}}}
          :process :conductor
          :service :conductor
          :query {:since :long}}
   :llm-audit {:url "/api/llm/audit"
               :methods {:get {:effects reads :requires {:op :read}}}
               :service :conductor
               :branch? true
               :query {:caller :str, :errors :bool, :since :str, :request :str, :run :str,
                       :limit :long, :until :str, :today :bool, :model :str}}
   :llm-cost {:url "/api/llm/cost"
              :methods {:get {:effects reads :requires {:op :read}}}
              :service :conductor
              :branch? true
              :query {:caller :str, :errors :bool, :since :str, :run :str, :limit :long,
                      :until :str, :today :bool, :per :str, :calls :bool, :model :str}}
   :object-type-drop-edits {:url "/api/object-types/:type/drop-edits"
                            :methods {:post {:effects writes
                                             :requires {:op :delete, :on [:object-types :type]}}}
                            :service :oms
                            :branch? true}
   :object-type-edit-history {:url "/api/object-types/:type/edit-history"
                              :methods {:get {:effects reads
                                              :requires {:op :read, :on [:object-types :type]}
                                              :response :a/object-type-edit-history.get.res}}
                              :service :oms
                              :branch? true
                              :query {:limit [:clamp 1 1000 50] :pk :trim :token :trim :sort :trim
                                      :kinds :csv :editors :csv :props :csv :timeline :bool}}
   :object-type-reindex {:url "/api/object-types/:type/reindex"
                         :methods {:post {:effects writes
                                          :requires {:op :delete, :on [:object-types :type]}}}
                         :service :oms
                         :branch? true
                         :query {:force :bool}}
   :preview-transform {:url "/api/transforms/:name/preview"
                       :methods {:get {:effects reads
                                       :requires {:op :read, :on [:transforms :name]}
                                       :response :a/preview-transform.get.res}}
                       :process :conductor
                       :service :conductor
                       :branch? true
                       :query {:limit [:clamp 1 1000 50], :sample [:clamp 1 1000000 nil],
                               :param :repeated, :output :str}}
   :test-transform {:url "/api/transforms/:name/tests"
                    :methods {:get {:effects reads :requires {:op :read, :on [:transforms :name]}
                                    :response :a/test-transform.get.res}}
                    :process :conductor
                    :service :conductor
                    :branch? true}
   :run {:url "/api/runs/:id"
         :methods {:get {:effects reads :requires c-run
                         :response :a/run.get.res}}
         :service :conductor
         :branch? true}
   :run-cancel {:url "/api/runs/:id/cancel"
                :methods {:post {:effects writes :requires {:op :update}}}
                :process :conductor
                :service :conductor
                :branch? true
                :query {:force :bool}}
   :run-jobs {:url "/api/runs/:id/jobs"
              :methods {:get {:effects reads :requires c-run
                              :response :a/run-jobs.get.res}}
              :service :conductor
              :branch? true}
   :run-retry {:url "/api/runs/:id/retry"
               :methods {:post {:effects writes :requires {:op :update}}}
               :process :conductor
               :service :conductor
               :branch? true}
   :runs {:url "/api/runs"
          :methods {:get {:effects reads :requires c-run
                          :response :a/runs.get.res}}
          :service :conductor
          :branch? true
          :query {:kind :kw, :hours :long, :limit [:clamp 1 2000 50], :brief :bool}}
   :sources {:url "/api/sources"
             :methods {:get {:effects reads :requires m-onto
                             :response :a/sources.get.res}}
             :service :source
             :branch? true}
   :transform-history {:url "/api/transforms/:name/history"
                       :methods {:get {:effects reads
                                       :requires {:op :read, :on [:transforms :name]}}}
                       :service :oms
                       :branch? true}})
