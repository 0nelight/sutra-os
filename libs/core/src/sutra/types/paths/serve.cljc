(ns sutra.types.paths.serve
  (:require [sutra.types.paths.rules :refer [reads writes m-onto m-instance c-function]]))

(defn- c
  [k]
  (keyword "sutra.foundation.contracts.c" (name k)))

(def endpoints
  {:audit {:url "/api/audit"
           :response (c :audit)
           :methods {:get {:effects reads :requires {:op :read}}}
           :service :platform
           :branch? true
           :query {:limit [:long 500]}}
   :instance-settings {:url "/api/admin/settings"
                       :methods {:get {:response (c :settings)
                                       :effects reads}
                                 :post {:request (c :settings-in)
                                        :response (c :settings)
                                        :effects writes}}
                       :service :platform
                       :auth :admin
                       :reason "the instance's operating policy — concurrency, retries, intervals, limits"}
   :telemetry {:url "/api/telemetry"
               :response (c :report)
               :methods {:get {:effects reads}}
               :service :platform
               :auth :admin
               :reason "what the deployment's processes and its store cost"}
   :telemetry-metrics {:url "/api/telemetry/metrics"
                       :methods {:get {:effects reads}}
                       :service :platform
                       :auth :admin
                       :reason "the telemetry as OpenMetrics text for a scraper"}
   :client-log {:url "/api/client-log"
                :response (c :client-logged)
                :methods {:post {:effects writes}}
                :service :auth
                :auth :public}
   :client-log-entries {:url "/api/client-log/entries"
                        :response (c :client-log-entries)
                        :methods {:get {:effects reads}}
                        :service :auth
                        :auth :admin
                        :reason "the browser errors the clients reported"
                        :query {:limit [:long 50], :level :str, :user :str, :since :str,
                                :q :str}}
   :ops-call {:url "/api/ops"
              :methods {:post {:effects writes}}
              :service :platform
              :auth :public}
   :openapi {:url "/api/openapi.json"
             :methods {:get {:effects reads}}
             :service :platform
             :auth :public}
   :code-reload {:url "/api/admin/code-reload"
                 :response (c :code-reloaded)
                 :methods {:post {:effects writes}}
                 :process :every
                 :service :platform
                 :auth :admin
                 :reason "hot code reload"}
   :datasource-query {:url "/api/datasources/:name/query"
                      :request (c :datasource-query-in)
                      :response (c :datasource-rows)
                      :methods {:post {:effects reads}}
                      :service :source
                      :auth :admin
                      :reason "ad-hoc source SQL"}
   :egress-allowlist {:url "/api/egress/allowlist"
                      :response (c :egress-allowlist)
                      :methods {:get {:effects reads}}
                      :service :source
                      :auth :admin
                      :reason "egress allowlist derivation"
                      :query {:format :str}}
   :eval-results {:url "/api/eval/:id/results"
                  :response (c :eval-results)
                  :methods {:get {:effects reads :requires {:op :read}}}
                  :service :platform
                  :branch? true}
   :eval-run-cases {:url "/api/eval/runs/:id/cases"
                    :response (c :eval-run-cases)
                    :methods {:get {:effects reads :requires {:op :read}}}
                    :service :platform
                    :branch? true}
   :eval-run {:url "/api/eval/run"
              :request (c :eval-run-in)
              :response (c :report)
              :methods {:post {:effects writes :requires {:op :update}}}
              :process :conductor
              :service :platform
              :branch? true}
   :eval-job {:url "/api/eval/jobs/:id/status"
              :response (c :report)
              :methods {:get {:effects reads :requires {:op :read}}}
              :process :conductor
              :service :platform
              :branch? true}
   :eval-suites {:url "/api/eval/suites"
                 :methods {:get {:response (c :eval-suites)
                                 :effects reads :requires {:op :read}}
                           :post {:response (c :report)
                                  :effects writes :requires {:op :update}}}
                 :service :platform
                 :branch? true}
   :eval-suite-cases {:url "/api/eval/suites/:name/cases"
                      :request (c :eval-case-in)
                      :response (c :report)
                      :methods {:post {:effects writes :requires {:op :update}}}
                      :service :platform
                      :branch? true}
   :function-run {:url "/api/function"
                  :request (c :function-run-in)
                  :response (c :function-run)
                  :methods {:post {:effects writes :requires c-function}}
                  :service :oms
                  :branch? true
                  :encode :keywordize-function}
   :login-audit {:url "/api/login-audit"
                 :response (c :login-audit)
                 :methods {:get {:effects reads}}
                 :service :platform
                 :auth :admin
                 :reason "the login audit log"
                 :query {:limit [:long 500], :user :str, :outcome :str, :provider :str}}
   :platform-experience {:url "/api/platform-experience"
                         :response (c :platform-experience)
                         :methods {:get {:effects reads}}
                         :service :platform
                         :auth :public
                         :reason "the sign-in gate names and badges the platform before a session exists"}
   :platform-experience-set {:url "/api/admin/platform-experience"
                             :request (c :platform-experience-in)
                             :methods {:post {:effects writes}}
                             :service :platform
                             :auth :admin
                             :reason "renaming the platform and replacing its logo"}
   :prune-resolution {:url "/api/ontology/prune-resolution"
                      :request (c :apply-in)
                      :response (c :prune-resolution)
                      :methods {:post {:effects writes}}
                      :service :platform
                      :auth :admin
                      :reason "sweeping entity-resolution state no resolved type claims"}
   :reboot {:url "/api/reboot"
            :response (c :rebooted)
            :methods {:post {:effects writes}}
            :service :platform
            :auth :admin
            :reason "restarting the instance"}
   :replay-history {:url "/api/admin/replay-history"
                    :methods {:get {:response (c :report)
                                    :effects reads}
                              :post {:request (c :replay-in)
                                     :response (c :report)
                                     :effects writes}}
                    :service :platform
                    :auth :admin
                    :reason "replaying the schema history into per-resource history"}
   :restore-point {:url "/api/admin/restore-point"
                   :methods {:get {:effects reads}
                             :post {:effects writes}}
                   :service :platform
                   :auth :admin
                   :reason "data-plane restore points"
                   :query {:to :long}}
   :schema-code-blobs {:url "/api/schema/code-blobs"
                       :request (c :apply-in)
                       :response (c :report)
                       :methods {:post {:effects writes}}
                       :service :platform
                       :auth :admin
                       :reason "migrating schema source bodies into the CAS"}
   :schema-reload {:url "/api/schema/reload"
                   :response (c :schema-reloaded)
                   :methods {:post {:effects writes}}
                   :service :oms
                   :auth :admin
                   :reason "schema reload"}
   :system {:url "/api/system"
            :response (c :system)
            :methods {:get {:effects reads :requires m-instance}}
            :service :platform}
   :stream-archive {:url "/api/streams/:name/archive"
                    :response (c :report)
                    :methods {:post {:effects writes
                                     :requires {:op :update, :on [:datasets :name]}}}
                    :process :conductor
                    :service :conductor
                    :branch? true}
   :stream-by {:url "/api/streams/:name"
               :response (c :stream-status)
               :methods {:get {:effects reads
                               :requires {:op :read, :on [:datasets :name]}}}
               :service :conductor
               :branch? true}
   :stream-offsets {:url "/api/streams/:name/offsets"
                    :response (c :stream-offsets)
                    :methods {:get {:effects reads :requires {:op :read, :on [:datasets :name]}}}
                    :service :conductor
                    :branch? true}
   :stream-records {:url "/api/streams/:name/records"
                    :methods {:get {:response (c :stream-records)
                                    :effects reads :requires {:op :read, :on [:datasets :name]}}
                              :post {:request (c :stream-publish-in)
                                     :response (c :stream-published)
                                     :effects writes
                                     :requires {:op :update, :on [:datasets :name]}}}
                    :service :conductor
                    :branch? true
                    :query {:partition [:long 0], :start-offset :long, :limit [:long 200]}}
   :stream-reset {:url "/api/streams/:name/reset"
                  :response (c :report)
                  :methods {:post {:effects writes :requires {:op :delete, :on [:datasets :name]}}}
                  :service :conductor
                  :branch? true}
   :ontology-watch {:url "/api/ontology/watch"
                    :methods {:get {:effects reads}}
                    :deadline :unbounded
                    :service :oms
                    :auth :consumer
                    :branch? true}
   :stream-subscribe {:url "/api/streams/:name/subscribe"
                      :methods {:get {:effects reads :requires {:op :read, :on [:datasets :name]}}}
                      :deadline :unbounded
                      :service :conductor
                      :branch? true}
   :stream-view {:url "/api/streams/:name/view"
                 :response (c :stream-view)
                 :methods {:get {:effects reads :requires {:op :read, :on [:datasets :name]}}}
                 :service :conductor
                 :branch? true
                 :query {:limit [:long 200]}}
   :streams {:url "/api/streams"
             :response (c :streams)
             :methods {:get {:effects reads :requires m-onto}}
             :service :conductor
             :branch? true}
   :trace {:url "/api/trace"
           :response (c :trace)
           :methods {:get {:effects reads}}
           :service :platform
           :auth :admin
           :reason "SQL trace"
           :query {:b [:long 0], :e :str}}
   :trace-arm {:url "/api/trace/arm"
               :request (c :trace-arm-in)
               :response (c :trace-armed)
               :methods {:post {:effects writes}}
               :service :platform
               :auth :admin
               :reason "SQL trace"}
   :trace-kill {:url "/api/trace/kill"
                :request (c :trace-kill-in)
                :response (c :trace-killed)
                :methods {:post {:effects writes}}
                :service :platform
                :auth :admin
                :reason "SQL trace"}
   :trace-explain {:url "/api/trace/explain"
                   :request (c :trace-explain-in)
                   :response (c :report)
                   :methods {:post {:effects reads}}
                   :service :platform
                   :auth :admin
                   :reason "SQL trace"}
   :decision-trace {:url "/api/trace/decisions"
                    :request (c :decision-trace-in)
                    :response (c :report)
                    :methods {:post {:effects writes}}
                    :service :platform
                    :auth :admin
                    :reason "decision recording"}
   :fn-trace {:url "/api/trace/fns"
              :request (c :fn-trace-in)
              :response (c :report)
              :methods {:post {:effects writes}}
              :service :platform
              :auth :admin
              :reason "function trace"}
   :trace-stuck {:url "/api/trace/stuck"
                 :response (c :requests-in-flight)
                 :methods {:get {:effects reads}}
                 :service :platform
                 :auth :admin
                 :reason "requests in flight"}
   :profile {:url "/api/profile"
             :request (c :profile-in)
             :response (c :report)
             :methods {:post {:effects writes}}
             :service :platform
             :auth :admin
             :reason "sampling profiler"}})
