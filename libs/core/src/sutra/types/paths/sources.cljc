(ns sutra.types.paths.sources
  (:require [sutra.types.paths.rules :refer [reads writes m-onto m-instance c-schema]]))

(defn- c
  [k]
  (keyword "sutra.foundation.contracts.c" (name k)))

(def endpoints
  {:datasource-columns {:url "/api/datasources/:name/columns"
                        :response (c :datasource-columns)
                        :methods {:get {:effects reads :requires {:op :read, :on [:sources :name]}}}
                        :service :source
                        :query {:table :str}}
   :datasource-preview {:url "/api/datasources/:name/preview"
                        :response (c :datasource-preview)
                        :methods {:get {:effects reads :requires {:op :read, :on [:sources :name]}}}
                        :service :source
                        :query {:as-of :str, :limit [:clamp 1 1000 200]}}
   :datasource-relations {:url "/api/datasources/:name/relations"
                          :response (c :datasource-relations)
                          :methods {:get {:effects reads
                                          :requires {:op :read, :on [:sources :name]}}}
                          :service :source}
   :datasource-sample {:url "/api/datasources/:name/sample"
                       :response (c :datasource-sample)
                       :methods {:get {:effects reads :requires {:op :read, :on [:sources :name]}}}
                       :service :source
                       :query {:table :str, :limit [:clamp 1 1000 20]}}
   :datasource-spec {:url "/api/datasources/:name/spec"
                     :response (c :datasource-spec)
                     :methods {:get {:effects reads :requires {:op :read, :on [:sources :name]}}}
                     :service :source
                     :branch? true
                     :query {:as-of :str}}
   :datasource-tables {:url "/api/datasources/:name/tables"
                       :methods {:get {:response (c :datasource-tables)
                                       :effects reads :requires {:op :read, :on [:sources :name]}}
                                 :post {:request (c :datasource-table-in)
                                        :effects writes
                                        :requires {:op :update, :on [:sources :name]}}}
                       :service :source}
   :datasource-test {:url "/api/datasource-test"
                     :request (c :datasource-test-in)
                     :response (c :datasource-probe)
                     :methods {:post {:effects writes :requires {:op :update}}}
                     :service :source}
   :datasource-writeback-check {:url "/api/datasources/:name/writeback-check"
                                :response (c :writeback-check)
                                :methods {:get {:effects reads
                                                :requires {:op :read, :on [:sources :name]}}}
                                :service :source
                                :branch? true
                                :query {:table :str}}
   :datasources {:url "/api/datasources"
                 :audit :access
                 :methods {:get {:response (c :datasources)
                                 :effects reads :requires m-onto}
                           :post {:effects writes :requires c-schema}}
                 :service :source
                 :branch? true}
   :egress-policies {:url "/api/egress/policies"
                     :audit :access
                     :methods {:get {:response (c :egress-policies)
                                     :effects reads :requires {:op :read}}
                               :post {:request (c :egress-policy-in)
                                      :response (c :egress-policy)
                                      :effects writes :requires {:op :update}}}
                     :service :source}
   :egress-policy-decide {:url "/api/egress/policies/decide"
                          :request (c :egress-decide-in)
                          :response (c :egress-policy)
                          :audit :access
                          :methods {:post
                                    {:effects writes}}
                          :service :source
                          :auth :admin
                          :reason "approving or revoking a network egress policy"}
   :egress-policies-adopt {:url "/api/egress/policies/adopt"
                           :request (c :egress-adopt-in)
                           :response (c :egress-adopted)
                           :audit :access
                           :methods {:post {:effects writes}}
                           :service :source
                           :auth :admin
                           :reason "adopting declared egress as policies"}
   :egress-policies-report {:url "/api/egress/policies/report"
                            :request (c :egress-report-in)
                            :response (c :egress-reported)
                            :methods {:post {:effects writes}}
                            :service :source
                            :auth :admin
                            :reason "the host reports the egress it has paused"}
   :agent-proxy-judge {:url "/api/agent-proxy/judge"
                       :methods {:get {:effects reads}}
                       :service :source
                       :auth :public}
   :agent-proxy-config {:url "/api/agent-proxy/config"
                        :methods {:get {:effects reads}}
                        :service :source
                        :auth :public}
   :agent-proxy-allowlist {:url "/api/agent-proxy/allowlist"
                           :request (c :agent-allowlist-in)
                           :response (c :agent-allowlist)
                           :methods {:post {:effects writes}}
                           :service :source
                           :auth :admin
                           :reason "setting what an egress agent may proxy to"}
   :egress-pubkey {:url "/api/egress/pubkey"
                   :response (c :egress-pubkey)
                   :methods {:get {:effects reads :requires {:op :read}}}
                   :service :source
                   :query {:agent :str :source :str}}
   :egress-status {:url "/api/egress/status"
                   :response (c :egress-status)
                   :methods {:get {:effects reads :requires {:op :read}}}
                   :service :source}
   :source-types {:url "/api/source-types"
                  :response (c :source-types)
                  :methods {:get {:effects reads :requires m-instance}}
                  :service :source}})
