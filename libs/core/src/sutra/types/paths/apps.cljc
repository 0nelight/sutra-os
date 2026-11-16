(ns sutra.types.paths.apps
  (:require [sutra.types.paths.rules
             :refer [reads writes m-onto m-own c-read c-app c-app-preview
                     c-workspace c-stage]]))

(def endpoints
  {:application-file {:url "/api/applications/:name/file"
                      :methods {:get {:effects reads
                                      :requires {:op :read, :on [:applications :name]}}
                                :post {:effects writes :requires c-stage}}
                      :service :platform
                      :branch? true}
   :application-preview {:url "/api/applications/:name/preview"
                         :methods {:post {:effects writes :requires c-app-preview
                                          :response :a/application-preview.post.res
                                          :request :a/application-preview.post.req}}
                         :service :platform
                         :branch? true}
   :application-rename {:url "/api/schema/application/:name/rename"
                        :methods {:post {:effects writes
                                         :requires {:op :update, :on [:applications :name]}}}
                        :service :platform}
   :applications {:url "/api/applications"
                  :methods {:get {:effects reads :requires m-onto
                                  :response :a/applications.get.res}}
                  :service :platform
                  :branch? true}
   :custom-apps {:url "/api/custom-apps"
                 :methods {:get {:effects reads :requires m-onto
                                 :response :a/custom-apps.get.res}}
                 :service :platform
                 :branch? true}
   :custom-app {:url "/api/custom-apps/:name"
                :methods {:get
                          {:effects reads
                           :requires {:op :read, :on [:custom-applications :name]}}}
                :service :platform
                :branch? true}
   :custom-app-sdk {:url "/api/custom-apps/:name/sdk"
                    :methods {:get {:effects reads
                                    :requires {:op :read, :on [:custom-applications :name]}}}
                    :service :platform
                    :branch? true}
   :custom-app-docs {:url "/api/custom-apps/:name/docs"
                     :methods {:get {:effects reads
                                     :requires {:op :read, :on [:custom-applications :name]}}}
                     :service :platform
                     :branch? true}
   :custom-app-metrics {:url "/api/custom-apps/:name/metrics"
                        :methods {:get
                                  {:effects reads
                                   :requires {:op :read, :on [:custom-applications :name]}}}
                        :service :platform
                        :branch? true}
   :custom-app-clients {:url "/api/custom-apps/:name/clients"
                        :methods {:post {:effects writes}}
                        :service :platform
                        :branch? true
                        :auth :admin
                        :reason "minting an authorization client for a custom application"}
   :custom-app-client {:url "/api/custom-apps/:name/clients/:client-id"
                       :methods {:delete {:effects writes}
                                 :post {:effects writes}}
                       :service :platform
                       :branch? true
                       :auth :admin
                       :reason "revoking or rotating an application credential"}
   :oauth-token {:url "/api/oauth/token"
                 :methods {:post {:effects writes}}
                 :service :auth
                 :auth :public}
   :oauth-authorize {:url "/api/oauth/authorize"
                     :methods {:post {:effects writes :requires m-own}}
                     :service :auth}
   :my-application {:url "/api/my-application/:rid"
                    :methods {:get {:effects reads :requires c-app
                                    :response :a/my-application.get.res}}
                    :service :platform
                    :branch? true
                    :query {:scenario :trim, :vars :json, :url :json, :data :trim, :v :trim}}
   :my-application-action-params {:url "/api/my-application/:rid/action-params"
                                  :methods {:post {:effects writes :requires c-app}}
                                  :service :platform
                                  :branch? true
                                  :query {:scenario :trim, :v :trim}}
   :my-application-aggregate {:url "/api/my-application/:rid/aggregate"
                              :methods {:post {:effects reads :requires c-app
                                               :response :a/my-application-aggregate.post.res
                                               :request :a/my-application-aggregate.post.req}}
                              :service :oss
                              :branch? true
                              :query {:scenario :trim, :v :trim}}
   :my-application-around {:url "/api/my-application/:rid/around"
                           :methods {:post {:effects reads :requires c-app
                                            :request :a/my-application-around.post.req
                                            :response :a/my-application-around.post.res}}
                           :service :oss
                           :branch? true
                           :query {:scenario :trim, :v :trim}}
   :my-application-search {:url "/api/my-application/:rid/search"
                           :methods {:post {:effects reads :requires c-app}}
                           :service :oss
                           :branch? true
                           :query {:scenario :trim, :v :trim}}
   :my-application-facets {:url "/api/my-application/:rid/facets"
                           :methods {:post {:effects reads :requires c-app}}
                           :service :oss
                           :branch? true
                           :query {:scenario :trim, :v :trim}}
   :my-application-users {:url "/api/my-application/:rid/users"
                          :methods {:post {:effects reads :requires c-app}}
                          :service :platform
                          :branch? true}
   :my-application-blob {:url "/api/my-application/:rid/blob/:hash"
                         :methods {:get {:effects reads :requires c-app}}
                         :service :platform
                         :branch? true}
   :my-application-upload-status {:url "/api/my-application/:rid/upload-status"
                                  :methods {:get {:effects reads :requires c-app}}
                                  :service :platform
                                  :branch? true}
   :my-application-edit-history {:url "/api/my-application/:rid/edit-history"
                                 :methods {:post {:effects reads :requires c-app}}
                                 :service :oss
                                 :branch? true
                                 :query {:scenario :trim, :v :trim}}
   :my-application-data-freshness {:url "/api/my-application/:rid/data-freshness"
                                   :methods {:post {:effects reads :requires c-app
                                                    :request :a/app-data-freshness.req
                                                    :response :a/app-data-freshness.res}}
                                   :service :oss
                                   :branch? true
                                   :query {:scenario :trim, :v :trim}}
   :my-application-export {:url "/api/my-application/:rid/export"
                           :methods {:post {:effects writes :requires c-app}}
                           :deadline :unbounded
                           :service :platform}
   :my-application-objects {:url "/api/my-application/:rid/objects"
                            :methods {:post {:effects reads :requires c-app}}
                            :service :oss
                            :branch? true
                            :query {:scenario :trim, :v :trim}}
   :my-application-function {:url "/api/my-application/:rid/function"
                             :methods {:post {:effects reads :requires c-app}}
                             :service :oss
                             :branch? true
                             :query {:scenario :trim, :v :trim}}
   :my-application-scenario {:url "/api/my-application/:rid/scenarios/:uuid"
                             :methods {:delete {:effects writes :requires c-app}}
                             :service :platform}
   :my-application-scenario-actions {:url "/api/my-application/:rid/scenarios/:uuid/actions"
                                     :methods {:get {:effects reads :requires c-app}}
                                     :service :platform}
   :my-application-scenario-apply {:url "/api/my-application/:rid/scenarios/:uuid/apply"
                                   :methods {:post {:effects writes :requires c-app}}
                                   :service :platform}
   :my-application-scenario-duplicate {:url "/api/my-application/:rid/scenarios/:uuid/duplicate"
                                       :methods {:post {:effects writes :requires c-app}}
                                       :service :platform}
   :my-application-scenario-rebuild {:url "/api/my-application/:rid/scenarios/:uuid/rebuild"
                                     :methods {:post {:effects writes :requires c-app}}
                                     :service :platform}
   :my-application-scenario-seal {:url "/api/my-application/:rid/scenarios/:uuid/seal"
                                  :methods {:post {:effects writes :requires c-app}}
                                  :service :platform}
   :my-application-scenarios {:url "/api/my-application/:rid/scenarios"
                              :methods {:get {:effects reads :requires c-app
                                              :response :a/my-application-scenarios.get.res}
                                        :post {:effects writes :requires c-app}}
                              :service :platform}
   :my-application-generate {:url "/api/my-application/:rid/generate"
                             :methods {:post {:effects writes :requires c-app}}
                             :service :platform
                             :branch? true
                             :query {:v :trim}}
   :my-application-comments {:url "/api/my-application/:rid/comments"
                             :methods {:post {:effects writes :requires c-app}}
                             :service :platform
                             :branch? true
                             :query {:v :trim}}
   :my-application-resources {:url "/api/my-application/:rid/resources"
                              :methods {:post {:effects reads :requires c-app}}
                              :service :oss
                              :branch? true
                              :query {:v :trim}}
   :my-application-linked-resources {:url "/api/my-application/:rid/linked-resources"
                                     :methods {:post {:effects writes :requires c-app}}
                                     :service :oss
                                     :branch? true
                                     :query {:v :trim}}
   :my-application-series {:url "/api/my-application/:rid/series"
                           :methods {:post {:effects reads :requires c-app}}
                           :service :oss
                           :branch? true
                           :query {:scenario :trim, :v :trim}}
   :my-application-submit {:url "/api/my-application/:rid/submit"
                           :methods {:post {:effects writes :requires c-app}}
                           :service :actions
                           :query {:scenario :trim, :v :trim}}
   :my-application-upload {:url "/api/my-application/:rid/upload"
                           :methods {:post {:effects writes :requires c-app}}
                           :process :conductor
                           :service :platform}
   :my-applications {:url "/api/my-applications"
                     :methods {:get {:effects reads
                                     :response :a/my-applications.get.res}}
                     :service :platform
                     :auth :consumer
                     :branch? true}
   :my-workspaces {:url "/api/my-workspaces"
                   :methods {:get {:effects reads
                                   :response :a/my-workspaces.get.res}}
                   :service :platform
                   :auth :consumer
                   :branch? true}
   :my-workspace {:url "/api/my-workspace/:rid"
                  :methods {:get {:effects reads :requires c-workspace
                                  :response :a/my-workspace.get.res}}
                  :service :platform
                  :branch? true}
   :my-workspace-around {:url "/api/my-workspace/:rid/around"
                         :methods {:post {:effects reads :requires c-workspace
                                          :request :a/my-workspace-around.post.req
                                          :response :a/my-workspace-around.post.res}}
                         :service :oss
                         :branch? true}
   :my-workspace-search {:url "/api/my-workspace/:rid/search"
                         :methods {:get {:effects reads :requires c-workspace
                                         :response :a/my-workspace-search.get.res}}
                         :service :oss
                         :branch? true
                         :query {:q :raw, :type :raw, :limit [:long 50], :offset [:long 0]}}
   :workspace-editable {:url "/api/workspaces/editable"
                        :methods {:get {:effects reads :requires c-workspace
                                        :response :a/workspace-editable.get.res}}
                        :service :platform
                        :branch? true}
   :workspace-file {:url "/api/workspaces/:name/file"
                    :methods {:get {:effects reads :requires c-workspace
                                    :response :a/workspace-file.get.res}
                              :post {:effects writes :requires c-workspace
                                     :response :a/workspace-file.post.res
                                     :request :a/workspace-file.post.req}}
                    :service :platform
                    :branch? true}
   :object-type-usage {:url "/api/object-types/:type/usage"
                       :methods {:get {:effects reads
                                       :requires {:op :read, :on [:object-types :type]}
                                       :response :a/object-type-usage.get.res}}
                       :service :oms
                       :branch? true}
   :object-view-standard {:url "/api/object-view/standard"
                          :methods {:post {:effects reads :requires c-read
                                           :request :a/object-view-standard.post.req
                                           :response :a/object-view-standard.post.res}}
                          :service :oss
                          :branch? true}
   :home-page {:url "/api/admin/home-page"
               :methods {:get {:effects reads}
                         :post {:effects writes}}
               :service :platform
               :auth :admin
               :branch? true
               :reason "where every user lands after signing in, per organization and group"}
   :default-workspace-set {:url "/api/admin/default-workspace"
                           :methods {:post {:effects writes}}
                           :service :platform
                           :auth :admin
                           :branch? true
                           :reason "which workspace a group opens at /workspace"}
   :my-home {:url "/api/my-home"
             :methods {:get {:effects reads
                             :response :a/my-home.get.res}}
             :service :platform
             :auth :consumer
             :branch? true}})
