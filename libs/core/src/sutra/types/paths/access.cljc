(ns sutra.types.paths.access
  (:require [sutra.types.paths.rules :refer [reads writes m-names m-onto]]))

(def endpoints
  {:access {:url "/api/access"
            :audit :access
            :methods {:get {:effects reads
                            :response :a/access.get.res}}
            :service :platform
            :auth :admin
            :reason "the access overview"}
   :access-explain {:url "/api/access/explain/:user"
                    :methods {:get {:effects reads
                                    :response :a/access-explain.get.res}}
                    :service :platform
                    :auth :admin
                    :reason "why a principal may or may not do an operation on a project"
                    :query {:op :str, :project :str}}
   :access-blocked {:url "/api/access/blocked/:user"
                    :methods {:get {:effects reads}}
                    :service :platform
                    :auth :admin
                    :reason "which markings and organization walls keep a principal from a resource"
                    :query {:resource :str}}
   :markings {:url "/api/markings"
              :methods {:get {:effects reads :requires m-onto}}
              :service :platform
              :reason "the declared markings and how many resources carry each"}
   :marking {:url "/api/markings/:marking"
             :methods {:get {:effects reads :requires m-onto}}
             :service :platform
             :reason "one marking: what carries it, what inherits it, which inputs stop it"}
   :access-role {:url "/api/access/role/:user"
                 :audit :access
                 :methods {:get {:effects reads
                                 :response :a/access-role.get.res}
                           :post {:effects writes
                                  :response :a/access-role.post.res
                                  :request :a/access-role.post.req}}
                 :service :platform
                 :auth :admin
                 :reason "a principal's platform role"}
   :login {:url "/api/login"
           :methods {:post {:effects writes
                            :response :a/login.post.res
                            :request :a/login.post.req}}
           :service :auth
           :auth :public}
   :logout {:url "/api/logout"
            :methods {:post {:effects writes}}
            :service :auth
            :auth :public}
   :me {:url "/api/me"
        :methods {:get {:effects reads
                        :response :a/me.get.res}}
        :service :auth
        :auth :public}
   :sessions {:url "/api/sessions"
              :methods {:get {:effects reads
                              :response :a/sessions.get.res}}
              :service :auth
              :auth :consumer}
   :sessions-end {:url "/api/sessions/end"
                  :audit :access
                  :methods {:post {:effects writes}}
                  :service :auth
                  :auth :consumer}
   :password {:url "/api/password"
              :methods {:post {:effects writes}}
              :service :auth
              :auth :consumer}
   :mfa-status {:url "/api/mfa/status"
                :methods {:get {:effects reads
                                :response :a/mfa-status.get.res}}
                :service :auth
                :auth :consumer}
   :mfa-enroll {:url "/api/mfa/enroll"
                :audit :access
                :methods {:post {:effects writes}}
                :service :auth
                :auth :consumer}
   :mfa-confirm {:url "/api/mfa/confirm"
                 :audit :access
                 :methods {:post {:effects writes}}
                 :service :auth
                 :auth :consumer}
   :mfa-disable {:url "/api/mfa/disable"
                 :audit :access
                 :methods {:post {:effects writes}}
                 :service :auth
                 :auth :consumer}
   :mfa-reset {:url "/api/mfa/reset/:user"
               :audit :access
               :methods {:post {:effects writes}}
               :service :auth
               :auth :admin
               :reason "removes another principal's second factor"}
   :identity-config {:url "/api/auth/:provider/config"
                     :methods {:get {:effects reads}
                               :post {:effects writes
                                      :response :a/identity-config.post.res
                                      :request :a/identity-config.post.req}}
                     :service :auth
                     :auth :admin
                     :reason "identity provider settings"}
   :identity-preview {:url "/api/auth/:provider/preview"
                      :methods {:get {:effects reads}}
                      :service :auth
                      :auth :admin
                      :reason "identity provider directory"}
   :identity-sync {:url "/api/auth/:provider/sync"
                   :methods {:post {:effects writes
                                    :request :a/identity-sync.post.req
                                    :response :a/identity-sync.post.res}}
                   :service :auth
                   :auth :admin
                   :reason "identity provider directory sync"}
   :identity-status {:url "/api/auth/:provider/status"
                     :methods {:get {:effects reads}}
                     :service :auth
                     :auth :admin
                     :reason "identity provider settings"}
   :negotiate {:url "/api/auth/negotiate"
               :methods {:get {:effects writes}
                         :post {:effects writes}}
               :service :auth
               :auth :public}
   :oidc-discovery {:url "/api/oauth/.well-known/openid-configuration"
                    :methods {:get {:effects reads}}
                    :service :auth
                    :auth :public}
   :oidc-jwks {:url "/api/oauth/jwks"
               :methods {:get {:effects reads}}
               :service :auth
               :auth :public}
   :oidc-userinfo {:url "/api/oauth/userinfo"
                   :methods {:get {:effects reads}
                             :post {:effects reads}}
                   :service :auth
                   :auth :public}
   :oauth-introspect {:url "/api/oauth/introspect"
                      :methods {:post {:effects reads}}
                      :service :auth
                      :auth :public}
   :oauth-revoke {:url "/api/oauth/revoke"
                  :methods {:post {:effects writes}}
                  :service :auth
                  :auth :public}
   :principals {:url "/api/principals"
                :audit :access
                :methods {:get {:effects reads :requires m-names
                                :response :a/principals.get.res}}
                :service :platform}
   :organizations {:url "/api/organizations"
                   :audit :access
                   :methods {:get {:effects reads :requires m-onto}}
                   :service :platform}
   :organization {:url "/api/organizations/:name"
                  :audit :access
                  :methods {:get {:effects reads :requires m-onto}}
                  :service :platform}
   :spaces {:url "/api/spaces"
            :methods {:get {:effects reads :requires m-onto}}
            :service :platform}
   :space {:url "/api/spaces/:name"
           :methods {:get {:effects reads :requires m-onto}}
           :service :platform}
   :ontologies {:url "/api/ontologies"
                :methods {:get {:effects reads :requires m-onto}}
                :service :platform}
   :ontology {:url "/api/ontologies/:name"
              :methods {:get {:effects reads :requires m-onto}}
              :service :platform}
   :federation-key {:url "/api/federation/key"
                    :methods {:get {:effects writes}}
                    :service :auth
                    :auth :public}
   :federation-trusts {:url "/api/federation/trusts"
                       :audit :access
                       :methods {:get {:effects reads}
                                 :post {:effects writes}}
                       :service :auth
                       :auth :admin
                       :reason "the instances this one trusts"}
   :federation-trust {:url "/api/federation/trusts/:name"
                      :audit :access
                      :methods {:delete {:effects writes}}
                      :service :auth
                      :auth :admin
                      :reason "revoking a trust between instances"}
   :federation-instances {:url "/api/federation/instances"
                          :methods {:get {:effects reads}}
                          :service :auth
                          :auth :consumer}
   :federation-assert {:url "/api/federation/assert"
                       :methods {:post {:effects writes}}
                       :service :auth
                       :auth :consumer}
   :federation-login {:url "/api/federation/login"
                      :methods {:post {:effects writes}}
                      :service :auth
                      :auth :public}
   :register {:url "/api/register"
              :methods {:post {:effects writes
                               :response :a/register.post.res
                               :request :a/register.post.req}}
              :service :auth
              :auth :admin
              :reason "registering a principal"}})
