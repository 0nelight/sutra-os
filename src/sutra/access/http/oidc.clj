(ns sutra.access.http.oidc
  (:require [sutra.access.oauth :as oauth]
            [sutra.access.oidc :as oidc]
            [sutra.access.sessions :as sessions]
            [sutra.access.throttle :as throttle]
            [sutra.http.request :as http-request]))

(set! *warn-on-reflection* true)

(defn discovery [req]
  (oidc/json-response 200 (oidc/discovery (oidc/issuer req))))

(defn jwks [{store :sutra/store}]
  (oidc/json-response 200 (oidc/jwks store)))

(defn userinfo
  [{:keys [principal]}]
  (if (:username principal)
    (oidc/json-response 200 {:sub (oidc/subject principal)
                             :preferred_username (str (:username principal))})
    (assoc-in (oidc/json-response 401 {:error "invalid_token"})
              [:headers "WWW-Authenticate"] "Bearer error=\"invalid_token\"")))

(def ^:private invalid-client
  (assoc-in (oidc/json-response 401 {:error "invalid_client"})
            [:headers "WWW-Authenticate"] "Basic realm=\"sutra\""))

(defn- client-authenticated
  [{store :sutra/store :as req} f]
  (let [params      (oidc/wire-params req)
        [cid secret] (oidc/client-credentials req params)
        attempt     {:client (str cid) :ip (http-request/client-ip req)}
        fields      {:client-id cid :provider :oauth-client}]
    (throttle/throttled
     req attempt fields
     (fn []
       (if-let [client (oauth/authenticate-client store cid secret)]
         (f client params)
         (do (throttle/attempted! req attempt false fields)
             invalid-client))))))

(defn introspect
  [{store :sutra/store :as req}]
  (client-authenticated
   req
   (fn [client params]
     (let [info (sessions/introspect store (get params "token"))]
       (oidc/json-response 200 (if (and (:active info)
                                        (= (:client-id client) (:client_id info)))
                                 info
                                 {:active false}))))))

(defn revoke
  [{store :sutra/store :as req}]
  (client-authenticated
   req
   (fn [client params]
     (sessions/revoke-client-token! store (get params "token") (:client-id client))
     (oidc/json-response 200 {}))))

(def module
  {:name        :oidc
   :contributes {:http/routes
                 {:oidc-discovery {:get discovery}
                  :oidc-jwks      {:get jwks}
                  :oidc-userinfo  {:get userinfo :post userinfo}
                  :oauth-introspect {:post introspect}
                  :oauth-revoke   {:post revoke}}}})
