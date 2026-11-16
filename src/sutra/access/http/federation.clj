(ns sutra.access.http.federation
  (:require [sutra.kit.refusal :as refusal]
            [sutra.access.federation :as fed]
            [sutra.access.http.auth :as auth]
            [sutra.access.principals :as pr]
            [sutra.access.throttle :as throttle]
            [sutra.http.request :as http-request]
            [sutra.http.responses :refer [ok]]))

(set! *warn-on-reflection* true)

(defn key-handler [{store :sutra/store}]
  (ok (fed/identity-of store)))

(defn trusts-handler
  [{:keys [request-method body-params principal] store :sutra/store}]
  (if (= :post request-method)
    (ok (fed/declare-trust! store (:username principal) body-params))
    (ok {:trusts (fed/trust-rows store)})))

(defn trust-handler
  [{{nm :name} :path-params store :sutra/store}]
  (if (fed/revoke-trust! store nm)
    (ok {:revoked nm})
    (refusal/refused :refusal.access/no-trust-named {} {:nm nm})))

(defn instances-handler [{store :sutra/store}]
  (ok {:instances (fed/instances store)}))

(defn assert-handler
  [{:keys [body-params principal] store :sutra/store}]
  (cond
    (some? (:scope principal))
    (refusal/refused :refusal.access/only-a-sign-in-session-2 {})

    (nil? (:username principal))
    {:status 401 :body {:error "authentication required"}}

    :else
    (if-let [a (fed/assertion store (str (:to body-params)) (:username principal)
                              (System/currentTimeMillis))]
      (ok a)
      (refusal/refused :refusal.access/no-outbound-trust-named {} {:to (:to body-params)}))))

(defn- local-principal
  [store {:keys [trust subject local]}]
  (if local
    (pr/fetch store local)
    (pr/fetch-by-external store (str "instance:" trust) subject)))

(defn- federated-login
  [store req attempt]
  (let [now (System/currentTimeMillis)
        v   (fed/verify store (:assertion (:body-params req)) now)
        fields {:provider :federation :username (:subject v) :trust (:trust v)}]
    (cond
      (:refused v)
      (do (throttle/attempted! req attempt false (assoc fields :reason (:refused v)))
          {:status 401 :body {:error (:refused v)}})

      (not (fed/spend! store (:claims v) now))
      (do (throttle/attempted! req attempt false (assoc fields :reason "replayed"))
          {:status 401 :body {:error "assertion already used"}})

      :else
      (let [p     (local-principal store v)
            token (auth/establish-session!
                   store {:principal p :provider :federation
                          :subject (str (:trust v) ":" (:subject v))
                          :client (auth/client-of req)})]
        (throttle/attempted! req attempt (some? token) fields)
        (if token
          {:status 200
           :headers {"Set-Cookie" (auth/session-cookies req token)}
           :body {:token token :username (:username p) :via (:trust v)}}
          (refusal/refused :refusal.access/maps-to-no-local-principal {} {:trust (:trust v) :subject (:subject v)}))))))

(defn login-handler
  [{store :sutra/store :as req}]
  (let [attempt {:ip (http-request/client-ip req)}]
    (throttle/throttled req attempt {:provider :federation}
                        #(federated-login store req attempt))))

(def module
  {:name        :federation
   :contributes {:http/routes
                 {:federation-key       {:get key-handler}
                  :federation-trusts    {:get trusts-handler :post trusts-handler}
                  :federation-trust     {:delete trust-handler}
                  :federation-instances {:get instances-handler}
                  :federation-assert    {:post assert-handler}
                  :federation-login     {:post login-handler}}}})
