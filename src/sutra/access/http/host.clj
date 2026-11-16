(ns sutra.access.http.host
  (:require [sutra.store.storage.instance-config :as ic]
            [sutra.access.oauth :as oauth]
            [sutra.access.principals :as pr]
            [sutra.access.sessions :as sessions]))

(set! *warn-on-reflection* true)

(def capabilities
  {:config     {:get-cfg      ic/get-cfg
                :on?          ic/on?
                :set-cfg!     ic/set-cfg!
                :set-present! ic/set-present!
                :relay-token  ic/relay-token}
   :sessions   {:record-login!     sessions/record-login!
                :minting           sessions/minting
                :new-token         sessions/new-token
                :mint-clone-token! sessions/mint-clone-token!
                :clone-ttl-seconds sessions/clone-ttl-seconds
                :scoped-to?        sessions/scoped-to?}
   :principals {:fetch              pr/fetch
                :fetch-by-external  pr/fetch-by-external
                :federated-subjects pr/federated-subjects}
   :oauth      {:app-scope           oauth/app-scope
                :code-ttl-seconds    oauth/code-ttl-seconds
                :register-client!    oauth/register-client!
                :fetch-client        oauth/fetch-client
                :clients-of          oauth/clients-of
                :revoke-client!      oauth/revoke-client!
                :rotate-secret!      oauth/rotate-secret!
                :authenticate-client oauth/authenticate-client
                :issuing!            oauth/issuing!
                :grants?             oauth/grants?
                :redirect-allowed?   oauth/redirect-allowed?
                :mint-code!          oauth/mint-code!
                :challenge-problem   oauth/challenge-problem
                :verifier-ok?        oauth/verifier-ok?
                :redeeming           oauth/redeeming
                :record-call!        oauth/record-call!
                :metrics-of          oauth/metrics-of
                :metrics-summary     oauth/metrics-summary}})

(defn wrap-host
  [handler]
  (fn [req] (handler (assoc req :sutra/host capabilities))))
