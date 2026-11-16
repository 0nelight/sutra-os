(ns sutra.sources.proxy.server
  (:require [clojure.edn :as edn]
            [sutra.foundation.egress :as egress]
            [sutra.foundation.env :as env]
            [sutra.foundation.outbound :as outbound]
            [sutra.foundation.rpc :as rpc]
            [sutra.sources.proxy.hub :as hub]))

(set! *warn-on-reflection* true)

(def ^:private ask-ms 10000)

(def ^:private threads 32)

(defonce ^:private server* (atom nil))

(defn- api-url
  []
  (or (not-empty (env/getenv "SUTRA_AGENT_PROXY_API_URL")) "http://127.0.0.1:8030"))

(defn- ask-api
  [path params]
  (let [{:keys [status body error]}
        (outbound/get! (str (api-url) path)
                       {:query-params params
                        :timeout      ask-ms
                        :as           :text
                        :headers      {"Authorization" (str "Bearer " (egress/agent-token
                                                                       egress/proxy-hub-peer))
                                       "Accept"        "application/edn"}})]
    (if (= 200 status)
      (edn/read-string body)
      {:unavailable (str path " answered " (or status (ex-message error)))})))

(def ^:private routes
  {"/linked" (fn [_] {:linked (vec (sort (hub/linked-agents)))})
   "/config" (fn [{:keys [agent allow-listed-cidrs]}]
               {:reached (hub/push-config! agent allow-listed-cidrs)})})

(defn handler
  [req]
  (let [with-params #(assoc % :params (rpc/query-params %))]
    (case (:uri req)
      "/api/agent-proxy/agent"
      (hub/agent-handler
       {:token-ok?    #(rpc/same-token? %2 (egress/agent-token %1))
        :allowlist-of #(ask-api "/api/agent-proxy/config" {:agent %})}
       (with-params req))

      "/api/agent-proxy/stream"
      (hub/client-handler
       {:token-ok? #(rpc/same-token? % (egress/agent-token egress/proxy-client-peer))
        :authorize #(ask-api "/api/agent-proxy/judge" %)}
       (with-params req))

      (rpc/handler routes req {:token (egress/agent-token egress/proxy-hub-peer)}))))

(defn -main
  [& _args]
  (let [port (parse-long (or (env/getenv "SUTRA_AGENT_PROXY_HUB_PORT") "8035"))]
    (rpc/serve! server* #'handler {:port port :ip "0.0.0.0" :thread threads})
    (println (str "agent proxy hub on 0.0.0.0:" port))
    @(promise)))
