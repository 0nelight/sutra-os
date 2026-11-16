(ns ^:api platform.agent-proxy-api-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [org.httpkit.client :as http]
            [org.httpkit.server :as http-kit]
            [support.pod :as pod]
            [sutra.foundation.egress :as egress]
            [sutra.foundation.rpc :as rpc]
            [sutra.sources.proxy.agent :as agent]
            [sutra.sources.proxy.client :as client]
            [sutra.sources.proxy.hub :as hub]
            [sutra.sources.proxy.server :as server])
  (:import (java.io IOException)
           (java.net InetSocketAddress)))

(use-fixtures :each pod/with-mem-pod-auth)

(defn- ask [path token]
  @(http/get (str (pod/base-url) path)
             (cond-> {:as :text :headers {"Accept" "application/edn"}}
               token (assoc-in [:headers "Authorization"] (str "Bearer " token)))))

(defn- hub-token [] (egress/agent-token egress/proxy-hub-peer))

(defn- with-hub
  [f]
  (let [s (http-kit/run-server #'server/handler {:port 0 :legacy-return-value? false})]
    (try
      (with-redefs-fn {#'server/api-url (fn [] (pod/base-url))}
        #(f (str "127.0.0.1:" (http-kit/server-port s))))
      (finally (http-kit/server-stop! s)))))

(defn- linked? []
  (loop [n 200]
    (cond
      (contains? (hub/linked-agents) "nas") true
      (pos? n) (do (Thread/sleep 25) (recur (dec n)))
      :else false)))

(deftest only-the-hub-may-ask-the-api
  (is (= 403 (:status (ask "/api/agent-proxy/judge?source=nope&host=h&port=1" nil))))
  (is (= 403 (:status (ask "/api/agent-proxy/config?agent=nas" "wrong"))))
  (let [{:keys [status body]} (ask "/api/agent-proxy/judge?source=nope&host=h&port=1"
                                   (hub-token))]
    (is (= 200 status))
    (is (= {:refused "no source nope"} (edn/read-string body)))))

(deftest an-agent-links-and-a-stream-is-judged-through-the-api
  (with-hub
    (fn [address]
      (let [stop (agent/start! {:url         (str "ws://" address "/api/agent-proxy/agent")
                                :token       (egress/agent-token "nas")
                                :agent       "nas"
                                :config-file "/nonexistent/agentProxyConfig.edn"})]
        (try
          (is (linked?))
          (testing "the api reads the hub's links"
            (is (some #{"nas"} (:linked (rpc/call! {:url     (str "http://" address "/linked")
                                                    :peer    "hub"
                                                    :timeout 2000
                                                    :token   (hub-token)}
                                                   {})))))
          (testing "a source the api does not know opens nothing"
            (let [f (client/socket-factory
                     {:url    (str "ws://" address "/api/agent-proxy/stream")
                      :token  (egress/agent-token egress/proxy-client-peer)
                      :source "nope"})]
              (is (thrown? IOException
                           (.connect (.createSocket f) (InetSocketAddress. "127.0.0.1" 445)
                                     1000)))))
          (finally (stop)))))))
