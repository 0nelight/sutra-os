(ns ^:unit platform.egress-chat-test
  (:require [sutra.foundation.egress :as egress-agents]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [org.httpkit.server :as http-kit]
            [sutra.sources.egress :as egress]))

(deftest the-agent-relays-a-chat-to-its-model
  (let [seen  (atom nil)
        srv   (http-kit/run-server (fn [{:keys [uri headers body]}]
                                     (reset! seen {:uri uri :auth (get headers "authorization")
                                                   :body (json/read-str (slurp body))})
                                     {:status 200 :body "{\"choices\":[]}"})
                                   {:port 0 :ip "127.0.0.1" :legacy-return-value? false})
        own   @#'egress/running*
        before @own]
    (swap! own assoc :agent egress-agents/default-agent)
    (try
      (let [r ((get @#'egress/routes "/chat")
               {:spec {:source-name :llm :external-transform true :api-key "k"
                       :base-url (str "http://127.0.0.1:" (http-kit/server-port srv) "/v1")}
                :payload {:model "m" :messages []}
                :timeout-ms 5000})]
        (is (= 200 (:status r)))
        (is (= "/v1/chat/completions" (:uri @seen)))
        (is (= "Bearer k" (:auth @seen)))
        (is (= "m" (get-in @seen [:body "model"]))))
      (finally
        (reset! own before)
        (http-kit/server-stop! srv)))))
