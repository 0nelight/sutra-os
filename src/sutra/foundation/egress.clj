(ns sutra.foundation.egress
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.foundation.env :as env]
            [sutra.foundation.profile :as prof]
            [sutra.foundation.rpc :as rpc]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(def ^:dynamic *agent-url*
  (or (env/getenv "SUTRA_EGRESS_URL") "http://127.0.0.1:8731"))

(def default-agent "default")

(defn parse-agents
  [s]
  (into {} (for [pair (str/split (str/trim (str s)) #"[,\s]+")
                 :when (seq pair)
                 :let [[n u] (str/split pair #"=" 2)]]
             (if (and (re-matches #"[a-z0-9-]+" (str n)) (seq u) (not= n default-agent))
               [n u]
               (anomaly/raise! :fault (str "SUTRA_EGRESS_AGENTS: " (pr-str pair) " is not name=url")
                                      {:pair pair})))))

(def ^:dynamic *agents*
  (parse-agents (env/getenv "SUTRA_EGRESS_AGENTS")))

(defn agent-of
  [spec]
  (or (some-> (:agent spec) name) default-agent))

(defn agent-names []
  (into [default-agent] (sort (keys *agents*))))

(defn agent-url
  [agent]
  (if (= agent default-agent)
    *agent-url*
    (or (get *agents* agent)
        (refusal/refuse! :refusal.foreign/unknown-egress-agent {:agent agent :known (agent-names)}
                         {:known (str/join ", " (agent-names))}))))

(defonce ^:private egress-master*
  (delay
    (or (some-> (env/getenv "SUTRA_EGRESS_MASTER_FILE") not-empty slurp str/trim not-empty)
        (some-> (env/getenv "SUTRA_EGRESS_MASTER") str/trim not-empty)
        (str (java.util.UUID/randomUUID) (java.util.UUID/randomUUID)))))

(defn egress-master
  []
  @egress-master*)

(defonce ^:private agent-tokens*
  (delay (some-> (env/getenv "SUTRA_EGRESS_TOKENS_FILE") not-empty slurp edn/read-string)))

(defn agent-token
  [agent]
  (if-let [tokens @agent-tokens*]
    (or (get tokens (str agent))
        (refusal/refuse! :refusal.foreign/unknown-egress-agent
                         {:agent agent :known (vec (keys tokens))}
                         {:known (str/join ", " (keys tokens))}))
    (rpc/token-for (egress-master) agent)))

(def proxy-client-peer
  "agent-proxy-worker")

(def proxy-hub-peer
  "agent-proxy-hub")

(defn proxy-hub-url
  []
  (or (not-empty (env/getenv "SUTRA_AGENT_PROXY_HUB_URL")) "http://127.0.0.1:8035"))

(def agent-call-ms 120000)

(def ^:private transport-margin-ms 5000)

(defn post-to
  [agent path payload]
  (let [t    (some-> (:timeout-ms payload) long (max 1) (min agent-call-ms))
        span (prof/current-span)]
    (rpc/call! (cond-> {:url (str (agent-url agent) path) :peer (str "egress agent " agent)
                        :token (agent-token agent)}
                 t (assoc :timeout (+ t transport-margin-ms)))
               (cond-> (dissoc payload :timeout-ms)
                 t    (assoc :timeout-ms t)
                 span (assoc :span span)))))

(defn post [path payload]
  (post-to (agent-of (:spec payload)) path payload))

(defn chat!
  ([spec payload] (chat! spec payload nil))
  ([spec payload {:keys [timeout-ms]}]
   (post "/chat" (cond-> {:spec spec :payload payload} timeout-ms (assoc :timeout-ms timeout-ms)))))

(defn embed!
  ([spec payload] (embed! spec payload nil))
  ([spec payload {:keys [timeout-ms]}]
   (post "/embed"
         (cond-> {:spec spec :payload payload} timeout-ms (assoc :timeout-ms timeout-ms)))))

(defn list-models!
  [spec models-path]
  (:body (post "/models" {:spec spec :models-path models-path})))
