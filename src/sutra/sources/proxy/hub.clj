(ns sutra.sources.proxy.hub
  (:require [org.httpkit.server :as http-kit]
            [sutra.sources.proxy.frames :as frames])
  (:import (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

(defonce ^:private state (atom {:agents {} :streams {} :next-id 0}))

(defn link
  [agents agent ch]
  (update agents agent (fnil conj []) ch))

(defn unlink
  [agents agent ch]
  (let [left (filterv #(not= ch %) (get agents agent))]
    (if (seq left) (assoc agents agent left) (dissoc agents agent))))

(defn carrier
  [agents agent]
  (peek (get agents agent)))

(defn linked-agents [] (set (keys (:agents @state))))

(defn- to-bytes ^bytes [^ByteBuffer b]
  (let [a (byte-array (.remaining b))] (.get (.duplicate b) a) a))

(defn- send-frame! [ch type id payload]
  (http-kit/send! ch {:body (to-bytes (frames/encode type id payload))}))

(defn- received [data]
  (frames/decode (ByteBuffer/wrap (if (string? data) (.getBytes ^String data) data))))

(defn- bearer [req]
  (some->> (get-in req [:headers "authorization"]) (re-find #"^Bearer (.+)$") second))

(defn- end-stream!
  [id why]
  (let [[old] (swap-vals! state update :streams dissoc id)]
    (when-let [{:keys [client agent-ch]} (get-in old [:streams id])]
      (when agent-ch (send-frame! agent-ch :close id nil))
      (when client
        (send-frame! client :close 0 (frames/edn-bytes why))
        (http-kit/close client)))))

(defn- from-agent!
  [{:keys [type id payload]}]
  (when-let [{:keys [client]} (get-in @state [:streams id])]
    (send-frame! client type 0 payload)
    (when (= :close type)
      (swap! state update :streams dissoc id)
      (http-kit/close client))))

(defn- from-client!
  [id {:keys [type payload]}]
  (when-let [{:keys [agent-ch]} (get-in @state [:streams id])]
    (if (= :close type)
      (end-stream! id {:reason :client})
      (send-frame! agent-ch type id payload))))

(defn- agent-gone!
  [ch]
  (doseq [[id s] (:streams @state) :when (= ch (:agent-ch s))]
    (swap! state update-in [:streams id] dissoc :agent-ch)
    (end-stream! id {:reason :agent-gone})))

(defn agent-handler
  [{:keys [token-ok? allowlist-of]} req]
  (let [agent (get-in req [:params :agent])
        token (bearer req)]
    (if-not (and agent token (token-ok? agent token))
      {:status 403 :body "agent token refused"}
      (let [{:keys [unavailable allow-listed-cidrs]} (allowlist-of agent)]
        (if unavailable
          {:status 503 :body (str unavailable)}
          (http-kit/as-channel
           req
           {:on-open    (fn [ch]
                          (swap! state update :agents link agent ch)
                          (send-frame! ch :config 0
                                       (frames/edn-bytes
                                        {:allow-listed-cidrs allow-listed-cidrs})))
            :on-receive (fn [_ data] (from-agent! (received data)))
            :on-close   (fn [ch _]
                          (swap! state update :agents unlink agent ch)
                          (agent-gone! ch))}))))))

(defn push-config!
  [agent allow-listed-cidrs]
  (let [links (get-in @state [:agents agent])]
    (doseq [ch links]
      (send-frame! ch :config 0 (frames/edn-bytes {:allow-listed-cidrs allow-listed-cidrs})))
    (count links)))

(defn- open-stream!
  [ch agent-ch request]
  (let [id (:next-id (swap! state update :next-id inc))]
    (swap! state assoc-in [:streams id] {:client ch :agent-ch agent-ch})
    (send-frame! agent-ch :open id (frames/edn-bytes request))
    id))

(defn client-handler
  [{:keys [token-ok? authorize]} req]
  (let [{:keys [source host port]} (:params req)
        token (bearer req)
        ask   {:source source :host host :port (some-> port parse-long)}]
    (if-not (and token (token-ok? token))
      {:status 403 :body "worker token refused"}
      (let [{:keys [agents refused unavailable]} (authorize ask)
            linked (seq (filter (:agents @state) agents))
            stream (atom nil)]
        (cond
          refused      {:status 403 :body (str refused)}
          unavailable  {:status 503 :body (str unavailable)}
          (not linked) {:status 503 :body (str "no agent linked of " (vec agents))}
          :else
          (let [agent-ch (carrier (:agents @state) (rand-nth (vec linked)))]
            (http-kit/as-channel
             req
             {:on-open    (fn [ch] (reset! stream (open-stream! ch agent-ch ask)))
              :on-receive (fn [_ data] (from-client! @stream (received data)))
              :on-close   (fn [_ _] (some-> @stream (end-stream! {:reason :client})))})))))))
