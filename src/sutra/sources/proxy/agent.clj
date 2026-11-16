(ns sutra.sources.proxy.agent
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [sutra.sources.proxy.frames :as frames])
  (:import (java.net InetAddress InetSocketAddress Socket URI)
           (java.net.http HttpClient WebSocket WebSocket$Listener)
           (java.nio.file Files LinkOption Path)
           (java.time Duration)
           (java.util.concurrent LinkedBlockingQueue Semaphore)))

(set! *warn-on-reflection* true)

(def ^:private connect-ms 10000)

(def ^:private redial-ms 3000)

(def ^:private ping-ms 15000)

(def ^:private dead-after-ms 45000)

(defn- padded-bits
  [^InetAddress a]
  (let [raw   (.getAddress a)
        bits  (.toString (BigInteger. 1 raw) 2)
        width (* 8 (alength raw))]
    (str (apply str (repeat (- width (count bits)) \0)) bits)))

(defn in-cidr?
  [^InetAddress address cidr]
  (let [[ip prefix] (.split (str cidr) "/")
        base (InetAddress/getByName ip)
        bits (padded-bits base)
        n    (if prefix (Integer/parseInt prefix) (count bits))]
    (and (= (alength (.getAddress base)) (alength (.getAddress address)))
         (= (subs bits 0 n) (subs (padded-bits address) 0 n)))))

(defn allowed?
  [entries ^InetAddress address port]
  (or (nil? entries)
      (boolean (some (fn [{:keys [cidr-block] from :port to :end-port}]
                       (and (in-cidr? address cidr-block)
                            (<= from port (or to from))))
                     entries))))

(defn- writable-chain
  [^Path p]
  (first (filter #(Files/isWritable %) (take-while some? (iterate #(.getParent ^Path %) p)))))

(defn local-config
  [path]
  (let [f (io/file path)]
    (if-not (Files/exists (.toPath f) (into-array LinkOption []))
      {:entries nil}
      (if-let [w (writable-chain (.toPath f))]
        {:refused (str w " is writable by the agent")}
        {:entries (:allow-listed-cidrs (edn/read-string (slurp f)))}))))

(defn- send-frame!
  [{:keys [ws lock]} type id payload]
  (locking lock
    (.join (.sendBinary ^WebSocket @ws (frames/encode type id payload) true))))

(defn- close-socket!
  [^Socket socket]
  (try (.close socket)
       (catch java.io.IOException e
         (println (str "[agent-proxy] socket did not close: " (ex-message e))))))

(defn- close-stream!
  [{:keys [streams] :as link} id reason]
  (when-let [{:keys [^Socket socket ^LinkedBlockingQueue out]} (get @streams id)]
    (swap! streams dissoc id)
    (.offer out ::end)
    (close-socket! socket)
    (try (send-frame! link :close id (frames/edn-bytes reason))
         (catch Exception e
           (println (str "[agent-proxy] close of stream " id " not sent: " (ex-message e)))))))

(defn- pump-out!
  [link id ^Socket socket ^Semaphore credit]
  (try
    (let [in  (.getInputStream socket)
          buf (byte-array frames/chunk-bytes)]
      (loop []
        (let [n (.read in buf)]
          (when-not (neg? n)
            (.acquire credit n)
            (send-frame! link :data id (java.util.Arrays/copyOf buf n))
            (recur)))))
    (close-stream! link id {:reason :eof})
    (catch Exception e
      (close-stream! link id {:reason :io :message (ex-message e)}))))

(defn- pump-in!
  [link id ^Socket socket ^LinkedBlockingQueue q]
  (try
    (let [out (.getOutputStream socket)]
      (loop []
        (let [b (.take q)]
          (when-not (= ::end b)
            (.write out ^bytes b)
            (.flush out)
            (send-frame! link :window id (frames/credit-bytes (alength ^bytes b)))
            (recur)))))
    (catch Exception e
      (close-stream! link id {:reason :io :message (ex-message e)}))))

(defn- deny!
  [link id why]
  (send-frame! link :close id (frames/edn-bytes {:status 403 :reason why})))

(defn- open-stream!
  [{:keys [streams local platform] :as link} id {:keys [host port]}]
  (try
    (let [address (InetAddress/getByName host)]
      (cond
        (not (allowed? @platform address port)) (deny! link id :agent-allowlist)
        (not (allowed? local address port))     (deny! link id :local-allowlist)
        :else
        (let [socket (doto (Socket.)
                       (.setTcpNoDelay true)
                       (.connect (InetSocketAddress. address (int port)) (int connect-ms)))
              credit (Semaphore. frames/window-bytes)
              q      (LinkedBlockingQueue.)]
          (swap! streams assoc id {:socket socket :credit credit :out q})
          (send-frame! link :opened id nil)
          (future (pump-in! link id socket q))
          (future (pump-out! link id socket credit)))))
    (catch Exception e
      (send-frame! link :close id (frames/edn-bytes {:reason :connect :message (ex-message e)})))))

(defn- on-frame!
  [{:keys [streams platform] :as link} {:keys [type id payload]}]
  (case type
    :config (reset! platform (:allow-listed-cidrs (frames/read-edn payload)))
    :open   (let [req (frames/read-edn payload)] (future (open-stream! link id req)))
    :data   (when-let [^LinkedBlockingQueue q (get-in @streams [id :out])]
              (.offer q payload))
    :window (when-let [^Semaphore c (get-in @streams [id :credit])]
              (.release c (int (frames/read-credit payload))))
    :close  (when-let [{:keys [^Socket socket ^LinkedBlockingQueue out]} (get @streams id)]
              (swap! streams dissoc id)
              (.offer out ::end)
              (close-socket! socket))
    nil))

(defn- listener
  [{:keys [last-pong] :as link} closed]
  (let [collect (frames/collector)]
    (reify WebSocket$Listener
      (onOpen [_ ws] (.request ws 1))
      (onPong [_ ws _]
        (reset! last-pong (System/currentTimeMillis))
        (.request ws 1)
        nil)
      (onBinary [_ ws buf last?]
        (when-let [whole (collect buf last?)]
          (on-frame! link (frames/decode whole)))
        (.request ws 1)
        nil)
      (onClose [_ _ _ _] (deliver closed :closed) nil)
      (onError [_ _ e] (deliver closed e) nil))))

(defn- drop-streams!
  [{:keys [streams]}]
  (doseq [[_ {:keys [^Socket socket ^LinkedBlockingQueue out]}] @streams]
    (.offer out ::end)
    (close-socket! socket))
  (reset! streams {}))

(defn- watch-link!
  [{:keys [last-pong lock]} ^WebSocket ws closed]
  (loop []
    (when (= ::open (deref closed ping-ms ::open))
      (if (> (- (System/currentTimeMillis) @last-pong) dead-after-ms)
        (do (println "[agent-proxy] the platform stopped answering — dialling again")
            (.abort ws))
        (do (try (locking lock (.join (.sendPing ws (java.nio.ByteBuffer/allocate 0))))
                 (catch Exception e
                   (println (str "[agent-proxy] ping failed, link given up: " (ex-message e)))
                   (.abort ws)))
            (when-not (.isOutputClosed ws) (recur)))))))

(defn- link-once!
  [^HttpClient client ^URI uri token link live agent]
  (let [closed (promise)
        ws     (.join (-> (.newWebSocketBuilder client)
                          (.header "Authorization" (str "Bearer " token))
                          (.buildAsync uri (listener link closed))))]
    (reset! (:ws link) ws)
    (reset! live ws)
    (reset! (:last-pong link) (System/currentTimeMillis))
    (println (str "[agent-proxy] " agent " linked to " uri))
    (watch-link! link ws closed)))

(defn start!
  [{:keys [url token agent config-file]}]
  (let [{:keys [entries refused]} (local-config config-file)]
    (if refused
      (do (println (str "[agent-proxy] not running: " refused)) nil)
      (let [stop?  (atom false)
            live   (atom nil)
            link   {:ws (atom nil) :lock (Object.) :streams (atom {})
                    :platform (atom nil) :local entries :last-pong (atom 0)}
            uri    (URI/create (str url "?agent=" agent))
            client (-> (HttpClient/newBuilder)
                       (.connectTimeout (Duration/ofMillis connect-ms))
                       (.build))]
        (future
          (while (not @stop?)
            (try
              (link-once! client uri token link live agent)
              (catch Exception e
                (println (str "[agent-proxy] cannot reach " url ": " (ex-message e)))))
            (drop-streams! link)
            (when-not @stop? (Thread/sleep (long redial-ms)))))
        (fn []
          (reset! stop? true)
          (some-> ^WebSocket @live (.abort))
          (drop-streams! link))))))
