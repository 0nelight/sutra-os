(ns sutra.sources.proxy.client
  (:require [sutra.sources.proxy.frames :as frames])
  (:import (java.io IOException InputStream OutputStream)
           (java.net InetSocketAddress Socket SocketTimeoutException URI URLEncoder)
           (java.net.http HttpClient WebSocket WebSocket$Listener)
           (java.util.concurrent LinkedBlockingQueue Semaphore TimeUnit)
           (javax.net SocketFactory)))

(set! *warn-on-reflection* true)

(def ^:private connect-seconds 15)

(defn- enc [v] (URLEncoder/encode (str v) "UTF-8"))

(defn- ended!
  [{:keys [^LinkedBlockingQueue incoming opened]} why]
  (deliver opened why)
  (.put incoming ::end))

(defn- on-frame!
  [{:keys [^LinkedBlockingQueue incoming ^Semaphore credit opened] :as link}
   {:keys [type payload]}]
  (case type
    :opened (deliver opened :opened)
    :data   (.put incoming payload)
    :window (.release credit (int (frames/read-credit payload)))
    :close  (ended! link (or (frames/read-edn payload) {:reason :closed}))
    nil))

(defn- listener
  [link]
  (let [collect (frames/collector)]
    (reify WebSocket$Listener
      (onOpen [_ ws] (.request ws 1))
      (onBinary [_ ws buf last?]
        (when-let [whole (collect buf last?)]
          (on-frame! link (frames/decode whole)))
        (.request ws 1)
        nil)
      (onClose [_ _ _ _] (ended! link {:reason :closed}) nil)
      (onError [_ _ e] (ended! link {:reason :error :message (ex-message e)}) nil))))

(defn- send-frame!
  [{:keys [ws lock]} type payload]
  (locking lock
    (.join (.sendBinary ^WebSocket @ws (frames/encode type 0 payload) true))))

(defn- next-chunk
  [^LinkedBlockingQueue incoming so-timeout]
  (let [ms (long @so-timeout)]
    (if (pos? ms)
      (or (.poll incoming ms TimeUnit/MILLISECONDS)
          (throw (SocketTimeoutException. "agent proxy read timed out")))
      (.take incoming))))

(defn- read-into!
  [{:keys [incoming so-timeout] :as link} held ^bytes b off len]
  (when-not (:chunk @held)
    (let [v (next-chunk incoming so-timeout)]
      (reset! held (if (= ::end v) {:done true} {:chunk v :pos 0}))))
  (if (:done @held)
    -1
    (let [{:keys [^bytes chunk pos]} @held
          n (min len (- (alength chunk) pos))]
      (System/arraycopy chunk pos b off n)
      (if (= (+ pos n) (alength chunk))
        (do (reset! held {})
            (send-frame! link :window (frames/credit-bytes (alength chunk))))
        (swap! held assoc :pos (+ pos n)))
      n)))

(defn- input-stream
  [link]
  (let [held (atom {})]
    (proxy [InputStream] []
      (read
        ([] (let [b (byte-array 1)
                  n (read-into! link held b 0 1)]
              (if (neg? n) -1 (bit-and (aget b 0) 0xff))))
        ([b] (read-into! link held b 0 (alength ^bytes b)))
        ([b off len] (if (zero? len) 0 (read-into! link held b off len)))))))

(defn- write-out!
  [{:keys [^Semaphore credit] :as link} ^bytes b off len]
  (loop [off off left len]
    (when (pos? left)
      (let [n (min left frames/chunk-bytes)]
        (.acquire credit (int n))
        (send-frame! link :data (java.util.Arrays/copyOfRange b (int off) (int (+ off n))))
        (recur (+ off n) (- left n))))))

(defn- output-stream
  [link]
  (proxy [OutputStream] []
    (write
      ([b] (if (bytes? b)
             (write-out! link b 0 (alength ^bytes b))
             (write-out! link (byte-array [(unchecked-byte b)]) 0 1)))
      ([b off len] (write-out! link b off len)))
    (flush [] nil)))

(defn- dial!
  [{:keys [url token source]} host port so-timeout]
  (let [link {:ws (atom nil) :lock (Object.) :incoming (LinkedBlockingQueue.)
              :credit (Semaphore. frames/window-bytes) :opened (promise) :so-timeout so-timeout}
        uri  (URI/create (str url "?source=" (enc source) "&host=" (enc host) "&port=" port))
        ws   (try
               (.join (-> (.newWebSocketBuilder (HttpClient/newHttpClient))
                          (.header "Authorization" (str "Bearer " token))
                          (.buildAsync uri (listener link))))
               (catch Exception e
                 (throw (IOException. (str "agent proxy refused " host ":" port ": "
                                           (ex-message e))
                                      e))))]
    (reset! (:ws link) ws)
    (let [r (deref (:opened link) (* 1000 connect-seconds) {:reason :timeout})]
      (when-not (= :opened r)
        (.abort ^WebSocket ws)
        (throw (IOException. (str "agent proxy could not open " host ":" port ": " (pr-str r))))))
    link))

(defn- tunnel-socket
  ^Socket [opts]
  (let [link (atom nil) so-timeout (atom 0) closed (atom false)]
    (proxy [Socket] []
      (connect
        ([a] (reset! link (dial! opts (.getHostString ^InetSocketAddress a)
                                 (.getPort ^InetSocketAddress a) so-timeout)))
        ([a _] (reset! link (dial! opts (.getHostString ^InetSocketAddress a)
                                   (.getPort ^InetSocketAddress a) so-timeout))))
      (setSoTimeout [ms] (reset! so-timeout ms))
      (getSoTimeout [] @so-timeout)
      (setTcpNoDelay [_] nil)
      (setKeepAlive [_] nil)
      (isConnected [] (some? @link))
      (isClosed [] @closed)
      (getInputStream [] (input-stream @link))
      (getOutputStream [] (output-stream @link))
      (close []
        (when (compare-and-set! closed false true)
          (when-let [{:keys [ws] :as l} @link]
            (try (send-frame! l :close nil)
                 (catch Exception e
                   (println (str "[proxy] close not sent to the agent: " (ex-message e)))))
            (.abort ^WebSocket @ws)))))))

(defn socket-factory
  ^SocketFactory [opts]
  (proxy [SocketFactory] []
    (createSocket
      ([] (tunnel-socket opts))
      ([host port]
       (doto (tunnel-socket opts)
         (.connect (InetSocketAddress. (str host) (int port)))))
      ([host port _ _]
       (doto (tunnel-socket opts)
         (.connect (InetSocketAddress. (str host) (int port))))))))
