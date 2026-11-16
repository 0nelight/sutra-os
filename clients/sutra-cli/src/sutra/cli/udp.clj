(ns sutra.cli.udp
  (:import (java.net DatagramPacket DatagramSocket InetSocketAddress SocketTimeoutException)
           (java.time Instant)
           (java.util Arrays Base64)
           (java.util.concurrent ArrayBlockingQueue TimeUnit)))

(def ^:private max-datagram 65535)

(def ^:private retry-max-ms 30000)

(defn record-of
  [^bytes data ^String source-ip source-port ^Instant at]
  (let [text (String. data "UTF-8")
        text? (Arrays/equals data (.getBytes text "UTF-8"))]
    {:message     (if text? text (.encodeToString (Base64/getEncoder) data))
     :encoding    (if text? "utf-8" "base64")
     :source_ip   source-ip
     :source_port source-port
     :received_at (str at)}))

(defn- receive!
  [^DatagramSocket socket ^ArrayBlockingQueue queue stats stop?]
  (let [buf    (byte-array max-datagram)
        packet (DatagramPacket. buf max-datagram)]
    (while (not (stop?))
      (try
        (.setLength packet max-datagram)
        (.receive socket packet)
        (let [data (Arrays/copyOf buf (.getLength packet))
              rec  (record-of data (.getHostAddress (.getAddress packet)) (.getPort packet)
                              (Instant/now))]
          (swap! stats update :received inc)
          (when-not (.offer queue rec)
            (swap! stats update :dropped inc)))
        (catch SocketTimeoutException _ nil)))))

(defn- next-batch
  [^ArrayBlockingQueue queue size flush-ms]
  (when-let [first-rec (.poll queue (long flush-ms) TimeUnit/MILLISECONDS)]
    (let [deadline (+ (System/currentTimeMillis) (long flush-ms))]
      (loop [batch [first-rec]]
        (let [left (- deadline (System/currentTimeMillis))]
          (if (or (>= (count batch) size) (not (pos? left)))
            batch
            (if-let [r (.poll queue left TimeUnit/MILLISECONDS)]
              (recur (conj batch r))
              batch)))))))

(defn- publish-until-settled!
  [publish batch stats stop? report]
  (loop [wait-ms 1000]
    (let [{:keys [status body]} (try (publish batch)
                                     (catch Exception e {:status 0 :body (ex-message e)}))]
      (cond
        (<= 200 status 299) (swap! stats update :published + (count batch))

        (or (zero? status) (<= 500 status 599))
        (do (report (str "platform away (" status "), holding " (count batch) " records"))
            (when-not (stop?)
              (Thread/sleep (long wait-ms))
              (recur (min retry-max-ms (* 2 wait-ms)))))

        :else
        (throw (ex-info (str "the stream refused the batch (HTTP " status "): " (pr-str body))
                        {:status status :body body}))))))

(defn forward!
  [{:keys [port bind batch flush-ms queue-size report-ms publish report stop? on-ready]
    :or   {bind "0.0.0.0" batch 500 flush-ms 1000 queue-size 100000 report-ms 60000
           report (fn [_]) stop? (constantly false) on-ready (fn [_])}}]
  (let [stats  (atom {:received 0 :published 0 :dropped 0})
        queue  (ArrayBlockingQueue. (int queue-size))
        halt   (atom false)
        done?  #(or @halt (stop?))
        socket (doto (DatagramSocket. (InetSocketAddress. ^String bind (int port)))
                 (.setSoTimeout 500)
                 (.setReceiveBufferSize (* 4 1024 1024)))
        reader (future (receive! socket queue stats done?))]
    (try
      (on-ready (.getLocalPort socket))
      (loop [reported (System/currentTimeMillis)]
        (when-let [records (next-batch queue batch flush-ms)]
          (publish-until-settled! publish records stats stop? report))
        (let [now (System/currentTimeMillis)
              due? (>= (- now reported) report-ms)]
          (when due? (report (pr-str @stats)))
          (when-not (and (stop?) (zero? (.size queue)))
            (recur (if due? now reported)))))
      @stats
      (finally
        (reset! halt true)
        @reader
        (.close socket)))))
