(ns sutra.sources.imap-idle
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.sources.tls :as tls])
  (:import (java.io BufferedInputStream ByteArrayOutputStream InputStream OutputStream)
           (java.net InetSocketAddress Socket SocketException SocketTimeoutException)
           (java.nio.charset StandardCharsets)
           (java.util Base64)
           (javax.net.ssl SSLSocketFactory)))

(set! *warn-on-reflection* true)

(def ^:const idle-max-ms
  (* 25 60 1000))

(def ^:private connect-ms 10000)

(def ^:private reply-ms 30000)

(defn- mutf7-chunk
  [^String run]
  (str "&"
       (-> (.encodeToString (Base64/getEncoder) (.getBytes run StandardCharsets/UTF_16BE))
           (str/replace "/" ",")
           (str/replace #"=+$" ""))
       "-"))

(defn mailbox-name
  [s]
  (->> (re-seq #"[\x20-\x7e]+|[^\x20-\x7e]+" (str s))
       (map (fn [run]
              (if (re-matches #"[\x20-\x7e]+" run) (str/replace run "&" "&-") (mutf7-chunk run))))
       str/join))

(defn quoted
  [s]
  (let [s (str s)]
    (when (re-find #"[\r\n\u0000]" s)
      (refusal/refuse! :refusal.sources/imap-quoted-control {}))
    (str "\"" (str/replace s #"[\\\"]" #(str "\\" %)) "\"")))

(defn untagged
  [line]
  (or (when-let [[_ n what] (re-matches #"(?i)\*\s+(\d+)\s+(EXISTS|RECENT|EXPUNGE)\b.*" line)]
        {(keyword (str/lower-case what)) (parse-long n)})
      (when-let [[_ code n] (re-matches #"(?i)\*\s+OK\s+\[(UIDNEXT|UIDVALIDITY)\s+(\d+)\].*" line)]
        {(keyword (str/lower-case code)) (parse-long n)})))

(defn tagged
  [tag line]
  (when-let [[_ s] (re-matches (re-pattern (str "(?i)" tag "\\s+(OK|NO|BAD)\\b.*")) line)]
    (keyword (str/lower-case s))))

(defn absorb
  [state line]
  (let [{:keys [exists expunge uidnext uidvalidity]} (untagged line)]
    (cond-> state
      exists      (assoc :exists exists)
      expunge     (update :exists #(max 0 (dec (or % 0))))
      uidnext     (assoc :uidnext uidnext)
      uidvalidity (assoc :uidvalidity uidvalidity))))

(defn moved?
  [prior now]
  (boolean (and prior
                (or (not= (:uidvalidity prior) (:uidvalidity now))
                    (and (:uidnext prior) (:uidnext now) (> (:uidnext now) (:uidnext prior)))))))

(defn arrival?
  [state line]
  (let [{:keys [exists recent]} (untagged line)]
    (boolean (or (and exists (> exists (or (:exists state) 0)))
                 (and recent (pos? recent))))))

(defn- read-line!
  [^InputStream in]
  (let [buf (ByteArrayOutputStream.)]
    (loop [prev -1]
      (let [b (.read in)]
        (cond
          (neg? b) (throw (SocketException. "IMAP server closed the connection"))
          (and (= b 10) (= prev 13))
          (let [bs   (.toByteArray buf)
                line (String. bs 0 (max 0 (dec (alength bs))) StandardCharsets/UTF_8)]
            (if-let [[_ n] (re-find #"\{(\d+)\}$" line)]
              (let [lit (.readNBytes in (int (parse-long n)))]
                (str line (String. lit StandardCharsets/UTF_8) (read-line! in)))
              line))
          :else (do (.write buf b) (recur b)))))))

(defn- send! [^OutputStream out ^String s]
  (.write out (.getBytes (str s "\r\n") StandardCharsets/UTF_8))
  (.flush out))

(defn- tls? [{:keys [tls port]}]
  (if (some? tls) (contains? #{true "true" "1" 1} tls) (= 993 (some-> port str parse-long))))

(defn port-of
  [{:keys [port] :as spec}]
  (or (some-> port str parse-long) (if (tls? spec) 993 143)))

(defn- open-socket
  ^Socket [{:keys [host tls-pin tls-name] :as spec}]
  (let [port (port-of spec)
        raw  (doto (Socket.) (.connect (InetSocketAddress. (str host) (int port)) connect-ms))]
    (if (tls? spec)
      (let [pin (tls/pin-of tls-pin)
            ctx (tls/context pin)
            s   (.createSocket ^SSLSocketFactory (.getSocketFactory ctx) raw
                               (str (or (not-empty (str tls-name)) host)) (int port) true)]
        (when-not pin
          (let [p (.getSSLParameters ^javax.net.ssl.SSLSocket s)]
            (.setEndpointIdentificationAlgorithm p "HTTPS")
            (.setSSLParameters ^javax.net.ssl.SSLSocket s p)))
        (doto ^javax.net.ssl.SSLSocket s (.startHandshake)))
      raw)))

(defn- command!
  [in out tag cmd what]
  (send! out (str tag " " cmd))
  (loop [seen []]
    (let [line (read-line! in)]
      (if-let [s (tagged tag line)]
        (if (= :ok s)
          seen
          (refusal/refuse! :refusal.sources/imap-refused {:status s :what what}
                           {:reply (str/replace line #"^\S+\s+" "")}))
        (recur (conj seen line))))))

(defn- idle!
  [^Socket sock in out state until]
  (send! out "l3 IDLE")
  (let [go (read-line! in)]
    (when-not (str/starts-with? go "+")
      (refusal/refuse! :refusal.sources/imap-refused {:what "IDLE"} {:reply go})))
  (let [changed (loop []
                  (let [left (- until (System/currentTimeMillis))]
                    (if (<= left 0)
                      false
                      (let [line (try (.setSoTimeout sock (int left)) (read-line! in)
                                      (catch SocketTimeoutException _ nil))]
                        (cond
                          (nil? line)            false
                          (arrival? @state line) (do (vswap! state absorb line) true)
                          :else                  (do (vswap! state absorb line) (recur)))))))]
    (.setSoTimeout sock reply-ms)
    (send! out "DONE")
    (loop [] (when-not (tagged "l3" (read-line! in)) (recur)))
    changed))

(defn- watch-folder!
  [spec folder prior window-ms sockets stopped]
  (let [^Socket sock (open-socket spec)
        _     (swap! sockets conj sock)
        in    (BufferedInputStream. (.getInputStream sock))
        out   (.getOutputStream sock)
        state (volatile! {})]
    (try
      (.setSoTimeout sock reply-ms)
      (read-line! in)
      (command! in out "l1" (str "LOGIN " (quoted (:username spec)) " " (quoted (:password spec)))
                "LOGIN")
      (vswap! state #(reduce absorb % (command! in out "l2"
                                                (str "SELECT " (quoted (mailbox-name folder)))
                                                (str "SELECT " folder))))
      (if (moved? prior @state)
        {:state @state :changed true}
        (let [changed (idle! sock in out state (+ (System/currentTimeMillis) (long window-ms)))]
          (when changed
            (vswap! state #(reduce absorb % (command! in out "l4" "NOOP" "NOOP"))))
          {:state @state :changed changed}))
      (catch SocketException e
        (if (realized? stopped) {:state @state :changed false} (throw e)))
      (finally
        (when-not (.isClosed sock)
          (try (send! out "l9 LOGOUT")
               (catch java.io.IOException e
                 (println (str "[imap-idle] LOGOUT failed: " (ex-message e)))))
          (try (.close sock)
               (catch java.io.IOException e
                 (println (str "[imap-idle] close failed: " (ex-message e))))))))))

(defn folders-of
  [spec folders]
  (or (seq (remove str/blank? (map str folders)))
      (seq (remove str/blank? (map str/trim (str/split (str (:include spec)) #","))))
      ["INBOX"]))

(defn- close-all! [sockets]
  (doseq [^Socket s @sockets]
    (try (.close s)
         (catch java.io.IOException e
           (println (str "[imap-idle] close failed: " (ex-message e)))))))

(defn await-change!
  [spec {:keys [folders window-ms state]}]
  (let [folders  (vec (folders-of spec folders))
        window   (-> (or window-ms idle-max-ms) long (max 1000) (min idle-max-ms))
        sockets  (atom [])
        stopped  (promise)
        watchers (mapv (fn [f]
                         [f (future
                              (let [r (watch-folder! spec f (get state f) window sockets stopped)]
                                (when (:changed r)
                                  (deliver stopped true)
                                  (close-all! sockets))
                                r))])
                       folders)
        results  (try (into {} (map (fn [[f w]] [f (deref w (+ window (* 3 reply-ms)) ::late)]))
                            watchers)
                      (catch java.util.concurrent.ExecutionException e
                        (close-all! sockets)
                        (throw (or (ex-cause e) e))))
        late     (vec (keep (fn [[f r]] (when (= ::late r) f)) results))]
    (when (seq late)
      (close-all! sockets)
      (refusal/refuse! :refusal.sources/imap-folders-silent {:folders late}
                       {:names (str/join ", " late)}))
    {:changed (boolean (some :changed (vals results)))
     :state   (into {} (map (fn [[f r]] [f (:state r)])) results)
     :folders folders}))
