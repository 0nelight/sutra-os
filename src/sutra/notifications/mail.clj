(ns sutra.notifications.mail
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.anomaly :as anomaly]
            [sutra.foundation.env :as fenv]
            [clojure.string :as str])
  (:import (java.io BufferedReader InputStreamReader OutputStreamWriter)
           (java.net InetSocketAddress Socket)
           (javax.net.ssl SSLParameters SSLSocket SSLSocketFactory)
           (java.nio.charset StandardCharsets)
           (java.time ZonedDateTime)
           (java.time.format DateTimeFormatter)
           (java.util Base64)))

(set! *warn-on-reflection* true)

(defn- env [k] (some-> (System/getenv k) str/trim not-empty))

(def tls-modes #{:starttls :implicit :none})

(defn- smtp-password []
  (or (some-> (env "SUTRA_SMTP_PASSWORD_FILE") slurp str/trim not-empty)
      (env "SUTRA_SMTP_PASSWORD")))

(defn config
  []
  (when-let [host (env "SUTRA_SMTP_HOST")]
    (when-let [domain (env "SUTRA_MAIL_DOMAIN")]
      (let [tls (keyword (str/lower-case
                          (or (env "SUTRA_SMTP_TLS")
                              (if (fenv/auth-hardening?) "starttls" "none"))))]
        (when-not (tls-modes tls)
          (anomaly/raise! :fault (str "SUTRA_SMTP_TLS is starttls, implicit or none, not "
                                      (name tls))
                          {:tls tls}))
        (cond-> {:host   host
                 :port   (Long/parseLong (or (env "SUTRA_SMTP_PORT")
                                             (if (= :implicit tls) "465" "25")))
                 :from   (or (env "SUTRA_SMTP_FROM") (str "sutra@" domain))
                 :domain domain
                 :tls    tls}
          (env "SUTRA_SMTP_USER") (assoc :user (env "SUTRA_SMTP_USER")
                                         :password (smtp-password)))))))

(defn auth-plain
  [user password]
  (.encodeToString (Base64/getEncoder) (.getBytes (str "\u0000" user "\u0000" password) "UTF-8")))

(defn refusal
  [{:keys [tls user password]}]
  (cond
    (and user (= :none tls)) "smtp: a login needs TLS — set SUTRA_SMTP_TLS to starttls"
    (and user (empty? password)) "smtp: SUTRA_SMTP_USER has no password"))

(defn- tls-socket
  ^SSLSocket [^Socket s ^String host port]
  (let [^SSLSocketFactory f (SSLSocketFactory/getDefault)
        ^SSLSocket sock (if s (.createSocket f s host (int port) true)
                            (.createSocket f host (int port)))
        ps (doto (SSLParameters.) (.setEndpointIdentificationAlgorithm "HTTPS"))]
    (.setSSLParameters sock ps)
    (.setSoTimeout sock 10000)
    (.startHandshake sock)
    sock))

(defn- b64 [^bytes bs] (.encodeToString (Base64/getMimeEncoder) bs))

(defn- utf8 [s] (.getBytes (str s) StandardCharsets/UTF_8))

(defn- one-line [s] (str/replace (str s) #"[\p{Cntrl}]" " "))

(defn- encoded-word [s]
  (str "=?UTF-8?B?" (.encodeToString (Base64/getEncoder) (utf8 (one-line s))) "?="))

(defn- part [content-type body & headers]
  (str/join "\r\n" (concat [(str "Content-Type: " content-type)
                            "Content-Transfer-Encoding: base64"]
                           headers
                           ["" (b64 body)])))

(defn- multipart [kind boundary parts]
  (str "Content-Type: multipart/" kind "; boundary=\"" boundary "\"\r\n\r\n"
       (str/join "" (map #(str "--" boundary "\r\n" % "\r\n") parts))
       "--" boundary "--"))

(defn- text-body [{:keys [body html]} boundary]
  (if html
    (multipart "alternative" (str boundary "-alt")
               [(part "text/plain; charset=UTF-8" (utf8 body))
                (part "text/html; charset=UTF-8" (utf8 html))])
    (part "text/plain; charset=UTF-8" (utf8 body))))

(defn- attachment [{:keys [filename content-type bytes]}]
  (part (str (or content-type "application/octet-stream") "; name=\"" (one-line filename) "\"")
        bytes
        (str "Content-Disposition: attachment; filename=\"" (one-line filename) "\"")))

(defn message
  [{:keys [from to subject attachments] :as m}]
  (let [boundary (str "sutra-" (Long/toHexString (hash [to subject])))]
    (str/join "\r\n"
              [(str "From: " from)
               (str "To: " to)
               (str "Subject: " (encoded-word subject))
               (str "Date: " (.format DateTimeFormatter/RFC_1123_DATE_TIME (ZonedDateTime/now)))
               "MIME-Version: 1.0"
               (if (seq attachments)
                 (multipart "mixed" boundary
                            (into [(text-body m boundary)] (map attachment) attachments))
                 (text-body m boundary))])))

(defn address
  [username email domain]
  (let [u (str username)
        a (cond (some-> email str (str/includes? "@")) (str email)
                (str/includes? u "@")                  u
                :else                                  (str u "@" domain))]
    (when (re-matches #"[^\s<>@,;:\"]+@[^\s<>@,;:\"]+" a) a)))

(defn- channel [^Socket sock]
  {:in  (BufferedReader. (InputStreamReader. (.getInputStream sock) StandardCharsets/UTF_8))
   :out (OutputStreamWriter. (.getOutputStream sock) StandardCharsets/UTF_8)})

(defn- dialogue!
  [{:keys [host port from domain tls user password]} to data]
  (let [plain (when-not (= :implicit tls)
                (doto (Socket.)
                  (.connect (InetSocketAddress. ^String host (int port)) 5000)
                  (.setSoTimeout 10000)))
        sock  (atom (or plain (tls-socket nil host port)))
        ch    (atom (channel @sock))
        answer (fn answer [acc]
                 (let [l (or (.readLine ^BufferedReader (:in @ch)) "")
                       acc (conj acc l)]
                   (if (and (> (count l) 3) (= \- (nth l 3))) (recur acc) acc)))
        expect (fn [code]
                 (let [ls (answer [])]
                   (when-not (str/starts-with? (peek ls) code)
                     (refusal/refuse! :refusal.notifications/smtp-answer {:expected code}
                                      {:line (peek ls)}))
                   ls))
        say    (fn [line code]
                 (.write ^OutputStreamWriter (:out @ch) (str line "\r\n"))
                 (.flush ^OutputStreamWriter (:out @ch))
                 (expect code))
        ehlo   #(say (str "EHLO " domain) "250")]
    (try
      (expect "220")
      (let [offers (ehlo)]
        (when (= :starttls tls)
          (when-not (some #(re-find #"(?i)^250[ -]STARTTLS" %) offers)
            (refusal/refuse! :refusal.notifications/smtp-no-starttls {:host host}))
          (say "STARTTLS" "220")
          (reset! sock (tls-socket @sock host port))
          (reset! ch (channel @sock))
          (ehlo)))
      (when user
        (say (str "AUTH PLAIN " (auth-plain user password)) "235"))
      (say (str "MAIL FROM:<" from ">") "250")
      (say (str "RCPT TO:<" to ">") "250")
      (say "DATA" "354")
      (say (str data "\r\n.") "250")
      (say "QUIT" "221")
      (finally (.close ^Socket @sock)))))

(defn send!
  [cfg m]
  (try (or (refusal cfg)
           (do (dialogue! cfg (:to m) (message (assoc m :from (:from cfg))))
               nil))
       (catch InterruptedException e (throw e))
       (catch Exception t (or (ex-message t) (str t)))))
