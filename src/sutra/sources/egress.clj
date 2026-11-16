(ns sutra.sources.egress
  (:require [sutra.kit.noted :as noted]
            [sutra.foundation.egress :as egress]
            [sutra.kit.refusal :as refusal]
            [sutra.foundation.profile :as prof]
            [sutra.kit.digest :as digest]
            [clojure.edn :as edn]
            [sutra.foundation.encoding :as enc]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.sources.protocols :as api]
            [sutra.sources.connector :as connector]
            [sutra.sources.sidecar :as sidecar]
            [sutra.sources.datasources :as ds]
            [sutra.sources.jdbc :as fjdbc]
            [sutra.sources.policy :as policy]
            [sutra.sources.proxy.agent :as proxy-agent]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [sutra.foundation.outbound :as out]
            [sutra.foundation.rpc :as rpc]
            [sutra.sources.http-connector :as fhttp]
            [sutra.sources.imap-idle :as imap-idle]
            [sutra.kit.system :as sys]
            [sutra.llm.api :as llm]
            [sutra.types.source :as source]
            [sutra.foundation.env :as env])
  (:import (java.nio.file Files OpenOption)
           (java.nio.file.attribute PosixFilePermission)
           (java.security KeyFactory KeyPairGenerator)
           (java.security.interfaces RSAPrivateCrtKey)
           (java.security.spec MGF1ParameterSpec PKCS8EncodedKeySpec RSAPublicKeySpec)
           (javax.crypto Cipher)
           (javax.crypto.spec OAEPParameterSpec PSource$PSpecified)
           (java.time Instant LocalDateTime OffsetDateTime ZonedDateTime)
           (java.util Base64 Date)
           (microsoft.sql DateTimeOffset)))

(set! *warn-on-reflection* true)

(defonce ^:private server* (atom nil))

(defonce ^:private running*
  (atom {:agent egress/default-agent :token nil :started-at nil :keypair nil}))

(defn- own-agent [] (:agent @running*))

(defn- public-key ^java.security.PublicKey [] (-> @running* :keypair :public))

(defn- private-key ^java.security.PrivateKey [] (-> @running* :keypair :private))

(defn- oaep-spec ^OAEPParameterSpec []
  (OAEPParameterSpec. "SHA-256" "MGF1" MGF1ParameterSpec/SHA256 PSource$PSpecified/DEFAULT))

(defn- public-of
  [^RSAPrivateCrtKey priv]
  (.generatePublic (KeyFactory/getInstance "RSA")
                   (RSAPublicKeySpec. (.getModulus priv) (.getPublicExponent priv))))

(defn- load-or-gen-keypair!
  [keyfile]
  (let [f (io/file keyfile)
        priv (if (.exists f)
               (.generatePrivate (KeyFactory/getInstance "RSA")
                                 (PKCS8EncodedKeySpec. (Files/readAllBytes (.toPath f))))
               (let [kp (.generateKeyPair (doto (KeyPairGenerator/getInstance "RSA")
                                            (.initialize 2048)))]
                 (io/make-parents f)
                 (Files/write (.toPath f) (.getEncoded (.getPrivate kp))
                              ^"[Ljava.nio.file.OpenOption;" (into-array OpenOption []))
                 (Files/setPosixFilePermissions
                  (.toPath f) #{PosixFilePermission/OWNER_READ PosixFilePermission/OWNER_WRITE})
                 (.getPrivate kp)))]
    (swap! running* assoc :keypair {:private priv :public (public-of priv)})
    (println "egress-agent RSA keypair ready —" keyfile)))

(defn- public-key-b64
  []
  (.encodeToString (Base64/getEncoder) (.getEncoded (public-key))))

(defn- rsa-decrypt
  [^String b64]
  (let [c (doto (Cipher/getInstance "RSA/ECB/OAEPWithSHA-256AndMGF1Padding")
            (.init Cipher/DECRYPT_MODE (private-key) (oaep-spec)))]
    (String. (.doFinal c (.decode (Base64/getDecoder) b64)) "UTF-8")))

(defn render
  [tmpl params]
  (fhttp/render tmpl params))

(defn- call-timeout
  [{:keys [timeout-ms]}]
  (min 120000 (or timeout-ms 120000)))

(defn invoke!
  [spec {:keys [webhook-id params] :as inv}]
  (when-not (:rest spec)
    (refusal/refuse! :refusal.foreign/webhook-no-rest {}))
  (fhttp/invoke (fhttp/source spec) webhook-id params inv))

(defn- deep-merge
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn- decrypt-secret
  [cipher]
  (reduce (fn [acc c]
            (let [m (edn/read-string (rsa-decrypt c))]
              (when-not (map? m)
                (throw (ex-info "decrypted egress secret must be an EDN map of credential fields"
                                {})))
              (deep-merge acc m)))
          {}
          (str/split (str/trim cipher) #"\s+")))

(defn- resolve-spec
  [spec]
  (let [spec (if (map? (:config spec))
               (merge (dissoc spec :config) (:config spec))
               spec)]
    (if-let [cipher (:secret-cipher spec)]
      (-> spec (dissoc :secret-cipher) (deep-merge (decrypt-secret cipher)))
      spec)))

(defn- wire-value
  [col v]
  (cond
    (or (nil? v) (string? v) (number? v) (boolean? v) (char? v) (uuid? v)
        (instance? Date v))           v
    (instance? Instant v)             (Date/from v)
    (instance? OffsetDateTime v)      (Date/from (.toInstant ^OffsetDateTime v))
    (instance? ZonedDateTime v)       (Date/from (.toInstant ^ZonedDateTime v))
    (instance? DateTimeOffset
               v)      (Date/from (.toInstant (.getOffsetDateTime ^DateTimeOffset v)))
    :else
    (refusal/refuse! :refusal.foreign/no-platform-type
                     {:column (name col) :class (.getName (class v))})))

(defn- wire-row [row]
  (reduce-kv (fn [m k v] (assoc m k (wire-value k v))) {} row))

(defn- perform-read [spec query params]
  (if (= :landed (policy/read-mode spec))
    {:path (ds/connector-reach! spec)}
    {:rows (api/with-source [s fjdbc/source spec]
             (mapv wire-row (api/read s query params)))}))

(defn- require-spec
  [{:keys [spec source-id]}]
  (when-not (map? spec)
    (throw (ex-info (str "egress agent requires an inline source spec"
                         (when source-id (str " (got bare source-id " (pr-str source-id) ")")))
                    {:source-id source-id})))
  (let [owner (egress/agent-of spec)]
    (when (not= owner (own-agent))
      (refusal/refuse! :refusal.foreign/source-other-agent
                       {:source-name (:source-name spec) :agent owner :refused-by (own-agent)})))
  (resolve-spec spec))

(defn- handle-reach
  [{:keys [query params] :as req}]
  (perform-read (require-spec req) query params))

(def ^:private session-ttl-ms 120000)

(defonce ^:private sessions (atom {}))

(defn- end-session!
  [id commit?]
  (let [[old] (swap-vals! sessions dissoc id)]
    (when-let [{:keys [^java.sql.Connection conn]} (get old id)]
      (try (if commit? (.commit conn) (.rollback conn))
           (finally (fjdbc/close! conn)))
      true)))

(defn- reap-sessions!
  []
  (let [now (System/currentTimeMillis)]
    (doseq [[id {:keys [at]}] @sessions
            :when (> (- now at) session-ttl-ms)]
      (noted/quietly "egress session reaper" (end-session! id false)))))

(defonce ^:private reaper
  (delay (doto (Thread. ^Runnable (fn [] (loop []
                                           (Thread/sleep 10000)
                                           (noted/quietly "egress session reaper"
                                             (reap-sessions!))
                                           (recur)))
                        "egress-session-reaper")
           (.setDaemon true)
           (.start))))

(defn- handle-write-end
  [{:keys [tx commit]}]
  (cond
    (end-session! tx (true? commit)) {:ended tx}
    (not (true? commit))             {:ended tx :gone true}
    :else (refusal/refuse! :refusal.foreign/write-session-gone {:tx tx}
                           {:seconds (quot session-ttl-ms 1000)})))

(defn- handle-write-begin
  [req]
  @reaper
  (reap-sessions!)
  (let [spec (require-spec req)]
    (when-not (policy/jdbc-network? spec)
      (refusal/refuse! :refusal.foreign/writeback-unsupported-type {:type (:type spec)}))
    (let [conn (fjdbc/open-writable! spec)
          id   (str (java.util.UUID/randomUUID))]
      (try (.setAutoCommit conn false)
           (catch Throwable t (fjdbc/close! conn) (throw t)))
      (swap! sessions assoc id {:conn conn :spec spec
                                :source (get-in req [:spec :source-name])
                                :at (System/currentTimeMillis)})
      {:tx id})))

(defn- session-write
  [id req write-spec]
  (let [{:keys [conn spec source]} (or (get @sessions id)
                                       (refusal/refuse! :refusal.foreign/write-session-gone {:tx id}
                                                        {:seconds (quot session-ttl-ms 1000)}))]
    (when (not= source (get-in req [:spec :source-name]))
      (refusal/refuse! :refusal.foreign/write-session-other-source {:tx id :source source}))
    (try
      (let [plan (policy/write-plan (LocalDateTime/now) spec write-spec)]
        (fjdbc/apply-plan! conn true (:type spec) plan))
      (catch Throwable t
        (noted/quietly "egress write session" (end-session! id false))
        (throw t)))))

(defn- handle-write
  [{:keys [write-spec tx] :as req}]
  (if tx
    (session-write tx req write-spec)
    (let [spec (require-spec req)]
      (when-not (policy/jdbc-network? spec)
        (refusal/refuse! :refusal.foreign/writeback-unsupported-type {:type (:type spec)}))
      (api/with-source [s fjdbc/source spec]
        (api/write! s write-spec)))))

(defn- handle-webhook
  [{:keys [webhook-id params timeout-ms idempotency-key cloudevent] :as req}]
  (invoke! (require-spec req) {:webhook-id webhook-id :params params :timeout-ms timeout-ms
                               :idempotency-key idempotency-key :cloudevent cloudevent}))

(defn- handle-rest
  [{:keys [op table scan] :as req}]
  (let [s (fhttp/source (require-spec req))]
    {:result (case op
               :tables       (api/tables s)
               :columns      (api/columns s table)
               :scan-profile (api/-scan-profile s table)
               :scan         (mapv wire-row (api/-scan s table scan)))}))

(def ^:private sidecars
  (delay (sidecar/parse-sidecars (env/getenv "SUTRA_CONNECTOR_SIDECARS"))))

(def ^:private sidecar-token
  (delay (some-> (env/getenv "SUTRA_CONNECTOR_SIDECAR_TOKEN_FILE") not-empty slurp str/trim
                 not-empty)))

(def ^:private sidecar-ms 60000)

(def ^:private describe-ms 3000)

(defonce ^:private described (atom {}))

(defonce ^:private described-at (atom 0))

(def ^:private describe-every-ms 30000)

(defn- describe-sidecars!
  []
  (when (< describe-every-ms (- (System/currentTimeMillis) @described-at))
    (reset! described-at (System/currentTimeMillis))
    (doseq [[t url] @sidecars]
      (try (let [d (sidecar/describe! url @sidecar-token describe-ms)]
             (swap! described assoc t
                    (assoc (or (get d t) {:missing true}) :via :sidecar :url url)))
           (catch InterruptedException e (throw e))
           (catch Exception e
             (println (str "[egress] sidecar " (name t) " at " url " did not describe itself: "
                           (.getMessage e)))))))
  @described)

(defn- connectors-here
  []
  (merge (into {} (for [[t c] (source/connectors) :when (:open c)]
                    [t (assoc (select-keys c [:profile :form]) :via :plugin)]))
         (into {} (for [[t c] (describe-sidecars!) :when (not (:missing c))]
                    [t (select-keys c [:profile :form :via])]))))

(defn- handle-connector
  [{:keys [op write-spec] :as req}]
  (let [spec (require-spec req)]
    (if-let [url (get @sidecars (:type spec))]
      (let [_    (when (= :host (get-in (describe-sidecars!) [(:type spec) :profile :endpoint]))
                   (policy/assert-egress-permits! spec (:host spec) (:port spec)))
            plan (when (= :write op) (connector/plan spec write-spec))
            r    (sidecar/op! url @sidecar-token (or (:timeout-ms req) sidecar-ms) spec req plan)]
        {:result (if (= :scan op) (mapv connector/lower-row r) r)})
      (let [r (connector/run-op spec req)]
        {:result (if (= :scan op) (mapv wire-row r) r)}))))

(defn- handle-listen
  [{:keys [folders window-ms state] :as req}]
  (let [spec (require-spec req)]
    (when-not (= "imap" (some-> (:type spec) name))
      (refusal/refuse! :refusal.sources/no-listener
                       {:source-name (:source-name spec)}
                       {:type (some-> (:type spec) name)}))
    (policy/assert-egress-permits! spec (:host spec) (imap-idle/port-of spec))
    (imap-idle/await-change! spec {:folders folders :window-ms window-ms :state state})))

(defn handle-secret
  [{:keys [spec] :as req}]
  (let [resolved (require-spec req)]
    (when-not (:external-transform resolved)
      (refusal/refuse! :refusal.foreign/secret-not-external {:source-name (:source-name spec)}))
    {:fields (dissoc resolved :external-transform)}))

(defn- model-key
  [resolved base]
  (or (:api-key resolved)
      (when-not (llm/local-endpoint? base)
        (refusal/refuse! :refusal.foreign/no-api-key {}))))

(defn- wires [] (merge llm/builtin (sys/table (sys/process) :llm/wires)))

(def ^:private connect-ms
  10000)

(defn- provider-post
  [url opts timeout-ms]
  (out/post! url (assoc opts :connect-timeout connect-ms :idle-timeout timeout-ms)))

(defn- egress-failure
  [what url error]
  (let [u    (java.net.URI. (str url))
        hp   (str (.getHost u) ":" (if (pos? (.getPort u)) (.getPort u) 443))
        msg  (str (or (ex-message error) error))]
    (cond
      (str/starts-with? msg "connect timeout")
      [:refusal.sources/egress-blocked {:what what :host hp :seconds (quot connect-ms 1000)}]

      (instance? java.net.UnknownHostException error)
      [:refusal.sources/egress-unresolved {:what what :host hp}]

      :else [:refusal.sources/egress-failed {:what what :why msg}])))

(defn- handle-chat
  [{:keys [payload] :as req}]
  (let [resolved (require-spec req)]
    (when-not (:external-transform resolved)
      (refusal/refuse! :refusal.foreign/model-not-external {:source-name (:source-name resolved)}))
    (let [base    (or (:base-url resolved) "https://api.x.ai/v1")
          [_ w]   (llm/wire-for (wires) resolved base)
          {:keys [url headers] :as out} (llm/request w base (model-key resolved base) payload)
          t0      (System/currentTimeMillis)
          {:keys [status body error] :as resp}
          (provider-post url {:headers headers :body (:body out)} (call-timeout req))
          latency (- (System/currentTimeMillis) t0)
          body    (when body (if (string? body) body (slurp body)))]
      (when error
        (let [[code params] (egress-failure "language-model egress" url error)]
          (refusal/refuse! code {:latency-ms latency} params)))
      (when-not (= 200 status)
        (refusal/refuse! :refusal.sources/model-call-status
                         (cond-> {:status status :provider-status status :latency-ms latency
                                  :body (enc/clip 2000 body)}
                           (get-in resp [:headers :retry-after])
                           (assoc :retry-after (get-in resp [:headers :retry-after])))))
      {:status status :latency-ms latency :body (llm/response w body)})))

(defn- handle-embed
  [{:keys [payload] :as req}]
  (let [resolved (require-spec req)]
    (when-not (:external-transform resolved)
      (refusal/refuse! :refusal.foreign/model-not-external {:source-name (:source-name resolved)}))
    (let [base  (or (:base-url resolved) "https://api.x.ai/v1")
          [id w] (llm/wire-for (wires) resolved base)
          out   (or (llm/embed-request w base (model-key resolved base) payload)
                    (refusal/refuse! :refusal.sources/no-embeddings
                                     {:wire id}
                                     {:wire-name (name id)}))
          t0    (System/currentTimeMillis)
          {:keys [status body error]}
          (provider-post (:url out) {:headers (:headers out) :body (:body out)}
                         (call-timeout req))
          latency (- (System/currentTimeMillis) t0)
          body  (when body (if (string? body) body (slurp body)))]
      (when error
        (let [[code params] (egress-failure "embedding egress" (:url out) error)]
          (refusal/refuse! code {:latency-ms latency} params)))
      (when-not (= 200 status)
        (refusal/refuse! :refusal.sources/embedding-call-status
                         {:status status :latency-ms latency :body (enc/clip 2000 body)}))
      {:status status :latency-ms latency :body body})))

(defn- handle-models
  [{:keys [models-path] :as req}]
  (let [resolved (require-spec req)]
    (when-not (:external-transform resolved)
      (refusal/refuse! :refusal.foreign/catalog-not-external
                       {:source-name (:source-name resolved)}))
    (let [base (or (:base-url resolved) "https://api.x.ai/v1")
          [_ w] (llm/wire-for (wires) resolved base)
          path (str/replace (str (or models-path (:models-path w) "models")) #"^/+" "")
          url  (llm/url base path)
          key  (model-key resolved base)
          t0   (System/currentTimeMillis)
          {:keys [status body error]}
          (out/get! url {:headers (or ((:headers w) key nil) {})
                         :timeout 60000})
          latency (- (System/currentTimeMillis) t0)]
      (when error
        (refusal/refuse! :refusal.sources/model-inventory-failed {:latency-ms latency}
                         {:why (str error)}))
      (when-not (= 200 status)
        (refusal/refuse! :refusal.sources/model-inventory-status
                         {:status status :latency-ms latency
                          :body (enc/clip 2000 body)}))
      {:status status :latency-ms latency
       :body (if (string? body) body (slurp body))})))

(defn- fingerprint
  []
  (->> (digest/sha256-hex (.getEncoded (public-key)))
       (partition 2) (take 16) (map str/join) (str/join ":")))

(def ^:private probe-ms 3000)

(defn- connect-probe
  [endpoint]
  (let [i (str/last-index-of endpoint ":")
        h (if i (subs endpoint 0 i) endpoint)
        p (or (some-> i inc (->> (subs endpoint)) parse-long) 443)
        t0 (System/currentTimeMillis)]
    (try (with-open [s (java.net.Socket.)]
           (.connect s (java.net.InetSocketAddress. ^String h (int p)) (int probe-ms))
           {:endpoint endpoint :open true :ms (- (System/currentTimeMillis) t0)})
         (catch java.net.SocketTimeoutException _
           {:endpoint endpoint :open false
            :error (str "no connection within " (quot probe-ms 1000)
                        " s — blocked by the host's kernel egress allowlist")})
         (catch InterruptedException e (throw e))
         (catch Exception e
           {:endpoint endpoint :open false :error (str (.getSimpleName (class e)) ": "
                                                       (ex-message e))}))))

(defn- handle-connect
  [{:keys [endpoints]}]
  {:endpoints (->> (take 64 (distinct (map str endpoints)))
                   (mapv #(future (connect-probe %)))
                   (mapv deref))})

(defn- handle-status
  [_]
  (let [now (System/currentTimeMillis)
        rt  (Runtime/getRuntime)
        tr  (sqltrace/since Long/MAX_VALUE)]
    {:agent       (own-agent)
     :alg         "RSA-OAEP-256"
     :fingerprint (fingerprint)
     :started-at  (:started-at @running*)
     :uptime-ms   (some->> (:started-at @running*) (- now))
     :sessions    (vec (for [[id {:keys [source at]}] @sessions]
                         {:tx id :source source :age-ms (- now at)
                          :expires-in-ms (- session-ttl-ms (- now at))}))
     :trace       {:armed (:armed tr) :file (:file tr) :inflight (count (:inflight tr))}
     :heap        {:used-mb (quot (- (.totalMemory rt) (.freeMemory rt)) 1048576)
                   :max-mb  (quot (.maxMemory rt) 1048576)}
     :connectors  (connectors-here)
     :java        (System/getProperty "java.version")}))

(defn- handle-trace-arm
  [{:keys [on file capture-params?]}]
  (if on
    (sqltrace/arm! {:file file :capture-params? capture-params?})
    (sqltrace/disarm!)))

(def ^:private routes
  {"/pubkey"     (fn [_] {:public-key (public-key-b64) :alg "RSA-OAEP-256"})
   "/status"     #'handle-status
   "/reach"      #'handle-reach
   "/connect"    #'handle-connect
   "/write"      #'handle-write
   "/write-begin" #'handle-write-begin
   "/write-end"  #'handle-write-end
   "/webhook"    #'handle-webhook
   "/rest"       #'handle-rest
   "/connector"  #'handle-connector
   "/listen"     #'handle-listen
   "/secret"     #'handle-secret
   "/chat"       #'handle-chat
   "/embed"      #'handle-embed
   "/models"     #'handle-models
   "/trace-arm"  #'handle-trace-arm
   "/trace"      (fn [body] (sqltrace/since (:after body)))
   "/trace-kill" (fn [body] (if (:all body)
                              (sqltrace/kill-all!)
                              (sqltrace/kill! (:seq body))))})

(def ^:private spanned-routes
  (into {} (map (fn [[path f]]
                  [path (fn [body]
                          (if-let [span (:span body)]
                            (prof/with-span span (f body))
                            (f body)))]))
        routes))

(defn handler [req]
  (rpc/handler spanned-routes req {:token (:token @running*) :open #{"/pubkey"}}))

(def ^:private default-keyfile "/etc/sutra/egress-agent-rsa.pkcs8")

(def ^:private default-proxy-config "/etc/sutra/agentProxyConfig/agentProxyConfig.edn")

(defn- own-token
  [agent]
  (or (some-> (env/getenv "SUTRA_EGRESS_TOKEN_FILE") not-empty slurp str/trim not-empty)
      (rpc/token-for (egress/egress-master) agent)))

(defn start!
  [{:keys [keyfile port ip agent token] :or {port 8731 ip "127.0.0.1"}}]
  (let [agent (or (not-empty agent) egress/default-agent)]
    (swap! running* assoc :agent agent :token (or (not-empty token) (own-token agent))
           :started-at (System/currentTimeMillis)))
  (load-or-gen-keypair! (or keyfile default-keyfile))
  (sqltrace/set-role! "egress")
  (when (seq (env/getenv "SUTRA_SQL_TRACE"))
    (sqltrace/arm! {:file (env/getenv "SUTRA_SQL_TRACE_FILE")}))
  (rpc/serve! server* #'handler {:port port :ip ip})
  (println (str "egress-agent " (pr-str (own-agent)) " (stateless) on " ip ":" port
                " · up since " (java.time.Instant/ofEpochMilli (:started-at @running*))))
  @server*)

(defn stop! [] (rpc/stop-server! server*))

(defn -main [& _args]
  (let [keyfile (or (env/getenv "SUTRA_EGRESS_KEYFILE") default-keyfile)
        port (Integer/parseInt (or (env/getenv "SUTRA_EGRESS_PORT") "8731"))
        ip   (or (env/getenv "SUTRA_EGRESS_IP") "127.0.0.1")]
    (sys/install! (sys/assemble (sys/discover :connectors)))
    (connector/assert-connectors!)
    (start! {:keyfile keyfile :port port :ip ip :agent (env/getenv "SUTRA_EGRESS_AGENT")})
    (when-let [url (not-empty (env/getenv "SUTRA_AGENT_PROXY_URL"))]
      (proxy-agent/start! {:url url :token (:token @running*) :agent (own-agent)
                           :config-file (or (env/getenv "SUTRA_AGENT_PROXY_CONFIG")
                                            default-proxy-config)}))
    @(promise)))
