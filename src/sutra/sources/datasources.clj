(ns sutra.sources.datasources
  (:require [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [sutra.foundation.egress :as egress]
            [sutra.kit.effects :as effects]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.redact :as redact]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.foundation.outbound :as out]
            [sutra.foundation.periodic :as periodic]
            [sutra.sources.protocols :as api]
            [sutra.sources.cloudevents :as cloudevents]
            [sutra.sources.connector :as connector]
            [sutra.sources.dialect :as dialect]
            [sutra.sources.http-connector :as fhttp]
            [sutra.sources.jdbc :as fjdbc]
            [sutra.sources.local-files :as local-files]
            [sutra.sources.policy :as policy]
            [sutra.sources.proxy.client :as proxy-client]
            [sutra.sources.proxy.seal :as seal]
            [sutra.store.access.scope :as ctx]
            [sutra.foundation.rpc :as rpc]
            [sutra.sources.rest :as rest]
            [sutra.types.source :as source])
  (:import (java.lang ProcessBuilder$Redirect)
           (java.util.concurrent TimeUnit)
           (net.schmizz.sshj SSHClient)
           (net.schmizz.sshj.common IOUtils)
           (net.schmizz.sshj.sftp SFTPClient)))

(set! *warn-on-reflection* true)

(declare fetch-sftp! read-file! run-command!)

(def ^:private reach-fns
  {:sftp       #'fetch-sftp!
   :command    #'run-command!
   :filesystem #'read-file!
   :glpi-api   #'rest/land-glpi-bundle!})

(defn assert-source-cleared!
  [auth {:keys [markings source-name] :as spec}]
  (when (and spec (seq markings)
             (not (grants/cleared? (grants/clearances auth) markings)))
    (refusal/refuse! :refusal.foreign/not-cleared
                     {:source source-name :markings (vec markings)
                      :clearances (grants/clearances auth)}
                     {:source (or source-name "?")}))
  spec)

(defn assert-source-write-cleared!
  [auth {:keys [access] :as spec}]
  (when-not (= :read-write access)
    (refusal/refuse! :refusal.foreign/source-not-read-write
                     {:access access :source (:source-name spec)}
                     {:source (or (:source-name spec) "?")}))
  (assert-source-cleared! auth spec)
  spec)

(def ^:private csv-virtual-table "csv")

(defn read-csv-rows
  [{:keys [path] :as spec}]
  (when-not (policy/csv? spec)
    (throw (ex-info "read-csv-rows requires :type :csv" {:spec spec})))
  (when-not path
    (refusal/refuse! :refusal.foreign/csv-path-missing {:spec spec}))
  (with-open [rdr (io/reader (io/file (local-files/path path)))]
    (let [[header & rows] (csv/read-csv rdr)]
      (mapv (fn [row]
              (into {} (map vector header row)))
            rows))))

(defn- csv-list-columns
  [{:keys [path]}]
  (with-open [rdr (io/reader (io/file (local-files/path path)))]
    (let [[header & _] (csv/read-csv rdr)]
      (mapv (fn [c] {:name c :type "TEXT" :ontology-type :string}) header))))
(defn reach!
  [spec]
  (:path (egress/post "/reach" {:spec spec})))

(defn read-rows
  ([spec query] (read-rows spec query nil))
  ([spec query params]
   (:rows (egress/post "/reach" (cond-> {:spec spec :query query}
                                  (seq params) (assoc :params (vec params)))))))

(defn agent-pubkey
  ([] (agent-pubkey egress/default-agent))
  ([agent] (egress/post-to agent "/pubkey" {})))

(defn agent-connect
  [agent endpoints timeout-ms]
  (attempt/or-answer nil
    (:endpoints
     (rpc/call! {:url (str (egress/agent-url agent) "/connect") :peer (str "egress agent " agent)
                 :timeout timeout-ms :token (egress/agent-token agent)}
                {:endpoints (vec endpoints)}))))

(defn agent-status
  [agent timeout-ms]
  (attempt/or-answer nil
    (rpc/call! {:url (str (egress/agent-url agent) "/status") :peer (str "egress agent " agent)
                :timeout timeout-ms :token (egress/agent-token agent)}
               {})))

(defn- each-agent
  [f]
  (into {} (for [a (egress/agent-names)]
             [a (attempt/or-answer nil (f a))])))

(defn trace-arm!
  [on opts]
  (each-agent #(egress/post-to % "/trace-arm" (merge {:on (boolean on)} opts))))

(defn parse-trace-cursor
  [s]
  (into {} (for [part (str/split (str s) #",")
                 :let [[a n] (str/split part #":" 2)]
                 :when (and (seq a) (re-matches #"\d+" (str n)))]
             [a (parse-long n)])))

(defn trace-since
  [cursor]
  (let [after (parse-trace-cursor cursor)]
    (each-agent #(egress/post-to % "/trace" {:after (get after % 0)}))))

(defn trace-kill!
  [{:keys [all] :as payload}]
  (if all
    (each-agent #(egress/post-to % "/trace-kill" payload))
    (reduce (fn [acc a]
              (let [r (try (egress/post-to a "/trace-kill" payload)
                           (catch InterruptedException e (throw e))
                           (catch Exception e
                             (println (str "[datasources] trace-kill: agent " a
                                           " did not answer: " (ex-message e)))
                             nil))]
                (if (:killed r) (reduced {a r}) acc)))
            {} (egress/agent-names))))

(defn agent-address
  ([] (agent-address egress/default-agent))
  ([agent] (str/replace (egress/agent-url agent) #"^https?://" "")))

(defn agent-reachable?
  ([] (every? agent-reachable? (egress/agent-names)))
  ([agent]
   (attempt/or-answer false
     (let [{:keys [status error]}
           (out/post! (str (egress/agent-url agent) "/pubkey")
                      {:headers {"Content-Type" "application/edn"}
                       :body (pr-str {})
                       :timeout 1500})]
       (and (nil? error) (= 200 status))))))

(defn write!
  ([spec write-spec] (write! spec write-spec nil))
  ([spec write-spec session]
   (if session
     (let [k     (:source-name spec)
           other (first (remove #{k} (keys (dissoc @session ::log))))
           _     (when other
                   (refusal/refuse! :refusal.foreign/multi-source {:source k :open other}
                                    {:source k :other other}))
           tx    (or (:tx (get @session k))
                     (let [t (:tx (egress/post "/write-begin" {:spec spec}))]
                       (swap! session assoc k {:tx t :agent (egress/agent-of spec)})
                       t))]
       (egress/post "/write" {:spec spec :write-spec write-spec :tx tx}))
     (egress/post "/write" {:spec spec :write-spec write-spec}))))

(defn write-session
  []
  (atom {}))

(defn hold-log!
  [session entry]
  (swap! session update ::log (fnil conj []) entry)
  nil)

(defn held-log [session] (vec (some-> session deref ::log)))

(defn end-write-session!
  [session commit?]
  (doseq [{:keys [tx agent]} (vals (dissoc @session ::log))]
    (egress/post-to agent "/write-end" {:tx tx :commit commit?})))

(defn invoke-webhook!
  ([spec webhook-id params] (invoke-webhook! spec webhook-id params nil))
  ([spec webhook-id params {:keys [timeout-ms idempotency-key event-type]}]
   (egress/post "/webhook" (cond-> {:spec spec :webhook-id webhook-id :params params
                                    :cloudevent (cloudevents/attributes
                                                 {:type        event-type
                                                  :source-name (:source-name spec)
                                                  :webhook-id  webhook-id
                                                  :id          (or idempotency-key
                                                                   (str (random-uuid)))
                                                  :time        (java.time.Instant/now)})}
                             timeout-ms      (assoc :timeout-ms timeout-ms)
                             idempotency-key (assoc :idempotency-key idempotency-key)))))

(defn listen!
  [spec {:keys [window-ms] :as req}]
  (let [agent (egress/agent-of spec)]
    (rpc/call! {:url     (str (egress/agent-url agent) "/listen")
                :peer    (str "egress agent " agent)
                :token   (egress/agent-token agent)
                :timeout (+ (long window-ms) egress/agent-call-ms)}
               (assoc req :spec spec))))

(def default-timeouts
  {:connect-ms      10000
   :command-seconds 60})

(defn- require-str [spec k]
  (let [v (get spec k)]
    (when-not (string? v)
      (refusal/refuse! :refusal.foreign/connector-field-missing {:spec (redact/redact spec)}
                       {:field k}))
    v))

(def ^:private ssh-field-patterns
  {:user          #"[A-Za-z0-9_][A-Za-z0-9._-]{0,63}"
   :host          #"[A-Za-z0-9][A-Za-z0-9.-]{0,252}|\[?[0-9A-Fa-f:.]+\]?"
   :known-hosts   #"/[A-Za-z0-9._/-]+"
   :identity-file #"/[A-Za-z0-9._/-]+"})

(defn- ssh-field
  [spec k]
  (let [v (require-str spec k)]
    (when-not (and (re-matches (ssh-field-patterns k) v)
                   (not (str/starts-with? v "-"))
                   (not-any? #{".."} (str/split v #"/")))
      (refusal/refuse! :refusal.foreign/connector-field-invalid
                       {:field k :value v} {:field k :value v}))
    v))

(defn- reach-path
  [spec k]
  (local-files/path :refusal.foreign/reach-path-outside (require-str spec k)))

(defn- open-client!
  [{:keys [port] :as spec} f]
  (let [{:keys [connect-ms]} default-timeouts
        kh   (ssh-field spec :known-hosts)
        idf  (ssh-field spec :identity-file)
        ^String usr  (ssh-field spec :user)
        ^String host (ssh-field spec :host)
        port (or port policy/default-ssh-port)
        client (SSHClient.)]
    (try
      (.setConnectTimeout client connect-ms)
      (.loadKnownHosts client (io/file kh))
      (.connect client host (int port))
      (if-let [pass (:passphrase spec)]
        (.authPublickey client usr
                        ^"[Lnet.schmizz.sshj.userauth.keyprovider.KeyProvider;"
                        (into-array net.schmizz.sshj.userauth.keyprovider.KeyProvider
                                    [(.loadKeys client ^String idf (.toCharArray ^String pass))]))
        (.authPublickey client usr ^"[Ljava.lang.String;" (into-array String [idf])))
      (f client)
      (finally
        (try (.disconnect client) (catch java.io.IOException _ nil))))))

(defn- ensure-parent! [^String path]
  (when-let [parent (.getParentFile (io/file path))]
    (.mkdirs parent)))

(defn- fetch-sftp!
  [spec]
  (policy/assert-egress-allowed! spec)
  (let [^String remote (require-str spec :remote-path)
        ^String local  (reach-path spec :path)]
    (ensure-parent! local)
    (open-client!
     spec
     (fn [^SSHClient client]
       (let [^SFTPClient sftp (.newSFTPClient client)]
         (try (.get sftp remote local)
              (finally (.close sftp))))))
    local))

(defn- run-command-agent!
  [{:keys [port] :as spec}]
  (let [{:keys [connect-ms command-seconds]} default-timeouts
        cmd   (require-str spec :command)
        local (reach-path spec :path)
        kh    (ssh-field spec :known-hosts)
        usr   (ssh-field spec :user)
        host  (ssh-field spec :host)
        args  ["ssh" "-F" "/dev/null"
               "-o" "BatchMode=yes"
               "-o" "StrictHostKeyChecking=yes"
               "-o" (str "UserKnownHostsFile=" kh)
               "-o" (str "ConnectTimeout=" (int (/ connect-ms 1000)))
               "-p" (str (parse-long (str (or port policy/default-ssh-port))))
               "-l" usr
               "--" host cmd]]
    (ensure-parent! local)
    (let [pb   (doto (ProcessBuilder. ^java.util.List args)
                 (.redirectInput (ProcessBuilder$Redirect/from (io/file "/dev/null")))
                 (.redirectOutput (io/file local))
                 (.redirectError ProcessBuilder$Redirect/PIPE))
          proc (.start pb)
          done (effects/observed (str "EXEC ssh " host)
                                 #(.waitFor proc command-seconds TimeUnit/SECONDS))]
      (if-not done
        (do (.destroyForcibly proc)
            (refusal/refuse! :refusal.sources/command-agent-timeout
                             {:command cmd :host host :seconds command-seconds}))
        (let [err  (slurp (.getErrorStream proc))
              exit (.exitValue proc)]
          (when-not (zero? exit)
            (refusal/refuse! :refusal.sources/command-agent-exit
                             {:command cmd :exit exit :stderr err})))))
    local))

(defn- run-command!
  [spec]
  (policy/assert-egress-allowed! spec)
  (policy/assert-command-allowed! spec)
  (if (= :agent (:auth-mode spec))
    (run-command-agent! spec)
    (let [{:keys [command-seconds]} default-timeouts
          cmd   (require-str spec :command)
          local (reach-path spec :path)]
      (ensure-parent! local)
      (open-client!
       spec
       (fn [^SSHClient client]
         (let [session (.startSession client)]
           (try
             (let [c   (.exec session cmd)
                   out (IOUtils/readFully (.getInputStream c))]
               (.join c command-seconds TimeUnit/SECONDS)
               (let [exit (.getExitStatus c)]
                 (when (and exit (not (zero? exit)))
                   (refusal/refuse! :refusal.sources/command-exit
                                    {:command cmd :exit exit
                                     :stderr (str (IOUtils/readFully (.getErrorStream c)))})))
               (with-open [w (io/output-stream local)]
                 (.write w (.toByteArray out))))
             (finally (.close session))))))
      local)))

(defn- read-file!
  [spec]
  (let [src   (reach-path spec :source-path)
        local (reach-path spec :path)]
    (when-not (.exists (io/file src))
      (refusal/refuse! :refusal.foreign/source-path-missing {:source-path src}))
    (ensure-parent! local)
    (io/copy (io/file src) (io/file local))
    local))

(defn connector-reach! [{:keys [type] :as spec}]
  (if-let [reach (reach-fns type)]
    (reach spec)
    (refusal/refuse! :refusal.sources/reach-unsupported-type
                     {:spec (redact/redact spec)}
                     {:type type})))

(defn- csv-rows [spec] (mapv #(update-keys % keyword) (read-csv-rows spec)))

(defn rest-call!
  [spec req]
  (:result (egress/post "/rest" (assoc req :spec spec))))

(defrecord CsvSource [spec]
  api/Source
  (-profile [_] (source/profile :csv))
  (-probe [_] (read-csv-rows spec) nil)
  (-close [_] nil)
  api/Catalog
  (-tables [_] [csv-virtual-table])
  (-columns [_ _] (csv-list-columns spec))
  (-relations [_] [])
  api/Scan
  (-scan-profile [_ _] (:scan (source/profile :csv)))
  (-scan [_ _ pushed] (api/run-local (csv-rows spec) pushed)))

(defrecord AgentSource [spec session]
  api/Source
  (-profile [_] (source/profile (:type spec)))
  (-probe [_] (read-rows spec "SELECT 1") nil)
  (-close [_] nil)
  api/Catalog
  (-tables [_]
    (->> (read-rows spec (dialect/tables-sql (:type spec))) (keep :name) distinct sort vec))
  (-columns [_this table]
    (mapv (fn [{:keys [name type]}]
            {:name name :type type :ontology-type (dialect/sql-type->ontology-type type)})
          (read-rows spec (dialect/columns-sql (:type spec) table))))
  (-relations [_] (mapv dialect/->relation (read-rows spec (dialect/relations-sql (:type spec)))))
  api/Scan
  (-scan-profile [_ _] (:scan (source/profile (:type spec))))
  (-scan [this table pushed]
    (fjdbc/sql-scan (:type spec) table #(api/columns this table)
                    (fn [[sql & params]] (read-rows spec sql params))
                    (policy/scan-key spec table) pushed))
  api/Sql
  (-select [_ sql params limit]
    (if limit
      (try (vec (take limit (read-rows spec (dialect/cap-wrap (:type spec) sql limit) params)))
           (catch Exception e (throw (or (dialect/capped-refusal e) e))))
      (vec (read-rows spec sql params))))
  api/Land
  (-reach! [_] (reach! spec))
  api/Write
  (-write! [_ write-spec] (write! spec write-spec session))
  api/Invoke
  (-invoke! [_ webhook-id params opts] (invoke-webhook! spec webhook-id params opts)))

(defrecord RestAgentSource [spec]
  api/Source
  (-profile [_] (source/profile :rest))
  (-probe [_] (rest-call! spec {:op :tables}) nil)
  (-close [_] nil)
  api/Catalog
  (-tables [_] (rest-call! spec {:op :tables}))
  (-columns [_ table]
    (or (fhttp/declared-columns spec table) (rest-call! spec {:op :columns :table table})))
  (-relations [_] [])
  api/Scan
  (-scan-profile [_ table] (rest-call! spec {:op :scan-profile :table table}))
  (-scan [_ table pushed] (rest-call! spec {:op :scan :table table :scan pushed}))
  api/Invoke
  (-invoke! [_ webhook-id params opts] (invoke-webhook! spec webhook-id params opts)))

(defrecord LandedSource [spec]
  api/Source
  (-profile [_] (source/profile (:type spec)))
  (-probe [_] (api/unsupported :probe spec))
  (-close [_] nil)
  api/Scan
  (-scan-profile [_ _] (:scan (source/profile (:type spec))))
  (-scan [_ _ pushed]
    (if (= :csv (source/file-format (:type spec)))
      (api/run-local (csv-rows {:type :csv :path (reach! spec)}) pushed)
      (api/unsupported :scan spec)))
  api/Land
  (-reach! [_] (reach! spec))
  api/Invoke
  (-invoke! [_ webhook-id params opts] (invoke-webhook! spec webhook-id params opts)))

(defn- connector-call! [spec req]
  (:result (egress/post "/connector" (assoc req :spec spec))))

(defrecord ConnectorAgentSource [spec]
  api/Source
  (-profile [_] (source/profile (:type spec)))
  (-probe [_] (connector-call! spec {:op :probe}) nil)
  (-close [_] nil)
  api/Catalog
  (-tables [_] (connector-call! spec {:op :tables}))
  (-columns [_ table] (connector-call! spec {:op :columns :table table}))
  (-relations [_] [])
  api/Scan
  (-scan-profile [_ table] (connector-call! spec {:op :scan-profile :table table}))
  (-scan [_ table pushed] (connector-call! spec {:op :scan :table table :scan pushed}))
  api/Write
  (-write! [_ write-spec]
    (connector/written write-spec (connector-call! spec {:op :write :write-spec write-spec})))
  api/Invoke
  (-invoke! [_ op params opts]
    (connector-call! spec
                     (cond-> {:op :invoke :webhook-id op :params params
                              :opts (dissoc opts :timeout-ms)}
                       (:timeout-ms opts) (assoc :timeout-ms (:timeout-ms opts)))))
  api/Token
  (-token [_ table] (connector-call! spec {:op :token :table table})))

(defn- platform-spec
  [spec]
  (let [spec (if (map? (:config spec)) (merge (dissoc spec :config) (:config spec)) spec)
        cipher (:secret-cipher spec)]
    (cond-> (dissoc spec :secret-cipher)
      cipher (merge (seal/open-cipher (ctx/system-store) cipher)))))

(defn- proxy-socket-factory
  [{:keys [source-name] :as spec}]
  (when (= :agent-proxy (policy/connection spec))
    (proxy-client/socket-factory
     {:url (str (str/replace (egress/proxy-hub-url) #"^http" "ws") "/api/agent-proxy/stream")
      :token (egress/agent-token egress/proxy-client-peer)
      :source (name source-name)})))

(defn- connector-source
  [spec via]
  (let [allowed (:route (source/profile (:type spec)))]
    (if (= :agent (if (contains? allowed via) via (policy/route spec)))
      (->ConnectorAgentSource spec)
      (let [spec (platform-spec spec)]
        (connector/open spec (proxy-socket-factory spec))))))

(defn connect
  ([auth spec] (connect auth spec nil))
  ([auth spec via] (connect auth spec via nil))
  ([auth spec via session]
   (assert-source-cleared! auth spec)
   (cond
     (= :connector (policy/read-mode spec)) (connector-source spec via)
     (= :landed (policy/read-mode spec)) (->LandedSource spec)
     (= :rest (policy/read-mode spec))   (->RestAgentSource spec)
     :else (case (or via (policy/route spec))
             :csv    (->CsvSource spec)
             :agent  (->AgentSource spec session)
             :direct (fjdbc/source spec)))))

(defonce ^:private agent-connectors (atom {}))

(defonce announced

  (atom {}))

(defonce ^:private silent-agents (atom #{}))

(defn refresh-announced!
  []
  (doseq [a (egress/agent-names)]
    (try (let [cs (:connectors (egress/post-to a "/status" {:timeout-ms 5000}))]
           (swap! agent-connectors assoc a
                  (into {} (for [[t c] (when (map? cs) cs)]
                             [(keyword t) (assoc (select-keys c [:profile :form]) :agent a)])))
           (swap! silent-agents disj a))
         (catch InterruptedException e (throw e))
         (catch Exception e
           (when-not (contains? @silent-agents a)
             (println (str "[connectors] egress agent " a " did not say which connectors it runs: "
                           (.getMessage e))))
           (swap! silent-agents conj a))))
  (let [{:keys [connectors
                left-out]} (source/announced
                            (apply merge (map @agent-connectors (egress/agent-names))))]
    (reset! announced connectors)
    (when (seq left-out)
      (println (str "[connectors] left out " left-out
                    ": an agent announces a type the platform has"))))
  :ok)

(defonce ^:private announcer (periodic/slot))

(defn- start-announcing! [ms]
  (periodic/start! announcer "connector-announcements" 0 ms
                   #(noted/quietly "connector announcements" (refresh-announced!))))

(defn- stop-announcing! [] (periodic/stop! announcer))

(defn datasource-path
  [auth spec land]
  (let [spec (assert-source-cleared! auth spec)]
    (case (policy/read-mode spec)
      :csv    (local-files/path (:path spec))
      :landed (land #(reach! spec))
      nil)))

(def module
  {:name :foreign-sources
   :provides {:foreign/connect (fn [] connect)
              :foreign/source-ops {:invoke! api/invoke! :close! api/close!}
              :foreign/datasources {:cleared!         assert-source-cleared!
                                    :path             datasource-path
                                    :read-rows        read-rows
                                    :agent-reachable? agent-reachable?}}
   :jobs [{:name :connectors :start connector/assert-connectors!}
          {:name :connector-announcements :every "SUTRA_CONNECTOR_ANNOUNCE_MS" :default 60000
           :start start-announcing! :stop stop-announcing!}]})

