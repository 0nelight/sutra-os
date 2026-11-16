(ns support.pod
  (:require [sutra.catalog.grants :as grants]
            [clojure.java.shell :as sh]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [sutra.repos.objects :as g]
            [sutra.store.pg.layout :as layout]
            [sutra.serve.lifecycle :as lc]
            [sutra.serve.server :as server]
            [sutra.access.http.auth :as auth]
            [sutra.access.principals :as pr]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.store.control.collections :as collections]
            [sutra.types.world-rules :as rules]
            [sutra.store.mem :as store-mem]
            [sutra.store.storage.heads :as heads]
            [sutra.datasets.mem :as datasets-mem]
            [sutra.streams.mem :as streams-mem]
            [sutra.coord.mem :as coord-mem]
            [sutra.blobs.mem :as blobs-mem]
            [sutra.blobs.fs]
            [sutra.store.access.scope]
            [clojure.java.io]
            [sutra.kit.system :as sys]
            [sutra.ref.clock :as clock])
  (:import [java.net Socket InetSocketAddress ServerSocket]
           [java.util UUID Base64]
           [java.security KeyFactory]
           [java.security.spec X509EncodedKeySpec MGF1ParameterSpec]
           [javax.crypto Cipher]
           [javax.crypto.spec OAEPParameterSpec PSource$PSpecified]))

(def ^:private source-mysql-image
  "docker.io/library/mariadb:11")

(defn- uuid-suffix []
  (str/replace (str (UUID/randomUUID)) "-" ""))

(def ^:private port-band
  (delay
    (let [base (some-> (System/getenv "SUTRA_TEST_PORT_BASE") Integer/parseInt)
          span (some-> (System/getenv "SUTRA_TEST_PORT_SPAN") Integer/parseInt)]
      (when (and base span (pos? span)) [base (+ base span)]))))

(def ^:private port-cursor (atom nil))

(defn- bindable?
  [port]
  (try (with-open [s (ServerSocket.)]
         (.bind s (InetSocketAddress. "0.0.0.0" (int port))) true)
       (catch Exception _ false)))

(defn free-port
  []
  (if-let [[lo hi] @port-band]
    (loop [attempts 0]
      (when (>= attempts (- hi lo))
        (throw (ex-info "no free port left in this shard's band"
                        {:band [lo hi]})))
      (let [p (swap! port-cursor
                     (fn [c] (if (and c (<= lo c) (< c (dec hi))) (inc c) lo)))]
        (if (bindable? p) p (recur (inc attempts)))))
    (with-open [s (ServerSocket. 0)] (.getLocalPort s))))

(defn egress-port
  []
  (Integer/parseInt (or (System/getenv "SUTRA_EGRESS_PORT") "8731")))

(defn- bind-failure?
  [^Throwable e]
  (boolean
   (or (some #(instance? java.net.BindException %)
             (take-while some? (iterate #(.getCause ^Throwable %) e)))
       (let [{:keys [err out]} (ex-data e)]
         (re-find #"(?i)address already in use|port is already allocated|bind: address"
                  (str err out))))))

(defn with-free-port
  [f]
  (loop [attempt 1]
    (let [r (try (f (free-port))
                 (catch Exception e
                   (if (and (< attempt 10) (bind-failure? e))
                     ::retry
                     (throw e))))]
      (if (= r ::retry) (recur (inc attempt)) r))))

(defn- sh! [& args]
  (let [{:keys [exit out err]} (apply sh/sh args)]
    (when-not (zero? exit)
      (throw (ex-info (str "command failed (" exit "): " (str/join " " args))
                      {:out out :err err})))
    (str/trim out)))

(defn- jdbc-ready?
  [url]
  (try
    (with-open [c (jdbc/get-connection url {:user "root" :password ""})]
      (jdbc/execute-one! c ["SELECT 1"]) true)
    (catch Exception _ false)))

(defn- wait-jdbc! [url timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (jdbc-ready? url) true
        (> (System/currentTimeMillis) deadline)
        (throw (ex-info "the MySQL source did not become query-ready in time" {:url url}))
        :else (do (Thread/sleep 25) (recur))))))

(defn start-source-mysql!
  []
  (with-free-port
    (fn [port]
      (let [name (str "sutra-test-srcdb-" (uuid-suffix))
            base (str "jdbc:mysql://127.0.0.1:" port)]
        (sh! "podman" "run" "-d" "--rm" "--name" name
             "-e" "MARIADB_ROOT_HOST=%" "-e" "MARIADB_ALLOW_EMPTY_ROOT_PASSWORD=1"
             "-p" (str port ":3306") source-mysql-image)
        (try
          (wait-jdbc! base 60000)
          (catch Exception e
            (try (sh! "podman" "rm" "-f" "-v" name) (catch Exception _ nil))
            (throw e)))
        {:name name :port port :base base :host (str "127.0.0.1:" port)}))))

(defn stop-source-mysql! [{:keys [name]}]
  (when name (try (sh! "podman" "rm" "-f" "-v" name) (catch Exception _ nil))))

(def ^:private imap-image "docker.io/greenmail/standalone:latest")

(defn- greeting-ready?
  [host port]
  (try
    (with-open [s (Socket.)]
      (.connect s (InetSocketAddress. ^String host (int port)) 1000)
      (.setSoTimeout s 1000)
      (pos? (.read (.getInputStream s))))
    (catch Exception _ false)))

(defn- wait-greeting! [host port timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond
        (greeting-ready? host port) true
        (> (System/currentTimeMillis) deadline)
        (throw (ex-info "service did not greet in time" {:host host :port port}))
        :else (do (Thread/sleep 300) (recur))))))

(defn start-imap!
  []
  (with-free-port
    (fn [imap]
      (with-free-port
        (fn [smtp]
          (let [name (str "sutra-test-imap-" (uuid-suffix))]
            (sh! "podman" "run" "-d" "--rm" "--name" name
                 "-e" (str "GREENMAIL_OPTS=-Dgreenmail.setup.test.all "
                           "-Dgreenmail.hostname=0.0.0.0 -Dgreenmail.auth.disabled")
                 "-p" (str "127.0.0.1:" imap ":3143")
                 "-p" (str "127.0.0.1:" smtp ":3025")
                 imap-image)
            (try
              (wait-greeting! "127.0.0.1" smtp 60000)
              (wait-greeting! "127.0.0.1" imap 60000)
              (catch Exception e
                (try (sh! "podman" "rm" "-f" "-v" name) (catch Exception _ nil))
                (throw e)))
            {:name name :host "127.0.0.1" :imap-port imap :smtp-port smtp}))))))

(defn stop-imap! [{:keys [name]}]
  (when name (try (sh! "podman" "rm" "-f" "-v" name) (catch Exception _ nil))))

(def ^:private pg-image "docker.io/library/postgres:16-alpine")

(defn- pg-ready?
  [url user password]
  (try
    (with-open [c (jdbc/get-connection url {:user user :password password})]
      (jdbc/execute-one! c ["SELECT 1"]) true)
    (catch Exception _ false)))

(def ^:private pg-server
  (atom nil))

(defn- start-pg-server!
  []
  (if-let [reuse (not-empty (System/getenv "SUTRA_TEST_PG_URL"))]
    {:base reuse :user (or (not-empty (System/getenv "SUTRA_TEST_PG_USER")) "sutra")
     :password (or (not-empty (System/getenv "SUTRA_TEST_PG_PASSWORD")) "sutra") :name nil}
    (with-free-port
      (fn [port]
        (let [name     (str "sutra-test-pg-" (uuid-suffix))
              user     "sutra"
              password "sutra"
              base     (str "jdbc:postgresql://127.0.0.1:" port "/")]
          (sh! "podman" "run" "-d" "--rm" "--name" name
               "-e" (str "POSTGRES_USER=" user)
               "-e" (str "POSTGRES_PASSWORD=" password)
               "-e" "POSTGRES_DB=postgres"
               "-p" (str "127.0.0.1:" port ":5432")
               "--tmpfs" "/var/lib/postgresql/data:rw,size=8g"
               pg-image
               "-c" "fsync=off" "-c" "synchronous_commit=off" "-c" "full_page_writes=off")
          (try
            (let [deadline (+ (System/currentTimeMillis) 60000)]
              (loop []
                (cond
                  (pg-ready? (str base "postgres") user password) true
                  (> (System/currentTimeMillis) deadline)
                  (throw (ex-info "postgres did not become query-ready in time"
                                  {:url (str base "postgres")}))
                  :else (do (Thread/sleep 100) (recur)))))
            (catch Exception e
              (try (sh! "podman" "rm" "-f" "-v" name) (catch Exception _ nil))
              (throw e)))
          (.addShutdownHook (Runtime/getRuntime)
                            (Thread. ^Runnable
                             (fn [] (try (sh! "podman" "rm" "-f" "-v" name)
                                         (catch Exception _ nil)))))
          {:base base :user user :password password :name name})))))

(defn- pg-server! []
  (or @pg-server (swap! pg-server #(or % (start-pg-server!)))))

(declare pg-template!)

(defn start-pg!
  []
  (let [{:keys [base user password]} (pg-server!)
        db (str "pod_" (uuid-suffix))]
    (with-open [c (jdbc/get-connection (str base "postgres")
                                       {:user user :password password})]
      (jdbc/execute! c [(str "CREATE DATABASE " db " TEMPLATE " (pg-template!))]))
    {:db db :url (str base db) :user user :password password}))

(defn start-empty-pg!
  []
  (let [{:keys [base user password]} (pg-server!)
        db (str "empty_" (uuid-suffix))]
    (with-open [c (jdbc/get-connection (str base "postgres") {:user user :password password})]
      (jdbc/execute! c [(str "CREATE DATABASE " db)]))
    {:db db :url (str base db) :user user :password password}))

(defn stop-pg!
  [{:keys [db]}]
  (when db
    (let [{:keys [base user password]} (pg-server!)]
      (try
        (with-open [c (jdbc/get-connection (str base "postgres")
                                           {:user user :password password})]
          (jdbc/execute! c [(str "SELECT pg_terminate_backend(pid) FROM pg_stat_activity"
                                 " WHERE datname = '" db "' AND pid <> pg_backend_pid()")])
          (jdbc/execute! c [(str "DROP DATABASE IF EXISTS " db)]))
        (catch Exception _ nil)))))

(defn send-mail!
  [{:keys [host smtp-port from to subject body attachments]}]
  (let [hexes (mapv (fn [{:keys [filename mime bytes]}]
                      [filename (or mime "application/octet-stream")
                       (apply str (map #(format "%02x" (bit-and % 0xff)) bytes))])
                    attachments)
        script "import sys, json, smtplib
from email.message import EmailMessage
host, port, frm, to, subj, body, atts = (sys.argv[1], int(sys.argv[2]), sys.argv[3], sys.argv[4],
                                        sys.argv[5], sys.argv[6], json.loads(sys.argv[7]))
msg = EmailMessage(); msg['From']=frm; msg['To']=to; msg['Subject']=subj; msg.set_content(body)
for fn, mime, hx in atts:
    maintype, _, subtype = mime.partition('/')
    msg.add_attachment(bytes.fromhex(hx), maintype=maintype,
                       subtype=subtype or 'octet-stream', filename=fn)
with smtplib.SMTP(host, port, timeout=10) as s: s.send_message(msg)
print('sent')"
        {:keys [exit out err]}
        (sh/sh "python3" "-c" script host (str smtp-port)
               (or from "dispatch@aviation.test") to subject (or body "")
               (json/write-str hexes))]
    (when-not (zero? exit)
      (throw (ex-info "send-mail! failed" {:exit exit :out out :err err})))))

(defonce ^:private served-url (atom nil))

(defn base-url [] @served-url)

(defn- installed [] @@#'sys/installed)

(defn- serve-pod!
  ([] (serve-pod! (installed)))
  ([system]
   (alter-var-root #'auth/*password-policy?* (constantly false))
   (alter-var-root #'auth/*initial-passwords?* (constantly false))
   (alter-var-root #'server/*enforce-all-contracts?* (constantly true))
   (let [port (with-free-port
                (fn [p]
                  (try
                    (server/start! {:port p :nrepl? false :system system})
                    (catch Exception e
                      (try (server/stop!) (catch Exception _ nil))
                      (throw e)))
                  p))]
     (tx/set-base! (str "http://localhost:" port))
     (reset! served-url (str "http://localhost:" port))
     port)))

(defn- rm-rf! [^String path]
  (try (doseq [^java.io.File x (reverse (file-seq (java.io.File. path)))] (.delete x))
       (catch Exception _ nil)))

(defn pg-config
  [{:keys [url user password]}]
  {"SUTRA_PG_URL" url "SUTRA_PG_USER" user "SUTRA_PG_PASSWORD" password})

(defn- on-pg!
  [pg-handle m]
  (let [prev (installed)
        base (if (seq (:provides prev)) prev (server/system))
        s    (-> (sys/configure base (merge (:config base) (pg-config pg-handle)))
                 (update :provides merge m))]
    (sys/install! s)
    (fn []
      (sys/close! s)
      (reset! @#'sys/installed prev))))

(defn- all-stores!
  [{:keys [pg-handle lake-root]}]
  (g/forget-all-repos!)
  (let [restore! (on-pg! pg-handle {:git/dir (constantly (str lake-root "/git"))})]
    (fn []
      (restore!)
      (g/forget-all-repos!))))

(defn reset-dataset-store!
  []
  (with-open [c (jdbc/get-connection ((sys/required (sys/process) :pg/ds) nil))]
    (doseq [n (map (comp str first vals)
                   (jdbc/execute!
                    c ["SELECT nspname FROM pg_namespace
                        WHERE nspname NOT LIKE 'pg\\_%'
                          AND nspname NOT IN ('information_schema', 'public')"]))]
      (jdbc/execute! c [(str "DROP SCHEMA IF EXISTS \"" n "\" CASCADE")]))
    (jdbc/execute! c ["DROP SEQUENCE IF EXISTS public.dataset_seq CASCADE"])
    (jdbc/with-transaction [t c] (layout/install! t))))

(defn with-dataset-store
  [f]
  (let [h        (start-pg!)
        restore! (on-pg! h {})]
    (try (f)
         (finally
           (restore!)
           (stop-pg! h)))))

(defonce ^:private pg-template-db (atom nil))

(defn pg-template!
  []
  (or @pg-template-db
      (swap! pg-template-db
             (fn [cur]
               (or cur
                   (let [{:keys [base user password]} (pg-server!)
                         db   (str "tmpl_" (uuid-suffix))
                         root (str (System/getProperty "java.io.tmpdir")
                                   "/sutra-pod-tmpl-" (uuid-suffix))]
                     (with-open [c (jdbc/get-connection (str base "postgres")
                                                        {:user user :password password})]
                       (jdbc/execute! c [(str "CREATE DATABASE " db)]))
                     (let [restore! (all-stores!
                                     {:pg-handle {:db db :url (str base db)
                                                  :user user :password password}
                                      :lake-root root})]
                       (try
                         (lc/start!)
                         (finally
                           (restore!)
                           (rm-rf! root))))
                     db))))))

(defn- all-stores-around
  [run! f]
  (let [pgh      (start-pg!)
        root     (str (System/getProperty "java.io.tmpdir") "/sutra-pod-allstores-"
                      (uuid-suffix))
        restore! (all-stores! {:pg-handle pgh :lake-root root})]
    (try
      (run! f)
      (finally
        (restore!)
        (stop-pg! pgh)
        (rm-rf! root)))))

(defn all-stores-pod-fixture
  [{:keys [allow-anon] :or {allow-anon true}}]
  (fn [f]
    (all-stores-around
     (fn [run-tests]
       (let [prev-anon auth/*allow-anon*
             prev-sys  (installed)]
         (try
           (alter-var-root #'auth/*allow-anon* (constantly (boolean allow-anon)))
           (serve-pod! prev-sys)
           (run-tests)
           (finally
             (try (server/stop!) (catch Exception _ nil))
             (reset! @#'sys/installed prev-sys)
             (alter-var-root #'auth/*allow-anon* (constantly prev-anon))))))
     f)))

(defn all-stores-only-fixture
  [f]
  (all-stores-around
   (fn [run-tests]
     (lc/start!)
     (run-tests))
   f))

(def with-stores-only
  all-stores-only-fixture)

(def ^:private store-modules
  #{:store-pg :datasets-pg :coord-pg :blobs-pg :streams-pg :blob-store :git-store
    :pg-ds :pg-telemetry :cluster-bus})

(defn- mem-system
  [root]
  (let [config  (into {} (System/getenv))
        db      (store-mem/store store-mem/empty-seed
                                 (merge (rules/store-opts clock/system)
                                        heads/store-opts
                                        {:collections collections/all}))
        ds      (datasets-mem/datasets)
        streams (streams-mem/streams)
        coord   (coord-mem/coordinator)
        blobs   (blobs-mem/blobs)]
    (sys/configure
     (sys/assemble
      (conj (into [] (remove (comp store-modules :name)) (server/modules))
            {:name     :mem-stores
             :provides {:git/dir     (constantly (str root "/git"))
                        :store/db    (fn [] db)
                        :datasets/db (fn [_] ds)
                        :streams/db  (fn [_] streams)
                        :coord/db    (fn [] coord)
                        :blobs/db    (fn [] blobs)}}))
     config)))

(defn- mem-root [] (str (System/getProperty "java.io.tmpdir") "/sutra-mempod-" (uuid-suffix)))

(defn mem-pod-fixture
  [{:keys [allow-anon] :or {allow-anon true}}]
  (fn [f]
    (let [root      (mem-root)
          prev-anon auth/*allow-anon*
          prev-sys  (installed)]
      (g/forget-all-repos!)
      (alter-var-root #'auth/*allow-anon* (constantly (boolean allow-anon)))
      (try
        (serve-pod! (mem-system root))
        (f)
        (finally
          (try (server/stop!) (catch Exception _ nil))
          (reset! @#'sys/installed prev-sys)
          (alter-var-root #'auth/*allow-anon* (constantly prev-anon))
          (g/forget-all-repos!)
          (rm-rf! root))))))

(defn with-mem-stores-only
  [f]
  (let [root     (mem-root)
        prev-sys (installed)]
    (g/forget-all-repos!)
    (try
      (sys/install! (mem-system root))
      (lc/start!)
      (f)
      (finally
        (reset! @#'sys/installed prev-sys)
        (g/forget-all-repos!)
        (rm-rf! root)))))

(def with-all-stores-pod (all-stores-pod-fixture {}))

(def with-store-serving-pod (all-stores-pod-fixture {}))

(def with-pod (all-stores-pod-fixture {}))

(def with-pod-auth (all-stores-pod-fixture {:allow-anon false}))

(def with-mem-pod
  (mem-pod-fixture {}))

(def with-mem-pod-auth (mem-pod-fixture {:allow-anon false}))

(defn seal-credential
  [pubkey-b64 cred]
  (let [spki (.decode (Base64/getDecoder) ^String pubkey-b64)
        pub  (.generatePublic (KeyFactory/getInstance "RSA") (X509EncodedKeySpec. spki))
        oaep (OAEPParameterSpec. "SHA-256" "MGF1" MGF1ParameterSpec/SHA256
                                 PSource$PSpecified/DEFAULT)
        ciph (doto (Cipher/getInstance "RSA/ECB/OAEPWithSHA-256AndMGF1Padding")
               (.init Cipher/ENCRYPT_MODE pub oaep))]
    (.encodeToString (Base64/getEncoder)
                     (.doFinal ciph (.getBytes (pr-str cred) "UTF-8")))))

(defn as-admin
  [f]
  (tx/with-token (get-in (api/post! :login {:username grants/admin-username
                                            :password pr/default-admin-password})
                         [:body :token])
    (f)))

(defn with-scratch-blobs
  [f]
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "sutra-test-blobs" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (with-redefs [sutra.store.access.scope/blobs (constantly (sutra.blobs.fs/blobs dir))]
        (f))
      (finally
        (doseq [^java.io.File x (reverse (file-seq (clojure.java.io/file dir)))] (.delete x))))))
