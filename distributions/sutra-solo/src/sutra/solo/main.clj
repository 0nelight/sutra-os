(ns sutra.solo.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.foundation.env :as env]
            [sutra.kit.system :as sys]
            [sutra.solo.connectors :as connectors]
            [sutra.solo.port :as port])
  (:import (io.zonky.test.db.postgres.embedded EmbeddedPostgres)
           (java.io File)
           (java.net ServerSocket)
           (java.nio.file Files)
           (java.nio.file.attribute PosixFilePermissions)
           (java.security SecureRandom)
           (java.sql Connection)))

(defn home
  ^File []
  (io/file (or (not-empty (System/getenv "SUTRA_HOME"))
               (some-> (not-empty (System/getenv "XDG_DATA_HOME")) (str "/sutra"))
               (str (System/getProperty "user.home") "/.local/share/sutra"))))

(defn- private-dir! ^File [^File d]
  (.mkdirs d)
  (try (Files/setPosixFilePermissions (.toPath d) (PosixFilePermissions/fromString "rwx------"))
       (catch UnsupportedOperationException _ nil))
  d)

(defn- random-secret []
  (let [b (byte-array 24)]
    (.nextBytes (SecureRandom.) b)
    (apply str (map #(format "%02x" %) b))))

(defn credentials!
  [^File dir]
  (let [f (io/file dir "zugang.edn")]
    (if (.exists f)
      (edn/read-string (slurp f))
      (let [c {:pg-password (random-secret) :admin-password (random-secret)}]
        (spit f (pr-str c))
        (try (Files/setPosixFilePermissions (.toPath f)
                                            (PosixFilePermissions/fromString "rw-------"))
             (catch UnsupportedOperationException _ nil))
        c))))

(def ^:private hba
  (str "local all all scram-sha-256\n"
       "host  all all 127.0.0.1/32 scram-sha-256\n"
       "host  all all ::1/128 scram-sha-256\n"))

(defn- exec! [^Connection c sql]
  (with-open [s (.createStatement c)] (.execute s sql)))

(defn- query-1 [^Connection c sql]
  (with-open [s (.createStatement c) r (.executeQuery s sql)]
    (when (.next r) (.getObject r 1))))

(defn start-postgres!
  [^File dir {:keys [pg-password]}]
  (let [data (io/file dir "pg")
        pg   (-> (EmbeddedPostgres/builder)
                 (.setDataDirectory data)
                 (.setCleanDataDirectory false)
                 (.setServerConfig "listen_addresses" "127.0.0.1")
                 (.setConnectConfig "password" pg-password)
                 (.start))]
    (with-open [c (.getConnection (.getPostgresDatabase pg))]
      (exec! c (str "ALTER ROLE postgres PASSWORD '" pg-password "'"))
      (let [f (io/file data "pg_hba.conf")]
        (when (not= hba (slurp f))
          (spit f hba)
          (query-1 c "SELECT pg_reload_conf()")))
      (when-not (query-1 c "SELECT 1 FROM pg_database WHERE datname = 'sutra'")
        (exec! c "CREATE DATABASE sutra")))
    {:pg pg :port (.getPort pg)}))

(defn- free-port []
  (with-open [s (ServerSocket. 0)] (.getLocalPort s)))

(defn profile
  [^File dir port egress-port http-port {:keys [pg-password admin-password]}]
  (let [p #(.getPath (io/file dir %))]
    {"SUTRA_PG_URL"          (str "jdbc:postgresql://127.0.0.1:" port "/sutra")
     "SUTRA_PG_USER"         "postgres"
     "SUTRA_PG_PASSWORD"     pg-password
     "SUTRA_PG_POOL_CONTROL" "8"
     "SUTRA_PG_POOL_BUILD"   "4"
     "SUTRA_PG_POOL_AUTH"    "2"
     "SUTRA_HTTP_BIND"       "127.0.0.1"
     "SUTRA_HTTP_PORT"       (str http-port)
     "SUTRA_ALLOW_ANON"      "0"
     "SUTRA_LOCAL_USER"      "admin"
     "SUTRA_PUBLIC_DIR"
     (.getPath (io/file (System/getProperty "user.dir") ".run" "solo-public"))
     "SUTRA_ADMIN_PASSWORD"  admin-password
     "SUTRA_BLOB_STORE"      "pg"
     "SUTRA_EGRESS_URL"      (str "http://127.0.0.1:" egress-port)
     "SUTRA_EGRESS_KEYFILE"  (p "egress-agent-rsa.pkcs8")
     "SUTRA_BLOB_DIR"        (p "blobs")
     "SUTRA_GIT_DIR"         (p "git")
     "SUTRA_XFER_DIR"        (p "xfer")
     "SUTRA_CLIENT_LOG"      (p "client.log")
     "SUTRA_CSV_ROOTS"       (p "eingang")
     "SUTRA_SCHEDULER_MS"    "5000"
     "SUTRA_AUTOMATE_MS"     "60000"
     "SUTRA_STREAM_MS"       "10000"}))

(defn- pakete? [] (some? (io/resource "sutra/packages/standard.clj")))

(defn- paket
  [wahl]
  (when (pakete?) ((requiring-resolve 'sutra.packages.standard/by-id) wahl)))

(defn- paket-ids []
  (when (pakete?) (map :id @(requiring-resolve 'sutra.packages.standard/catalogue))))

(defn- transit-bytes ^bytes [v]
  (let [out (java.io.ByteArrayOutputStream.)]
    ((requiring-resolve 'cognitect.transit/write)
     ((requiring-resolve 'cognitect.transit/writer) out :json) v)
    (.toByteArray out)))

(defn- call! [base token path body]
  (let [req  (requiring-resolve 'org.httpkit.client/request)
        read (requiring-resolve 'cognitect.transit/read)
        rdr  (requiring-resolve 'cognitect.transit/reader)
        {:keys [status body error]}
        @(req {:method :post :url (str base path) :timeout 600000
               :headers (cond-> {"content-type" "application/transit+json"
                                 "accept"       "application/transit+json"}
                          token (assoc "authorization" (str "Bearer " token)))
               :body (transit-bytes body) :as :stream})
        v (when body (try (read (rdr body :json)) (catch Exception _ nil)))]
    (when (or error (not (<= 200 (or status 0) 299)))
      (throw (ex-info (str path " -> HTTP " status) {:status status :body v :error error})))
    v))

(defn seed!
  [^File dir {:keys [admin-password]}]
  (let [marker (io/file dir "paket.edn")
        wahl   (or (env/getenv "SUTRA_SOLO_PAKET") (if (pakete?) "privat" "keins"))
        eintrag (when-not (or (.exists marker) (= "keins" wahl)) (paket wahl))
        datei  (:file eintrag)]
    (cond
      (.exists marker) nil
      (= "keins" wahl) (spit marker (pr-str {:paket "keins"}))
      (nil? datei)     (throw (ex-info (str "SUTRA_SOLO_PAKET=" wahl " -- bekannt: keins, "
                                            (str/join ", " (paket-ids)))
                                       {:wahl wahl}))
      :else
      (let [base   (str "http://127.0.0.1:" (env/getenv "SUTRA_HTTP_PORT") "/api")
            token  (:token (call! base nil "/login" {:username "admin" :password admin-password}))
            r      (call! base token "/packages/install" {:package wahl :land? true})]
        (spit marker
              (pr-str {:paket wahl :datei datei :version (:version r)
                       :am (str (java.time.Instant/now))}))
        (println (str "Standardpaket '" wahl "' " (:version r) " eingespielt (" datei "); "
                      "spaetere Versionen: bin/sutra package upgrade " wahl))))))

(defn land-pushes!
  [^File dir {:keys [admin-password]}]
  (let [marker (io/file dir "regeln.edn")]
    (when-not (.exists marker)
      (let [base  (str "http://127.0.0.1:" (env/getenv "SUTRA_HTTP_PORT") "/api")
            token (:token (call! base nil "/login" {:username "admin" :password admin-password}))]
        (call! base token "/branches/main/policy" {:land-pushes true})
        (spit marker (pr-str {:land-pushes {:main true} :am (str (java.time.Instant/now))}))))))

(defn cli-creds!
  ^File [^File dir http-port {:keys [admin-password]}]
  (let [f (io/file dir "cli-creds.edn")]
    (spit f (pr-str {:base-url (str "http://127.0.0.1:" http-port)
                     :users {:admin {:username "admin" :password admin-password}}}))
    (try (Files/setPosixFilePermissions (.toPath f) (PosixFilePermissions/fromString "rw-------"))
         (catch UnsupportedOperationException _ nil))
    f))

(defn- http-port! [^File dir]
  (let [{p :port via :via} (port/choose dir {:env-port (not-empty (System/getenv "SUTRA_HTTP_PORT"))
                                             :stored   (port/stored dir)})]
    (when-not p
      (throw (ex-info (str "kein freier Port in " port/range-start ".."
                           (+ port/range-start port/range-size -1) " — SUTRA_HTTP_PORT setzen")
                      {:dir (.getPath dir)})))
    (when (= :derived via) (port/persist! dir p))
    [p via]))

(defn -main [& _]
  (let [dir   (private-dir! (home))
        _     (doseq [d ["eingang" "git" "xfer"]] (.mkdirs (io/file dir d)))
        creds (credentials! dir)
        {:keys [pg port]} (start-postgres! dir creds)
        eport (free-port)
        [hport via] (http-port! dir)
        conns (connectors/start! dir (System/getenv "SUTRA_SOLO_CONNECTORS"))
        cfg   (merge (profile dir port eport hport creds) (:config conns) (into {} (System/getenv)))
        _     (sys/install! {:config cfg})
        ccred (cli-creds! dir hport creds)
        _     ((requiring-resolve 'sutra.sources.egress/start!)
               {:keyfile (env/getenv "SUTRA_EGRESS_KEYFILE") :port eport :ip "127.0.0.1"})
        start (requiring-resolve 'sutra.serve.server/start!)
        stop  (requiring-resolve 'sutra.serve.server/stop!)]
    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. ^Runnable (fn [] (stop)
                                           ((requiring-resolve
                                             'sutra.sources.egress/stop!))
                                           (connectors/stop! conns) (.close ^EmbeddedPostgres pg))))
    (start {:nrepl? false
            :system (sys/configure ((requiring-resolve 'sutra.serve.server/assembled)) cfg)})
    (when-let [p (some-> (System/getenv "SUTRA_SOLO_NREPL_PORT") parse-long)]
      ((requiring-resolve 'nrepl.server/start-server)
       :bind "127.0.0.1" :port p :handler @(requiring-resolve 'cider.nrepl/cider-nrepl-handler))
      (spit (io/file dir ".nrepl-port") (str p))
      (println (str "nREPL auf 127.0.0.1:" p)))
    (land-pushes! dir creds)
    (seed! dir creds)
    (println
     (str "\nSutra-OS laeuft allein auf diesem Rechner.\n"
          "  Daten:    " (.getPath dir) "\n"
          (if (.isFile (io/file (env/getenv "SUTRA_PUBLIC_DIR") "index.html"))
            (str "  Oeffnen:  http://127.0.0.1:" hport
                 "/  (ohne Anmeldung als admin; Passwort fuer andere Wege in "
                 (.getPath (io/file dir "zugang.edn")) ")\n")
            (str "  API:      http://127.0.0.1:" hport
                 "/api  (ohne Web-Oberflaeche; Passwort in "
                 (.getPath (io/file dir "zugang.edn")) ")\n"))
          "  Port:     " hport
          (case via
            :env     " (SUTRA_HTTP_PORT)"
            :stored  (str " (aus " (.getPath (port/instance-file dir)) ")")
            :derived (str " (neu gewaehlt, festgehalten in "
                          (.getPath (port/instance-file dir)) ")")) "\n"
          "  CLI:      einmal im Arbeitsordner: bin/sutra bind http://127.0.0.1:" hport
          " --creds " (.getPath ccred) "\n"
          "            danach dort ohne --base/--creds; ein Push auf main landet sofort (Regel "
          "land-pushes)\n"
          "  Import:   Dateien (z. B. Kontoauszug-CSV) nach "
          (.getPath (io/file dir "eingang")) "\n"
          (apply str (for [{:keys [type url log]} (:children conns)]
                       (str "  Connector: " (name type) " auf " url " (Log " log ")\n")))))
    @(promise)))
