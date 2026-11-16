(ns sutra.solo.connectors
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)
           (java.lang ProcessBuilder ProcessBuilder$Redirect)
           (java.net ServerSocket)
           (java.nio.file Files)
           (java.nio.file.attribute PosixFilePermissions)
           (java.security SecureRandom)))

(defn parse
  [s]
  (vec (for [pair (str/split (str/trim (str s)) #"[,\s]+")
             :when (seq pair)
             :let [[t d] (str/split pair #"=" 2)]]
         (if (and (re-matches #"[a-z][a-z0-9-]*" (str t)) (seq d))
           [(keyword t) d]
           (throw (ex-info (str "SUTRA_SOLO_CONNECTORS: " (pr-str pair) " is not type=verzeichnis")
                           {:pair pair}))))))

(defn- token-file! ^File [^File dir]
  (let [f (io/file dir "connector-sidecar.token")]
    (when-not (.exists f)
      (let [b (byte-array 24)]
        (.nextBytes (SecureRandom.) b)
        (spit f (apply str (map #(format "%02x" %) b))))
      (try (Files/setPosixFilePermissions (.toPath f) (PosixFilePermissions/fromString "rw-------"))
           (catch UnsupportedOperationException _ nil)))
    f))

(defn- free-port []
  (with-open [s (ServerSocket. 0)] (.getLocalPort s)))

(defn- start-one! [^File logs ^File token [t dir]]
  (let [d (io/file dir)]
    (when-not (.isFile (io/file d "deps.edn"))
      (throw (ex-info (str "SUTRA_SOLO_CONNECTORS: " (name t) " -> " dir " has no deps.edn")
                      {:type t :dir dir})))
    (let [port (free-port)
          log  (io/file logs (str (name t) "-sidecar.log"))
          pb   (doto (ProcessBuilder. ^java.util.List ["clojure" "-M:run"])
                 (.directory d)
                 (.redirectErrorStream true)
                 (.redirectOutput (ProcessBuilder$Redirect/appendTo log)))
          env  (.environment pb)]
      (.put env "SUTRA_SIDECAR_PORT" (str port))
      (.put env "SUTRA_SIDECAR_IP" "127.0.0.1")
      (.put env "SUTRA_SIDECAR_TOKEN_FILE" (.getPath token))
      {:type t :url (str "http://127.0.0.1:" port) :process (.start pb) :log (.getPath log)})))

(defn start!
  [^File dir spec]
  (let [pairs (parse spec)]
    (when (seq pairs)
      (let [logs     (doto (io/file dir "logs") .mkdirs)
            token    (token-file! dir)
            children (mapv #(start-one! logs token %) pairs)]
        {:config
           {"SUTRA_CONNECTOR_SIDECARS"
              (str/join "," (map #(str (name (:type %)) "=" (:url %)) children))
            "SUTRA_CONNECTOR_SIDECAR_TOKEN_FILE" (.getPath token)}
         :children children}))))

(defn stop! [{:keys [children]}]
  (doseq [{:keys [^Process process]} children]
    (.destroy process)))
