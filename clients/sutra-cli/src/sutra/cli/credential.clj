(ns sutra.cli.credential
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.types.rid :as rid])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute PosixFilePermission)))

(def cache-path ".run/clone-tokens.edn")

(def cache-ttl-ms (* 30 60 1000))

(defn cache-file ^File [] (io/file (System/getProperty "user.dir") cache-path))

(defn cached-token
  [k]
  (try
    (let [f (cache-file)]
      (when (.exists f)
        (let [{:keys [token at]} (get (edn/read-string (slurp f)) k)]
          (when (and token at (< (- (System/currentTimeMillis) (long at)) cache-ttl-ms))
            token))))
    (catch Exception _ nil)))

(defn nur-fuer-mich!
  [^File f]
  (try (.createNewFile f) (catch Exception _ nil))
  (try
    (Files/setPosixFilePermissions
     (.toPath f)
     #{PosixFilePermission/OWNER_READ PosixFilePermission/OWNER_WRITE})
    (catch Exception _ nil)))

(defn- vorhandene-eintraege
  [^File f]
  (or (try (when (.canRead f) (edn/read-string (slurp f)))
           (catch Exception _ nil))
      {}))

(defn cache-token!
  [k token]
  (try
    (let [f   (cache-file)
          cur (vorhandene-eintraege f)]
      (io/make-parents f)
      (nur-fuer-mich! f)
      (spit f (pr-str (assoc cur k {:token token :at (System/currentTimeMillis)}))))
    (catch Exception _ nil))
  token)

(defn read-request
  []
  (->> (repeatedly #(read-line))
       (take-while (complement str/blank?))
       (keep #(let [[k v] (str/split (str %) #"=" 2)] (when v [k v])))
       (into {})))

(defn rid-without-server
  [path]
  (or (second (re-find #"git/r/([^/]+)\.git" path))
      (when (or (str/blank? path) (= "/" path) (re-find #"git/ontology\.git" path))
        rid/ontology-repo-rid)))

(defn cache-key
  [{:strs [protocol host path]}]
  (when-let [r (rid-without-server (str path))]
    [(str (or protocol "http") "://" host) (str r)]))

(defn reply!
  [token]
  (println "username=sutra")
  (println (str "password=" token)))

(defn- wieder-eingelesen
  ^String [in]
  (str (str/join "\n" (map (fn [[k v]] (str k "=" v)) in)) "\n\n"))

(defn -main
  [& args]
  (let [op (last args)]
    (if-not (= "get" (str op))
      (System/exit 0)
      (let [in (read-request)]
        (if-let [token (some-> (cache-key in) cached-token)]
          (do (reply! token) (System/exit 0))
          (binding [*in* (java.io.BufferedReader.
                          (java.io.StringReader. (wieder-eingelesen in)))]
            (apply (requiring-resolve 'sutra.cli.main/-main)
                   "git-credential" (concat (butlast args) ["get"]))))))))
