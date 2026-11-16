(ns sutra.cli.site
  (:require [clojure.edn :as edn]
            [clojure.pprint]
            [clojure.java.io :as io]
            [clojure.string]
            [sutra.cli.out :as out]))

(def ^:private config-file ".sutra-site.edn")

(defn- candidates
  []
  (let [caller (some-> (System/getenv "SUTRA_CALLER_PWD") not-empty io/file)]
    (concat (when caller
              (take-while some? (iterate #(.getParentFile ^java.io.File %)
                                         (.getAbsoluteFile caller))))
            [(io/file ".")])))

(def ^:private config
  (delay
    (or (some (fn [dir]
                (let [f (io/file dir config-file)]
                  (when (.isFile f)
                    (try (assoc (edn/read-string (slurp f)) ::file (.getCanonicalPath f))
                         (catch Exception e
                           (out/warn! (str "[site] ignoring unreadable " f
                                           ": " (.getMessage e)))
                           nil)))))
              (candidates))
        {})))

(defn clones-dir
  []
  (some-> (:clones-dir @config)
          (clojure.string/replace #"^~" (System/getProperty "user.home"))))

(defn prod
  []
  (if-let [env (not-empty (System/getenv "SUTRA_PROD_URL"))]
    {:url env :via "$SUTRA_PROD_URL"}
    (when-let [url (not-empty (:prod-url @config))]
      {:url url :via (::file @config)})))

(defn prod-url [] (:url (prod)))

(defn- solo-port
  []
  (let [home (or (not-empty (System/getenv "SUTRA_HOME"))
                 (some-> (not-empty (System/getenv "XDG_DATA_HOME")) (str "/sutra"))
                 (str (System/getProperty "user.home") "/.local/share/sutra"))
        f    (io/file home "instance.edn")]
    (when-let [port (when (.isFile f)
                      (try (:http-port (edn/read-string (slurp f))) (catch Exception _ nil)))]
      {:port port :file (.getPath f)})))

(defn local
  []
  (if-let [{:keys [port file]} (solo-port)]
    {:url (str "http://127.0.0.1:" port) :via file}
    {:url "http://localhost:8030" :via "default (no .sutra-site.edn)"}))

(defn bound
  []
  (when-let [url (not-empty (:url @config))]
    (let [dir (some-> (::file @config) io/file .getParentFile)]
      (cond-> {:url url :via (::file @config)}
        (:creds @config) (assoc :creds
                                (let [c (io/file (:creds @config))]
                                  (.getPath (if (or (.isAbsolute c) (nil? dir)) c
                                              (io/file dir (:creds @config))))))))))

(defn bind!
  [url creds]
  (let [dir  (io/file (or (not-empty (System/getenv "SUTRA_CALLER_PWD")) "."))
        f    (io/file dir config-file)
        old  (when (.isFile f) (edn/read-string (slurp f)))
        new  (cond-> (assoc old :url url)
               creds       (assoc :creds (.getCanonicalPath (io/file creds)))
               (nil? creds) (dissoc :creds))]
    (spit f (pr-str new))
    (println (str "  ✓ " (.getCanonicalPath dir) " → " url
                  (when creds (str "  (creds " (:creds new) ")"))))
    (println "    every bin/sutra call from here and below now talks to it — no --base, no --creds")
    0))

(defn prod-host
  []
  (try (some-> (prod-url) java.net.URI. .getHost)
       (catch Exception _ nil)))

(defn instances
  []
  (let [file (::file @config)]
    (into (if-let [p (prod)] (sorted-map "prod" p) (sorted-map))
          (for [[nm v] (:instances @config)
                :let [m (if (map? v) v {:url v})]
                :when (not-empty (str (:url m)))]
            [(name nm) (cond-> {:url (str (:url m)) :label (name nm)
                                :via (str file " :instances " (name nm))}
                         (:creds m) (assoc :creds (str (:creds m))))]))))

(defn instance
  [nm]
  (get (instances) (name nm)))

(defn- site-file []
  (or (some-> (::file @config) io/file)
      (io/file (or (not-empty (System/getenv "SUTRA_CALLER_PWD")) ".") config-file)))

(defn set-instance!
  [nm url]
  (let [f   (site-file)
        old (when (.isFile f) (edn/read-string (slurp f)))
        new (if url
              (assoc-in old [:instances (name nm)] url)
              (update old :instances dissoc (name nm)))]
    (spit f (with-out-str (clojure.pprint/pprint new)))
    (.getCanonicalPath f)))
