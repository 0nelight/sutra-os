(ns sutra.serve.hot-patches
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.attempt :as attempt]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.foundation.confine :as confine]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(def ^:private app-root "/app")

(defn- hot-dir [] (not-empty (env/getenv "SUTRA_HOT_DIR")))
(defn- image-id [] (not-empty (env/getenv "SUTRA_IMAGE_ID")))

(def ^:private production-instances #{"sutra" "prod" "production"})

(defn dev-surfaces?
  []
  (let [inst (some-> (env/getenv "SUTRA_INSTANCE") str/trim str/lower-case not-empty)]
    (and (not (contains? production-instances inst))
         (or (some? inst) (nil? (image-id))))))

(defn armed?
  []
  (and (contains? #{"1" "true" "yes" "on"}
                  (some-> (env/getenv "SUTRA_HOT_RELOAD") str/trim str/lower-case))
       (dev-surfaces?)))

(defn- manifest-file ^java.io.File [dir] (io/file dir "manifest.edn"))

(defn- source-path
  ^java.io.File [root path]
  (when-not (re-matches #"[A-Za-z0-9_][A-Za-z0-9_./-]*\.cljc?" (str path))
    (refusal/refuse! :refusal.serve/hot-patch-path {:path path}))
  (confine/confine-path root path))

(defn- patch-file [dir path] (source-path (io/file dir "files") path))

(defn- read-manifest [dir]
  (let [f (manifest-file dir)]
    (when (.exists f) (edn/read-string (slurp f)))))

(defn- ns-of
  [code]
  (let [form (binding [*read-eval* false]
               (read-string {:read-cond :allow :features #{:clj}} code))]
    (when-not (and (seq? form) (= 'ns (first form)) (simple-symbol? (second form)))
      (refusal/refuse! :refusal.serve/hot-patch-no-ns {}))
    (second form)))

(defn- path-names?
  [path nsym]
  (str/ends-with? (str/replace (str path) #"\.cljc?$" "")
                  (-> (str nsym) (str/replace "." "/") (str/replace "-" "_"))))

(defn load-source!
  [path code]
  (when-not (armed?)
    (refusal/refuse! :refusal.serve/hot-reload-disarmed {:path path}))
  (let [tgt  (source-path app-root path)
        nsym (ns-of code)]
    (when-not (path-names? path nsym)
      (refusal/refuse! :refusal.serve/hot-patch-wrong-ns {:path path} {:ns nsym}))
    (some-> (.getParentFile tgt) .mkdirs)
    (spit tgt code)
    (require nsym :reload)
    (str nsym)))

(defn- sha
  [^String s]
  (let [d (.digest (java.security.MessageDigest/getInstance "SHA-256")
                   (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" %) (take 8 d)))))

(defn- resource-path
  [path]
  (str/replace-first (str path) #"^[^/]+/" ""))

(defn- url-mtime
  ^long [^java.net.URL u]
  (try (.getLastModified (.openConnection u)) (catch java.io.IOException _ 0)))

(defn proof
  [path code]
  (try
    (let [rp    (resource-path path)
          src   (io/resource rp)
          klass (io/resource (str/replace rp #"\.cljc?$" "__init.class"))
          want  (sha code)
          got   (some-> src slurp sha)]
      (cond
        (nil? src)
        {:path path :ok false
         :why (str "no classpath resource for " rp " — the patched file is not on the "
                   "classpath, so the reload read the image's copy")}

        (not= want got)
        {:path path :ok false :patch want :loaded got
         :why (str "the classpath resolves " rp " to different content (" (str src)
                   ") — the reload did not read the patch")}

        (and klass (> (url-mtime klass) (url-mtime src)))
        {:path path :ok false
         :why (str "an AOT class shadows " rp " (" (str klass)
                   " is newer than the source) — the reload loaded compiled code")}

        :else {:path path :ok true :sha want}))
    (catch Throwable t
      (attempt/rethrow-fatal! t)
      {:path path :ok false :why (str "could not be verified: " (.getMessage t))})))

(defn verify
  [files]
  (mapv (fn [{:keys [path code]}] (proof path code)) files))

(defn- reload-server! [] (require 'sutra.serve.server :reload))

(defn- wipe! [dir]
  (doseq [f (reverse (file-seq (io/file dir))) :when (not= f (io/file dir))]
    (.delete ^java.io.File f)))

(defn record!
  [files]
  (when-let [dir (hot-dir)]
    (let [img  (image-id)
          prev (read-manifest dir)
          prev (if (= img (:image prev)) prev (do (wipe! dir) nil))
          new  (mapv :path files)
          kept (vec (remove (set new) (:files prev)))]
      (doseq [{:keys [path code]} files]
        (let [f (patch-file dir path)]
          (io/make-parents f)
          (spit f code)))
      (spit (manifest-file dir)
            (pr-str {:image img
                     :files (into kept new)
                     :at    (str (java.time.Instant/now))})))))

(defn follow!
  []
  (when-let [dir (and (armed?) (hot-dir))]
    (let [{:keys [image files]} (read-manifest dir)]
      (when (and image (= image (image-id)))
        (reduce (fn [acc path]
                  (try (conj acc (load-source! path (slurp (patch-file dir path))))
                       (catch Throwable t
                         (attempt/rethrow-fatal! t)
                         (println (str "[hot] WARN: follow of " path " failed — "
                                       (.getMessage t)))
                         acc)))
                [] files)))))

(defn replay!
  []
  (when-let [dir (and (armed?) (hot-dir))]
    (when-let [{:keys [image files at]} (read-manifest dir)]
      (let [img (image-id)]
        (if (or (nil? img) (not= img image))
          (do (println (str "[hot] WARN: " (count files) " hot patch(es) from " at
                            " belong to image " image ", this is " (or img "unknown")
                            " — discarded"))
              (wipe! dir))
          (let [done (reduce (fn [acc path]
                               (try
                                 (conj acc (load-source! path (slurp (patch-file dir path))))
                                 (catch Throwable t
                                   (attempt/rethrow-fatal! t)
                                   (println (str "[hot] WARN: replay of " path " failed — "
                                                 (.getMessage t) "; the image's version stays"))
                                   acc)))
                             [] files)]
            (when (seq done)
              (when-not (some #{"sutra.serve.server"} done)
                (reload-server!))
              (println (str "[hot] replayed " (count done) " hot patch(es) from " at ": "
                            (str/join " " done))))
            done))))))
