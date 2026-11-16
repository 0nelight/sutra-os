(ns sutra.cli.local
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.cli.git :as git]))

(defn- on-first-call [sym] (fn [& args] (apply (requiring-resolve sym) args)))

(def authoring-read-file      (on-first-call 'sutra.parser.authoring/read-file))
(def ent-merge-entity-schemas (on-first-call 'sutra.parser.ent/merge-entity-schemas))
(def pe-template-unit         (on-first-call 'sutra.parser.project-emit/template-unit))
(def pe-unit-files            (on-first-call 'sutra.parser.project-export/unit-files))
(def pe-document-template-spec
  (on-first-call 'sutra.parser.project-emit/document-template-spec))
(def ^:private pe-unit-file?  (on-first-call 'sutra.parser.project-export/unit-file?))
(def ^:private pe-checkout-formats
  (on-first-call 'sutra.parser.project-export/checkout-formats))

(defn vom-aufrufer
  [p]
  (if (.exists (io/file ^String p))
    p
    (let [pwd (System/getenv "SUTRA_CALLER_PWD")
          f   (when pwd (io/file pwd ^String p))]
      (if (and f (.exists f)) (.getPath f) p))))

(defn checkout-root
  [path]
  (let [parent (or (.getParentFile (io/file ^String path)) (io/file "."))
        marker? (fn [^java.io.File d]
                  (or (.exists (io/file d ".git"))
                      (.exists (io/file d ".sutra-checkout.edn"))))]
    (loop [d (.getAbsoluteFile parent)]
      (cond
        (nil? d)     parent
        (marker? d)  d
        :else        (recur (.getParentFile d))))))

(defn commit-home
  [file]
  (let [f    (.getCanonicalFile (io/file ^String file))
        root (some-> (git/worktree-root (.getParent f)) (java.io.File.) .getCanonicalPath)
        path (.getPath f)]
    (when (and root (str/starts-with? path (str root java.io.File/separator)))
      {:root root :rel (subs path (inc (count root)))})))

(defn resolve-code-files
  [schema root-path]
  (let [ent-dir (checkout-root root-path)]
    (walk/postwalk
     (fn [x]
       (if-not (map? x)
         x
         (cond-> x
           (and (:code-file x) (not (:code x)))
           (-> (assoc :code (slurp (io/file ent-dir (:code-file x)))) (dissoc :code-file))
           (and (:source-file x) (not (:source x)))
           (-> (assoc :source (slurp (io/file ent-dir (:source-file x)))) (dissoc :source-file))
           (and (:template-file x) (not (:template-b64 x)))
           (-> (assoc :template-b64
                      (.encodeToString (java.util.Base64/getEncoder)
                                       (java.nio.file.Files/readAllBytes
                                        (.toPath (io/file ent-dir (:template-file x))))))
               (dissoc :template-file)))))
     schema)))

(defn checkout-of
  [live path]
  (let [ent-dir (checkout-root path)
        root    (.toPath ent-dir)
        rel     #(str/replace (str (.relativize root (.toPath (.getAbsoluteFile ^java.io.File %))))
                              java.io.File/separatorChar \/)
        files   (filter #(.isFile ^java.io.File %) (file-seq ent-dir))]
    {:root    ent-dir
     :files   files
     :rel     rel
     :formats (pe-checkout-formats live (map rel files)
                                   #(let [f (io/file ent-dir %)] (when (.isFile f) (slurp f))))}))

(defn merge-view-files
  [schema preset-path live]
  (let [{:keys [files rel formats]} (checkout-of live preset-path)
        units (->> files
                   (filter #(pe-unit-file? formats (rel %)))
                   (sort-by #(.getPath ^java.io.File %)))]
    (if (empty? units)
      schema
      (:schema (ent-merge-entity-schemas
                (cons {:schema schema}
                      (map (fn [f]
                             (-> (authoring-read-file formats (rel f) (slurp f))
                                 (select-keys [:schema])
                                 (update :schema resolve-code-files preset-path)))
                           units)))))))
