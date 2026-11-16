(ns sutra.cli.checkout
  (:require [sutra.kit.digest :as digest]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sutra.types.languages :as langs]
            [sutra.types.paths :as paths]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.parser.project-emit :as pemit]
            [sutra.cli.errors :as errors]
            [sutra.cli.out :as out]
            [sutra.cli.git :as git]
            [sutra.cli.sandbox :as sandbox]))

(defn- legacy-retired!
  [why]
  (throw (ex-info
          (str "the legacy checkout protocol is retired (" why ").\n"
               "  Code lives in a Source Repository now, and clone/push/pull are real git:\n"
               "    sutra repository list                    # what repositories exist\n"
               "    sutra clone <project>/<repo> [dir]       # a real `git clone`\n"
               "    git add -p && git commit && git push     # checks run server-side\n"
               "  A resource in no repository is not checkoutable — put it in one:\n"
               "    sutra repository create <name> --project <project>\n"
               "    sutra repository place <name> <resource>…\n"
               "  For a NON-editable snapshot (unchanged): `clone --archive` / `export`.")
          {:kind ::legacy-retired :why why})))

(def manifest-name ".sutra-checkout.edn")

(def ^:private with-auth
  git/with-auth)

(defn- code-transforms
  [schema only]
  (for [spec (vals (:transforms schema))
        :when (and (langs/code-language? (:language spec))
                   (or (nil? only) (contains? only (:transform-name spec))))]
    {:name (:transform-name spec) :code (:code spec) :spec spec}))

(defn- project-transform-names
  [pname]
  (when pname
    (let [{:keys [projects]} (errors/req! "projects" (api/get! :projects))
          hit (some #(when (= (name pname) (name (:name %))) %) projects)]
      (when-not hit
        (throw (ex-info (str "no project named " (name pname)
                             " (have: " (str/join ", " (map (comp name :name) projects)) ")")
                        {:project pname})))
      (set (map keyword (get-in hit [:members :transforms]))))))

(def ^:private cljs-filename pemit/cljs-filename)

(defn- rid->name
  [section name-key]
  (into {} (map (fn [[rid v]] [rid (name-key v)])) section))

(defn- source-apps
  [schema proj]
  (let [ot->name   (rid->name (:object-types schema) :object-type-name)
        proj->name (rid->name (:projects schema) :project-name)]
    (for [a (vals (:applications schema))
          :when (and (:source a)
                     (or (nil? proj)
                         (= (name proj) (some-> (get proj->name (:project a)) name))))
          :let [{:keys [layout file]}
                (pemit/extract-app-template (:application-name a) (:layout a))]]
      {:name     (:application-name a)
       :source   (:source a)
       :template file
       :spec     (into {} (remove (comp nil? val))
                       {:auto-publish (:auto-publish a)
                        :icon        (:icon a)
                        :description (:description a)
                        :layout      layout
                        :object-type (get ot->name (:object-type a) (:object-type a))
                        :project     (get proj->name (:project a) (:project a))})})))

(defn- serialize
  [schema {:keys [branch base project only head]}]
  (let [codes   (sort-by (comp name :name) (code-transforms schema only))
        apps    (if only [] (sort-by (comp name :name) (source-apps schema project)))
        n-skip  (- (count (:transforms schema)) (count codes))
        emitted (vec (for [{:keys [name spec]} codes
                           :let [{:keys [entry file]} (pemit/emit-transform spec {:inline? false})]]
                       {:name name :terse (second entry) :file (:path file)
                        :code (:content file)}))
        tx-ent  (vec (for [{:keys [name file code terse]} emitted]
                       {:kind :transform :name name :file file :code code :terse terse}))
        app-ent (vec (for [{:keys [name source spec]} apps]
                       {:kind :application :name name
                        :file (str "apps/" (cljs-filename name)) :source source :spec spec}))
        asset-ent (vec (for [{:keys [name template]} apps
                             :when template]
                         {:kind :asset :name name
                          :file (:path template) :bytes (:bytes template)}))
        entries (into (into tx-ent app-ent) asset-ent)
        ent     (cond-> {:schema {:transforms (into (sorted-map)
                                                    (for [{:keys [name terse]} tx-ent]
                                                      [name terse]))}}
                  (seq app-ent)
                  (assoc-in [:schema :applications]
                            (into (sorted-map)
                                  (for [{:keys [name file spec]} app-ent]
                                    [name (assoc spec :source-file file)]))))
        manifest (cond-> {:base       base
                          :branch     branch
                          :head       head
                          :project    (some-> project keyword)
                          :transforms (into (sorted-map)
                                            (for [{:keys [name file code]} tx-ent]
                                              [name {:file file :sha (digest/sha256-hex code)}]))}
                   (seq app-ent)
                   (assoc :applications
                          (into (sorted-map)
                                (for [{:keys [name file source]} app-ent]
                                  [name {:file file :sha (digest/sha256-hex source)}]))))]
    {:entries entries :ent ent :manifest manifest :n-skip n-skip}))

(def ^:private mime->ext
  {"application/pdf"                                                   ".pdf"
   "application/vnd.oasis.opendocument.spreadsheet"                    ".ods"
   "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" ".xlsx"
   "application/vnd.ms-excel"                                          ".xls"
   "text/csv"                                                          ".csv"
   "text/plain"                                                        ".txt"
   "image/png"                                                         ".png"
   "image/jpeg"                                                        ".jpg"
   "image/gif"                                                         ".gif"
   "application/zip"                                                   ".zip"
   "application/json"                                                  ".json"})

(defn- blob-ext
  [mime filename]
  (or (some->> filename (re-find #"\.[A-Za-z0-9]+$"))
      (some-> mime (str/split #";") first str/trim mime->ext)
      ".bin"))

(defn- media-columns
  [ds]
  (filter #(#{:media-reference :attachment} (:attribute-type %)) (:columns ds)))

(defn- row-filename
  [values]
  (some (fn [k] (let [v (get values k)] (when (and (string? v) (seq v)) v)))
        [:dateiname :filename :file-name :name]))

(defn- project-rid
  [schema pname]
  (when pname
    (some (fn [[rid s]] (when (= (name pname) (some-> (:project-name s) name)) rid))
          (:projects schema))))

(defn- collect-uploaded-files
  [schema proj-rid]
  (vec
   (for [[rid ds] (:datasets schema)
         :when (and (or (nil? proj-rid) (= proj-rid (:project ds)))
                    (seq (media-columns ds)))
         :let  [dsname   (name (:dataset-name ds))
                cols     (media-columns ds)
                {:keys [rows]} (errors/req! (str "rows " dsname)
                                            (api/get! :dataset-preview {:id rid}))]
         {:keys [values]} rows
         col cols
         :let  [mcol (:attribute-name col)
                sha  (get values mcol)]
         :when (and (string? sha) (re-matches #"[0-9a-f]{64}" sha))
         :let  [{:keys [status ^bytes bytes content-type]} (tx/get-bytes! (str "/api/blob/" sha))]
         :when (<= 200 status 299)]
     (let [fname (row-filename values)
           ext   (blob-ext content-type fname)
           base  (or (some-> fname (str/replace #"[/\\]" "_"))
                     (str (subs sha 0 12) ext))]
       {:dataset      dsname
        :media-column mcol
        :sha          sha
        :file         (str "files/" dsname "/" base)
        :mime         content-type
        :bytes        bytes
        :fields       (dissoc values mcol)}))))

(defn clone!
  [opts project dir]
  (legacy-retired! (if project
                     (str "`clone " (clojure.core/name project)
                          "` checks out a whole PROJECT, which has no git counterpart")
                     "`clone --only`/whole-instance checkout has no git counterpart"))
  (with-auth opts
    (fn []
      (let [dir     (or dir (some-> project clojure.core/name) "checkout")
            schema  (errors/req! "schema" (api/get! :schema))
            {branch :current head :head} (errors/req! "branches" (api/list-branches))
            only    (or (:only opts) (project-transform-names project))
            {:keys [entries ent manifest n-skip]}
            (serialize schema {:branch branch :base (:base-url opts) :head head
                               :project project :only only})
            root    (io/file dir)
            n-code  (count (filter (comp #{:transform} :kind) entries))
            n-app   (count (filter (comp #{:application} :kind) entries))]
        (when (empty? entries)
          (println (out/mark :yellow "⚠" (str "nothing to clone"
                                              (when project (str " in project " (name project)))
                                              " (no code transforms or app views)")))
          (throw (ex-info "nothing to clone" {:project project})))
        (io/make-parents (io/file root "transforms" ".keep"))
        (io/make-parents (io/file root "apps" ".keep"))
        (doseq [{:keys [name file code source bytes]} entries]
          (io/make-parents (io/file root file))
          (if bytes
            (with-open [o (io/output-stream (io/file root file))] (.write o ^bytes bytes))
            (spit (io/file root file) (or code source "")))
          (println (out/mark :green "↓"
                             (str (out/rpad (clojure.core/name name) 32) " "
                                  (out/paint :dim (str file "  "
                                                       (if bytes (str (count bytes) " bytes")
                                                           (str (count (str (or code source "")))
                                                                " chars"))))))))
        (let [uploaded (when-not (:only opts)
                         (collect-uploaded-files schema (project-rid schema project)))]
          (doseq [{:keys [file bytes dataset sha]} uploaded]
            (io/make-parents (io/file root file))
            (with-open [o (io/output-stream (io/file root file))] (.write o ^bytes bytes))
            (println (out/mark :green "↓"
                               (str (out/rpad dataset 32) " "
                                    (out/paint :dim (str file "  " (count bytes) " bytes  "
                                                         (subs sha 0 12) "…"))))))
          (let [ent-file  (io/file root (str (clojure.core/name (or project "ontology")) ".ent"))
                files-man (mapv #(dissoc % :bytes) uploaded)
                manifest  (cond-> manifest (seq files-man) (assoc :files files-man))]
            (with-open [w (io/writer ent-file)]
              (binding [*out* w] (pprint/pprint ent)))
            (with-open [w (io/writer (io/file root manifest-name))]
              (binding [*out* w] (pprint/pprint manifest)))
            (println (out/mark :green "✓"
                               (str "cloned " n-code " code transform" (when (not= 1 n-code) "s")
                                    (when (pos? n-app)
                                      (str " + " n-app " app view" (when (not= 1 n-app) "s")))
                                    (when (seq files-man)
                                      (str " + " (count files-man) " uploaded file"
                                           (when (not= 1 (count files-man)) "s")))
                                    " from " (out/paint :bold branch) " → " (.getPath root))))
            (when (pos? n-skip)
              (println (out/mark :dim "•"
                                 (str n-skip " non-code transform" (when (not= 1 n-skip) "s")
                                      " left in the ontology (SQL/link — no code file to edit)"))))
            (println (out/mark :dim "→"
                               (str "edit transforms/*.{py,clj} + apps/*.cljs, then `sutra push "
                                    (.getPath root) "`")))
            0))))))

(defn- fetch-scope-zip
  [project]
  (let [{:keys [status ^bytes bytes]}
        (tx/get-bytes! (paths/address :kompass-export {:format "zip" :project project}))]
    (if (= 200 status)
      bytes
      (throw (ex-info (str "export '" (or project "unassigned") "' failed — HTTP " status)
                      {:project project :status status})))))

(defn- copy-scope-entries!
  [^bytes zip-bytes ^java.util.zip.ZipOutputStream zout label]
  (let [counts (atom {:n-py 0 :n-cljs 0 :ent nil})]
    (with-open [zin (java.util.zip.ZipInputStream.
                     (java.io.ByteArrayInputStream. zip-bytes))]
      (loop []
        (when-let [e (.getNextEntry zin)]
          (when-not (.isDirectory e)
            (let [nm   (.getName e)
                  data (.readAllBytes zin)]
              (cond (or (str/ends-with? nm ".py")
                        (str/ends-with? nm ".clj"))   (swap! counts update :n-py inc)
                    (str/ends-with? nm ".cljs") (swap! counts update :n-cljs inc)
                    (str/ends-with? nm ".ent")  (swap! counts assoc :ent (str label "/" nm)))
              (.putNextEntry zout (java.util.zip.ZipEntry. (str label "/" nm)))
              (.write zout data)
              (.closeEntry zout)))
          (recur))))
    @counts))

(defn- export-scope!
  [{:keys [zip? repository]} project out]
  (let [fmt   (if zip? "zip" "ent")
        base  (or (some-> repository clojure.core/name)
                  (some-> project clojure.core/name) "ontology")
        {:keys [status ^bytes bytes]}
        (tx/get-bytes! (paths/address :kompass-export
                                      {:format fmt :project project :repository repository}))]
    (if (= 200 status)
      (let [file (if (str/blank? (str out))
                   (sandbox/scratch-file (str base "." fmt))
                   (doto (io/file out) (-> .getParentFile (some-> .mkdirs))))]
        (with-open [o (io/output-stream file)] (.write o bytes))
        (println (out/mark :green "✓"
                           (str "exported " (out/paint :bold base)
                                " → " (.getPath file) "  "
                                (out/paint :dim (str (count bytes) " bytes")))))
        (println (out/mark :dim "→"
                           (str "re-install with `sutra --yes " (.getPath file) "`"
                                (when zip? "  (unzip first)"))))
        0)
      (out/fail! (out/mark :red "✗"
                           (str "export failed — HTTP " status
                                (some->> (try (String. bytes "UTF-8") (catch Exception _ nil))
                                         not-empty (str ": "))))))))

(defn- export-backup!
  [base-url ^java.io.File out]
  (let [{:keys [projects]} (errors/req! "projects" (api/get! :projects))
        scopes (cons nil (mapv (comp name :name) projects))]
    (some-> (.getParentFile out) io/make-parents)
    (println (out/mark :dim "→" (str "backing up all scopes of " (out/paint :bold base-url)
                                     " → " (.getPath out))))
    (with-open [zout (java.util.zip.ZipOutputStream. (io/output-stream out))]
      (let [results (doall
                     (for [scope scopes
                           :let  [label (or scope "unassigned")]]
                       (let [c (copy-scope-entries! (fetch-scope-zip scope) zout label)]
                         (println (out/mark :green "✓"
                                            (str "  " label
                                                 (out/paint :dim (str "  (" (:n-py c) " code, "
                                                                      (:n-cljs c) " cljs)")))))
                         (assoc c :scope label))))
            n-py   (reduce + (map :n-py results))
            n-cljs (reduce + (map :n-cljs results))]
        (.putNextEntry zout (java.util.zip.ZipEntry. "MANIFEST.txt"))
        (.write zout (.getBytes
                      (str "# backup manifest — a re-installable mirror of the live ontology\n"
                           "source:  " base-url "\n"
                           "taken:   " (java.time.Instant/now) "\n"
                           "scopes:  " (str/join ", " (map :scope results)) "\n"
                           "content: " n-py " code transforms, " n-cljs " app views, "
                           (count results) " .ent files\n\n"
                           "# restore — unzip, then (types before the apps that bind them):\n"
                           (str/join "\n" (for [{:keys [ent]} results :when ent]
                                            (str "  sutra --yes " ent)))
                           "\n  # then re-declare sealed :sources with `sutra source-add …`\n")
                      "UTF-8"))
        (.closeEntry zout)
        (println (out/mark :green "✓"
                           (str "backup complete — " n-py " transforms, " n-cljs " apps, "
                                (count results) " scopes → " (.getPath out))))
        0))))

(defn archive!
  [{:keys [base-url] :as opts} project out]
  (cond
    (and (not project) (str/blank? (str out)))
    (do (out/warn! "a whole-instance backup must be named: sutra clone --archive --out backup.zip")
        2)

    (and (not project) (not (str/ends-with? (str/lower-case (str out)) ".zip")))
    (do (out/warn! "a whole-instance backup spans every scope, so it is a .zip — got" out)
        2)

    :else
    (with-auth opts
      (fn []
        (if project
          (export-scope! opts project out)
          (export-backup! base-url (io/file out)))))))

(defn export!
  [opts project out]
  (out/warn! (out/mark :yellow "⚠"
                       (str "`export` is deprecated — use `clone"
                            (when project (str " " (clojure.core/name project)))
                            " --archive`" (when-not project " (whole-instance backup)")
                            " for the same output. Redirecting…")))
  (archive! opts project out))
