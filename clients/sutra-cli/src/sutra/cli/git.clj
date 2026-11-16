(ns sutra.cli.git
  (:require [sutra.i18n.core :refer [t]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.tooling.git-hooks :as hooks]
            [sutra.tooling.platform-files :as pf]
            [sutra.types.rid :as rid]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.cli.credential :as cred]
            [sutra.cli.site :as site]
            [sutra.cli.auth :as auth]
            [sutra.cli.dispatch :as dispatch]
            [sutra.cli.out :as out])
  (:import (java.io File)
           (java.nio.file Files)
           (java.nio.file.attribute FileAttribute PosixFilePermissions)))

(defn with-auth
  [{:keys [base-url] :as opts} f]
  (when base-url (tx/set-base! base-url))
  (let [ctx (dispatch/new-ctx opts)]
    (binding [tx/*auth-token* nil] (auth/ensure-prod-login! ctx))
    (binding [tx/*auth-token* @(:token ctx)] (f))))

(defn- askpass-script
  ^File [token]
  (let [only-me (PosixFilePermissions/asFileAttribute
                 (PosixFilePermissions/fromString "rwx------"))
        f       (.toFile (Files/createTempFile "sutra-askpass" ".sh"
                                               (into-array FileAttribute [only-me])))]
    (spit f (str "#!/bin/sh\nprintf '%s' " (pr-str token) "\n"))
    f))

(defn exec!
  [{:keys [token dir]} & args]
  (let [helper (when token (askpass-script token))]
    (try
      (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
        (when dir (.directory pb (io/file dir)))
        (doto (.environment pb)
          (cond-> helper (.put "GIT_ASKPASS" (.getAbsolutePath ^File helper)))
          (.put "GIT_TERMINAL_PROMPT" "0"))
        (.inheritIO pb)
        (.waitFor (.start pb)))
      (finally (some-> helper .delete)))))

(defn- capture
  [dir & args]
  (try
    (let [pb (ProcessBuilder. ^java.util.List (into ["git"] (map str) args))]
      (when dir (.directory pb (io/file dir)))
      (.redirectErrorStream pb false)
      (let [p   (.start pb)
            out (slurp (.getInputStream p))]
        (when (zero? (.waitFor p)) (str/trim out))))
    (catch Exception _ nil)))

(defn git-checkout?
  [dir]
  (boolean (and dir (.exists (io/file dir ".git")))))

(defn worktree-root
  [dir]
  (capture dir "rev-parse" "--show-toplevel"))

(defn repo-identity
  [dir]
  (when-let [url (capture dir "config" "--get" "remote.origin.url")]
    (or (when-let [[_ base rid] (re-find #"^(.*)/git/r/([^/]+)\.git/?$" url)]
          {:base base :rid rid})
        (when-let [[_ base proj nm] (re-find #"^(.*)/git/([^/]+)/([^/]+)\.git/?$" url)]
          {:base base :repo (str proj "/" nm)})
        (when-let [[_ base] (re-find #"^(https?://[^/]+)(?:/|/git/ontology\.git/?)?$" url)]
          {:base base :rid rid/ontology-repo-rid}))))

(defn- named-repo-rid
  [repo]
  (let [want   (last (str/split (str (name repo)) #"/"))
        proj   (when (str/includes? (str (name repo)) "/")
                 (first (str/split (str (name repo)) #"/")))
        repos  (:repositories (:body (api/get! :repositories)))
        match  (filter (fn [r] (and (= want (str (some-> (:name r) name)))
                                    (or (nil? proj)
                                        (= proj (str (some-> (:project r) name))))))
                       repos)]
    (cond
      (= 1 (count match)) (:id (first match))
      (empty? match)
      (throw (ex-info (t :cli/git-no-such-repo
                         {:repo (name repo)
                          :bekannt (str/join ", " (sort (map #(str (some-> (:project %) name)
                                                                   "/"
                                                                   (some-> (:name %) name))
                                                             repos)))})
                      {:repository (name repo)}))
      :else
      (throw (ex-info (t :cli/git-ambiguous-repo {:repo want})
                      {:repository (name repo)})))))

(defn identity-rid
  [opts dir]
  (let [{:keys [rid repo]} (repo-identity dir)]
    (or rid
        (when repo
          (try (with-auth opts #(named-repo-rid repo))
               (catch Exception _ nil))))))

(defn- repo-rid
  [repo]
  (if (= "ontology" (str (name repo)))
    rid/ontology-repo-rid
    (named-repo-rid repo)))

(defn- repo-rid-on
  [opts repo]
  (binding [tx/*branch* (:branch opts)] (repo-rid repo)))

(defn credential!
  [opts op]
  (if-not (= "get" (str op))
    0
    (let [in    (->> (repeatedly #(read-line))
                     (take-while (complement str/blank?))
                     (keep #(let [[k v] (str/split (str %) #"=" 2)] (when v [k v])))
                     (into {}))
          proto (get in "protocol" "http")
          host  (get in "host")
          path  (str (get in "path"))
          base  (str proto "://" host)
          buf   (java.io.StringWriter.)
          why   (volatile! nil)
          rid   (or (cred/rid-without-server path)
                    (when (or (str/blank? path)
                              (= "/" path)
                              (re-find #"git/ontology\.git" path))
                      rid/ontology-repo-rid)
                    (when-let [[_ p r] (re-find #"git/([^/]+)/([^/]+)\.git" path)]
                      (try (binding [*out* buf *err* buf]
                             (with-auth (assoc opts :base-url base)
                               #(repo-rid (str p "/" r))))
                           (catch Exception e (vreset! why (ex-message e)) nil))))
          ck    (when rid [base (str rid)])
          token (when rid
                  (or (cred/cached-token ck)
                      (some->> (binding [*out* buf *err* buf]
                                 (:token (:body (with-auth (assoc opts :base-url base)
                                                  #(api/post! :repo-clone-token {:rid rid} {})))))
                               (cred/cache-token! ck))))]
      (when-not token
        (apply out/fail!
               (str "sutra: "
                    (cond @why @why
                          rid   (t :cli/git-no-clone-token {:rid rid})
                          :else (t :cli/git-url-names-no-repo {:pfad path})))
               (some-> (not-empty (str buf)) vector)))
      (when token
        (println "username=sutra")
        (println (str "password=" token)))
      0)))

(defn- helper-cmd
  ([] (helper-cmd nil))
  ([creds-path]
   (str "!" (.getAbsolutePath (io/file (System/getProperty "user.dir") "bin" "sutra"))
        " git-credential"
        (when (and creds-path (not= creds-path auth/default-creds-path))
          (str " --creds " (.getAbsolutePath (io/file creds-path)))))))

(defn- wire-pre-push!
  [dir]
  (when-let [hooks (capture dir "rev-parse" "--git-path" "hooks")]
    (hooks/install! (hooks/absolutize dir hooks)
                    (.getAbsolutePath (io/file (System/getProperty "user.dir") "bin" "change"))
                    (try (let [r (api/get! :branch-policy {:name "main"})]
                           (when (<= 200 (:status r) 299) (boolean (:protected (:body r)))))
                         (catch Exception _ nil)))))

(def ^:private cache-ttl-seconds
  3600)

(defn- credential-cache-available?
  []
  (some? (capture nil "credential-cache" "exit")))

(defn- wire-credentials!
  [dir creds-path]
  (if (credential-cache-available?)
    (do (exec! {:dir dir} "config" "--replace-all" "credential.helper"
               (str "cache --timeout=" cache-ttl-seconds))
        (exec! {:dir dir} "config" "--add" "credential.helper" (helper-cmd creds-path)))
    (exec! {:dir dir} "config" "--replace-all" "credential.helper" (helper-cmd creds-path)))
  (exec! {:dir dir} "config" "credential.useHttpPath" "true")
  (exec! {:dir dir} "config" "fetch.prune" "true")
  (wire-pre-push! dir))

(defn- code-repo-layout
  [repos]
  (let [rows  (for [r repos
                    :let [nm (some-> (:name r) name)
                          pj (some-> (:project r) name)]
                    :when nm]
                {:rid (:id r) :name nm :project pj})
        dupes (into #{} (for [[nm g] (group-by :name rows) :when (< 1 (count g))] nm))]
    (into (sorted-map)
          (map (fn [{:keys [rid name project]}]
                 [(if (contains? dupes name) (str (or project rid) "/" name) name)
                  {:rid rid :name name}]))
          rows)))

(defn- exclude-locally!
  [dir paths]
  (when-let [info (some-> (capture dir "rev-parse" "--git-path" "info")
                          (->> (hooks/absolutize dir)))]
    (let [f    (io/file info "exclude")
          cur  (if (.exists f) (slurp f) "")
          want (remove (set (str/split-lines cur)) (map #(str "/" %) paths))]
      (when (seq want)
        (.mkdirs (io/file info))
        (spit f (str cur
                     (when (and (seq cur) (not (str/ends-with? cur "\n"))) "\n")
                     (str/join "\n" want) "\n"))))))

(defn- exclude-from-enclosing!
  [out]
  (let [abs (.getCanonicalFile (io/file out))]
    (when-let [top (some-> (capture (.getPath (.getParentFile abs)) "rev-parse" "--show-toplevel")
                           str/trim not-empty)]
      (exclude-locally! top [(str (.relativize (.toPath (io/file top)) (.toPath abs)))]))))

(defn- clone-one!
  [token url dir creds-path]
  (let [code (exec! {:token token} "clone" url dir)]
    (when (zero? code) (wire-credentials! dir creds-path))
    code))

(defn- clone-code-repos!
  [{:keys [token base-url] :as opts} layout dir]
  (when (seq layout)
    (println (str "    " (t :cli/git-code-repos-header)))
    (doseq [[path {:keys [rid]}] layout
            :let [target (io/file dir path)]]
      (cond
        (.exists (io/file target ".git"))
        (println "    ·" path (t :cli/git-already-there))

        (zero? (clone-one! token (str base-url "/git/r/" rid ".git") (.getPath target)
                           (:creds-path opts)))
        (println "    ✓" path)

        :else
        (println "    !" path (t :cli/git-not-fetched {:pfad path}))))
    (exclude-locally! dir (keys layout))))

(defn- clone-target
  [base-url repo dir]
  (let [nm (last (str/split (str (name repo)) #"/"))]
    (or dir
        (some-> (site/clones-dir)
                (io/file (str/replace (str base-url) #"^[a-z]+://|[:/].*$" "") nm)
                .getPath)
        nm)))

(defn- same-origin?
  [out url]
  (and (.exists (io/file out ".git"))
       (= (str/replace (str url) #"/+$" "")
          (some-> (capture out "remote" "get-url" "origin") (str/replace #"/+$" "")))))

(defn- refresh-clone!
  [token out creds-path]
  (let [code (exec! {:token token :dir out} "fetch" "--prune" "origin")
        clean? (and (zero? code) (= "" (capture out "status" "--porcelain")))]
    (when (zero? code) (wire-credentials! out creds-path))
    (when (and clean? (capture out "rev-parse" "--abbrev-ref" "@{u}"))
      (exec! {:dir out} "merge" "--ff-only" "--quiet" "@{u}"))
    (println (str "  ✓ " (t :cli/git-reused {:out out})))
    (when-not clean? (println (str "    " (t :cli/git-reused-dirty))))
    code))

(defn clone!
  [{:keys [base-url] :as opts} repo dir]
  (let [ontology? (= "ontology" (str (name repo)))
        layout    (when ontology?
                    (code-repo-layout
                     (:repositories (:body (with-auth opts #(api/get! :repositories))))))
        {:keys [token remote expires-in]}
        (:body (with-auth opts #(api/post! :repo-clone-token {:rid (repo-rid-on opts repo)}
                                           {:with (mapv :rid (vals layout))})))
        url   (str base-url remote)
        out   (clone-target base-url repo dir)
        reuse? (same-origin? out url)
        code  (if reuse?
                (refresh-clone! token out (:creds-path opts))
                (do (some-> (io/file out) .getAbsoluteFile .getParentFile .mkdirs)
                    (clone-one! token url out (:creds-path opts))))]
    (when (and (zero? code) (not reuse?))
      (exclude-from-enclosing! out)
      (println (str "  ✓ " (t :cli/git-cloned {:repo (name repo) :out out})))
      (println (str "    " (t :cli/git-remote {:url url})))
      (when ontology?
        (some->> (:files (:body (with-auth opts
                                  #(api/get! :repo-platform-files {:rid (repo-rid-on opts repo)}))))
                 (pf/lay-down! out))
        (clone-code-repos! (assoc opts :token token) layout out))
      (println (str "    " (t :cli/git-token-expires
                              {:tage (quot (long (or expires-in 0)) 86400)}))))
    code))

(defn report-checks!
  [opts rid sha]
  (let [r (try (:body (with-auth opts #(api/get! :repo-checks {:rid rid :commit sha})))
               (catch Exception _ nil))]
    (when r
      (out/show! r)
      (when (= :failed (:status r))
        (println (str "  " (t :cli/git-commit-inert)))))
    r))

(defn checks!
  [opts dir rev]
  (let [dir (or dir ".")
        rid (when (git-checkout? dir) (identity-rid opts dir))
        sha (capture dir "rev-parse" (or rev "HEAD"))]
    (cond
      (not (git-checkout? dir))
      (do (println (str "  " (t :cli/git-not-a-checkout))) 1)

      (nil? rid)
      (do (println (str "  " (t :cli/git-remote-not-sutra))) 1)

      (nil? sha)
      (do (println (str "  " (t :cli/git-no-such-rev {:rev (or rev "HEAD")}))) 1)

      :else
      (if (report-checks! opts rid sha) 0 1))))
