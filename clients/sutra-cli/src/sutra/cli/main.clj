(ns sutra.cli.main
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.cli.auth :as auth]
            [sutra.cli.dispatch :as dispatch]
            [sutra.cli.errors :as errors]
            [sutra.cli.flags :as flags]
            [sutra.cli.git :as git]
            [sutra.cli.local :as local]
            [sutra.cli.out :as out :refer [mark paint]]
            [sutra.cli.site :as site]
            [sutra.cli.table :as table]
            [sutra.i18n.core :as i18n]))

(defn- call
  [sym & args]
  (apply (requiring-resolve sym) args))

(defn- client-call
  [sym fallback opts & more]
  (try (apply call sym opts more)
       (catch Exception e (errors/print-error! e fallback) 1)))

(defn- explicit-dev-base
  []
  (when-let [port (not-empty (System/getenv "SUTRA_HTTP_PORT"))]
    (str "http://localhost:" port)))

(defn- path-token?
  [s]
  (boolean (and s (re-find #"[/.~]" (str s)))))

(defn- repo-ref?
  [s]
  (boolean (and s (or (= "ontology" (str s))
                      (re-matches #"[A-Za-z0-9][A-Za-z0-9_-]*/[A-Za-z0-9][A-Za-z0-9_-]*"
                                  (str s))))))

(defn- bare-ent?
  [m]
  (and (map? m) (contains? m :schema) (not (contains? m :steps))))

(defn- named-instance
  [nm]
  (or (site/instance nm)
      (do (out/fail! (mark :red "✗" (str "no instance named " nm " — the site names: "
                                         (str/join ", " (keys (site/instances)))))
                     (str "    " (paint :bold "bin/sutra instances add <name> <url>")))
          (System/exit 2))))

(defn resolve-target
  [{:keys [base-url instance]}]
  (or (when base-url {:url base-url :via "--base"})
      (when instance (named-instance instance))
      (some->> (not-empty (System/getenv "SUTRA_HTTP_URL")) (hash-map :via "$SUTRA_HTTP_URL" :url))
      (some->> (explicit-dev-base) (hash-map :via "$SUTRA_HTTP_PORT" :url))
      (site/bound)
      (some->> (:base (git/repo-identity ".")) (hash-map :via "git remote" :url))
      (site/prod)
      (site/local)))

(defn- target-label
  [url]
  (let [host (try (.getHost (java.net.URI. url)) (catch java.net.URISyntaxException _ nil))]
    (cond (and host (= host (site/prod-host))) "prod"
          (#{"localhost" "127.0.0.1"} host)      "dev"
          :else                                  "fremd")))

(defn announce-target!
  [{:keys [url via label]}]
  (out/warn! (paint :dim (str "→ " url " · " (or label (target-label url)) " · via " via))))

(defn- refuse-branch-env!
  [v]
  (out/fail! (mark :red "✗" (i18n/t :cli/branch-env-refused {:wert v}))
             (str "    " (i18n/t :cli/branch-env-name-it))
             (str "    " (i18n/t :cli/branch-env-pipe))
             (str "    " (i18n/t :cli/branch-env-why)))
  2)

(defn file-install-retired!
  [file note]
  (let [{:keys [root rel]} (local/commit-home file)]
    (apply out/fail!
           (concat
            [(mark :red "✗" (i18n/t :cli/install-from-disk-retired))]
            (when note [(str "    " note)])
            (if root
              [(str "    " (i18n/t :cli/file-inside-checkout
                                   {:datei (paint :bold file) :klon (paint :dim root)}) ":")
               (str "        " (paint :bold (str "git -C " root " add " rel)))
               (str "        " (paint :bold (str "git -C " root " commit -m \"…\"")))
               (str "        " (paint :bold (str "git -C " root " push")))]
              [(str "    " (i18n/t :cli/file-outside-checkout {:datei (paint :bold file)}) ":")
               (str "        " (paint :bold "clone ontology") "            "
                    (paint :dim (i18n/t :cli/clone-ontology-legend)))
               (str "        " (paint :bold "clone <project>/<repo>") "    "
                    (paint :dim (i18n/t :cli/clone-repo-legend)))])
            [(str "    " (i18n/t :cli/push-runs-same-install))
             (str "    " (i18n/t :cli/preview-without-shipping) ": "
                  (paint :bold (str "changeset diff " file)))]))))

(defn- install-steps
  [runbook]
  (filterv #(= :install (first %)) (:steps runbook)))

(defn- run-runbook-file!
  [runbook base {:keys [creds-path yes? secrets]}]
  (try
    (let [{:keys [aborted?]} (dispatch/run-runbook! runbook {:base-url   base
                                                             :creds-path creds-path
                                                             :yes?       yes?
                                                             :secrets    secrets})]
      (if aborted? 2 0))
    (catch Exception e
      (errors/print-error! e (i18n/t :cli/backend-up-at {:klasse (.getName (class e))
                                                         :url    base}))
      1)))

(defn run-file!
  [file base {:keys [creds-path yes? secrets]}]
  (let [raw     (-> file slurp edn/read-string)
        runbook (when-not (bare-ent? raw) (call 'sutra.parser.entr/validate! raw file))]
    (cond
      (bare-ent? raw)
      (file-install-retired! file nil)

      (seq (install-steps runbook))
      (let [steps (install-steps runbook)]
        (file-install-retired!
         (or (some-> steps first second :preset) file)
         (str file " carries " (count steps) " `:install` step"
              (when (not= 1 (count steps)) "s")
              " — a runbook may OPERATE (run-import, build, action, schedule),"
              " not edit the ontology.")))

      :else
      (run-runbook-file! runbook base
                         {:creds-path creds-path :yes? yes? :secrets secrets}))))

(defn requote-arg
  [t]
  (cond
    (and (seq t) (not (re-find #"[\s\"']" t))) t
    (not (str/includes? t "\""))              (str \" t \")
    (not (str/includes? t "'"))               (str \' t \')
    :else                                     t))

(defn- wrong-host-refusal
  []
  (when-not (System/getProperty "babashka.version")
    ["✗ the sutra CLI runs on babashka, not the JVM."
     "    Start it with `bin/sutra <verb>` (or `bb --config clients/sutra-cli/bb.edn -m sutra.cli.main <verb>`)."
     "    There is no `:cli` alias any more: a JVM start cost ~2.7 s per invocation,"
     "    a babashka one costs ~0.4 s including login. See clients/sutra-cli/bb.edn."]))

(defn- require-babashka!
  []
  (when-let [lines (wrong-host-refusal)]
    (apply out/fail! lines)
    (System/exit 2)))

(defn- output
  [opts]
  {:json?  (and (not (:text? opts)) (or (boolean (:json? opts)) (not (out/tty?))))
   :fields (:fields opts)})

(defn- call-opts [{:keys [opts base]}] (assoc opts :base-url base))

(defn- repl! [env sym & more]
  (apply call (symbol "sutra.cli.repl" (name sym)) (call-opts env) more))

(defn- git! [env f & more]
  (try (apply f (call-opts env) more)
       (catch Exception e (errors/print-error! e "git operation failed") 1)))

(defn- checkout! [env sym & more]
  (apply client-call (symbol "sutra.cli.checkout" (name sym))
         (i18n/t :cli/checkout-failed-backend {:url (:base env)})
         (call-opts env) more))

(defn main-bind [{:keys [opts0 argv]}]
  (if-let [url (or (second argv) (:base-url opts0))]
    (site/bind! url (:creds-path opts0))
    (out/fail! (str "  " (i18n/t :cli/bind-usage)))))

(defn main-instances [{:keys [argv]}]
  (call 'sutra.cli.instances/main! (vec (rest argv))))

(defn main-clone [{:keys [opts argv] :as env}]
  (let [pos1  (second argv)
        repo? (repo-ref? pos1)
        proj  (when (or repo? (not (path-token? pos1))) (some-> pos1 keyword))
        out   (if (and (not repo?) (path-token? pos1)) pos1 (nth argv 2 nil))]
    (cond
      (:archive? opts) (checkout! env :archive! proj (or (:out opts) out))
      (:only opts)     (checkout! env :clone! nil (second argv))
      (and proj repo?) (git! env git/clone! proj out)
      :else            (checkout! env :clone! proj out))))

(defn main-serve [_]
  (out/fail! (mark :red "✗" (i18n/t :cli/serve-retired))
             (str "    " (i18n/t :cli/cli-on-babashka))))

(defn main-retired-git [{:keys [argv]}]
  (out/fail! (mark :red "✗" (i18n/t :cli/verb-retired-use-git {:verb (first argv)}))
             (str "    " (i18n/t :cli/clone-is-real-git))
             (str "    " (i18n/t :cli/push-prints-verdict))
             (str "    " (i18n/t :cli/lost-that-output))))

(defn- verb-word?
  [w]
  (some #{w} @(requiring-resolve 'sutra.cli.parse/verbs)))

(defn- main-verbs
  [{:keys [opts argv base] :as env}]
  (let [file (first argv)]
    (cond
      (and file
           (.isFile (io/file ^String (local/vom-aufrufer file)))
           (not (verb-word? file)))
      (run-file! (local/vom-aufrufer file) base opts)
      (seq argv)
      (repl! (update env :opts merge (output opts))
             :run-once! (str/join " " (map requote-arg argv)))
      (out/tty?)
      (repl! env :start!)
      :else
      (let [lines (line-seq (java.io.BufferedReader. *in*))]
        (if (seq lines)
          (repl! (update env :opts assoc :json? true) :run-lines! lines)
          (repl! (update env :opts merge (output opts)) :run-landing!))))))

(defn main-export [{:keys [opts argv] :as env}]
  (if (#{"list" "show"} (second argv))
    (main-verbs env)
    (checkout! env :export! (some-> (second argv) keyword) (or (:out opts) (nth argv 2 nil)))))

(defn main-spec [{:keys [argv] :as env}]
  (if (= "dump" (second argv))
    (do (apply call 'sutra.cli.spec-dump/-main (drop 2 argv)) 0)
    (main-verbs env)))

(defn main-help [{:keys [opts]}]
  (dispatch/step! (dispatch/new-ctx (merge opts (output opts))) [:guide {}])
  0)

(defn main-git-credential [{:keys [argv] :as env}]
  (git! env git/credential! (second argv)))

(defn main-checks [{:keys [argv] :as env}]
  (git! env git/checks!
        (or (nth argv 2 nil) (System/getenv "SUTRA_CALLER_PWD") ".")
        (some-> (second argv) str)))

(defn- process-run
  [word]
  (if-let [sym (:run (table/process-words word))]
    @(ns-resolve 'sutra.cli.main sym)
    main-verbs))

(defn- offline? [[word second-word]]
  (let [{:keys [offline? offline-sub]} (table/process-words word)]
    (boolean (or offline? (contains? offline-sub second-word)))))

(defn- main-opts
  [opts]
  (if (and (= "code" (first (:argv opts)))
           (:code? (table/process-words (second (:argv opts)))))
    (update opts :argv rest)
    opts))

(defn- gate
  [target [file :as argv]]
  (cond (and (some? (System/getenv "SUTRA_BRANCH")) (not= file "git-credential"))
        (refuse-branch-env! (System/getenv "SUTRA_BRANCH"))
        (offline? argv) nil
        :else           (when-not (= file "git-credential") (announce-target! target))))

(defn parse-args
  [args]
  (let [{:keys [opts words]} (flags/parse flags/globals {} args)]
    (assoc opts :argv words)))

(defn -main
  [& args]
  (require-babashka!)
  (let [opts0  (parse-args args)
        _      (i18n/set-locale! (or (i18n/from-tag (:lang opts0)) (i18n/from-env)))
        _      (when (:no-color? opts0) (out/no-colour!))
        opts   (main-opts opts0)
        target (resolve-target opts)
        opts   (cond-> opts
                 (nil? (:creds-path opts))
                 (assoc :creds-path (or (:creds target) (auth/creds-path-for (:url target)))))
        argv   (:argv opts)
        env    {:opts0 opts0 :opts opts :argv argv :base (:url target)}
        code   (try (or (gate target argv)
                        ((process-run (first argv)) env))
                    (catch clojure.lang.ExceptionInfo e
                      (if (errors/fatal? e)
                        (do (out/warn! (mark :red "✗" (ex-message e))) 3)
                        (throw e))))]
    (shutdown-agents)
    (System/exit (or code 0))))
