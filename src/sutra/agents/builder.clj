(ns sutra.agents.builder
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.access.api :as access]
            [sutra.agents.ports :as ports]
            [sutra.store.access.scope :as ctx]
            [sutra.repos.api :as repos]
            [sutra.store.protection :as protection]
            [sutra.governance.api :as governance]
            [sutra.catalog.branch-policy :as policy]
            [sutra.store.control :as ctl]
            [sutra.catalog.grants :as grants]
            [sutra.store.world :as world]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.types.media :as mt]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.store.edge :as edge]))

(set! *warn-on-reflection* true)

(def agent-name :builder)
(def max-turns 40)
(def ^:private text-max 40000)
(def ^:private lines-default 400)
(def ^:private hits-max 60)

(def ^:private preamble
  (str "You are Sutra's build agent — a forward deployed engineer working in the platform for "
       "the asker, with the asker's rights.\n"
       "You change the platform only through your tools, as a person would. You cannot write "
       "platform plugins, read local files or reach anything outside the instance: when a "
       "request needs a new connector or other platform code (an IMAP connector, say), say that "
       "this is platform work, and offer what can be built on what exists.\n"
       "Explore first, read-only: list_object_types, describe_object_types, list_texts, "
       "search_texts, read_text, list_code_files, read_doc (the templates show the grammar by "
       "example). When a file the asker uploaded is refused or misread, read_upload it before "
       "you name a cause or advise anything: what you say about its columns, dates or numbers "
       "comes from its lines, never from a guess. If the request is ambiguous or large, answer "
       "with a short plan and your questions instead of acting; the asker replies in this "
       "thread.\n"
       "Then start_change once per task — it forks your branch and opens its proposal; every "
       "write names that branch. In a thread that already has a change, start_change returns "
       "that change again: continue it rather than opening another. Never push code only to see "
       "data — read_upload and preview_transform read it without a commit. Check your work: "
       "push_texts with preview=true before staging, preview_transform before you finish. Finish "
       "by naming the branch, what you changed and the proposal: a person approves and merges "
       "it, you never do.\n"
       "The rules below are written for a person at the CLI. Your tools are those verbs: "
       "projection pull → pull_texts (then read_text, edit_text, write_text), projection push → "
       "push_texts, commit → commit, discard → discard_staged, branch and propose → "
       "start_change, git commit and push of Python → write_code_file/edit_code_file and "
       "push_code. Ignore paths, --to and sidecars: your checkout is held for you."))

(declare platform-docs)

(defn system-prompt
  [store]
  (let [agents (get (platform-docs store) "AGENTS.md")]
    (cond-> preamble agents (str "\n\n" agents))))

(defn builtin
  [model]
  {:agent-name   agent-name
   :builtin      true
   :display-name "Builder"
   :description  "Builds on the platform for you — object types, actions, transforms — on a branch of its own, proposed for your review."
   :model        model
   :max-turns    max-turns
   :writes       :propose
   :tools        [{:kind :ontology} {:kind :builder}]})

(defn- refuse
  ([code] (refuse code nil))
  ([code params] (refusal/refuse! code {} params)))

(defn- owner [store] (str (policy/actor-name store)))

(def api-routes
  #{:projection-files :proposals :proposal :repo-commit :repo-platform-files :repo-tree
    :repo-file :branches :staging :staging-preview :preview-transform :dataset-preview :blob})

(defn- answer
  [{:keys [status body]}]
  (if (<= 200 status 299)
    body
    (let [c   (or (some (fn [[c s]] (when (= s status) c)) anomaly/statuses) :fault)
          msg (str (or (when (map? body) (:error body)) (str "HTTP " status)))
          d   (or (when (map? body) (:data body)) {})]
      (if (= :fault c)
        (anomaly/raise! :fault msg d)
        (refusal/refuse! :refusal.agents/api-refused
                         (cond-> (assoc d ::anomaly/category c)
                           (:refusal/code d) (assoc :remote-code (:refusal/code d)))
                         {:why msg})))))

(defn- api!
  [store method id m body branch]
  (answer (ports/call! store method id m body branch)))

(defn- open-change
  [store b]
  (first (:proposals (api! store :get :proposals {:status "open" :source b :brief "true"} nil b))))

(defn with-api-token
  [store f]
  (if-let [p (access/fetch store (owner store))]
    (let [token (access/mint-agent-token! store p (name agent-name) api-routes)]
      (try (binding [ports/*token* token] (f))
           (finally (access/end-session! store token))))
    (f)))

(defn- on
  [store b]
  (let [branch-store (ctx/at (world/viewing store) b)
        e            (edge/read-edge branch-store b
                                     (grants/current-user-id (grants/actor branch-store))
                                     {:writes? true})
        schema       (or (:schema e) (refuse :refusal.agents/no-branch {:branch b}))]
    {:store branch-store :schema schema :branch b :edge e
     :project (delay (protection/context-project branch-store schema nil))}))

(defn- branch-or-thread [store b] (or (not-empty (str b)) (str (:branch store))))

(defn- own-change
  [store b]
  (let [b (str b)
        p (when-not (str/blank? b) (open-change store b))]
    (cond
      (str/blank? b)                      (refuse :refusal.agents/branch-unnamed)
      (nil? p)
      (refuse :refusal.agents/no-open-change {:branch b})
      (not= (str (:author p))
            (owner store)) (refuse :refusal.agents/foreign-change {:branch b})
      (:protected (governance/policy store b))
      (refuse :refusal.agents/branch-protected {:branch b})
      :else                               p)))

(def ^:private checkout-place "checkout")
(defn- repo-place [r] (str "repo:" r))

(defn- held [store b place]
  (:value (ctl/get store :agent-checkouts {:owner (owner store) :branch b :place place})))

(defn- hold! [store b place v]
  (ctl/upsert! store :agent-checkouts {:owner (owner store) :branch b :place place :value v}))

(defn- let-go! [store b place]
  (ctl/delete! store :agent-checkouts {:owner (owner store) :branch b :place place}))

(defn- fdef [nm description props required]
  {"type" "function"
   "function" {"name" nm "description" description
               "parameters" (cond-> {"type" "object" "properties" props}
                              (seq required) (assoc "required" required))}})

(def ^:private s {"type" "string"})
(def ^:private n {"type" "integer"})
(def ^:private yes {"type" "boolean"})
(def ^:private mine {"branch" (assoc s "description" "the branch start_change gave you")})
(def ^:private any-branch {"branch" (assoc s "description" "default: the thread's branch")})

(def ^:private specs
  [[:read-doc "read_doc"
    "The platform's documentation for authors: without name, what there is; with name, its lines (AGENTS.md, AUTHORING.md, DESIGN.md, templates/template.* by example, templates/grammar.* every key)."
    {"name" s "offset" n "limit" n} []]
   [:list-texts "list_texts"
    "The texts a projection (default ontologie) consists of, with their line counts — to find which to read or pull."
    (assoc any-branch "projection" s) []]
   [:search-texts "search_texts"
    "Lines of a projection's texts matching a regular expression, as path:line."
    (assoc any-branch "pattern" s "projection" s) ["pattern"]]
   [:read-text "read_text"
    "Lines of a text, numbered from 1: your checkout's when you pulled it on that branch, else as the branch shows it now."
    (assoc any-branch "path" s "projection" s "offset" n "limit" n) ["path"]]
   [:start-change "start_change"
    "Fork a branch of your own from the thread's branch and open its proposal for review. Once per task, before any write; returns the branch every write names."
    {"title" s "description" s} ["title"]]
   [:pull-texts "pull_texts"
    "Check out texts of a projection (default ontologie) on your branch to edit: the named texts, or a project's. Replaces your checkout on that branch; a push sends exactly these texts."
    (assoc mine "projection" s "texts" {"type" "array" "items" s} "project" s) ["branch"]]
   [:edit-text "edit_text"
    "Replace one exact passage of a text in your checkout; old must occur exactly once."
    (assoc mine "path" s "old" s "new" s) ["branch" "path" "old" "new"]]
   [:write-text "write_text"
    "Replace a whole text in your checkout; empty content drops it, which deletes the nodes it held."
    (assoc mine "path" s "content" s) ["branch" "path" "content"]]
   [:push-texts "push_texts"
    "Send your checkout: merged per element over what you pulled, then staged on your branch. preview=true says what it would change and stages nothing — preview first."
    (assoc mine "preview" yes "resolve" (assoc s "enum" ["mine" "theirs"])) ["branch"]]
   [:staged "staged_changes"
    "What is staged on your branch and what a commit would change."
    mine ["branch"]]
   [:discard-staged "discard_staged"
    "Take back everything staged on your branch (not what is committed); drops your checkout — pull again."
    mine ["branch"]]
   [:commit "commit"
    "Commit what is staged on your branch."
    mine ["branch"]]
   [:list-code-files "list_code_files"
    "The files of a code repository on a branch, and the ones you edited there and have not pushed."
    (assoc any-branch "repository" s) ["repository"]]
   [:read-code-file "read_code_file"
    "Lines of a code repository's file — your unpushed edit when there is one — numbered from 1."
    (assoc any-branch "repository" s "path" s "offset" n "limit" n) ["repository" "path"]]
   [:write-code-file "write_code_file"
    "Write a whole file of a code repository on your branch, held until push_code; empty content deletes it. Not for .ent texts."
    (assoc mine "repository" s "path" s "content" s) ["branch" "repository" "path" "content"]]
   [:edit-code-file "edit_code_file"
    "Replace one exact passage of a code repository's file on your branch, held until push_code."
    (assoc mine "repository" s "path" s "old" s "new" s) ["branch" "repository" "path" "old" "new"]]
   [:push-code "push_code"
    "Commit your edited files of a repository onto your branch; the checks a git push meets judge and publish it, and say so."
    (assoc mine "repository" s "message" s) ["branch" "repository" "message"]]
   [:preview-transform "preview_transform"
    "Run a transform on a branch without writing anything: its first rows, or its error. A transform with several outputs needs output, the name of one of them (the error names them)."
    (assoc any-branch "transform" s "output" s "limit" n) ["transform"]]
   [:read-upload "read_upload"
    "The lines of an uploaded file: the files of a dataset holding uploads whose row mentions file (a name or part of one), and the text of the newest — to see a file's header and rows before judging why it was refused or misread."
    (assoc any-branch "dataset" s "file" s "offset" n "limit" n) ["dataset" "file"]]
   [:change-status "change_status"
    "Your change's proposal: its stage, open tasks, checks and what merging it would change."
    mine ["branch"]]])

(defn bindings
  [_schema _tool]
  (mapv (fn [[op nm d props req]] {:kind :builder :op op :name nm :def (fdef nm d props req)})
        specs))

(defn- clip [x]
  (let [x (str x)]
    (if (> (count x) text-max)
      (str (subs x 0 text-max) "\n… [cut at " text-max " of " (count x) " chars]") x)))

(defn- lines-of [text] (str/split-lines (str text)))

(defn- numbered [text offset limit]
  (let [ls  (lines-of text)
        off (max 1 (or offset 1))
        k   (max 1 (min 2000 (or limit lines-default)))]
    {"lines" (count ls)
     "text"  (clip (str/join "\n"
                             (map-indexed (fn [j l] (str (+ off j) "\t" l))
                                          (take k (drop (dec off) ls)))))}))

(defn- line-counts [files]
  (into (sorted-map) (map (fn [[p t]] [p (count (lines-of t))])) files))

(defn- projection-of [x] (or (not-empty (str x)) "ontologie"))

(defn- texts-now
  [store b nm]
  (update-keys (:files (api! store :get :projection-files {:name nm} nil b)) name))

(defn- platform-docs
  [store]
  (update-keys (:files (api! store :get :repo-platform-files {:rid rid/ontology-repo-rid} nil nil))
               name))

(defn- read-doc [store {:strs [name offset limit]}]
  (let [docs (platform-docs store)]
    (if (str/blank? name)
      {"docs" (vec (sort (keys docs)))}
      (numbered (or (get docs name)
                    (refuse :refusal.agents/no-doc {:name name}))
                offset limit))))

(defn- search-texts [store {:strs [pattern projection branch]}]
  (let [re   (try (re-pattern (str "(?i)" pattern))
                  (catch Exception e (refuse :refusal.agents/not-a-pattern {:why (ex-message e)})))
        hits (for [[p
                    t] (sort (texts-now store (branch-or-thread store branch)
                                        (projection-of projection)))
                   [j l] (map-indexed vector (lines-of t))
                   :when (re-find re l)]
               (str p ":" (inc j) "\t" (str/trim l)))]
    {"hits" (vec (take hits-max hits)) "more" (max 0 (- (count hits) hits-max))}))

(defn- read-text [store {:strs [path projection branch offset limit]}]
  (let [b  (branch-or-thread store branch)
        co (held store b checkout-place)
        t  (if (contains? (:texts co) path)
             (get-in co [:texts path])
             (get (texts-now store b (projection-of projection)) path ::none))]
    (cond
      (= ::none t) (refuse :refusal.agents/no-text {:path path})
      (nil? t)     (refuse :refusal.agents/text-dropped {:path path})
      :else        (numbered t offset limit))))

(defn- slug [x]
  (let [v (-> (str/lower-case (str x)) (str/replace #"[^a-z0-9]+" "-") (str/replace #"^-|-$" ""))]
    (subs v 0 (min 32 (count v)))))

(defn- thread-place [thread-id] (str "thread:" thread-id))

(defn- thread-change
  [store parent thread-id]
  (when-let [{:keys [branch] :as c} (and thread-id (held store parent (thread-place thread-id)))]
    (when (open-change store branch) c)))

(defn- start-change [store {:strs [title description]} {:keys [thread-id run-id]}]
  (when (str/blank? title) (refuse :refusal.agents/change-title))
  (let [parent (str (:branch store))]
    (if-let [{:keys [branch proposal]} (thread-change store parent thread-id)]
      {"branch" branch "proposal" proposal "target" parent
       "note"
       "this thread's change, still open: continue it — read what it holds before you change more"}
      (let [b (str "fde-" (slug title) "-" (subs (str (random-uuid)) 0 6))]
        (when-not (branch-kinds/projectable-branch? b)
          (refuse :refusal.agents/branch-name {:branch b}))
        (api! store :post :branches nil {:parent parent :name b} nil)
        (let [p (api! store :post :proposals nil
                                     {:source b :target parent :title title
                                      :description (str (or (not-empty description) title)
                                                        "\n\nProposed by the build agent"
                                                        (when thread-id
                                                          (str " — thread " thread-id ", run "
                                                               run-id)))}
                                     nil)]
          (when thread-id
            (hold! store parent (thread-place thread-id) {:branch b :proposal (:id p)}))
          {"branch" b "proposal" (:id p) "target" parent
           "note" "every write names this branch; a person approves and merges the proposal"})))))

(defn- checkout [store b]
  (or (held store b checkout-place)
      (refuse :refusal.agents/nothing-checked-out {:branch b})))

(defn- pull-texts [store {:strs [branch projection texts project]}]
  (own-change store branch)
  (let [nm (projection-of projection)
        r  (api! store :get :projection-files
                 (cond-> {:name nm}
                   (seq texts)          (assoc :texts (str/join "," texts))
                   (not-empty project)  (assoc :project project))
                 nil branch)
        co {:projection nm :texts (update-keys (:files r) name) :base (update-keys (:base r) name)
            :shape (:shape r)}]
    (hold! store branch checkout-place co)
    {"projection" nm "texts" (line-counts (:texts co))}))

(defn- text-in [co path]
  (if (contains? (:texts co) path)
    (get-in co [:texts path])
    (refuse :refusal.agents/not-checked-out {:path path})))

(defn- replace-once [text old new what]
  (let [hits (count (re-seq (re-pattern (java.util.regex.Pattern/quote (str old))) (str text)))]
    (cond
      (str/blank? old) (refuse :refusal.agents/old-empty)
      (zero? hits)     (refuse :refusal.agents/old-missing {:what what})
      (> hits 1)       (refuse :refusal.agents/old-ambiguous {:hits hits :what what})
      :else            (str/replace-first (str text) (str old) (str new)))))

(defn- edit-text [store {:strs [branch path old new]}]
  (own-change store branch)
  (let [co (checkout store branch)
        t  (replace-once (or (text-in co path)
                             (refuse :refusal.agents/path-dropped {:path path}))
                         old new path)]
    (hold! store branch checkout-place (assoc-in co [:texts path] t))
    {"edited" path "lines" (count (lines-of t))}))

(defn- write-text [store {:strs [branch path content]}]
  (own-change store branch)
  (let [co (checkout store branch)]
    (text-in co path)
    (hold! store branch checkout-place (assoc-in co [:texts path] (not-empty (str content))))
    {"written" path "dropped" (str/blank? content)}))

(defn- json-safe [x]
  (walk/postwalk #(if (or (coll? %) (string? %) (number? %) (boolean? %) (nil? %) (keyword? %)) %
                      (str %)) x))

(defn- push-texts [store {:strs [branch preview resolve]}]
  (own-change store branch)
  (let [{:keys [projection texts base shape] :as co} (checkout store branch)
        r (api! store :post :projection-files {:name projection}
                {:files texts :base base :shape shape :preview? (boolean preview)
                 :resolve resolve :land? false}
                branch)]
    (when-not preview
      (hold! store branch checkout-place
             (-> co
                 (update :base merge (update-keys (:base r) name))
                 (update :texts #(into {} (remove (comp nil? val)) %)))))
    {"preview" (boolean preview) "result" (json-safe (dissoc r :base))}))

(defn- staged [store {:strs [branch]}]
  (own-change store branch)
  {"staged" (json-safe (api! store :get :staging-preview nil nil branch))})

(defn- discard-staged [store {:strs [branch]}]
  (own-change store branch)
  (let-go! store branch checkout-place)
  {"cleared" (:cleared (api! store :delete :staging nil nil branch))
   "note" "your checkout is dropped too — pull again"})

(defn- commit [store {:strs [branch]}]
  (own-change store branch)
  {"committed" (json-safe (api! store :post :staging nil nil branch))})

(defn- change-status [store {:strs [branch]}]
  (let [p (own-change store branch)]
    {"proposal" (json-safe (select-keys (api! store :get :proposal {:id (:id p)} nil nil)
                                        [:id :title :status :source-branch :target-branch
                                         :stage :open-tasks :checks :preview]))}))

(defn- repo! [schema repository]
  (let [r (repos/repository-rid schema repository)]
    (cond
      (nil? r) (refuse :refusal.agents/no-repository {:repository repository})
      (rid/ontology-repo? r)
      (refuse :refusal.agents/ontology-via-projection)
      :else r)))

(defn- ent-text? [path] (boolean (re-find #"\.ent[a-z]?(\.yaml)?$" (str path))))

(defn- list-code-files [store schema {:strs [repository branch]}]
  (let [r (repo! schema repository)
        b (branch-or-thread store branch)]
    {"branch"   b
     "files"    (:files (api! store :get :repo-tree {:rid r} nil b))
     "unpushed" (into (sorted-map) (map (fn [[p t]] [p (if (nil? t) "deleted" "edited")]))
                      (held store b (repo-place r)))}))

(defn- code-text [store r b path]
  (let [edits (held store b (repo-place r))]
    (if (contains? edits path)
      (or (get edits path) (refuse :refusal.agents/path-deleted {:path path}))
      (let [res (ports/call! store :get :repo-file {:rid r :path path} nil b)]
        (if (= 404 (:status res)) ::none (:text (answer res)))))))

(defn- read-code-file [store schema {:strs [repository path branch offset limit]}]
  (let [r (repo! schema repository)
        t (code-text store r (branch-or-thread store branch) path)]
    (if (= ::none t) (refuse :refusal.agents/no-file {:path path}) (numbered t offset limit))))

(defn- edit-held! [store r b path text]
  (cond
    (str/blank? path) (refuse :refusal.agents/path-unnamed)
    (ent-text? path)  (refuse :refusal.agents/ent-not-a-file {:path path})
    :else (hold! store b (repo-place r) (assoc (held store b (repo-place r)) path text))))

(defn- write-code-file [store schema {:strs [branch repository path content]}]
  (own-change store branch)
  (edit-held! store (repo! schema repository) branch path (not-empty (str content)))
  {"held" path "note" "push_code commits it"})

(defn- edit-code-file [store schema {:strs [branch repository path old new]}]
  (own-change store branch)
  (let [r (repo! schema repository)
        t (code-text store r branch path)]
    (when (= ::none t) (refuse :refusal.agents/no-file-to-edit {:path path}))
    (edit-held! store r branch path (replace-once t old new path))
    {"edited" path "note" "push_code commits it"}))

(defn- push-code [store schema {:strs [branch repository message]}]
  (own-change store branch)
  (let [r     (repo! schema repository)
        edits (held store branch (repo-place r))]
    (when (empty? edits) (refuse :refusal.agents/nothing-to-push))
    (let [out (api! store :post :repo-commit {:rid r}
                                      {:files   edits
                                       :message (str (or (not-empty message) "build agent")
                                                     "\n\nby the build agent for " (owner store))
                                       :author  {:name (str (owner store) " (build agent)")}}
                    branch)]
      (let-go! store branch (repo-place r))
      {"commit" (:commit out) "checks" (vec (:lines out))})))

(defn- preview [store {:strs [branch transform output limit]}]
  (let [{:keys [rows count error]}
        (api! store :get :preview-transform
              (cond-> {:name (str transform) :limit (max 1 (min 50 (or limit 10)))}
                (not-empty (str output)) (assoc :output (str output)))
              nil (branch-or-thread store branch))]
    (cond-> {"count" count "rows" (clip (json/write-str (json-safe (vec rows))))}
      error (assoc "error" (str error)))))

(def ^:private upload-bytes-max (* 4 1024 1024))

(defn- decoded
  [^bytes bs]
  (when-not (some zero? (take 4096 bs))
    (let [utf8 (try (.toString (.decode (.newDecoder java.nio.charset.StandardCharsets/UTF_8)
                                        (java.nio.ByteBuffer/wrap bs)))
                    (catch java.nio.charset.CharacterCodingException _ nil))]
      (str/replace-first (or utf8 (String. bs "windows-1252")) #"^\uFEFF" ""))))

(defn- bytes-of [x]
  (cond (bytes? x) x
        (instance? java.io.InputStream x) (with-open [^java.io.InputStream is x]
                                            (.readNBytes is upload-bytes-max))
        :else (.getBytes (str x) "UTF-8")))

(defn- read-upload [store {:strs [branch dataset file offset limit]}]
  (let [b      (branch-or-thread store branch)
        schema (:schema (on store b))
        id     (when-not (str/blank? dataset)
                 (rid/resolve-name->rid (sch/name-index schema) #{:datasets} (keyword dataset)))
        ds     (when id (get-in schema [:datasets id]))
        _      (when-not ds (refuse :refusal.agents/no-dataset {:dataset dataset}))
        media  (keep #(when-let [m (mt/set-of %)] [(:attribute-name %) m]) (:columns ds))
        _      (when (empty? media) (refuse :refusal.agents/no-media-column {:dataset dataset}))
        cell   (fn [v k] (or (get v k) (get v (name k))))
        needle (str/lower-case (str file))
        hits   (->> (:rows (api! store :get :dataset-preview {:id id} nil b))
                    (map :values)
                    (filter (fn [v] (some #(str/includes? (str/lower-case (str %)) needle)
                                          (vals v))))
                    (keep (fn [v] (some (fn [[k m]] (when-let [ref (cell v k)]
                                                      (when (re-matches #"[0-9a-f]{64}" (str ref))
                                                        [v (str ref) m])))
                                        media)))
                    (sort-by (fn [[v]]
                               (str (some #(when (re-find #"^\d{4}-\d{2}-\d{2}T" (str %)) (str %))
                                          (vals v)))))
                    reverse)
        _      (when (empty? hits)
                 (refuse :refusal.agents/no-file-mention {:dataset dataset :file file}))
        [row ref mset] (first hits)
        bs     (bytes-of (api! store :get :blob {:hash ref :media-set (name mset)} nil b))
        bs     (if (> (alength ^bytes bs) upload-bytes-max)
                 (java.util.Arrays/copyOf ^bytes bs (int upload-bytes-max))
                 bs)
        text   (decoded bs)]
    (cond-> {"files" (vec (take 20 (map (fn [[v r]] (json-safe (assoc v "blob" r))) hits)))
             "read"  (json-safe (assoc row "blob" ref))}
      text       (merge (numbered text offset (or limit 40)))
      (nil? text) (assoc "binary" true "note"
                         "not a text file: its text is what the transform extracts"))))

(defn execute
  [store schema {:keys [op]} args opts]
  {:result
   (case op
     :read-doc          (read-doc store args)
     :list-texts
     {"texts" (line-counts (texts-now store (branch-or-thread store (get args "branch"))
                                      (projection-of (get args "projection"))))}
     :search-texts      (search-texts store args)
     :read-text         (read-text store args)
     :start-change      (start-change store args opts)
     :pull-texts        (pull-texts store args)
     :edit-text         (edit-text store args)
     :write-text        (write-text store args)
     :push-texts        (push-texts store args)
     :staged            (staged store args)
     :discard-staged    (discard-staged store args)
     :commit            (commit store args)
     :list-code-files   (list-code-files store schema args)
     :read-code-file    (read-code-file store schema args)
     :write-code-file   (write-code-file store schema args)
     :edit-code-file    (edit-code-file store schema args)
     :push-code         (push-code store schema args)
     :preview-transform (preview store args)
     :read-upload       (read-upload store args)
     :change-status     (change-status store args))})
