(ns sutra.repos.projection
  (:require [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.repos.records :as git-rows]
            [sutra.store.world :as world]
            [sutra.kit.cache :as cache]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.blobs.api :as blobs]
            [sutra.repos.edn-patch :as ep]
            [sutra.foundation.ops :as op]
            [sutra.catalog.branch-policy :as policy]
            [sutra.repos.ports :as ports]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.registry :as registry]
            [sutra.parser.project-export :as pe]
            [sutra.repos.code-blobs :as cb]
            [sutra.repos.objects :as g]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.types.review :as review]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn- put-bytes!
  [blobs bytes]
  (:hash (blobs/put! blobs bytes)))

(defn- get-bytes
  ^bytes [blobs hash]
  (when-let [is (blobs/open blobs hash)]
    (with-open [in is]
      (let [bos (java.io.ByteArrayOutputStream.)]
        (io/copy in bos)
        (.toByteArray bos)))))

(def bot-ident
  {:name "sutra[bot]" :email "bot@sutra.local"})

(def ontology-repo-name
  "ontology")

(defn repo-name-in
  [schema repo-rid]
  (if (rid/ontology-repo? repo-rid)
    ontology-repo-name
    (some-> (get-in schema [:repositories repo-rid :repository-name]) name)))

(defn tree-formats
  [schema repo-rid]
  (registry/texts schema (pe/git-projections (if (rid/ontology-repo? repo-rid) :ontology :repo))))

(defn- formats-at
  [store repo-rid branch]
  (tree-formats (world/authored (world/at store
                                          (if (world/exists? store branch) branch pe/trunk-branch)))
                repo-rid))

(defn- ->entry
  [{:keys [content bytes]}]
  (or bytes (let [^String text (or content "")] (.getBytes text "UTF-8"))))

(def ^:private template-files
  (merge
   (into {} (for [ext [".ent" ".entv" ".enta" ".entp" ".entt"]
                  fam ["template" "grammar"]
                  :let [f (str fam ext)]]
              [f (str "templates/" f)]))
   {"template-ablauf.enta" "templates/template-ablauf.enta"
    "AGENTS.md"            "AGENTS.md"
    "DESIGN.md"            "DESIGN.md"
    "AUTHORING.md"         "AUTHORING.md"}))

(def platform-file-paths
  (set (vals template-files)))

(defn landing-rules
  [store]
  (noted/quietly "landing rules of the trunk"
    (let [p (op/query {:store store} :governance/policy-in {:branch pe/trunk-branch})]
      {:protected?      (boolean (:protected p))
       :review          (policy/review-rules p)
       :require-checks? (boolean (:require-checks p))})))

(defn- approval-text
  [{:keys [review require-checks?] :as rules}]
  (let [checks (when require-checks? " Die Checks des Quell-Tips müssen grün sein.")]
    (cond
      (nil? rules)
      ["sutra approve <id>                          # ob nötig, sagt `sutra protect main`"
       (str "Welche Freigaben `main` verlangt, war beim Projizieren nicht zu lesen —"
            " `sutra protect main` zeigt die `review`-Regeln.")]

      (empty? review)
      ["sutra approve <id>                          # optional — `main` verlangt keine Freigabe"
       (str "`merge-proposal` landet ohne Freigabe: `main` erklärt keine `review`-Regel." checks)]

      :else
      ["sutra approve <id>                          # nur, wo eine Aufgabe es verlangt"
       (str "Ob eine Ressource eine Freigabe braucht, entscheiden die `review`-Regeln von `main`"
            " je Ressource: " (str/join "; " (map review/describe review)) "."
            " `sutra proposal <id>` zeigt je Aufgabe, was sie verlangt (`requires`), ob sie"
            " bricht (`kind`) und wen (`consumers`). Ein Zuwachs wartet nicht; ein Bruch an einer"
            " Ressource, die dir nicht gehört, wartet auf ihren Eigentümer. `merge-proposal`"
            " verweigert, solange eine verlangte Freigabe fehlt." checks)])))

(defn agents-md
  ^String [^String template rules]
  (let [{:keys [protected?]} rules
        [approve freigabe] (approval-text rules)]
    (-> template
        (str/replace "{{main}}"
                     (cond
                       (nil? rules)
                       (str "Ob er geschützt ist, war beim Projizieren nicht zu lesen — frag"
                            " `sutra protect main`, bevor du auf `main` schreibst.")
                       protected?
                       (str "Er ist **geschützt**: ein Schreiben auf `main` wird abgelehnt."
                            " Landen nur über einen Zweig und `sutra propose` → `sutra approve`"
                            " → `sutra merge-proposal`.")
                       :else
                       (str "Er ist auf dieser Instanz **nicht geschützt**: ein Commit auf `main`"
                            " landet ohne Proposal. Der Proposal-Weg ist der für eine Änderung, die"
                            " vor dem Landen jemand sehen soll; `sutra protect main --on` macht ihn"
                            " zur Pflicht.")))
        (str/replace "{{landen}}" (if (false? protected?)
                                    "ein Push auf `main` oder `sutra propose`"
                                    "`sutra propose`"))
        (str/replace "{{approve-zeile}}" approve)
        (str/replace "{{freigabe}}" freigabe))))

(def ^:private agents-path "AGENTS.md")

(defn platform-files
  [store repo-rid]
  (if-not (rid/ontology-repo? repo-rid)
    {}
    (into {}
          (keep (fn [[res path]]
                  (when-let [r (io/resource (str "presets/" res))]
                    (let [^bytes b (with-open [^java.io.InputStream in (io/input-stream r)]
                                     (.readAllBytes in))]
                      [path (if (= agents-path path)
                              (.getBytes (agents-md (String. b "UTF-8") (landing-rules store))
                                         "UTF-8")
                              b)]))))
          template-files)))

(defn- entity-tree*
  [schema repo-rid prior memory]
  (if (rid/ontology-repo? repo-rid)
    (into {} (map (fn [[p e]] [p (->entry e)])) (pe/ontology-tree schema prior memory))
    (if-let [nm (repo-name-in schema repo-rid)]
      (into {} (map (fn [[p e]] [p (->entry e)])) (pe/repo-tree schema nm))
      {})))

(defonce ^:private tree-cache
  (cache/lru 4))

(defn entity-tree
  ([schema repo-rid] (entity-tree schema repo-rid nil nil))
  ([schema repo-rid prior] (entity-tree schema repo-rid prior nil))
  ([schema repo-rid prior memory]
   (let [k   [(hash schema) repo-rid (hash prior) (hash memory)]
         hit (.get ^java.util.Map tree-cache k)]
     (if (and hit (= schema (:schema hit)) (= prior (:prior hit)) (= memory (:memory hit)))
       (:tree hit)
       (let [tree (entity-tree* schema repo-rid prior memory)]
         (.put ^java.util.Map tree-cache k {:schema schema :prior prior :memory memory :tree tree})
         tree)))))

(defn- ontology-memory
  [store repo-rid _branch]
  (when (rid/ontology-repo? repo-rid)
    (ports/placements store (pe/git-projections :ontology))))

(defn- prior-ontology-files
  [repos repo-rid sha]
  (when (and sha (rid/ontology-repo? repo-rid))
    (when-let [tree (g/read-tree repos repo-rid sha)]
      (into (sorted-map)
            (map (fn [[path {:keys [blob]}]]
                   [path
                    (delay (some-> ^bytes (g/blob-bytes repos repo-rid blob) (String. "UTF-8")))]))
            tree))))

(defn ontology-texts
  [store repos branch]
  (let [r   rid/ontology-repo-rid
        tip (or (:tip-sha (git-rows/repo-anchor store r branch))
                (:tip-sha (git-rows/repo-anchor store r pe/trunk-branch)))]
    (or (prior-ontology-files repos r tip) {})))

(defn- carried-file-plane
  [store repos blobs repo-rid formats commit-sha]
  (when commit-sha
    (into {}
          (keep (fn [{:keys [path blob-sha plane mode]}]
                  (when (and (= "file" plane) (not (g/gitlink? mode))
                             (not (contains? platform-file-paths path))
                             (= :file (pe/plane-of formats path)))
                    (when-let [b (or (and blobs (get-bytes blobs blob-sha))
                                     (g/blob-bytes repos repo-rid blob-sha))]
                      [path (g/entry b mode)]))))
          (git-rows/files-of store repo-rid commit-sha))))

(defn- seed-text
  ^String [res subst fallback]
  (reduce-kv (fn [s k v] (str/replace s (str "{{" k "}}") v))
             (if-let [r (io/resource (str "presets/seed/" res))] (slurp r) fallback)
             subst))

(defn ontology-readme []
  (seed-text "ontology-README.md.tmpl" {"ontology-ent" pe/ontology-ent-path
                                   "transform-ent" pe/transform-ent-path}
             "# ontology\n"))

(defn seed-readme [repo-name]
  (seed-text "repo-README.md.tmpl" {"repo" repo-name} "# {{repo}}\n"))

(defn seed-gitignore []
  (seed-text "gitignore" {} ""))

(defn- bytes-at
  [repos repo-rid tree path]
  (some->> (get tree path) :blob (g/blob-bytes repos repo-rid)))

(defn- patched
  [formats path ^bytes authored ^bytes derived]
  (when (authoring/file? formats path)
    (attempt/or-answer nil
      (some-> ^String (ep/patch (String. authored "UTF-8")
                                (edn/read-string (String. derived "UTF-8"))
                                (pe/restated-keys formats path)
                                #(pe/same-authored-content?
                                  formats path (.getBytes ^String % "UTF-8") derived))
              (.getBytes "UTF-8")))))

(defn- as-authored
  [repos repo-rid formats parent-sha entity]
  (if-let [tree (when parent-sha (g/read-tree repos repo-rid parent-sha))]
    (into {}
          (map (fn [[path bytes]]
                 (let [cur (bytes-at repos repo-rid tree path)]
                   [path (g/entry (cond
                                    (nil? cur) bytes
                                    (pe/same-authored-content? formats path cur bytes) cur
                                    :else (or (patched formats path cur bytes) bytes))
                                  (let [m (some-> (get tree path) :mode)]
                                    (when-not (g/gitlink? m) m)))])))
          entity)
    entity))

(defn- tree-of-commit [repos repo-rid sha]
  (some-> (g/commit-info repos repo-rid sha) :tree))

(defn change-subject
  [{:keys [added changed removed]}]
  (let [paths (concat changed added removed)
        verb  (cond (= (count paths) (count added))   "Add"
                    (= (count paths) (count removed)) "Remove"
                    :else                             "Update")
        shown (take 3 paths)
        more  (- (count paths) (count shown))]
    (if (empty? paths)
      "Update repository"
      (str verb " " (str/join ", " shown) (when (pos? more) (str " and " more " more"))))))

(defn- mint-one!
  [repos repo-rid entries {:keys [parent-sha extra-parents force? author message]}]
  (g/with-inserter [ins repos repo-rid]
    (let [tree (g/write-tree! ins entries)
          prev (tree-of-commit repos repo-rid parent-sha)]
      (if (and (not force?) (= (.name tree) prev))
        {:sha parent-sha :tree prev :suppressed? true}
        {:sha (g/commit! ins {:tree    tree
                              :parents (concat (if parent-sha [parent-sha] [])
                                               extra-parents)
                              :author  author
                              :message (if (fn? message)
                                         (message (g/changed-paths repos repo-rid parent-sha
                                                                   entries))
                                         message)})
         :tree (.name tree)
         :suppressed? false}))))

(defn- manifest-of
  [repos repo-rid formats sha]
  (mapv (fn [[path {:keys [blob mode]}]]
          {:path path :blob-sha blob :mode mode
           :plane (if (g/gitlink? mode) :entity (pe/plane-of formats path))})
        (g/read-tree repos repo-rid sha)))

(def ^:private keyframe-every
  32)

(defn- manifest-base
  [store repos repo-rid sha]
  (when-let [parent (first (:parents (g/commit-info repos repo-rid sha)))]
    (when-let [row (git-rows/commit-row store repo-rid parent)]
      (let [depth (loop [b (:manifest-base row) n 1]
                    (cond (nil? b)                 n
                          (>= n keyframe-every)    n
                          :else (recur (:manifest-base (git-rows/commit-row store repo-rid b))
                                       (inc n))))]
        (when (< depth keyframe-every) parent)))))

(defn- record!
  [store repos blobs repo-rid formats branch sha seq source]
  (let [entries (manifest-of repos repo-rid formats sha)]
    (when (nil? blobs)
      (throw (ex-info (str "refusing to record commit " sha " with no content-addressed"
                           " store: $SUTRA_GIT_DIR would become a STORE rather than a"
                           " cache, and the repository could not be rebuilt")
                      {:kind ::no-cas :repo repo-rid :commit sha})))
    (let [base   (manifest-base store repos repo-rid sha)
          prior  (when base (g/read-tree repos repo-rid base))
          same?  (fn [{:keys [path blob-sha mode]}]
                   (when-let [p (get prior path)]
                     (and (= (str mode) (str (:mode p)))
                          (= (str blob-sha) (str (:blob p))))))
          changed (if prior (remove same? entries) entries)
          gone    (when prior
                    (let [now (into #{} (map :path) entries)]
                      (for [p (keys prior) :when (not (now p))]
                        {:path p :mode "100644" :plane (pe/plane-of formats p) :blob-sha nil})))]
      (git-rows/record-files! store repo-rid sha
                              (into (vec gone)
                                    (map (fn [{:keys [path blob-sha mode plane]}]
                                           {:path path :mode mode :plane plane
                                            :blob-sha (if (g/gitlink? mode)
                                                        blob-sha
                                                        (put-bytes! blobs
                                                                    (g/blob-bytes
                                                                     repos repo-rid blob-sha)))}))
                                    changed))
      (git-rows/record-commit! store {:repo-rid repo-rid :branch branch :seq seq
                                      :commit-sha sha :source source
                                      :manifest-base base
                                      :tree-sha (or (tree-of-commit repos repo-rid sha) "")
                                      :object-blob
                                      (some->> (g/raw-object repos repo-rid sha)
                                               :bytes (put-bytes! blobs))}))
    sha))

(defn- set-anchor!
  [store repos repo-rid branch {:keys [tip-sha] :as anchor} expected]
  (g/pin-anchor! repos repo-rid branch tip-sha)
  (git-rows/set-repo-anchor! store repo-rid branch (assoc anchor :expect-tip expected)))

(defn- live!
  [store repo-rid branch]
  (when-not (world/exists? store branch)
    (refusal/refuse! :refusal.repos/branch-gone
                     {:kind ::branch-gone :repo repo-rid :branch branch})))

(defn forget-branch!
  [store repos repo-rid branch]
  (g/with-repo-lock repos repo-rid
    (when (g/repo-exists? repos repo-rid)
      (when-let [ref (g/branch->ref branch)]
        (g/delete-ref! repos repo-rid ref))
      (g/delete-ref! repos repo-rid (g/anchor-ref branch)))
    (ctl/delete! store :repo-anchor {:repo-rid repo-rid :branch branch})
    nil))

(def ^:private batch-limit
  500)

(defn- reconcile-tip!
  [store repos repo-rid branch ref anchor]
  (let [recorded (:tip-sha anchor)
        actual   (when ref (g/ref-sha repos repo-rid ref))
        adopt!   (fn [sha]
                   (set-anchor! store repos repo-rid branch {:tip-sha sha} recorded)
                   sha)]
    (cond
      (nil? ref)          recorded
      (= recorded actual) recorded

      (nil? recorded)     (adopt! actual)

      (nil? actual)
      (if (g/commit-info repos repo-rid recorded)
        (do (g/update-ref! repos repo-rid ref recorded {:force? true}) recorded)
        (throw (ex-info (str "the git projection of " repo-rid " on " branch
                             " has neither a ref nor the objects for its recorded tip "
                             recorded " — rebuild the repository from the CAS")
                        {:kind ::projection-lost :repo repo-rid :branch branch
                         :recorded recorded})))

      (nil? (g/commit-info repos repo-rid recorded)) (adopt! actual)

      (g/descends-from? repos repo-rid actual recorded) (adopt! actual)

      (g/descends-from? repos repo-rid recorded actual)
      (do (g/update-ref! repos repo-rid ref recorded {:force? true}) recorded)

      :else
      (throw (ex-info (str "the git projection of " repo-rid " on " branch
                           " has DIVERGED: ref " actual " and recorded tip " recorded
                           " share no ancestry")
                      {:kind ::projection-diverged :repo repo-rid :branch branch
                       :ref actual :recorded recorded})))))

(defn- schema-commits
  [store branch opts]
  (let [store (world/at store branch)]
    (mapv (fn [{:keys [rev actor at message]}]
            {:seq rev :actor (:principal actor) :written-at at :message message
             :schema-map (world/world store rev)})
          (world/schema-commits store opts))))

(defn- fork-tip
  [store repos repo-rid branch]
  (let [parent (or (world/parent store branch) "main")]
    (when (not= parent branch)
      (some->> (git-rows/repo-anchor store repo-rid parent) :tip-sha
               (#(when (g/commit-info repos repo-rid %) %))))))

(defn- pending-commits
  [store branch anchor]
  (if anchor
    [(schema-commits store branch {:after (or (:tip-seq anchor) 0) :limit batch-limit}) false]
    [(not-empty (schema-commits store branch {:newest? true :limit 1})) true]))

(defn- seed-files
  [schema repo-rid]
  {"README.md"  (let [^String readme (if (rid/ontology-repo? repo-rid)
                                        (ontology-readme)
                                        (seed-readme (or (repo-name-in schema repo-rid)
                                                         (str repo-rid))))]
                  (.getBytes readme "UTF-8"))
   ".gitignore" (.getBytes ^String (seed-gitignore) "UTF-8")})

(defn- commit-entries
  [store repos blobs repo-rid branch anchored? tip schema]
  (let [formats (tree-formats schema repo-rid)
        entity  (as-authored repos repo-rid formats tip
                             (entity-tree schema repo-rid
                                          (prior-ontology-files repos repo-rid tip)
                                          (ontology-memory store repo-rid branch)))
        carried (or (carried-file-plane store repos blobs repo-rid formats tip)
                    (when anchored?
                      (some->> (world/parent store branch)
                               (git-rows/repo-anchor store repo-rid)
                               :tip-sha
                               (carried-file-plane store repos blobs repo-rid formats))))
        seeded  (when (and anchored? (empty? carried) (seq entity))
                  (seed-files schema repo-rid))]
    [formats (merge seeded carried entity)]))

(defn- commit-meta
  [schema repo-rid anchored? tip {row-seq :seq :keys [actor ^java.util.Date written-at message]}]
  {:author  {:name (or actor "sutra") :email "sutra@localhost"
             :when-ms (some-> written-at .getTime)}
   :message (fn [change]
              (str (if (and anchored? (nil? tip))
                     (str "Initial commit of " (or (repo-name-in schema repo-rid) repo-rid))
                     (str (when (seq message) (str message ": ")) (change-subject change)))
                   "\n\nSchema revision " row-seq (when actor (str " by " actor))
                   (when anchored? " (branch history anchored here)")))})

(defn- unpublished?
  [store repo-rid sha]
  (boolean
   (when sha
     (some #(and (= "ci/sutra-publish" (str (:check-name %))) (not= "passed" (str (:status %))))
           (git-rows/checks-of store repo-rid sha)))))

(defn project-branch!
  [store repos blobs repo-rid branch]
  (if-not (branch-kinds/projectable-branch? branch)
    {:minted 0 :suppressed 0 :tip nil :skipped :machine-namespace}
    (g/with-repo-lock repos repo-rid
      (live! store repo-rid branch)
      (when (and (git-rows/raw-repo-anchor store repo-rid branch)
                 (nil? (git-rows/repo-anchor store repo-rid branch)))
        (forget-branch! store repos repo-rid branch))
      (let [anchor (git-rows/repo-anchor store repo-rid branch)
            ref    (g/branch->ref branch)
            base   (reconcile-tip! store repos repo-rid branch ref anchor)
            [rows anchored?] (pending-commits store branch anchor)]
        (if (and (seq rows) (unpublished? store repo-rid base))
          {:minted 0 :suppressed 0 :tip base :skipped :unpublished-tip}
          (loop [rows       (seq rows)
                 tip        (or base (when (nil? anchor) (fork-tip store repos repo-rid branch)))
                 tip-seq    (:tip-seq anchor)
                 minted     0
                 suppressed 0]
            (if-let [{row-seq :seq sm :schema-map :as row} (first rows)]
              (let [schema            (cb/lower blobs sm)
                    [formats entries] (commit-entries store repos blobs repo-rid branch
                                                      anchored? tip schema)
                    {:keys [sha suppressed?]}
                    (mint-one! repos repo-rid entries
                               (assoc (commit-meta schema repo-rid anchored? tip row)
                                      :parent-sha tip))]
                (when-not suppressed?
                  (record! store repos blobs repo-rid formats branch sha row-seq :minted))
                (recur (next rows) sha (long row-seq)
                       (cond-> minted (not suppressed?) inc)
                       (cond-> suppressed suppressed? inc)))
              (do
                (when (and ref tip (not= tip base))
                  (g/update-ref! repos repo-rid ref tip {:expected base}))
                (set-anchor! store repos repo-rid branch
                             {:watermark (if anchored? (or tip-seq 0) (:watermark anchor))
                              :tip-sha tip :tip-seq tip-seq}
                             base)
                {:minted minted :suppressed suppressed :tip tip}))))))))

(defn adopt-push!
  [store repos blobs repo-rid commands]
  (into []
        (for [{:keys [ref new]} commands
              :let [branch (g/ref->branch ref)]
              :when (and branch new)]
          (do (record! store repos blobs repo-rid (formats-at store repo-rid branch) branch new nil
                       :pushed)
              new))))

(defn rewind-push!
  [repos repo-rid ref old new]
  (g/with-repo-lock repos repo-rid
    (when (= new (g/ref-sha repos repo-rid ref))
      (if old
        (g/update-ref! repos repo-rid ref old {:force? true})
        (g/delete-ref! repos repo-rid ref))
      true)))

(defn bind-published!
  [store repos repo-rid branch commit-sha seq]
  (git-rows/record-commit! store {:repo-rid repo-rid :branch branch :seq seq
                                  :commit-sha commit-sha :source :pushed
                                  :tree-sha (or (tree-of-commit repos repo-rid commit-sha) "")})
  nil)

(defn- mint-followup!
  [store repos blobs repo-rid formats branch parent-sha entries message opts]
  (live! store repo-rid branch)
  (let [anchored (:tip-sha (git-rows/repo-anchor store repo-rid branch))
        {:keys [sha suppressed?]}
        (mint-one! repos repo-rid entries
                   (merge {:parent-sha parent-sha
                           :author (assoc bot-ident :when-ms (System/currentTimeMillis))
                           :message message}
                          opts))]
    (if suppressed?
      (do (set-anchor! store repos repo-rid branch {:tip-sha parent-sha} anchored)
          nil)
      (do
        (record! store repos blobs repo-rid formats branch sha nil :minted)
        (when-let [ref (g/branch->ref branch)]
          (g/update-ref! repos repo-rid ref sha {:expected parent-sha}))
        (set-anchor! store repos repo-rid branch {:tip-sha sha} anchored)
        sha))))

(defn- followup!
  [store repos blobs repo-rid branch parent-sha schema message]
  (g/with-repo-lock repos repo-rid
    (loop [parent parent-sha, attempts 0]
      (let [formats (tree-formats schema repo-rid)
            entity  (as-authored repos repo-rid formats parent
                                 (entity-tree schema repo-rid
                                              (prior-ontology-files repos repo-rid parent)
                                              (ontology-memory store repo-rid branch)))
            carried (carried-file-plane store repos blobs repo-rid formats parent)
            entries (merge carried entity)
            outcome
            (try {:sha (mint-followup! store repos blobs repo-rid formats branch parent entries
                                       message nil)}
                 (catch clojure.lang.ExceptionInfo e
                   (if (and (= ::g/ref-refused (:kind (ex-data e)))
                            (< attempts 4)
                            (g/branch->ref branch))
                     ::retry
                     (throw e))))]
        (if (= outcome ::retry)
          (recur (or (g/ref-sha repos repo-rid (g/branch->ref branch)) parent) (inc attempts))
          (:sha outcome))))))

(defn normalize-followup!
  [store repos blobs repo-rid branch pushed-sha installed-schema]
  (followup! store repos blobs repo-rid branch pushed-sha installed-schema
             #(str (change-subject %) " (normalized by install)")))

(defn- file-plane-blobs
  [repos repo-rid formats commit-sha]
  (if-not commit-sha
    {}
    (into {}
          (keep (fn [[path {:keys [blob mode]}]]
                  (when (and (not (g/gitlink? mode))
                             (= :file (pe/plane-of formats path)))
                    [path [blob (str mode)]])))
          (g/read-tree repos repo-rid commit-sha))))

(defn- tree-entries
  [repos repo-rid tree]
  (into {}
        (map (fn [[path {:keys [blob mode]}]]
               [path (if (g/gitlink? mode)
                       {:gitlink blob}
                       (g/entry (g/blob-bytes repos repo-rid blob) mode))]))
        tree))

(defn root-texts
  [store repos repo-rid sha]
  (when sha
    (let [tree    (g/read-tree repos repo-rid sha)
          formats (formats-at store repo-rid pe/trunk-branch)]
      (into {} (for [[path {:keys [blob]}] tree
                     :when (and (authoring/file? formats path) (not (str/includes? path "/")))]
                 [path (String. ^bytes (g/blob-bytes repos repo-rid blob) "UTF-8")])))))

(defn commit-files!
  [store repos blobs repo-rid branch files message]
  (when (seq files)
    (g/with-repo-lock repos repo-rid
      (when-let [tip (:tip-sha (git-rows/repo-anchor store repo-rid branch))]
        (let [tree    (g/read-tree repos repo-rid tip)
              entries (into (tree-entries repos repo-rid tree)
                            (for [[path text] files]
                              [path
                               (g/entry (.getBytes ^String text "UTF-8")
                                        (:mode (get tree path)))]))]
          (mint-followup! store repos blobs repo-rid (formats-at store repo-rid branch) branch tip
                          entries
                          message nil))))))

(defn merge-git-half!
  [store repos blobs repo-rid source-branch target-branch {:keys [merge-mode actor]}]
  (let [src-tip (:tip-sha (git-rows/repo-anchor store repo-rid source-branch))
        tgt-tip (:tip-sha (git-rows/repo-anchor store repo-rid target-branch))
        merge?  (= :merge merge-mode)]
    (when (and src-tip tgt-tip (not= src-tip tgt-tip)
               (not (and merge? (g/descends-from? repos repo-rid tgt-tip src-tip))))
      (g/with-repo-lock repos repo-rid
        (let [formats (formats-at store repo-rid target-branch)
              fp      (partial file-plane-blobs repos repo-rid formats)
              base-fp (fp (g/merge-base repos repo-rid src-tip tgt-tip))
              src-fp  (fp src-tip)
              tgt-fp  (fp tgt-tip)
              {:keys [winners conflicts]}
              (reduce (fn [acc [path src-blob]]
                        (let [base-blob (get base-fp path)
                              tgt-blob  (get tgt-fp path)]
                          (cond
                            (= src-blob base-blob) acc
                            (or (= tgt-blob base-blob) (= tgt-blob src-blob))
                            (update acc :winners assoc path src-blob)
                            :else (update acc :conflicts conj path))))
                      {:winners {} :conflicts []}
                      src-fp)
              {:keys [removed conflicts]}
              (reduce (fn [acc [path base-blob]]
                        (let [tgt-blob (get tgt-fp path)]
                          (cond
                            (contains? src-fp path) acc
                            (nil? tgt-blob) acc
                            (= tgt-blob base-blob) (update acc :removed conj path)
                            :else (update acc :conflicts conj path))))
                      {:removed [] :conflicts conflicts}
                      base-fp)
              overlay (into {} (keep (fn [[path [blob mode]]]
                                       (when-let [b (g/blob-bytes repos repo-rid blob)]
                                         [path (g/entry b mode)])))
                            winners)]
          (if (and (empty? overlay) (empty? removed) (not merge?))
            {:sha nil :carried [] :removed [] :conflicts conflicts :merged? false}
            (let [entries (apply dissoc
                                 (merge (tree-entries repos repo-rid
                                                      (g/read-tree repos repo-rid tgt-tip))
                                        overlay)
                                 removed)
                  sha (mint-followup!
                       store repos blobs repo-rid formats target-branch tgt-tip entries
                       (if merge?
                         (str "Merge branch '" source-branch "' into " target-branch)
                         #(str "Merge " source-branch ": " (change-subject %)))
                       (when merge?
                         {:extra-parents [src-tip]
                          :force? true
                          :author {:name (or actor "sutra")
                                   :email (str (or actor "sutra") "@sutra.local")
                                   :when-ms (System/currentTimeMillis)}}))]
              {:sha sha :carried (vec (keys overlay)) :removed (vec removed)
               :conflicts conflicts
               :merged? (boolean (and merge? sha))})))))))

(defn rebuild!
  [store repos blobs repo-rid]
  (when (nil? blobs)
    (throw (ex-info "rebuild needs the content-addressed store"
                    {:kind ::no-cas :repo repo-rid})))
  (g/forget-repo! repos repo-rid)
  (g/with-repo-lock repos repo-rid
    (let [n (atom 0)]
      (doseq [branch (git-rows/repo-branches store repo-rid)]
        (doseq [{:keys [commit-sha object-blob]} (git-rows/commits-of store repo-rid branch)]
          (g/with-inserter [ins repos repo-rid]
            (let [entries (into {}
                                (map (fn [{:keys [path blob-sha mode]}]
                                       [path
                                        (if (g/gitlink? mode)
                                          {:gitlink (str/trim blob-sha)}
                                          (g/entry
                                           (or (get-bytes blobs blob-sha)
                                               (throw (ex-info "CAS is missing a recorded blob"
                                                               {:kind ::missing-blob
                                                                :repo repo-rid
                                                                :commit commit-sha
                                                                :path path
                                                                :blob blob-sha})))
                                           mode))]))
                                (git-rows/files-of store repo-rid commit-sha))]
              (g/write-tree! ins entries))
            (when object-blob
              (let [raw (or (get-bytes blobs object-blob)
                            (throw (ex-info "CAS is missing a recorded commit object"
                                            {:kind ::missing-blob :repo repo-rid
                                             :commit commit-sha})))
                    got (g/insert-raw! ins g/obj-commit raw)]
                (when-not (= got commit-sha)
                  (throw (ex-info (str "rebuild produced a different sha: " got
                                       " ≠ " commit-sha)
                                  {:kind ::sha-drift :repo repo-rid
                                   :expected commit-sha :got got})))
                (swap! n inc))))))
      (doseq [branch (git-rows/repo-branches store repo-rid)
              :let [tip (:tip-sha (git-rows/repo-anchor store repo-rid branch))
                    ref (g/branch->ref branch)]
              :when tip]
        (g/pin-anchor! repos repo-rid branch tip)
        (when ref (g/update-ref! repos repo-rid ref tip {:force? true})))
      (doseq [{:keys [tag commit-sha]} (git-rows/tags-of store repo-rid)]
        (g/update-ref! repos repo-rid (str "refs/tags/" tag) commit-sha {:force? true}))
      {:commits @n :refs (g/all-refs repos repo-rid)})))

(defn rebuild-missing!
  [store repos blobs]
  (reduce (fn [acc rid]
            (try (rebuild! store repos blobs rid)
                 (update acc :rebuilt conj rid)
                 (catch InterruptedException e (throw e))
                 (catch Exception t
                   (assoc-in acc [:failed rid] (ex-message t)))))
          {:rebuilt [] :failed {}}
          (remove #(g/repo-exists? repos %)
                  (distinct (map :repo-rid (git-rows/repo-anchors store))))))
