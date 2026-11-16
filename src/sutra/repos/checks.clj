(ns sutra.repos.checks
  (:require [sutra.repos.records :as git-rows]
            [sutra.kit.attempt :as attempt]
            [sutra.kit.noted :as noted]
            [sutra.kit.cache :as cache]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [sutra.types.transform-io :as tio]
            [sutra.types.entities :as ents]
            [sutra.types.kinds :as kinds]
            [sutra.repos.decorated :as decorated]
            [sutra.repos.projection :as gp]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.project-export :as pe]
            [sutra.parser.project-emit :as pemit]
            [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.registry :as registry]
            [sutra.store.world :as world]
            [sutra.repos.objects :as g]
            [sutra.types.advisory :as adv]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn owned-sections
  [repo-rid]
  (if (rid/ontology-repo? repo-rid) pe/ontology-sections pe/repo-sections))

(declare projected-names)

(defn push-plan-opts
  [repo-rid schema]
  (let [projected (delay (projected-names schema repo-rid))]
    (if (rid/ontology-repo? repo-rid)
      {:strict?           true
       :strict-sections   (pe/carried-sections true)
       :strict-projected  projected}
      {:strict?           true
       :strict-sections   (owned-sections repo-rid)
       :strict-repository repo-rid
       :strict-projected  projected})))

(defn repository-rid
  [schema x]
  (let [s (some-> x rid/->name str/trim)]
    (cond
      (str/blank? s)                           nil
      (rid/rid? s)                             s
      (#{"ontology" "ontologie"} s)            rid/ontology-repo-rid
      :else
      (let [[proj nm] (if (str/includes? s "/") (str/split s #"/" 2) [nil s])
            proj-name (fn [r] (some-> (get-in schema [:projects (str r) :project-name]) rid/->name))
            hits (for [[r spec] (:repositories schema)
                       :when (= nm (rid/->name (:repository-name spec)))
                       :when (or (nil? proj) (= proj (proj-name (:project spec)))
                                 (= proj (rid/->name (:project spec))))]
                   (str r))]
        (when (= 1 (count hits)) (first hits))))))

(defn- out-of-scope-detail [repo-rid stray]
  (str (if (rid/ontology-repo? repo-rid)
         (str "section(s) the ontology repository does not own: "
              (str/join ", " (map str stray))
              ". Code lives in a Source Repository — clone that repository and"
              " push there. ")
         (str "section(s) a code repository does not own: "
              (str/join ", " (map str stray))
              ". Ontology types belong to the ontology repository — clone the"
              " instance root and push there. "))
       "This repository may declare "
       (str/join ", " (map str (sort (owned-sections repo-rid))))
       "."))

(defn- ent-sections
  [ent]
  (into (sorted-set) (keys (:schema ent))))

(defn sources-carrying-credentials
  [ent]
  (->> (get-in ent [:schema :sources])
       (keep (fn [[nm spec]]
               (when (and (map? spec)
                          (or (seq (:secrets spec)) (seq (:secret-cipher spec))))
                 [nm (if (seq (:secrets spec)) :secrets :secret-cipher)])))
       (into (sorted-map))))

(defn changed-paths
  [repos repo-rid sha]
  (let [parent (first (:parents (g/commit-info repos repo-rid sha)))
        now    (g/read-tree repos repo-rid sha)
        before (if parent (g/read-tree repos repo-rid parent) {})]
    (into (sorted-set)
          (for [k (into (set (keys now)) (keys before))
                :when (not= (get now k) (get before k))]
            k))))

(defn- root-ent-paths
  [_repo-rid tree]
  (vec (sort (keys tree))))

(defn- tree-of
  [repo-rid]
  (if (rid/ontology-repo? repo-rid) :ontology :repo))

(def ^:private projection-of
  pe/git-projections)

(defn- formats-for
  ([repo-rid schema] (registry/texts schema (projection-of (tree-of repo-rid))))
  ([repo-rid schema paths text-at]
   (if (rid/ontology-repo? repo-rid)
     (pe/tree-formats schema (projection-of :ontology) paths text-at)
     (formats-for repo-rid schema))))

(defn- live-schema
  [store]
  (or (::schema store) (attempt/or-answer nil (world/world (world/at store nil)))))

(defn- schema-endings
  [formats]
  (into #{} (for [[_ d] formats
                  :when (nil? (:kind d))
                  g (:name d)
                  :let [file (subs g (inc (or (str/last-index-of g "/") -1)))
                        i    (str/last-index-of file "*")
                        dot  (str/index-of file ".")
                        e    (cond i (subs file (inc i)) dot (subs file dot))]
                  :when (seq e)]
              e)))

(defn- looks-like-schema?
  [formats path]
  (let [p (str path)]
    (and (not (str/includes? p "/"))
         (some #(str/ends-with? p %) (schema-endings formats)))))

(defn- read-root-ent
  [formats paths text-at]
  (when (seq paths)
    (let [{:keys [schema files]} (pe/read-texts formats paths text-at)]
      (when (seq files)
        (assoc (apply merge (keep :more (vals files))) :schema schema)))))

(defn- tree-declarations
  [schema repo-rid paths text-at]
  (let [t (tree-of repo-rid)]
    {:mine  (formats-for repo-rid schema paths text-at)
     :other (registry/texts schema (projection-of ({:ontology :repo :repo :ontology} t)))}))

(defn- entity? [formats path]
  (= :entity (pe/plane-of formats path)))

(defn- code-root-ents
  [mine tree]
  (vec (sort (filter #(and (not (str/includes? % "/")) (authoring/file? mine %)) (keys tree)))))

(defn foreign-files
  [{:keys [mine other]} tree]
  (vec (sort (filter #(and (entity? other %) (not (entity? mine %))) (keys tree)))))

(defn- unit-suffixes
  [{:keys [mine other]}]
  (into #{} (for [d (concat (vals mine) (vals other))
                  :when (:kind d)
                  g (:name d)
                  :let [i (str/last-index-of g "*")]
                  :when (and i (< (inc i) (count g)))]
              (subs g (inc i)))))

(defn misplaced-lean-files
  [{:keys [mine other] :as decls} tree]
  (let [sfx (unit-suffixes decls)]
    (vec (sort (filter #(and (some (fn [x] (str/ends-with? % x)) sfx)
                             (not (entity? mine %))
                             (not (entity? other %))
                             (not (contains? gp/platform-file-paths %)))
                       (keys tree))))))

(defn- unit-globs
  [formats]
  (sort (mapcat :name (filter :kind (vals formats)))))

(defn- blob-text
  [repos repo-rid tree]
  (fn [path] (String. ^bytes (g/blob-bytes repos repo-rid (:blob (get tree path))) "UTF-8")))

(defn- bad-paths
  [paths]
  (->> paths
       (filter (fn [p]
                 (or (str/blank? p)
                     (str/starts-with? p "/")
                     (some #{"." ".." ""} (str/split p #"/" -1)))))
       sort vec))

(defn- read-texts-or
  [formats paths text-at on-error]
  (try (read-root-ent formats paths text-at)
       (catch Exception e (on-error e))))

(defn- scope-facts
  [live repo-rid tree text-at]
  (let [paths     (keys tree)
        {:keys [mine] :as decls} (tree-declarations live repo-rid paths text-at)
        ontology? (rid/ontology-repo? repo-rid)
        root-ents (code-root-ents mine tree)
        misfiled  (when (and (not ontology?) (< 1 (count root-ents))) root-ents)
        schemas   (filter #(looks-like-schema? mine %) paths)
        ent-ps    (if ontology? (vec (sort schemas)) root-ents)
        read?     (and (seq ent-ps) (not misfiled))
        ent       (when read?
                    (read-texts-or mine (root-ent-paths repo-rid tree) text-at
                                   (fn [e] {::unreadable (.getMessage ^Exception e)})))
        declared  (when read? (read-texts-or mine ent-ps text-at (constantly nil)))]
    {:repo-rid     repo-rid
     :ontology?    ontology?
     :bad-path     (bad-paths paths)
     :unsound      (when ontology? (not-empty (pe/kinds-problems mine)))
     :misplaced    (misplaced-lean-files decls tree)
     :unit-globs   (unit-globs mine)
     :foreign      (foreign-files decls tree)
     :misfiled     misfiled
     :ent-ps       ent-ps
     :unread       (when (map? ent) (not-empty (remove #(entity? mine %) schemas)))
     :unreadable   (::unreadable ent)
     :stray        (when (map? declared)
                     (vec (remove (owned-sections repo-rid) (ent-sections declared))))
     :credentialed (when (map? ent) (sources-carrying-credentials ent))
     :planes       (frequencies (map #(pe/plane-of mine %) paths))}))

(defn- scope-verdict
  [{:keys [repo-rid ontology? bad-path unsound misplaced unit-globs foreign misfiled ent-ps
           unread unreadable stray credentialed planes]}]
  (cond
    (seq bad-path)
    {:status :failed
     :detail (str "path(s) outside the repository tree shape: " (str/join ", " bad-path))}

    unsound
    {:status :failed
     :detail (str "the projection's kinds of text do not fit together: " (str/join "; " unsound)
                  ". Fix its :filename in the :projections section first — a tree read with"
                  " them could be written back into texts they refuse.")}

    (seq misplaced)
    {:status :failed
     :detail (str "lean entity file(s) no text kind of the projection reads: "
                  (str/join ", " misplaced)
                  ". A .entv/.enta/.entp/.entt is only READ where a declaration matches it ("
                  (str/join " / " unit-globs)
                  ") — anywhere else it is carried as content, so it would parse,"
                  " look like the source of what it declares, and publish nothing."
                  " Move it to a declared path, or declare its place in the projection's "
                  ":filename.")}

    (seq foreign)
    {:status :failed
     :detail (str "file(s) belonging to the other tree: " (str/join ", " foreign)
                  ". The text kinds of "
                  (if ontology? "a code repository" "the ontology repository")
                  " read them, none of this one's — an Object View or a project's own text "
                  "belongs"
                  " to the ontology repository, an app, export, transform or pipeline to a"
                  " code repository. Push it to the tree that owns it.")}

    misfiled
    {:status :failed
     :detail (str "a code repository declares exactly one root `.ent`; this commit has: "
                  (str/join ", " misfiled))}

    (empty? ent-ps)
    {:status :passed :detail "no entity file in this commit"}

    unread
    {:status :failed
     :detail (str "no text kind of the projection reads " (str/join ", " unread)
                  " — declare it in the projection's :filename (language, envelope, name, owner),"
                  " or rename it to a declared pattern")}

    unreadable
    {:status :failed :detail (str "the `.ent` does not parse: " unreadable)}

    (seq stray)
    {:status :failed :detail (out-of-scope-detail repo-rid stray)}

    (seq credentialed)
    {:status :failed
     :detail (str "credential(s) in a git tree: "
                  (str/join ", " (map (fn [[nm k]] (str (name nm) " (" (name k) ")"))
                                      credentialed))
                  ". A source is declared here; its credential is set IN-PLATFORM"
                  " (`source-add`, `source secret <name>`, or the Source Manager"
                  " UI), sealed to the egress agent. Drop the key and push again"
                  " — an existing source keeps the credential it already has.")}

    :else
    {:status :passed
     :detail (str (get planes :entity 0) " entity-plane path(s), "
                  (get planes :file 0) " file-plane")}))

(defn scope-check
  [store repos repo-rid sha]
  (let [tree (g/read-tree repos repo-rid sha)]
    (scope-verdict (scope-facts (live-schema store) repo-rid tree
                                (blob-text repos repo-rid tree)))))

(def ^:private python-import
  #"(?m)^\s*(?:from\s+([A-Za-z_]\w*)(?:\.\w+)*\s+import|import\s+([A-Za-z_]\w*))")

(defn python-modules
  [text-at path]
  (let [dir     (if-let [i (str/last-index-of path "/")] (subs path 0 (inc i)) "")
        file-of (fn [m] (str dir m ".py"))
        imports (fn [src] (into #{} (keep #(or (second %) (nth % 2))) (re-seq python-import src)))]
    (loop [todo (vec (imports (or (text-at path) ""))) seen {}]
      (if-let [m (first todo)]
        (let [f   (file-of m)
              src (when-not (or (contains? seen m) (= f path)) (text-at f))]
          (if src
            (recur (into (subvec todo 1) (imports src)) (assoc seen m src))
            (recur (subvec todo 1) seen)))
        (into (sorted-map) seen)))))

(defn- code-from-file
  [node text-at]
  (let [f (:code-file node)]
    (-> (assoc node :code (text-at f))
        (cond-> (str/ends-with? f ".py")
          (as-> y (let [ms (python-modules text-at f)]
                    (cond-> y (seq ms) (assoc :modules ms)))))
        (dissoc :code-file))))

(defn resolve-code-files
  ([schema repos body branch] (resolve-code-files schema repos body branch nil))
  ([schema repos body branch default-repo]
  (let [tree-at (memoize
                 (fn [repo]
                   (when-let [rr (repository-rid schema repo)]
                     (when-let [sha (some #(some->> (g/branch->ref %) (g/ref-sha repos rr))
                                          (distinct [branch "main"]))]
                       (let [tree (g/read-tree repos rr sha)]
                         (fn [path]
                           (some->> (:blob (get tree path)) (g/blob-bytes repos rr)
                                    (#(String. ^bytes % "UTF-8")))))))))
        repo-of (fn [node] (some-> (or (:repository node) default-repo) tree-at))
        resolve (fn [node]
                  (cond
                    (and (:code-file node) (not (:code node)))
                    (let [text-at (repo-of node)]
                      (if (and text-at (text-at (:code-file node)))
                        (code-from-file node text-at)
                        node))

                    (and (= :python (some-> (:language node) keyword)) (:code node)
                         (not (:modules node)))
                    (let [text-at (repo-of node)
                          own     "transforms/__inline__.py"
                          ms      (when text-at
                                    (python-modules #(if (= % own) (:code node) (text-at %))
                                                    own))]
                      (cond-> node (seq ms) (assoc :modules ms)))

                    :else node))]
    (reduce (fn [b section]
              (cond-> b
                (map? (get b section))
                (update section #(into (empty %)
                                       (map (fn [[k node]]
                                              [k (resolve (decorated/with-code-path k node))]))
                                       %))))
            body [:transforms :functions]))))

(defn- inline-siblings
  [repos repo-rid tree schema]
  (let [bytes-at (fn [path]
                   (some->> (:blob (get tree path)) (g/blob-bytes repos repo-rid)))
        text-at  (fn [path] (some-> ^bytes (bytes-at path) (String. "UTF-8")))]
    (walk/postwalk
     (fn [x]
       (if-not (map? x)
         x
         (cond-> x
           (and (:code-file x) (not (:code x)) (text-at (:code-file x)))
           (code-from-file text-at)

           (and (:source-file x) (not (:source x)) (text-at (:source-file x)))
           (-> (assoc :source (pemit/strip-migrate-banner (text-at (:source-file x))))
               (dissoc :source-file))

           (and (:template-file x) (not (:template-b64 x)) (bytes-at (:template-file x)))
           (-> (assoc :template-b64
                      (.encodeToString (java.util.Base64/getEncoder)
                                       ^bytes (bytes-at (:template-file x))))
               (dissoc :template-file)))))
     schema)))

(defn- merged-tree-schema
  [root-schema]
  (cond-> root-schema (map? root-schema) sugar/lower-schema))

(defn- commit-ent*
  [store repos repo-rid sha]
  (let [tree    (g/read-tree repos repo-rid sha)
        text-at (blob-text repos repo-rid tree)]
    (when-let [ps (not-empty (root-ent-paths repo-rid tree))]
      (try
        (let [root (read-root-ent (formats-for repo-rid (live-schema store) (keys tree) text-at) ps
                                  text-at)]
          (cond-> root
            (map? root)
            (update :schema
                    #(inline-siblings repos repo-rid tree (merged-tree-schema %)))))
        (catch InterruptedException e (throw e))
        (catch Exception e
          {::unreadable (or (ex-message e) (str e))})))))

(defn projected-names
  [schema repo-rid]
  (let [tree    (gp/entity-tree schema repo-rid)
        text-at (fn [path] (String. ^bytes (get tree path) "UTF-8"))
        root    (:schema (read-root-ent (formats-for repo-rid schema)
                                        (root-ent-paths repo-rid tree) text-at))
        merged  (merged-tree-schema (or root {}))]
    (into {}
          (for [[section m] merged
                :when (coll? m)]
            [section (into #{}
                           (map #(rid/->name (if (map-entry? %) (key %) (first %))))
                           m)]))))

(defonce ^:private ent-cache
  (cache/lru 32))

(defn commit-ent
  [store repos repo-rid sha]
  (when sha
    (cache/through ent-cache [(str repo-rid) (str sha)] #(commit-ent* store repos repo-rid sha))))

(defn- without-deleted-code
  [schema text-at]
  (let [kept (fn [nodes] (into (empty nodes)
                               (remove (fn [[_ {f :code-file}]] (and f (not (text-at f)))))
                               nodes))]
    (reduce (fn [s section] (cond-> s (map? (get s section)) (update section kept)))
            schema
            [:transforms :functions])))

(defn declaration
  [formats rendered tree-paths pushed owner]
  (let [text?   #(authoring/file? formats %)
        text-at (fn [p]
                  (if (text? p)
                    (some-> ^bytes (get rendered p) (String. "UTF-8"))
                    (pushed p)))
        paths   (concat (filter text? (keys rendered)) (remove text? tree-paths))
        root    (or (read-root-ent formats paths text-at) {})]
    (update root :schema
            (fn [schema]
              (let [whole (merged-tree-schema (or schema {}))
                    kept  (without-deleted-code whole pushed)
                    gone  (apply dissoc (:transforms whole) (keys (:transforms kept)))]
                (decorated/with-decorated formats tree-paths pushed (assoc owner :gone gone)
                                          kept))))))

(defn- repository-outputs
  [live repo-rid]
  (let [repo    (gp/repo-name-in live repo-rid)
        ours?   #(or (= repo-rid (:repository %))
                     (= (str repo) (some-> (:repository %) rid/->name)))
        name-of (fn [section nm-key r]
                  (some-> (or (get-in live [section r nm-key]) r) rid/->name))]
    (->> (for [t (vals (:transforms live))
               :when (ours? t)
               {:keys [kind] :as o} (tio/outputs-of t)
               :let [[section r] (when (kinds/ddl-family kind) (kinds/output-slot o))
                     nm          (when section (name-of section (ents/name-field section) r))]
               :when nm]
           [[kind nm] (keyword (rid/->name (:transform-name t)))])
         (group-by first)
         (keep (fn [[out writers]] (when (= 1 (count writers)) [out (second (first writers))])))
         (into {}))))

(defn- output-names
  [live]
  (into {}
        (for [[section k] [[:datasets :dataset-name]
                           [:object-types :object-type-name]
                           [:link-types :link-type-name]]
              [r spec] (get live section)
              :when (get spec k)]
          [(str r) (rid/->name (get spec k))])))

(defn- former-dataset-names
  [live]
  (let [ds   (vals (:datasets live))
        now? (into #{} (keep #(some-> (:dataset-name %) rid/->name)) ds)]
    (into {}
          (for [d ds f (:former-names d)
                :let [former (rid/->name f)]
                :when (not (now? former))]
            [former (rid/->name (:dataset-name d))]))))

(defn- repository-owner
  [live repo-rid]
  (let [p (get-in live [:repositories repo-rid :project])]
    {:repository (keyword (gp/repo-name-in live repo-rid))
     :project    (some-> (or (get-in live [:projects (str p) :project-name]) p)
                         rid/->name keyword)
     :outputs    (repository-outputs live repo-rid)
     :names      (output-names live)
     :former     (former-dataset-names live)}))

(defn push-ent
  [live repos repo-rid sha]
  (when-not (rid/ontology-repo? repo-rid)
    (try
      (let [tree (g/read-tree repos repo-rid sha)]
        (update (declaration (formats-for repo-rid live) (gp/entity-tree live repo-rid)
                             (keys tree) (blob-text repos repo-rid tree)
                             (repository-owner live repo-rid))
                :schema #(inline-siblings repos repo-rid tree %)))
      (catch InterruptedException e (throw e))
      (catch Exception e
        {::unreadable (or (ex-message e) (str e))}))))

(defn texts-changed
  [formats now before base]
  (let [blob (fn [tree p] (:blob (get tree p)))]
    (vec (sort (for [p (distinct (concat (keys now) (keys before)))
                     :when (authoring/file? formats p)
                     :let [b (blob now p)]
                     :when (and (not= b (blob before p)) (not= b (blob base p)))]
                 p)))))

(defn changed-texts
  [schema repos repo-rid old base-sha new]
  (let [tree-at #(if % (g/read-tree repos repo-rid %) {})]
    (texts-changed (formats-for repo-rid schema)
                   (tree-at new) (tree-at (or old base-sha)) (tree-at base-sha))))

(defn member-retire-verdict
  [{:keys [retiring problems]}]
  (let [render (fn [xs] (str/join ", " (for [{:keys [kind entity status]} xs]
                                         (str (name kind) " " (name entity)
                                              (when status (str " (" (name status) ")"))))))]
    (cond
      (seq problems)
      {:status :failed
       :detail (str "this commit leaves out " (count retiring) " member(s) it may not remove yet: "
                    (str/join "; " (map :message problems)))}

      (seq retiring)
      {:status :passed
       :detail (str "retiring " (count retiring) " member(s) of this repository: "
                    (render retiring) " — none :active, none still referenced")}

      :else
      {:status :passed :detail "no member of this repository is left out; none retired"})))

(defn derived-file-check
  [repos repo-rid sha]
  (if-not (rid/ontology-repo? repo-rid)
    {:status :passed :detail "not the ontology repository; nothing injected here"}
    (let [tree    (g/read-tree repos repo-rid sha)
          touched (filterv #(and (gp/platform-file-paths %) (contains? tree %))
                           (changed-paths repos repo-rid sha))]
      (if (empty? touched)
        {:status :passed :detail "no platform-supplied file added or changed"}
        {:status :failed
         :detail (str "this commit adds or changes platform-supplied file(s): "
                      (str/join ", " touched)
                      ". The instance serves them and the client lays them into the"
                      " checkout, excluded — they belong to no commit, so this change"
                      " is inert. Author real Object Views under"
                      " object-views/<type>.entv; change a template by editing its"
                      " preset in the platform repository, not here.")}))))

(def ^:private ent-gates
  {:retire [:retire-fn "nothing declared, so nothing retired"]
   :revert [:revert-fn "nothing declared, so nothing re-stated"]
   :lint   [:lint-fn   "nothing to lint"]
   :plan   [:plan-fn   "nothing to plan"]})

(def check-order
  [:scope :derived :retire :revert :lint :plan :test :ci/sutra-publish])

(defn- check-name [check]
  (subs (str check) 1))

(defn- record [store repo-rid sha check status detail]
  (git-rows/set-check! store repo-rid sha (check-name check)
                       {:status status :detail detail :started? true :finished? true})
  {:check check :status status :detail detail})

(defn run-checks!
  [store repos repo-rid sha {:keys [test-fn publish-fn decide-only?] :as opts}]
  (doseq [c check-order]
    (git-rows/set-check! store repo-rid sha (check-name c)
                         {:status "queued" :started? false :finished? false}))
  (let [ent (if (contains? opts :ent) (:ent opts) (commit-ent store repos repo-rid sha))
        run (fn [check f]
              (let [{:keys [status detail] :as r}
                    (try (f) (catch InterruptedException e (throw e))
                             (catch Exception t
                               {:status :failed :detail (or (ex-message t) (str t))}))]
                (record store repo-rid sha check (name (or status :failed)) detail)
                r))]
    (loop [[c & more] check-order
           acc        []
           schema     nil]
      (cond
        (nil? c) {:status :passed :checks acc :schema schema}

        (and (= c :ci/sutra-publish) decide-only?)
        {:status :passed :checks acc :schema schema :pending-publish? true}

        (= c :ci/sutra-publish)
        (let [r (run c #(if publish-fn
                          (publish-fn ent)
                          {:status :failed
                           :detail "no publish gate is wired into this instance"}))]
          {:status (if (= :passed (:status r)) :passed :failed)
           :checks (conj acc r)
           :schema (or (:schema r) schema)})

        :else
        (let [r (run c (if-let [[fn-key unwired] (ent-gates c)]
                         (let [f (get opts fn-key)]
                           #(if (and f ent) (f ent)
                                {:status :passed :detail unwired}))
                         (case c
                           :scope  #(if-let [e (::unreadable ent)]
                                      {:status :failed
                                       :detail (str "an entity-plane file does not parse: " e)}
                                      (scope-check store repos repo-rid sha))
                           :derived #(derived-file-check repos repo-rid sha)
                           :test  #(if test-fn (test-fn repo-rid sha)
                                       {:status :passed :detail "no test runner wired"}))))]
          (if (= :passed (:status r))
            (recur more (conj acc r) schema)
            (let [skipped (str "skipped: " (check-name c) " failed")]
              (doseq [d more]
                (record store repo-rid sha d "failed" skipped))
              {:status :failed
               :checks (into (conj acc r)
                             (map (fn [d] {:check d :status :failed :detail skipped}))
                             more)
               :schema schema})))))))

(defn publish-only!
  [store repo-rid sha ent publish-fn]
  (let [r   (try (cond
                   (::unreadable ent)
                   {:status :failed
                    :detail (str "an entity-plane file does not parse: "
                                 (::unreadable ent))}

                   publish-fn (publish-fn ent)

                   :else
                   {:status :failed
                    :detail "no publish gate is wired into this instance"})
                 (catch InterruptedException e (throw e))
                 (catch Exception t
                   {:status :failed :detail (or (ex-message t) (str t))}))]
    (record store repo-rid sha :ci/sutra-publish
            (name (or (:status r) :failed)) (:detail r))
    {:status (if (= :passed (:status r)) :passed :failed)
     :checks [r]
     :schema (:schema r)}))

(defn summary
  [store repo-rid sha]
  (let [rows (git-rows/checks-of store repo-rid sha)]
    {:commit sha
     :status (cond (empty? rows)                          :unknown
                   (some #(= "failed" (:status %)) rows)  :failed
                   (every? #(= "passed" (:status %)) rows) :passed
                   :else                                   :running)
     :checks (mapv (fn [r] {:check  (:check-name r)
                            :status (keyword (:status r))
                            :detail (:detail r)})
                   rows)}))

(def repo-settings-path
  "sutra.repo.edn")

(defn repo-settings
  [repos repo-rid base-branch]
  (or (noted/quietly (str repo-settings-path " of " repo-rid)
        (when-let [ref (g/branch->ref base-branch)]
          (when-let [tip (g/ref-sha repos repo-rid ref)]
            (when-let [e (get (g/read-tree repos repo-rid tip) repo-settings-path)]
              (let [m (edn/read-string (String. ^bytes (g/blob-bytes repos repo-rid (:blob e))
                                                "UTF-8"))]
                (when (map? m) m))))))
      {}))

(defn repo-commits-on-branch
  [store schema branch]
  (into [] (for [r (cons rid/ontology-repo-rid (keys (:repositories schema)))
                 :let [rows (git-rows/commits-of store r branch)]
                 :when (seq rows)]
             [r rows])))

(defn repos-on-branch
  [store schema branch]
  (mapv first (repo-commits-on-branch store schema branch)))

(defn pushed-tip
  [rows]
  (last (filter #(= "pushed" (some-> (:source %) str)) rows)))

(defn reviewable-tip
  [rows]
  (or (pushed-tip rows) (last rows)))

(defn check-row
  [row]
  {:check  (:check-name row)
   :status (keyword (:status row))
   :detail (:detail row)})

(defn commit-ledger
  [store schema branches]
  (reduce (fn [acc r]
            (reduce-kv (fn [a b rows] (cond-> a (seq rows) (update b (fnil conj []) [r rows])))
                       acc
                       (git-rows/commits-of-branches store r branches)))
          {}
          (cons rid/ontology-repo-rid (keys (:repositories schema)))))

(defn- tips-of [rows] (mapv (fn [[r cs]] [r (:commit-sha (reviewable-tip cs))]) rows))

(defn checks-of
  ([store rows] (checks-of store rows nil))
  ([store rows known]
   (let [tips (tips-of rows)]
     {:repos      (mapv first rows)
      :tips       tips
      :source-tip (some (fn [[_ cs]] (some-> (:commit-sha (pushed-tip cs)) str)) rows)
      :check-status-fn
      (fn [_sha]
        (into []
              (for [[r tip] tips
                    row (if (and known tip)
                          (get known [r (str tip)] [])
                          (git-rows/checks-of store r tip))]
                (check-row row))))})))

(defn checks-of-ledger
  [store ledger]
  (let [by-repo (group-by first (remove (comp nil? second) (mapcat tips-of (vals ledger))))]
    (into {}
          (for [[r pairs] by-repo
                [sha rows] (git-rows/checks-of-commits store r (map second pairs))]
            [[r sha] rows]))))

(defn proposal-checks
  [store schema source]
  (checks-of store (repo-commits-on-branch store schema source)))

(defn proposal-facts
  [store repos schema source target]
  (let [{repo-rids :repos :keys [tips] :as checks} (proposal-checks store schema source)]
    (assoc (select-keys checks [:source-tip :check-status-fn])
           :changed-paths
           (into (sorted-set) (for [[r tip] tips p (changed-paths repos r tip)] p))
           :approval-policy
           (into [] (mapcat #(:approval-policy (repo-settings repos % target))) repo-rids))))

(defn authored-apps
  [pushed live]
  (into #{}
        (keep (fn [[k v]]
                (when (not= v (get-in live [:applications k]))
                  (str (rid/->name k)))))
        (:applications pushed)))

(defn advisories-for
  [findings authored]
  (->> findings
       (filter #(= :warn (:severity %)))
       (filter #(authored (first (str/split (str (:entity %)) #"\." 2))))
       (mapv #(str (:entity %) " — " (:message %)))))

(defn changed-type-advisories
  [findings changed]
  (->> findings
       (filter #(#{:warn :error} (:severity %)))
       (filter #(contains? (get changed (:entity-class %))
                           (first (str/split (str (:entity %)) #"\." 2))))
       (mapv #(str (:entity %) " — " (:message %)))))

(defn with-advisories
  [detail advisories]
  (adv/attach detail advisories))

(defn side-band-lines
  [{:keys [status checks]}]
  (into [(str "sutra: checks " (name status))]
        (mapcat (fn [{:keys [check status detail]}]
                  (if (= :passed status)
                    (map #(str "sutra: " check " " adv/marker %) (adv/lines detail))
                    [(str "sutra: " check " " (name status)
                          (when detail (str " — " detail)))])))
        checks))
