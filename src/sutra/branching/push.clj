(ns sutra.branching.push
  (:require [sutra.kit.attempt :as attempt]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.repos.api :as repos]
            [sutra.store.world :as world]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.governance.api :as governance]
            [sutra.store.protection :as protection]
            [sutra.catalog.branch-policy :as policy]
            [sutra.authoring.api :as authoring]
            [sutra.store.seat :as seat]
            [sutra.actions.api :as actions]
            [sutra.parser.ent :as ent]
            [sutra.parser.ent-errors :as ent-errors]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.lint :as lint]
            [sutra.parser.spec :as spec]
            [sutra.parser.project-emit :as pemit]
            [sutra.parser.authoring.workflow :as workflow]
            [sutra.kit.refusal :as refusal]
            [sutra.parser.resolve :as res]
            [sutra.foundation.ops :as op]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.types.rid :as rid])
  (:import (java.io ByteArrayOutputStream InputStream)))

(set! *warn-on-reflection* true)

(defn pre-receive-rejections
  [repos repo-rid commands {:keys [protected? tag-ok? changeset-open? changed-texts]}]
  (into {}
        (keep
         (fn [{:keys [ref old new]}]
           (let [branch (repos/ref->branch ref)
                 tag?   (str/starts-with? (str ref) "refs/tags/")]
             (cond
               (and tag? tag-ok?)
               (when-let [why (tag-ok? ref new)] [ref why])

               tag? [ref (refusal/message :refusal.git/tag-not-accepted {})]

               (nil? branch)
               [ref (refusal/message :refusal.git/unsupported-ref {:ref ref})]

               (not (branch-kinds/projectable-branch? branch))
               [ref (refusal/message :refusal.git/machine-namespace {:branch branch})]

               (and new old (not (repos/descends-from? repos repo-rid new old)))
               [ref (refusal/message :refusal.git/fetch-first {:branch branch})]

               (and new (protected? branch))
               [ref (refusal/message :refusal.git/protected
                                     {:branch branch
                                      :steps  (workflow/sentence :land-protected
                                                                 {:branch "<name>"
                                                                  :trunk  branch})})]

               (and new changeset-open? (not= branch ctl/trunk)
                    (not (changeset-open? branch)))
               [ref (refusal/message :refusal.git/no-open-proposal
                                     {:branch branch
                                      :steps  (workflow/sentence :open-proposal
                                                                 {:branch branch
                                                                  :trunk  ctl/trunk})})]

               :else
               (when-let [texts (and new changed-texts (seq (changed-texts old new)))]
                 [ref (refusal/message :refusal.git/text-through-projection
                                       {:texts (str/join ", " texts)})]))))
         commands)))

(defn ooux-advisories
  [schema current vocab]
  (let [{:keys [changed] planned :schema} (authoring/planned-ontology schema current {})]
    (repos/changed-type-advisories
     (filter (comp lint/ooux-rules :rule) (lint/run-lint planned vocab))
     changed)))

(defn- lint-advisories
  [live live* schema]
  (let [vocab   (spec/vocabulary)
        attempt (fn [what f]
                  (try (f)
                       (catch Exception e
                         [(str what " could not run: "
                               (or (ex-message e) (.getName (class e))))])))]
    (into (attempt "lint" #(repos/advisories-for (lint/run-lint schema vocab)
                                                 (repos/authored-apps schema @live*)))
          (attempt "ooux check" #(ooux-advisories schema live vocab)))))

(defn- lint-gate
  [live live* ent]
  (let [schema (:schema ent)]
    (if-let [errs (ent/explain-schema schema)]
      {:status :failed
       :detail (str "unallowed structure or keys:\n  - "
                    (str/join "\n  - " (ent-errors/humanize->lines errs)))}
      (if-let [rerrs (try (res/explain (sugar/lower-schema schema)
                                       (authoring/live-ref-symbols live))
                          (catch Exception e
                            [(str "reference check could not run: " (ex-message e))]))]
        {:status :failed
         :detail (str "unresolved references:\n  - " (str/join "\n  - " rerrs))}
        {:status :passed
         :detail (repos/with-advisories "schema is structurally valid and fully resolved"
                   (lint-advisories live live* schema))}))))

(defn- strict-opts
  [store live repo-rid origin]
  (assoc (repos/push-plan-opts repo-rid live)
         :provenance origin
         :planned    (authoring/on-index store (:branch store))))

(defn- plan-gate
  [store live repo-rid origin ent]
  (try
    {:status :passed
     :detail (str "plan is clean: "
                  (pr-str (authoring/check-install store (:schema ent) live
                                                   (strict-opts store live repo-rid origin))))}
    (catch clojure.lang.ExceptionInfo e
      (let [d (ex-data e)]
        (if (= :plan-problems (:sutra/gate d))
          {:status :failed
           :detail (str (ex-message e) " — " (str/join "; " (map :message (:problems d))))}
          {:status :failed :detail (or (ex-message e) (str e))})))
    (catch Throwable t
      (attempt/rethrow-fatal! t)
      {:status :failed :detail (or (ex-message t) (str t))})))

(defn- ensure-ontology-branch!
  [store parent branch]
  (when (and branch (not= branch parent))
    (try
      (when-not (governance/branch-exists? store branch)
        (governance/create-branch! store {:parent parent :name branch}))
      (catch clojure.lang.ExceptionInfo e
        (when-not (governance/branch-exists? store branch) (throw e))))))

(defn- describe-batch
  [{:keys [owner n origin origin-ref opened-at mine?]}]
  (str n " change(s) staged by " (or owner "an unknown principal")
       (when mine? " (you)")
       (when opened-at (str " at " opened-at))
       (when origin (str " via " (name origin)))
       (when origin-ref (str " — " origin-ref))))

(defn- stage-only?
  [store repo-rid]
  (= :stage-only
     (some-> (get-in (seat/branch-schema store) [:repositories repo-rid :publish-mode]) keyword)))

(defn- open-trunk?
  [rows schema repo-rid branch]
  (and (= ctl/trunk (str branch)) (not (rid/ontology-repo? repo-rid))
       (not (protection/repository-protected? rows schema repo-rid))))

(defn- publish-guard
  [seen b open?]
  (fn [store touched rows]
    (-> (governance/write-guard store seen b rows {:open-trunk? open?})
        (protection/protection-guard b touched))))

(defn- failure-detail
  [e]
  (let [d (ex-data e)]
    (str (cond
           (= :plan-problems (:sutra/gate d))
           (str (ex-message e) " — " (str/join "; " (map :message (:problems d))))
           (#{:conflict :stale-object} (:store/refusal d))
           (str "commit refused: the branch changed under this push — " (ex-message e)
                ". Pull, rebase and push again")
           :else (or (ex-message e) (str e)))
         (when-let [rs (:reasons d)] (str " — " (pr-str rs)))
         ". Nothing was staged or published.")))

(defn- open-changeset-detail
  [open]
  (str "refused to publish onto an open changeset — " (describe-batch open)
       ". Commit it (`changeset commit`) or abandon it (`changeset discard`),"
       " then push again"))

(defn- publish-gate
  [store schema live repo-rid origin ent]
  (cond
    (nil? ent)
    {:status :passed :detail "this tree declares nothing" :schema live}

    (actions/open-transaction store)
    {:status :failed :detail (open-changeset-detail (actions/open-transaction store))}

    :else
    (try
      (let [b     (:branch store)
            seen  (protection/guard-reads store)
            open? (open-trunk? (:protection-rows seen) schema repo-rid b)
            opts  (strict-opts store live repo-rid origin)
            only? (stage-only? store repo-rid)
            tally (if only?
                    (authoring/stage-schema! (governance/write-guard store seen b nil
                                                                     {:open-trunk? open?})
                                             (:schema ent) live
                                             opts)
                    (:tally (authoring/publish-schema!
                             store (:schema ent) live opts
                             (assoc actions/commit-callbacks
                                    :guard (publish-guard seen b open?)))))]
        {:status :passed
         :detail (str (if only?
                        "staged (repository is :stage-only, a human commits): "
                        "published: ")
                      (pr-str tally))
         :schema (when-not only? (seat/branch-schema store))})
      (catch clojure.lang.ExceptionInfo e
        {:status :failed :detail (failure-detail e)})
      (catch Throwable t
        (attempt/rethrow-fatal! t)
        {:status :failed
         :detail (str (or (ex-message t) (str t)) ". Nothing was staged or published.")}))))

(defn- live-export
  [live]
  (delay (-> (pemit/export live {:all-projects? true :inline? false})
             :ent :schema)))

(defn- retire-gate
  [store live repo-rid ent]
  (let [opts (strict-opts store live repo-rid nil)]
    (repos/member-retire-verdict
     ((:planned opts) :retire #(authoring/plan-retirements (:schema ent) live (merge opts %))))))

(defn- stable-version? [tag]
  (boolean (re-matches #"v?\d+\.\d+\.\d+" (str tag))))

(defn tag-refusal
  [repos repo-rid settings protected-branch-tips ref sha]
  (let [tag  (subs (str ref) (count "refs/tags/"))
        {:keys [regex error-message]} (:tag-name-validation settings)]
    (cond
      (and regex (not (re-matches (re-pattern regex) tag)))
      (or error-message (str "tag `" tag "` does not match " regex))

      (and (:stable-tags-protected-only settings)
           (stable-version? tag)
           (not (some #(repos/descends-from? repos repo-rid % sha) protected-branch-tips)))
      (str "`" tag "` is a stable version and this repository only allows stable"
           " tags from a protected branch — tag a prerelease (e.g. " tag "-rc1)"
           " or land the commit on a protected branch first")

      :else nil)))

(defn- staging-origin-for
  [schema repo-rid branch new]
  {:origin     :git-push
   :origin-ref (str (or (some-> (get-in schema [:repositories repo-rid :repository-name]) name)
                        "ontology")
                    " " branch " " (subs (str new) 0 8))})

(defn- gates [store repo-rid schema live branch new]
  (let [branch-store    (ctx/at store branch)
        live*  (live-export live)
        origin (staging-origin-for schema repo-rid branch new)]
    {:ent        (repos/push-ent live (ctx/repos) repo-rid new)
     :lint-fn    #(lint-gate live live* %)
     :plan-fn    #(plan-gate branch-store live repo-rid origin %)
     :retire-fn  #(retire-gate branch-store live repo-rid %)
     :publish-fn #(publish-gate branch-store schema live repo-rid origin %)}))

(defn- decide-gates!
  [store repo-rid schema branch new]
  (repos/run-checks!
   store (ctx/repos) repo-rid new
   (assoc (gates store repo-rid schema (seat/branch-schema (ctx/at store branch)) branch new)
          :decide-only? true)))

(defn judge-before-adoption?
  [branch]
  (boolean (and branch (not (branch-kinds/sketch-branch? branch)))))

(defn- gate-refusal
  [r]
  (let [d (->> (:checks r)
               (remove #(= :passed (:status %)))
               (map :detail)
               (remove #(str/starts-with? (str %) "skipped:"))
               first)]
    (str "checks failed — " (or d "see refs/sutra/checks/<sha>")
         " · this commit was not written; fix and push again")))

(defn- time-phase!
  [phases k f]
  (let [t0 (System/nanoTime)
        r  (f)]
    (swap! phases update k (fnil + 0) (long (/ (- (System/nanoTime) t0) 1000000)))
    r))

(defn- checks-ref!
  [repo-rid sha]
  (repos/update-ref! (ctx/repos) repo-rid (str "refs/sutra/checks/" sha) sha {:force? true}))

(defn- push-reads
  [store schema repo-rid base]
  (let [gv         (governance/governance store)
        rows       (protection/protection-rows store)
        protected? #(if (and (= ctl/trunk (str %)) (not (rid/ontology-repo? repo-rid)))
                      (protection/repository-protected? rows schema repo-rid)
                      (boolean (:protected (policy/policy-of gv %))))]
    {:settings       (repos/repo-settings (ctx/repos) repo-rid base)
     :protected?     protected?
     :protected-tips (into [] (keep (fn [[ref sha]]
                                      (when (and (str/starts-with? ref "refs/heads/")
                                                 (protected? (repos/ref->branch ref)))
                                        sha)))
                           (repos/all-refs (ctx/repos) repo-rid))}))

(defn- structural-refusals
  [store repo-rid schema base {:keys [settings protected? protected-tips]} commands]
  (pre-receive-rejections
   (ctx/repos) repo-rid commands
   {:protected?       protected?
    :changed-texts    (fn [old new]
                        (repos/changed-texts schema (ctx/repos) repo-rid old
                                             (repos/ref-sha (ctx/repos) repo-rid
                                                            (repos/branch->ref base))
                                             new))
    :changeset-open? #(governance/changeset-offen? store %)
    :tag-ok?          (fn [ref sha]
                        (tag-refusal (ctx/repos) repo-rid settings protected-tips ref sha))}))

(defn- gated!
  [store repo-rid schema base phases branch new]
  (let [r (try
            (time-phase! phases :branch #(ensure-ontology-branch! store base branch))
            (if-let [open (actions/open-transaction (ctx/at store branch))]
              {:status :failed :checks [{:status :failed :detail (open-changeset-detail open)}]}
              (time-phase! phases :gates
                           #(decide-gates! store repo-rid schema branch new)))
            (catch Throwable t
              (attempt/rethrow-fatal! t)
              {:status :failed :checks [{:status :failed :detail (or (ex-message t) (str t))}]}))]
    (try (checks-ref! repo-rid new)
         (catch InterruptedException e (throw e))
         (catch Exception e
           (println "[git] checks ref of" repo-rid new "not written:" (ex-message e))))
    r))

(defn- refusals!
  [store repo-rid schema base phases reads judged commands]
  (reduce
   (fn [refused {:keys [ref new]}]
     (let [branch (repos/ref->branch ref)]
       (if (or (contains? refused ref) (nil? new) (not (judge-before-adoption? branch)))
         refused
         (let [r (gated! store repo-rid schema base phases branch new)]
           (swap! judged assoc new r)
           (cond-> refused
             (not= :passed (:status r)) (assoc ref (gate-refusal r)))))))
   (structural-refusals store repo-rid schema base reads commands)
   commands))

(defn- rewound!
  [repo-rid {:keys [ref old new]}]
  (if (repos/rewind-push! (ctx/repos) repo-rid ref old new)
    [(str "sutra: " ref " set back to " (or (some-> old (subs 0 8)) "nothing")
          " — the commit did NOT take effect and is on no branch; push it again")]
    [(str "sutra: " ref " moved on since this push — its commit did NOT take effect"
          " and a later commit on the branch may not carry it")]))

(defn- not-published!
  [repo-rid branch cmd]
  (if (judge-before-adoption? branch)
    (rewound! repo-rid cmd)
    [(str "sutra: " branch " keeps this unpublished commit; schema commits on it reach"
          " git only once a push on top publishes")]))

(defn- receive-failed!
  [store repo-rid new t published?]
  (let [msg (or (ex-message t) (str t))]
    (try
      (repos/set-check! store repo-rid new
                        (if published? "ci/sutra-followup" "ci/sutra-publish")
                        {:status "failed" :started? true :finished? true
                         :detail (str (if published?
                                        "the commit published, but what follows it errored: "
                                        "the push was accepted but errored before it published: ")
                                      msg)})
      (checks-ref! repo-rid new)
      (catch InterruptedException e (throw e))
      (catch Exception e
        (println "[git] failed check of" repo-rid new "not recorded:" (ex-message e))))
    [(str "sutra: SERVER ERROR after the commit was accepted — " msg)
     (if published?
       "sutra: the commit IS published; only the follow-up after it failed"
       "sutra: the commit did NOT take effect")]))

(defn- publish-tracked
  [published gs]
  (update gs :publish-fn
          (fn [f] (fn [ent]
                    (let [r (f ent)]
                      (when (= :passed (:status r)) (vreset! published true))
                      r)))))

(defn- changed-paths
  [repo-rid old new]
  (let [tree #(when % (repos/read-tree (ctx/repos) repo-rid %))
        a    (tree old)
        b    (tree new)]
    (for [p (distinct (concat (keys a) (keys b)))
          :when (not= (:blob (get a p)) (:blob (get b p)))]
      p)))

(defn- feeds-lines
  [store repo-rid branch {:keys [old new]}]
  (try
    (let [schema (seat/branch-schema (ctx/at store branch))
          hit    (set (changed-paths repo-rid old new))
          ids    (for [[_ t] (:transforms schema)
                       :when (and (= repo-rid (:repository t)) (hit (pemit/code-path t)))]
                   (str "transform/" (rid/->name (:transform-name t))))]
      (when (and (seq ids) (op/bound? :lineage/feeds-line))
        (some-> (op/query {:schema schema} :lineage/feeds-line {:ids (vec ids)}) vector)))
    (catch InterruptedException e (throw e))
    (catch Exception e
      (println "[git] feeds of" repo-rid branch "not read:" (ex-message e))
      nil)))

(defn- branch-received!
  [store repo-rid schema base phases judged branch {:keys [ref new] :as cmd}]
  (let [published (volatile! false)]
    (try
      (time-phase! phases :adopt
                   #(repos/adopt-push! store (ctx/repos) (ctx/blobs) repo-rid
                                       [{:ref ref :new new}]))
      (ensure-ontology-branch! store base branch)
      (let [r (time-phase!
               phases :publish
               (fn []
                 (let [gs (publish-tracked
                           published
                           (gates store repo-rid schema
                                  (seat/branch-schema (ctx/at store branch)) branch new))]
                   (if (contains? @judged new)
                     (repos/publish-only! store repo-rid new (:ent gs) (:publish-fn gs))
                     (repos/run-checks! store (ctx/repos) repo-rid new gs)))))]
        (when (= :passed (:status r))
          (repos/bind-published! store (ctx/repos) repo-rid branch new nil)
          (when-let [installed (:schema r)]
            (time-phase! phases :write-back
                         #(repos/normalize-followup! store (ctx/repos) (ctx/blobs) repo-rid
                                                     branch new installed))))
        (checks-ref! repo-rid new)
        (cond-> (vec (repos/side-band-lines (repos/summary store repo-rid new)))
          (= :passed (:status r))    (into (feeds-lines store repo-rid branch cmd))
          (not= :passed (:status r)) (into (not-published! repo-rid branch cmd))))
      (catch Throwable t
        (attempt/rethrow-fatal! t)
        (cond-> (receive-failed! store repo-rid new t @published)
          (not @published) (into (not-published! repo-rid branch cmd)))))))

(defn- received!
  [store repo-rid schema base phases judged {:keys [ref new] :as cmd}]
  (try
    (cond
      (nil? new) [(str "sutra: " ref " deleted")]

      (str/starts-with? (str ref) "refs/tags/")
      (do (repos/record-tag! store repo-rid (subs (str ref) (count "refs/tags/"))
                             new (some-> (grants/acting-for store) str))
          ["sutra: tag recorded"])

      :else
      (branch-received! store repo-rid schema base phases judged (repos/ref->branch ref) cmd))
    (catch Throwable t
      (attempt/rethrow-fatal! t)
      (receive-failed! store repo-rid new t false))))

(defn- push-hooks
  [store repo-rid schema base phases]
  (let [reads  (time-phase! phases :reads #(push-reads store schema repo-rid base))
        judged (atom {})]
    {:pre-receive  #(refusals! store repo-rid schema base phases reads judged %)
     :post-receive #(into [] (mapcat (partial received! store repo-rid schema base phases judged))
                          %)}))

(defn run-push!
  [store repo-rid schema req-branch ^InputStream in]
  (let [t-start (System/nanoTime)
        phases  (atom {})
        out     (ByteArrayOutputStream.)
        hooks   (push-hooks store repo-rid schema (or req-branch ctl/trunk) phases)]
    (time-phase! phases :jgit
                 #(repos/receive-pack (ctx/repos) repo-rid in out hooks))
    (let [total (long (/ (- (System/nanoTime) t-start) 1000000))
          {:keys [reads jgit gates adopt publish write-back]} @phases
          ours  (+ (or gates 0) (or adopt 0) (or publish 0) (or write-back 0))]
      (println
       (format (str "[git] push %s: total %dms · reads %dms · jgit %dms (our hooks %dms:"
                    " gates %dms · adopt %dms · publish %dms · write-back %dms)")
               (str repo-rid) total (or reads 0) (or jgit 0) ours
               (or gates 0) (or adopt 0) (or publish 0) (or write-back 0))))
    (.toByteArray out)))

(defn- tree-entries
  [repo-rid sha]
  (into {}
        (map (fn [[path {:keys [blob mode]}]]
               [path
                (if (repos/gitlink? mode)
                  {:gitlink blob}
                  (repos/entry (repos/blob-bytes (ctx/repos) repo-rid blob) mode))]))
        (when sha (repos/read-tree (ctx/repos) repo-rid sha))))

(defn- with-files
  [entries files]
  (reduce-kv (fn [m path text]
               (if (nil? text) (dissoc m path) (assoc m path (.getBytes (str text) "UTF-8"))))
             entries files))

(defn- files-commit!
  [repo-rid branch {:keys [files message author]}]
  (let [ref     (repos/branch->ref branch)
        old     (repos/ref-sha (ctx/repos) repo-rid ref)
        parent  (or old (repos/ref-sha (ctx/repos) repo-rid (repos/branch->ref ctl/trunk)))
        entries (with-files (tree-entries repo-rid parent) files)]
    {:ref ref
     :old old
     :new (repos/with-inserter [ins (ctx/repos) repo-rid]
            (repos/commit! ins
                           {:tree (repos/write-tree! ins entries)
                            :parents (if parent [parent] [])
                            :author author :message message}))}))

(defn check-files
  [store live repo-rid branch commit]
  (let [{:keys [new]} (files-commit! repo-rid branch commit)
        r (repos/run-checks! store (ctx/repos) repo-rid new
                             (assoc (gates store repo-rid live live branch new)
                                    :decide-only? true))]
    (when-not (= :passed (:status r))
      (gate-refusal r))))

(defn commit-files!
  [store schema repo-rid branch commit]
  (let [{:keys [ref old new] :as cmd} (files-commit! repo-rid branch commit)
        sha     new
        {:keys [pre-receive post-receive]} (push-hooks store repo-rid schema ctl/trunk (atom {}))]
    (when-let [why (get (pre-receive [cmd]) ref)]
      (refusal/refuse! :refusal.branching/push-refused {:commit sha :branch branch}
                       {:why why}))
    (repos/update-ref! (ctx/repos) repo-rid ref sha {:expected old})
    {:commit sha :branch branch :lines (post-receive [cmd])}))

(def ^:const max-advance-per-request
  4)

(defn advance-projection!
  [store repo-rid]
  (let [names    (mapv :name (world/branches store))
        tips     (world/tips store names)
        heads    (into {} (keep (fn [b]
                                  (when-let [rev (get-in tips [(str b) :last :rev])]
                                    [b rev])))
                       names)
        anchors  (group-by :repo-rid (repos/repo-anchors store))
        moved?   (fn [rid branch]
                   (when-let [h (get heads branch)]
                     (let [a (some #(when (= branch (:branch %)) (:tip-seq %))
                                   (get anchors rid))]
                       (or (nil? a) (> (long h) (long a))))))
        due      (fn [rid]
                   (let [moved (filter #(and (branch-kinds/projectable-branch? %)
                                             (moved? rid %))
                                       (distinct (cons ctl/trunk (keys heads))))
                         ended (when (seq moved) (governance/ended-branches store))]
                     (->> moved
                          (remove (set ended))
                          (sort-by (juxt #(if (= ctl/trunk %) 0 1) str))
                          (take max-advance-per-request))))
        advance  (fn [rid]
                   (into []
                         (keep (fn [branch]
                                 (try {:repo rid :branch branch
                                       :result
                                       (repos/project-branch! store (ctx/repos) (ctx/blobs)
                                                              rid branch)}
                                      (catch InterruptedException e (throw e))
                                      (catch Exception t
                                        (println "[git] projection of" rid "on" branch
                                                 "failed:" (.getMessage t))
                                        nil))))
                         (due rid)))]
    (advance repo-rid)))

(defn gc-tick! [store]
  (try
    (let [rids (distinct (map :repo-rid (repos/repo-anchors store)))]
      (doseq [rid rids]
        (try (repos/gc! (ctx/repos) rid)
             (catch InterruptedException e (throw e))
             (catch Exception t
               (println "[git-gc]" rid "failed:" (.getMessage t)))))
      (when (seq rids)
        (println (str "[git-gc] repacked " (count rids) " repo(s)"))))
    (catch InterruptedException e (throw e))
    (catch Exception t
      (println "[git-gc] tick error:" (.getMessage t)))))
