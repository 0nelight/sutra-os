(ns sutra.branching.proposals
  (:require [sutra.kit.noted :as noted]
            [clojure.string :as str]
            [sutra.store.access.scope :as ctx]
            [sutra.repos.api :as repos]
            [sutra.builds.api :as builds]
            [sutra.notifications.api :as notifications]
            [sutra.actions.api :as actions]
            [sutra.catalog.grants :as grants]
            [sutra.governance.api :as governance]
            [sutra.store.protection :as protection]
            [sutra.catalog.branch-policy :as policy]
            [sutra.branching.landing :as pl]
            [sutra.access.api :as access]
            [sutra.types.project-graph :as pgraph]
            [sutra.types.authority :as policy-auth]
            [sutra.types.rid :as rid]
            [sutra.store.memo :as memo]
            [sutra.store.read-only :as read-only]
            [sutra.store.world :as world]
            [sutra.store.seat :as seat]))

(set! *warn-on-reflection* true)

(defn- git-facts
  ([store schema p] (git-facts store schema p nil nil))
  ([store schema p ledger known]
   (try
     (let [{:keys [source-tip check-status-fn]}
           (if ledger
             (repos/checks-of store (get ledger (str (:source-branch p)) []) known)
             (repos/proposal-checks store schema (:source-branch p)))]
       (when source-tip
         (let [rows (vec (check-status-fn source-tip))]
           {:commit source-tip
            :status (cond (empty? rows)                            "unknown"
                          (some #(= :failed (:status %)) rows)     "failed"
                          (every? #(= :passed (:status %)) rows)   "passed"
                          :else                                    "running")
            :checks (mapv #(update % :status name) rows)})))
     (catch InterruptedException e (throw e))
     (catch Exception e
       (println "[governance] checks of" (:source-branch p) "not read:" (ex-message e))
       nil))))

(declare target-reads with-stage)

(def ^:private judging-width 6)

(declare land!)

(defn- proposing-reads
  [store {:keys [source target]}]
  (let [missing (vec (remove #(world/exists? store (str %)) [source target]))]
    {:missing missing
     :open    (when (empty? missing) (governance/open-proposal-of store source target))}))

(defn propose!
  [store schema {:keys [source target merge-mode] :as body} admin?*]
  (let [asked (assoc (select-keys body [:title :description :justification])
                     :source     (some-> source clojure.core/name)
                     :target     (or (some-> target clojure.core/name)
                                     policy/default-branch-name)
                     :merge-mode (some-> merge-mode clojure.core/name keyword))
        r     (or (governance/proposal-refusal (merge asked (proposing-reads store asked)))
                  (let [preview (governance/preview-branch-schema-merge
                                 store (:source asked) (:target asked))]
                    (assoc (governance/open-proposal! store asked)
                           :preview (policy/merge-preview-of preview))))]
    (cond
      (:error r) (governance/raise-refused! r {} :refusal.branching/refused)

      (and (governance/auto-land? store (:target-branch r))
           (not= :empty (get-in r [:preview :status]))
           (not (contains? policy/unjudgeable-previews (get-in r [:preview :status]))))
      (let [m (land! store schema (:id r) admin?*)]
        (if (:error m)
          (assoc r :auto-land (select-keys m [:error :conflicts :reasons :checks]))
          (assoc m :auto-landed true)))

      :else r)))

(defn owner?
  [store branch]
  (let [of (memoize #(access/approver store branch (str %)))]
    (fn [who project]
      (cond-> ((of who) project)
        (nil? project) (assoc :owner? true :editor? true)))))

(declare previewed)

(defonce ^:private previews (atom {}))

(def ^:private previews-spec {::preview {:cap 256}})

(defn- preview-key
  [store {src :source-branch tgt :target-branch} opts]
  (let [tick #(world/schema-tick (world/at store %))
        home (when-let [h (world/instance-home (world/at store tgt))] (tick h))]
    [(:db store) src tgt (:base (world/branch store src)) (tick src) (tick tgt) home opts]))

(defn- remembered-preview
  [store p opts]
  (if-let [k (try (preview-key store p opts) (catch clojure.lang.ExceptionInfo _ nil))]
    (memo/through previews previews-spec ::preview k #(previewed store p opts))
    (previewed store p opts)))

(defn target-reads
  [store]
  (let [store  (read-only/of store)
        gv     (delay (governance/governance store))
        schema (memoize #(try (seat/branch-schema (ctx/at store %))
                              (catch clojure.lang.ExceptionInfo e
                                (noted/note (str "schema of target " %) e))))]
    {:policy    (memoize #(policy/policy-of @gv %))
     :protection (memoize (fn [target]
                            (when (and (= policy/default-branch-name (str target))
                                       (not (:protected (policy/policy-of @gv target))))
                              (protection/protection store))))
     :contributors (memoize (fn [source]
                              (try (governance/contributors store source)
                                   (catch clojure.lang.ExceptionInfo e
                                     (noted/note (str "contributors of " source) e)
                                     []))))
     :schema    schema
     :consumers (memoize #(try (pgraph/consumers (schema %))
                               (catch clojure.lang.ExceptionInfo e
                                 (noted/note (str "consumers on " %) e)
                                 {})))
     :owner     (memoize #(owner? store %))
     :policies  (memoize #(protection/container-policies store %))
     :owns      (memoize #(access/owns store %1 %2))
     :edits     (memoize (fn [branch who]
                           (let [of (access/approver store branch who)]
                             #(:editor? (of %)))))
     :preview   (fn [p opts] (remembered-preview store p opts))}))

(defn- annotated
  [reads p tasks]
  (when (seq tasks)
    (let [target (:target-branch p)
          schema ((:schema reads) target)
          chain  #(or (policy-auth/container-chain schema %) [%])]
      (policy/annotate-tasks
       tasks
       {:branch-rules (policy/review-rules ((:policy reads) target))
        :policies     ((:policies reads) (mapcat chain (distinct (keep :project tasks))))
        :schema       schema
        :rctx         {:author (:author p) :author-owns? ((:owns reads) target (:author p))
                       :author-edits? ((:edits reads) target (:author p))
                       :justification (:justification p)}
        :protection   ((:protection reads) target)
        :contributors (fn [_] ((:contributors reads) (:source-branch p)))
        :consumers    ((:consumers reads) target)}))))

(def ^:private judge-only
  {:changes? false})

(declare preview-block unjudgeable-status)

(defn- judged
  [reads p opts]
  (let [pv    ((:preview reads) p opts)
        block (preview-block pv)
        bad?  (contains? unjudgeable-status (:status block))
        got   (when-not bad?
                (try {:tasks (annotated reads p (governance/preview-tasks (:prev pv)))}
                     (catch InterruptedException e (throw e))
                     (catch Exception t {:error (or (ex-message t) (str t))})))
        tasks (:tasks got)
        why   (or (:error block) (:error got))
        own?  ((:owner reads) (:target-branch p))]
    {:preview    block
     :tasks      tasks
     :error      why
     :stage      (if (or bad? (:error got)) "unjudgeable" (policy/stage-of p tasks own?))
     :open-tasks (vec (:open-tasks (policy/tasks-block p tasks own?)))}))

(defn tasks-of [reads p]
  (when p (:tasks (judged reads p judge-only))))

(defn with-stage
  [reads p]
  (when p
    (let [{:keys [stage open-tasks preview error]} (judged reads p judge-only)]
      (cond-> (assoc p :stage stage :open-tasks open-tasks :preview preview
                     :auto-land (boolean (:auto-land ((:policy reads) (:target-branch p)))))
        error (assoc :judgement-error error)))))

(defn with-reviews
  ([p tasks] (with-reviews p tasks nil))
  ([p tasks comments]
   (let [by-task (group-by #(some-> (:task %) str) comments)]
     (for [t tasks
           :let [wer-by (group-by (fn [[_ u]] (policy/approval-state t u))
                                  (for [r (:reviews p)
                                        :let [u (get (:task-verdicts r) (:task t))]
                                        :when u]
                                    [(:approver r) u]))
                 wer    (fn [state] (mapv first (get wer-by state)))]]
       (assoc t
              :approvals  (wer :approved)
              :stale      (wer :stale)
              :rejections (wer :rejected)
              :comments   (vec (get by-task (str (:task t)))))))))

(defn- previewed
  ([store p] (previewed store p nil))
  ([store p opts]
   (try {:prev
         (governance/preview-branch-schema-merge store (:source-branch p) (:target-branch p)
                                                 opts)}
        (catch InterruptedException e (throw e))
        (catch Exception t {:error (or (ex-message t) (str t))}))))

(def ^:private unjudgeable-preview
  {:gone            "gone"
   :no-base         "no-base"
   :base-unreadable "base-unreadable"})

(def ^:private unjudgeable-status
  (conj (set (vals unjudgeable-preview)) "unreadable"))

(defn- preview-block
  [{:keys [prev error]}]
  (let [why (or error (:error prev))]
    (cond-> {:status    (cond error                                 "unreadable"
                              (seq (:conflicts prev))               "conflicts"
                              (unjudgeable-preview (:status prev))
                              (unjudgeable-preview (:status prev))
                              (seq (:diff prev))                    "clean"
                              :else                                 "empty")
             :conflicts (vec (:conflicts prev))}
      why (assoc :error why))))

(defn- unit-test-results
  [store src tasks]
  (builds/of-transforms
   (ctx/at store src) (world/world (world/at store src))
   (keep #(when (= "transforms" (some-> (:section %) name)) (:rid %)) tasks)))

(defn- unit-tests-view
  [store p tasks]
  (try (unit-test-results store (:source-branch p) tasks)
       (catch InterruptedException e (throw e))
       (catch Exception t [{:error (or (ex-message t) (str t))}])))

(defn proposal
  [store schema id]
  (when-let [p (governance/fetch store id)]
    (let [reads (target-reads store)
          {:keys [stage open-tasks tasks preview error]} (judged reads p nil)
          komm  (vec (governance/comments-of store id))]
      (cond-> (assoc p
                     :stage    stage
                     :open-tasks open-tasks
                     :tasks    (vec (with-reviews p tasks komm))
                     :comments komm
                     :checks   (git-facts store schema p)
                     :unit-tests (unit-tests-view store p tasks)
                     :merge-history (governance/merge-attempts store id)
                     :preview  preview)
        error (assoc :judgement-error error)))))

(defn notify-review!
  [store schema p]
  (let [tasks (tasks-of (target-reads store) p)
        open  (:open-tasks (policy/tasks-block p tasks (owner? store (:target-branch p))))
        db    (grants/policy-database schema {:who "system"})]
    (when-let [{:keys [payload]} (policy/review-notice
                                  p tasks open #(policy-auth/holders db :manage-permissions %))]
      (notifications/send!
       store policy/default-branch-name
       (remove #{(str (:author p))} (mapcat :owners (:tasks payload)))
       {:source     :review
        :heading    (str "Prüfung angefragt: " (or (:title p) (:id p)))
        :message    (str (:author p) " bittet um Prüfung von "
                         (str/join ", " (keep :name (:tasks payload))))
        :link-url   (str "/proposals/" (:id p))
        :link-label "Proposal öffnen"}))))

(defn- run-git-merge-half!
  [store schema {:keys [source target merge-mode actor]}]
  (into {}
        (keep (fn [repo-rid]
                (when (and (repos/repo-anchor store repo-rid target)
                           (world/exists? store source))
                  (repos/project-branch! store (ctx/repos) (ctx/blobs) repo-rid source))
                (let [r  (repos/merge-git-half! store (ctx/repos) (ctx/blobs) repo-rid
                                                source target
                                                {:merge-mode merge-mode :actor actor})
                      rr (try (repos/merge! store (ctx/blobs) repo-rid source target)
                              (catch InterruptedException e (throw e))
                              (catch Exception t {:error (or (ex-message t) (str t))}))
                      r  (cond-> r rr (assoc :riders rr))]
                  (when (or (:sha r) (seq (:conflicts r)) rr)
                    [repo-rid r]))))
        (cons rid/ontology-repo-rid (keys (:repositories schema)))))

(defn git-pending-tick!
  [store]
  (doseq [{:keys [proposal-id source target merge-mode actor authority]}
          (governance/due-git-halves store)
          :let [store (ctx/act-for store (grants/stored authority))]]
    (let [fp (try (run-git-merge-half! store (seat/branch-schema (ctx/at store target))
                                       {:source source :target target :actor actor
                                        :merge-mode (some-> merge-mode keyword)})
                  (catch InterruptedException e (throw e))
                  (catch Exception e {:error (or (ex-message e) (str e))}))]
      (governance/settle-git-half! store proposal-id fp))))

(defn- authored-world
  [store branch]
  (or (world/authored (world/at store branch)) {}))

(defn- landing-reads
  [store schema id admin?]
  (let [p     (governance/fetch store id)
        src   (:source-branch p)
        tgt   (:target-branch p)
        known {:id id :proposal p :admin? (or admin? (grants/auth-off? store))
               :actor (policy/actor-name store)
               :source-exists? (boolean (and src (world/exists? store src)))}]
    (if-not (and p (:source-exists? known))
      known
      (let [heads   {:source-rev (:rev (world/head (world/at store src)))
                     :target-rev (:rev (world/head (world/at store tgt)))}
            _       (world/pin! store {:branch src :rev (:source-rev heads)})
            _       (world/pin! store {:branch tgt :rev (:target-rev heads)})
            rs      (read-only/of store)
            attempt (fn [what f]
                      (try {:ok (f)}
                           (catch InterruptedException e (throw e))
                           (catch Exception t {:error (str what ": " (or (ex-message t) t))})))
            repo    (future
                      (attempt "repository checks"
                               #(let [{:keys [source-tip check-status-fn] :as f}
                                      (repos/proposal-facts rs (ctx/repos) schema src tgt)]
                                  (cond-> (select-keys f [:source-tip :changed-paths
                                                          :approval-policy])
                                    (and source-tip check-status-fn)
                                    (assoc :check-rows
                                           (vec (or (check-status-fn source-tip) [])))))))
            judged  (attempt "tasks"
                             #(let [row   (world/branch rs src)
                                    check (when (:base row)
                                            (governance/staging-merge-check rs tgt src (:base row)))
                                    pv    (governance/branch-merge-preview row check nil)]
                                {:check   check
                                 :preview pv
                                 :tasks   (annotated (target-reads store) p
                                                     (governance/preview-tasks pv))}))
            unit    (attempt "unit tests"
                             #(unit-test-results store src (:tasks (:ok judged))))
            repo    @repo
            gv      (governance/governance rs)
            errors  (keep :error [judged repo unit])]
        (cond-> (merge known heads (:ok repo) (:ok judged) {:unit-tests (:ok unit)}
                       {:owner?        (owner? rs tgt)
                        :owners        (governance/owners rs src)
                        :target-policy (policy/policy-of gv tgt)
                        :locked        (actions/locked-projects
                                        store tgt (seat/branch-schema (world/at store tgt)))
                        :source-policy (policy/policy-of gv src)
                        :target-world  (authored-world store tgt)})
          (seq errors) (assoc :facts-error (str/join "; " errors)))))))

(defonce ^:private landing-queues
  (atom {}))

(defn- queue-of [target]
  (get (swap! landing-queues update (str target) #(or % (Object.))) (str target)))

(def ^:private landing-attempts 3)

(defn- moved-under? [^Throwable e]
  (= :conflict (:store/refusal (ex-data e))))

(defn- reconciled
  [store target base r]
  (if (or (:error r) (nil? target))
    r
    (try (governance/reconcile-after-commit! (ctx/at store target) actions/after-commit-callbacks
                                             base)
         r
         (catch InterruptedException e (throw e))
         (catch Exception t
           (println "[governance] landed on" target "— reconcile after the merge failed:"
                    (or (ex-message t) t))
           (assoc r :reconcile-error (or (ex-message t) (str t)))))))

(defn- land-once!
  [store schema id admin?*]
  (let [store (world/viewing store)
        reads (landing-reads store schema id admin?*)
        {:keys [refused land]} (pl/decide reads)]
    [(:target-world reads)
     (or refused
         (let [committed (pl/commit! store id (:proposal reads) land)]
           (pl/landed (governance/fetch store id) committed land)))]))

(defn- retry-landing?
  [e attempt]
  (and (moved-under? e) (< attempt landing-attempts)))

(defn- in-queue!
  [target attempt!]
  (let [queue (queue-of (or target "main"))]
    (locking queue
      (loop [attempt 1]
        (let [r (try (attempt!)
                     (catch clojure.lang.ExceptionInfo e
                       (if (retry-landing? e attempt) ::moved (throw e))))]
          (if (= ::moved r) (recur (inc attempt)) r))))))

(defn- attempted!
  [store id error]
  (try (when (governance/fetch store id) (governance/merge-attempted! store id error))
       (catch InterruptedException e (throw e))
       (catch Exception t
         (println "[governance] merge attempt not recorded:" (or (ex-message t) t)))))

(defn land!
  [store schema id admin?*]
  (let [target   (:target-branch (governance/fetch store id))
        [base r] (try (in-queue! target #(land-once! store schema id admin?*))
                      (catch Throwable t
                        (attempted! store id (or (ex-message t) (str t)))
                        (throw t)))]
    (attempted! store id (:error r))
    (reconciled store target base r)))

(defn listed
  [store schema filters]
  (let [reads  (target-reads store)
        ps     (governance/list-proposals store filters)
        ledger (repos/commit-ledger store schema (distinct (map :source-branch ps)))
        known  (repos/checks-of-ledger store ledger)
        judge  #(assoc (with-stage reads %) :checks (git-facts store schema % ledger known))]
    (into [] (comp (partition-all judging-width) (mapcat #(doall (pmap judge %)))) ps)))
