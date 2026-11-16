(ns sutra.catalog.branch-policy
  (:require [sutra.kit.refusal :as refusal]
            [sutra.foundation.side-effect :as se]
            [sutra.foundation.encoding :as enc]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.lifecycle :as lc]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.types.authority :as authority]
            [sutra.types.entities :as ents]
            [sutra.types.review :as review]))

(set! *warn-on-reflection* true)

(def default-branch-name
  se/default-branch-name)

(defn actor-name [store] (or (some-> (grants/acting-user (grants/actor store)) name) "local"))

(def policy-fields
  [{:key :protected :kind :bool :default false}
   {:key :auto-land :kind :bool :default false}
   {:key :land-pushes :kind :bool :default false}
   {:key :review :kind :rules :default nil}
   {:key :retention-days :kind :long :default 28}
   {:key :inactive-days :kind :long :default 7}
   {:key :require-checks :kind :bool :default false}
   {:key :merge-modes :kind :edn-keywords :default [:squash :merge :fast-forward]}])

(def default-policy
  (into {} (map (juxt :key :default)) policy-fields))

(defn- ->policy
  [r]
  (reduce (fn [acc {:keys [key kind default]}]
            (let [v (get r key)]
              (case kind
                :bool (cond-> acc (some? v) (assoc key (pos? (long v))))
                :long (cond-> acc (some? v) (assoc key (long v)))
                :rules (cond-> acc
                         (seq v) (assoc key (or (let [x (enc/edn-or-nil v)]
                                                  (when (coll? x) (vec x)))
                                                default)))
                :edn-keywords
                (cond-> acc
                  (seq v) (assoc key (or (let [x (enc/edn-or-nil v)]
                                           (when (coll? x) (mapv keyword x)))
                                         default))))))
          {}
          policy-fields))

(def space-prefix
  "space/")

(def ^:private space-fields [:retention-days :inactive-days])

(defn space-key [space] (str space-prefix space))

(defn effective
  [gov branch]
  (let [space (or (some-> (get-in gov [branch :space]) str not-empty) authority/default-space)]
    (merge default-policy
           (->policy (get gov "*"))
           (select-keys (->policy (get gov (space-key space))) space-fields)
           (->policy (get gov branch)))))

(defn policy-of
  [gov branch]
  (assoc (effective gov branch)
         :branch branch
         :inactive-at (some-> (get-in gov [branch :inactive-at]) str)
         :closed-at   (some-> (get-in gov [branch :closed-at]) str)
         :merged-at   (some-> (get-in gov [branch :merged-at]) str)
         :purged-at   (some-> (get-in gov [branch :purged-at]) str)
         :space       (some-> (get-in gov [branch :space]) str)))

(defn review-rules
  [policy*]
  (or (:review policy*) review/default-rules))

(defn- review-refusal
  [rules]
  (when-let [ps (when (some? rules) (seq (review/problems rules)))]
    {:error (str ":review is not a rule vector: " (str/join "; " ps)) :problems (vec ps)}))

(defn policy-change
  [opts]
  (or (review-refusal (:review opts))
      {:sets (into {}
                   (keep (fn [{:keys [key kind]}]
                           (when (contains? opts key)
                             (let [v (get opts key)]
                               [key (case kind
                                      :bool         (if v 1 0)
                                      :long         (some-> v long)
                                      :rules        (some-> v vec pr-str)
                                      :edn-keywords (pr-str (mapv keyword v)))]))))
                   policy-fields)}))

(defn container-policy-of
  [container row]
  {:container       (str container)
   :protect-all     (boolean (some-> (:protect-all row) long pos?))
   :review          (some-> (:review row) not-empty edn/read-string vec)
   :default-project (some-> (:default-project row) not-empty)})

(defn container-policy-change
  [old opts]
  (or (review-refusal (:review opts))
      (let [new (merge old (select-keys opts [:protect-all :review :default-project]))]
        {:row {:container       (:container old)
               :protect-all     (if (:protect-all new) 1 0)
               :review          (some-> (:review new) vec pr-str)
               :default-project (some-> (:default-project new) str not-empty)}})))

(defn applicable-rules
  ([branch-rules policies schema p] (applicable-rules branch-rules policies schema p true))
  ([branch-rules policies schema p protected?]
   (into (vec branch-rules)
         (when (and p protected?)
           (mapcat #(get policies (str %)) (or (authority/container-chain schema p) [p]))))))

(declare protected-row?)

(defn annotate-tasks
  [tasks {:keys [branch-rules policies schema rctx consumers protection contributors]}]
  (mapv (fn [t]
          (let [prot? (or (nil? protection) (protected-row? protection t))
                rules (applicable-rules branch-rules policies schema (:project t) prot?)]
            (cond-> (assoc t
                           :protected (boolean prot?)
                           :requires (review/requires rules t rctx))
              (and contributors (review/asks-contributors? rules))
              (assoc :contributors (vec (contributors t)))
              (= :breaking (:kind t)) (assoc :consumers (vec (get consumers (str (:rid t))))))))
        tasks))

(def purge-after-days
  7)

(defn status-of
  [{:keys [closed-at merged-at inactive-at]}]
  (cond merged-at   "merged"
        closed-at   "closed"
        inactive-at "inactive"
        :else       "active"))

(def branch-lifecycle
  (lc/table {:name   :branch
             :states #{:active :inactive :closed :merged :purged}
             :final  #{:purged}
             :edges  {:fork       {:from #{} :to :active}
                      :write      {:from #{:active :inactive} :to (fn [from _] from)}
                      :idle       {:from #{:active} :to :inactive}
                      :reactivate {:from #{:inactive} :to :active}
                      :close      {:from #{:active :inactive} :to :closed}
                      :merge      {:from #{:active :inactive} :to :merged}
                      :purge      {:from #{:closed :merged} :to :purged}}}))

(defn branch-state
  [r]
  (cond (:purged-at r) :purged
        :else          (keyword (status-of r))))

(defn ended? [b] (some? (or (:closed-at b) (:merged-at b))))

(defn ended-in
  [gov]
  (into #{} (keep (fn [[nm r]] (when (ended? r) nm))) gov))

(defn- governance-exempt?
  [b]
  (or (= (:name b) default-branch-name)
      (boolean (:protected b))
      (and (nil? (:age-days b)) (not (ended? b)))))

(defn- aged?
  [b d]
  (boolean (and d (some? (:age-days b)) (>= (long (:age-days b)) (long d)))))

(defn stale?
  [b days]
  (and (not (governance-exempt? b))
       (not (ended? b))
       (aged? b (or days (:retention-days b)))))

(defn purgeable?
  [b]
  (boolean (and (not (governance-exempt? b))
                (ended? b)
                (nil? (:purged-at b))
                (some? (:ended-days b))
                (>= (long (:ended-days b)) purge-after-days))))

(defn retention-action
  [b {:keys [days open-proposals]}]
  (let [inactive-d (:inactive-days b)
        labelled?  (some? (:inactive-at b))
        landing?   (contains? open-proposals (:name b))]
    (cond
      (governance-exempt? b)                      nil
      (and (purgeable? b) (not landing?))         :purge
      (ended? b)                                  nil
      (and (stale? b days) (not landing?))        :close
      (and (aged? b inactive-d) (not labelled?))  :mark-inactive
      (and labelled? (not (aged? b inactive-d)))  :reactivate
      :else                                       nil)))

(def owner-role "owner")

(defn owner-allowed?
  [os who admin?]
  (boolean (or admin? (contains? (set (map str os)) (str who)))))

(defn owner-refusal
  [os who branch act admin?]
  (when-not (owner-allowed? os who admin?)
    {:error  (str "only an owner of " branch " (or the admin) may " act)
     :branch branch
     :owners os}))

(def approved-verdict "approved")

(def rejected-verdict "rejected")

(def proposal-lifecycle
  (lc/table {:name   :proposal
             :states #{:open :merged :closed}
             :final  #{:merged :closed}
             :edges  {:open   {:from #{} :to :open}
                      :review {:from #{:open} :to :open}
                      :land   {:from #{:open} :to :merged}
                      :close  {:from #{:open} :to :closed}}}))

(defn proposal-state [r] (some-> (:status r) keyword))

(defn reviews
  [facts]
  (vec (for [[who fs] (group-by (comp str :approver) facts)
             :let [standing (reduce (fn [m f] (assoc m (some-> (:task f) str) f)) {} fs)
                   latest   (last fs)]]
         {:approver      who
          :task-verdicts (into {} (for [[t f] standing :when t]
                                    [t
                                     {:verdict (str (:verdict f))
                                      :fingerprint (str (:fingerprint f))}]))
          :whole         (some-> (get standing nil) :verdict str)
          :verdict       (if (some #(= rejected-verdict (str (:verdict %))) (vals standing))
                           rejected-verdict
                           (str (:verdict latest)))
          :source-rev    (:source-rev latest)
          :note          (:note latest)
          :approved-at   (str (:at latest))})))

(defn hydrated
  [reviews r]
  (when r
    (let [verdict (fn [v] (into [] (comp (filter #(= v (str (:verdict %))))
                                         (map :approver)
                                         (distinct))
                                reviews))
          aps     (verdict approved-verdict)
          rejs    (verdict rejected-verdict)]
      (-> r
          (update :created-at str)
          (assoc :reviewers (or (enc/edn-or-nil (:reviewers r)) [])
                 :review-requested-at (some-> (:review-requested-at r) str))
          (assoc :approvals aps
                 :approval-count (count aps)
                 :rejections rejs
                 :reviews reviews
                 :merge-mode (some-> (:merge-mode r) not-empty keyword))))))

(defn fresh-approvals
  [p r-a]
  (into [] (comp (filter #(and (= approved-verdict (:verdict %)) (= r-a (:source-rev %))))
                 (map :approver) (distinct))
        (:reviews p)))

(def unjudgeable-previews
  {:gone
   {:why    "its source branch no longer exists, so there is nothing left to merge from"
    :remedy "close this proposal"}
   :no-base
   {:why    (str "its source branch records no fork base, so what it would change cannot "
                 "be computed — which is not the same as it changing nothing")
    :remedy "re-fork the branch from its target, re-apply the change, and propose again"}
   :base-unreadable
   {:why    "its fork base could not be read, so what it would change is unknown"
    :remedy "re-fork the branch from its target, re-apply the change, and propose again"}})

(defn unclean?
  [status]
  (or (= :conflicts status) (contains? unjudgeable-previews status)))

(defn merge-check-block
  [status]
  (when-let [{:keys [why remedy]} (get unjudgeable-previews status)]
    {:error          (str "this proposal cannot be judged: " why)
     :remedy         remedy
     :preview-status (name status)}))

(defn merge-preview-of
  [sch]
  (let [status (:status sch)]
    {:status    (cond (seq (:conflicts sch))                 :conflicts
                      (contains? unjudgeable-previews status) status
                      (seq (:diff sch))                      :clean
                      :else                                  :empty)
     :diff      (vec (:diff sch))
     :conflicts (vec (:conflicts sch))}))

(def partial-verdict
  "partial")

(defn checks-block
  [policy* commit-sha check-status-fn]
  (when (and (:require-checks policy*) check-status-fn commit-sha)
    (let [rows    (or (check-status-fn commit-sha) [])
          pending (remove #(= :passed (:status %)) rows)]
      (cond
        (empty? rows)
        {:error (str "this target requires checks, and no check has run on " commit-sha)
         :remedy "push the branch so its checks run, then merge"}

        (seq pending)
        {:error (str "checks are not green on " commit-sha ": "
                     (str/join ", " (map (fn [{:keys [check status]}]
                                           (str check " " (name status)))
                                         pending)))
         :checks (vec rows)}))))

(defn approval-state
  [t u]
  (cond
    (= rejected-verdict (:verdict u))                 :rejected
    (not= approved-verdict (:verdict u))              nil
    (= (str (:fingerprint u)) (str (:fingerprint t))) :approved
    :else                                             :stale))

(def verdict-lifecycle
  (let [any  #{:none :approved :rejected :stale}
        may? (fn [_ {:keys [eligible? needs]}]
               (when-not eligible?
                 (str "not eligible — it needs " (or needs "an eligible reviewer"))))]
    (lc/table {:name   :verdict
               :states any
               :final  #{}
               :edges  {:approve {:from any :to :approved :guard may?}
                        :reject  {:from any :to :rejected :guard may?}
                        :change  {:from #{:approved} :to :stale}}})))

(defn verdict-state
  [t u]
  (or (approval-state t u) :none))

(defn approver-facts
  [approver who project]
  (let [f (approver who project)]
    (if (map? f)
      (update f :owner? boolean)
      {:owner? (boolean f) :editor? (boolean f)})))

(defn ontology-block
  [space tasks]
  (when-let [s (some-> space str not-empty)]
    (let [outside (filterv #(and (:ontology %)
                                 (not (contains? #{(str (:ontology %)) (str (:ontology-name %))}
                                                 s)))
                           tasks)]
      (when (seq outside)
        {:error    (str "branch is scoped to the ontology of space " s ", but changes "
                        (str/join ", " (map #(str (:task %) " (" (:ontology-name %) ")")
                                            outside))
                        " — a branch stays in one ontology; move them to a branch of their space")
         :outside-ontology (mapv #(str (:task %)) outside)}))))

(defn tasks-block
  [p tasks owner?]
  (let [reviews   (:reviews p)
        ctx       {:author (:author p) :justification (:justification p)}
        urteil    (fn [t v] (get (:task-verdicts v) (str (:task t))))
        namen     (fn [ts] (str/join ", " (map #(str (:task %)) ts)))
        klasse    (fn [pred] (filterv (fn [t] (some #(pred t %) reviews)) tasks))
        abgelehnt (klasse (fn [t v] (= :rejected (approval-state t (urteil t v)))))
        frisch    (fn [t] (for [v reviews
                                :let [u (urteil t v)]
                                :when (= :approved (approval-state t u))]
                            (assoc (approver-facts owner? (:approver v) (:project t))
                                   :who (:approver v))))
        offen     (into [] (keep (fn [t] (when-let [rs (seq (review/unmet t (frisch t) ctx))]
                                           [t rs])))
                        tasks)]
    (cond
      (seq abgelehnt)
      {:error (str "rejected task(s): " (namen abgelehnt)
                   " — a rejection from any reviewer blocks the merge")
       :rejected-tasks (mapv #(str (:task %)) abgelehnt)}

      (seq offen)
      (let [veraltet     (set (klasse (fn [t v] (= :stale (approval-state t (urteil t v))))))
            nur-veraltet (filterv veraltet (map first offen))]
        {:error (str "still open: "
                     (str/join "; " (for [[t rs] offen]
                                      (str (:task t) " needs "
                                           (str/join " and " (map review/describe rs)))))
                     (when (seq nur-veraltet)
                       (str " — the approval on " (namen nur-veraltet)
                            " is stale: the resource changed after it was approved")))
         :open-tasks  (mapv (comp str :task first) offen)
         :stale-tasks (mapv #(str (:task %)) nur-veraltet)}))))

(defn stage-of
  [p tasks owner?]
  (cond
    (empty? (tasks-block p tasks owner?))           "merge"
    (nil? (:review-requested-at p))                 "preparing"
    :else                                           "in-review"))

(defn reviewers-block
  [required approvals]
  (let [missing (remove (set (map str approvals)) (map str (distinct required)))]
    (when (seq missing)
      {:error (str "still needs approval from " (str/join ", " missing))
       :missing-reviewers (vec missing)})))

(defn merge-mode-block
  [policy* mode]
  (let [allowed (set (:merge-modes policy*))
        m       (some-> mode keyword)]
    (when (and m (not (contains? allowed m)))
      {:error (str "merge mode " m " is not permitted on this target (allowed: "
                   (str/join ", " (sort (map str allowed))) ")")
       :allowed (vec (sort allowed))})))

(defn- path-pattern
  [pat]
  (try (re-pattern (str pat)) (catch java.util.regex.PatternSyntaxException _ nil)))

(defn approval-policy-reviewers
  [policy changed-paths]
  (into []
        (comp (filter (fn [{:keys [paths]}]
                        (some (fn [pat]
                                (when-let [re (path-pattern pat)]
                                  (some #(re-find re (str %)) changed-paths)))
                              paths)))
              (mapcat :reviewers)
              (distinct))
        policy))

(defn file-plane-status
  ([fp] (file-plane-status fp false))
  ([fp owed?]
   (cond
     (and (empty? fp) owed?)                   :pending
     (empty? fp)                                :none
     (:error fp)                                :error
     (some #(seq (:conflicts %)) (vals fp))     :conflicts
     :else                                      :ok)))

(defn git-half-failed?
  [fp]
  (boolean (or (string? (:error fp))
               (some (fn [r]
                       (and (map? r)
                            (or (string? (:error r)) (string? (get-in r [:riders :error])))))
                     (vals fp)))))

(defn git-half-conflicted?
  [fp]
  (boolean (some (fn [r] (and (map? r) (seq (:conflicts r)))) (vals fp))))

(defn git-half-outcome
  [fp]
  (cond
    (git-half-conflicted? fp)   :unresolved
    (git-half-failed? fp)       :retry
    :else                       :done))

(defn git-half-reason
  [fp]
  (or (when (string? (:error fp)) (:error fp))
      (let [named (for [[repo r] fp
                        :when (and (map? r) (seq (:conflicts r)))]
                    (str (name repo) ": " (str/join ", " (map str (:conflicts r)))))]
        (when (seq named) (str "not carried — " (str/join "; " named))))
      (some (fn [r] (when (map? r) (or (:error r) (get-in r [:riders :error])))) (vals fp))
      (pr-str fp)))

(defn git-half-backoff-ms
  [attempts]
  (let [n (long (or attempts 1))]
    (if (<= n 1)
      0
      (min 3600000 (* 60000 (bit-shift-left 1 (min 16 (- n 2))))))))

(defn landing-block
  [p
   {:keys [policy tasks facts-error owner? fresh path-reviewers source-tip check-status-fn]
    :as facts}]
  (or (when (or facts-error (not (contains? facts :tasks)) (nil? owner?) (nil? (:source-rev facts)))
        {:error (str "this proposal cannot be judged — refusing to land it unjudged"
                     (when facts-error (str ": " facts-error)))})
      (when-let [wer (seq (keep #(when (= rejected-verdict (:whole %)) (:approver %))
                                (:reviews p)))]
        {:error (str "rejected by " (str/join ", " wer)
                     " — a rejection blocks until its author approves")
         :rejections (vec wer)})
      (checks-block policy source-tip check-status-fn)
      (tasks-block p tasks owner?)
      (reviewers-block (or path-reviewers []) fresh)
      (merge-mode-block policy (:merge-mode p))))

(def kompass-file-classes
  #{"graph"})

(defn kompass-file-batch?
  [staged]
  (boolean (and (seq staged)
                (every? #(contains? kompass-file-classes (str (:type %))) staged))))

(defn- coded-block
  [code data params]
  (cond-> (assoc data :error (refusal/message code (merge data params)) :refusal/code code)
    (seq params) (assoc :refusal/params params)))

(defn write-block
  ([policy branch] (write-block policy branch nil))
  ([policy branch staged]
   (cond
     (and branch (not (lc/allowed? branch-lifecycle :write (branch-state policy))))
     (assoc (coded-block :refusal.catalog/branch-closed {:branch branch}
                         {:state (name (branch-state policy))})
            ::anomaly/category :conflict
            :remedy "fork a new branch for further changes")

     (and branch (:protected policy)
          (not (kompass-file-batch? staged)))
     (assoc (coded-block :refusal.catalog/branch-protected {:branch branch} nil)
            :remedy (str "branch off it, make the change there, then: propose --target " branch)))))

(defn instance-block
  [branch unrestricted?]
  (when-not unrestricted?
    (assoc (coded-block :refusal.catalog/instance-wide {:branch branch} nil)
           ::anomaly/category :forbidden
           :remedy "ask the admin; a proposal cannot carry it")))

(defn review-notice
  [p tasks open holders]
  (when-let [rows (seq (for [t tasks :when (contains? (set open) (str (:task t)))]
                         {:task      (str (:task t))
                          :name      (:name t)
                          :kind      (some-> (:kind t) name)
                          :owners    (vec (holders (some-> (:project t) str)))
                          :consumers (vec (:consumers t))}))]
    {:channel "review-requested"
     :payload {:proposal (:id p) :title (:title p) :author (:author p) :tasks (vec rows)}
     :actor   (str (:author p))}))

(defn protected-row?
  [{:keys [resources projects released]}
   {:keys [rid project expands-access security-approvals status]}]
  (boolean (or expands-access
               security-approvals
               (contains? resources (str rid))
               (and project
                    (contains? projects (str project))
                    (not (contains? released (str rid)))
                    (not= "added" status)))))

(defn- instance-row?
  [{:keys [section instance]}]
  (boolean (or instance (some-> section keyword ents/instance-section?))))

(defn protection-block
  [protection branch touched]
  (when (= default-branch-name (str branch))
    (when-let [hit (seq (filter #(and (not (instance-row? %)) (protected-row? protection %))
                                touched))]
      (assoc (coded-block :refusal.catalog/resource-protected {:branch branch}
                          {:names (str/join ", " (map #(or (:name %) (:rid %)) hit))})
             ::anomaly/category :forbidden
             :protected (mapv #(select-keys % [:task :rid :name :project]) hit)
             :remedy (str "branch off " branch ", make the change there, then: propose --target "
                          branch)))))

(defn request-gate
  ([branch] (request-gate branch #{}))
  ([branch allowed] {:branch branch :allowed (set allowed)}))

(defn held-back
  [effects]
  (let [by (group-by :verdict effects)]
    (cond-> {}
      (seq (by :suppress)) (assoc :suppressed-side-effects (vec (by :suppress)))
      (seq (by :defer))    (assoc :deferred-side-effects (vec (by :defer))))))
