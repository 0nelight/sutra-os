(ns sutra.actions.staged
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.grants :as grants]
            [sutra.kit.lifecycle :as lc]
            [sutra.ref.install :as ri]
            [sutra.store.access.run-log :as run-log]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.world-gates :as world-gates]
            [sutra.catalog.actions :as actions]
            [sutra.store.kept :as kept]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.signature :as signature]
            [sutra.store.control :as ctl]
            [sutra.actions.staged-rows :as staged]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.store.tx-vector :as tv]
            [sutra.store.world :as world]
            [sutra.actions.action :as action]
            [sutra.catalog.backing-checks :as backing]
            [sutra.governance.api :as governance]
            [sutra.store.edge :as edge]
            [sutra.actions.operations :as ops]
            [sutra.actions.request :as request]
            [sutra.store.seat :as seat]
            [sutra.types.schema :as sch]
            [sutra.actions.staging :as stg]))

(set! *warn-on-reflection* true)

(defn- staged-seq
  [r]
  (some #(when (= :ctl/staged-actions (:coll %)) (:key %)) (:records r)))

(defn- admitted
  [{:keys [authority branch store provenance edge]} actions]
  (run! action/refuse-sys! actions)
  (let [user  (grants/current-user-id authority)
        b     (stg/staging-branch-of branch user)
        seen  (:staging edge)
        live? (if (= b (:branch seen)) (:live? seen) (world/exists? store b))]
    (stg/decide! (assoc store :branch branch :authority authority) :stage nil
                 (if live? :staged :empty))
    {:staging b :parent branch :live? live? :provenance (merge {:author user} provenance)}))

(defn- on-staging
  [req {:keys [staging parent live? provenance]}]
  (assoc req :branch (if live? staging parent) :staging staging :live? live? :parent parent
         :effect-gate (policy/request-gate staging)
         :store (ctx/at (:store req) (if live? staging parent)) :provenance provenance))

(defn- held-run
  [req]
  (fn [schema action args extra] (action/run-held (assoc req :schema schema) action args extra)))

(defn plan-requests
  [run provenance start calls]
  (reduce (fn [{:keys [unit installed] :as acc} {:keys [action args] :as call}]
            (let [W0 (or installed start)
                  r  (run W0 action args {:installed installed :prior (:ops unit)})]
              (-> acc
                  (assoc :installed (:installed r))
                  (update :unit action/add-unit (:commit-opts r))
                  (update :rows conj
                          {:action action :args args :tx-vector (seq (:tx-vector r))
                           :provenance (or (:provenance call) provenance)
                           :delta (kept/row-delta W0 (or (:installed r) W0))}))))
          {:rows [] :unit {:ops [] :log [] :records []} :installed nil}
          calls))

(defn plan-stage
  ([req calls] (plan-stage req calls nil))
  ([req calls {:keys [rows? start] :or {rows? true}}]
   (let [{:keys [store staging] :as sreq} (on-staging req (admitted req (map :action calls)))
         authored (world/authored store)
         {:keys [rows unit installed]} (plan-requests (held-run sreq) (:provenance sreq)
                                                      (or start (stg/staging-schema store))
                                                      calls)
         installed (or installed start)
         records (when rows?
                   (for [{:keys [action args tx-vector provenance delta]} rows]
                     (staged/staged-entry store staging action args tx-vector provenance
                                          delta)))]
     (assoc (select-keys sreq [:staging :live? :parent :store])
            :rows      rows
            :installed installed
            :before    (when installed authored)
            :unit      (signature/installing
                        (-> unit
                            (update :records into records)
                            (assoc :actor (grants/principal store)))
                        installed)))))

(defn planned-schema
  [{:keys [installed store]}]
  (if installed (sysds/as-staging-schema installed) (stg/staging-schema store)))

(defn planned-rows
  [{:keys [rows store] :as plan}]
  (->> (map-indexed (fn [i r] (assoc r :seq (- -1 i))) rows)
       (stg/enrich-rows store (planned-schema plan))
       (mapv #(assoc % :seq nil))))

(defn judge-stage!
  [{:keys [unit before store] :as plan}]
  (action/judge-end-world! unit before store)
  plan)

(defn- land-stage!
  [store {:keys [staging live? parent unit before]}]
  (when-not live? (stg/fork-staging! store parent staging))
  (anomaly/unwrap! (action/save! (ctx/at store staging) unit before)))

(defn stage-requests!
  ([req calls] (stage-requests! req calls nil))
  ([req calls opts]
   (let [plan (judge-stage! (plan-stage req calls opts))
         r    (land-stage! (:store req) plan)
         seqs (keep (fn [x] (when (= :ctl/staged-actions (:coll x)) (:key x))) (:records r))]
     (mapv (fn [row s] {:seq s :tx-vector (:tx-vector row)})
           (:rows plan) (concat seqs (repeat nil))))))

(defn stage-action!
  [{:keys [action args store] :as req}]
  (let [{b :staging :keys [live? parent provenance]} (admitted req [action])
        _              (when-not live? (stg/fork-staging! store parent b))
        branch-store   (ctx/at store b)
        schema         (seat/branch-schema branch-store)
        staging-schema (stg/staging-schema branch-store)
        req            (assoc req :branch b :store branch-store :schema schema
                              :provenance provenance)]
    (anomaly/then
     (action/settled-run
      req
      (action/action-policy schema action)
      (fn [run]
        (let [[ctx io] (action/run-for (assoc req :schema staging-schema) run :as :stage)]
          (action/eval-action ctx io action {:arg0 args})))
      {:recheck    #(ops/check-submission! (:schema %1) %1 %2 action {:arg0 args})
       :records-fn (fn [txv installed]
                     [(staged/staged-entry branch-store b action args (seq txv) provenance
                                           (kept/row-delta staging-schema
                                                           (or installed staging-schema)))])})
     (fn [r]
       {:seq (staged-seq (:commit r)) :tx-vector (seq (:tx-vector r))}))))

(defn stage-actions!
  ([store calls] (stage-actions! store calls nil))
  ([store calls asked] (stage-actions! store calls asked nil))
  ([store calls asked opts]
   (stage-requests! (request/request-on store (merge {:branch (:branch store) :schema nil} asked))
                    calls opts)))

(defn plan-actions
  [store calls asked]
  (plan-stage (request/request-on store (merge {:branch (:branch store) :schema nil} asked))
              calls))

(defn- rebuild-preview!
  [store staging rows]
  (when (world/exists? store staging)
    (world/retire! store staging))
  (when (seq rows)
    (stage-requests! (request/request-on store {:branch (:branch store) :schema nil})
                     (mapv (fn [{:keys [action args author origin origin-ref]}]
                             {:action action :args args
                              :provenance {:author author :origin origin :origin-ref origin-ref}})
                           rows)
                     {:rows? false})))

(defn- ensure-preview!
  [store {:keys [staging live?] :as v}]
  (let [rows (stg/staged-rows store staging)]
    (assoc (if (and staging (not live?) (seq rows))
             (do (rebuild-preview! store staging rows) (stg/staging-view store))
             v)
           :rows rows)))

(defn- replay-calls [rows]
  (mapv (fn [{:keys [action args author origin origin-ref]}]
          {:action action :args args
           :provenance {:author author :origin origin :origin-ref origin-ref}})
        rows))

(defn- marked
  [{:keys [tx-vector] :as r}]
  (assoc r
         :data?    (boolean (some tv/store-write? tx-vector))
         :effects? (boolean (some #(not (or (tv/event? %) (tv/store-op? %))) tx-vector))))

(defn- rebase-plan
  [store staging rows i base]
  (let [W (stg/staging-schema store)
        p (kept/held W)]
    (if-not p
      {:form :rebuild :why :not-held}
      (let [moved (try (kept/moved store (:branch store) staging base)
                       (catch clojure.lang.ExceptionInfo e
                         (if-let [why (:precondition (ex-data e))]
                           why
                           (throw e))))]
        (if (keyword? moved)
          {:form :rebuild :why moved}
          (assoc (kept/rebasing p moved (mapv marked rows) i) :from W))))))

(defn- discard-plan
  [store staging rows i]
  (let [base (get-in (world/branch store staging) [:base :rev])
        head (:rev (world/head (world/at store (:branch store))))]
    (if (not= base head)
      (rebase-plan store staging rows i base)
      (let [W (stg/staging-schema (ctx/at store staging))
            d (kept/held W)]
        (if-not d
          {:form :rebuild :why :not-held}
          (assoc (kept/discarding d (mapv marked rows) i) :from W))))))

(defn- restaged-plan
  [store {:keys [db replay from]}]
  (plan-stage (request/request-on store {:branch (:branch store) :schema nil})
              (replay-calls replay)
              {:rows? false :start (sch/indexed (kept/world-of db from))}))

(defn- redelta!
  [store staging rows planned]
  (when (seq rows)
    (ctl/commit! store (for [[{s :seq} {:keys [delta]}] (map vector rows planned)]
                         [:update :staged-actions {:branch staging :seq s} {:delta delta}]))))

(defn- refused-as-replayed?
  [store {:keys [installed]}]
  (not (:ok (world-gates/verdict (kept/held (stg/staging-schema store)) (kept/held installed)))))

(defn- land-discard!
  [store staging keep {:keys [form replay] :as p}]
  (when (#{:rebuild :rebase} form)
    (when (world/exists? store staging) (world/retire! store staging)))
  (if (= :rebuild form)
    (when-let [plan (when (seq keep)
                      (judge-stage! (plan-stage (request/request-on store
                                                                    {:branch (:branch store)
                                                                     :schema nil})
                                                (replay-calls keep) {:rows? false})))]
      (land-stage! store plan)
      (redelta! store staging keep (:rows plan)))
    (when-not (and (= :rebase form) (empty? keep))
      (let [plan (restaged-plan store p)]
        (if (refused-as-replayed? store plan)
          (land-discard! store staging keep {:form :rebuild})
          (do (land-stage! store plan)
              (redelta! store staging replay (:rows plan))))))))

(defn discard-staged!
  [store {:keys [staging live?]} seq]
  (if-not live?
    0
    (let [old  (stg/staged-rows store staging)
          i    (first (keep-indexed (fn [i r] (when (= seq (:seq r)) i)) old))
          keep (vec (remove (fn [r] (= seq (:seq r))) old))]
      (if-not i
        0
        (do (stg/decide! store :unstage {:left (count keep)} :staged)
            (let [p (discard-plan store staging old i)]
              (ctl/delete! store :staged-actions {:branch staging :seq seq})
              (land-discard! store staging keep p))
            1)))))

(defn- applied-rows
  [store b full staged-rows]
  (let [store-b (ctx/at store b)]
    (reduce
     (fn [acc [i {:keys [seq action args tx-vector]}]]
       (cond
         (contains? actions/schema-actions action)
         (-> acc
             (update :ops into (filter tv/store-write?) tx-vector)
             (update :log into
                     (run-log/effect-entries (remove tv/store-op? tx-vector)
                                             (action/effect-opts
                                              {:actor     (grants/acting-for store)
                                               :authority (grants/request-authority store)}))))

         :else
         (let [r (try
                   (action/run-held (request/request-on store-b {:action action :args args
                                                                 :branch b :schema full})
                                    action args {:installed full :prior (:ops acc)})
                   (catch clojure.lang.ExceptionInfo e
                     (refusal/refuse! :refusal.actions/staged-action-stale
                                      {:refused [{:index i :seq seq :action action
                                                  :reason (ex-message e) :data (ex-data e)}]}
                                      {:n (inc i) :action-name (name action) :branch b
                                       :why (ex-message e)}
                                      e)))]
           (action/add-unit acc (:commit-opts r)))))
     {:ops [] :log [] :records []}
     (map-indexed vector staged-rows))))

(defn- judge-merged!
  [v]
  (when-not (:ok v)
    (governance/refuse-invalid!
     (cond-> v
       (not (contains? v :error))
       (assoc :error (or (:reasons v) ["schema failed validation without an explanation"]))))))

(defn- merge-plan
  [store {:keys [parent staging rows old check]}]
  (governance/refuse-unclean! check parent staging)
  (let [{:keys [heads]} check
        merged  (governance/heads-merge heads {})
        full    (ri/ensure-actions (governance/rebase-schema merged))
        _       (judge-merged! (governance/judged merged))
        _       (backing/validate-external-sources! full)
        applied (applied-rows store parent full rows)]
    (action/guard-tx-vector! (:ops applied))
    (governance/plan {:parent       parent
                      :old          (or old {})
                      :full         full
                      :staged-rows  rows
                      :applied      applied
                      :retired      (when (seq rows)
                                      (:records
                                       (ctl/deleting store :staged-actions
                                                     {:branch staging
                                                      :seq    (into #{} (map :seq) rows)})))
                      :actor        (grants/acting-for store)})))

(defn- land!
  [store parent tx summary schema run-fn]
  (run-fn (ctx/at store parent)
          {:id (random-uuid) :in-commit? true :kind :publish :inputs summary :schema schema}
          (fn [pst _ run-entry]
            (anomaly/unwrap! (action/try-save! pst (-> tx
                                                       (update :records conj (run-entry summary))
                                                       (assoc :at-head? true))))
            {:summary summary :run-committed? true})))

(defn- after-commit!
  [pst callbacks old tx]
  (let [seen (governance/after-commit-reads pst)]
    (when (:world tx) (action/sweep-graphs! pst old (:head seen)))
    (governance/reconcile-after-commit! pst callbacks old seen)))

(defn- landed [rows run]
  {:status      :ok
   :committed   (count rows)
   :results     (mapv (fn [r] {:action (:action r)}) rows)
   :run-id      (:run-id run)
   :snapshot-id (:snapshot-id run)})

(defn- summary-of [rows]
  {:staged-count (count rows) :actions (mapv :action rows)})

(defn- commit-reads
  [store seen]
  (let [{:keys [parent staging live? base] :as v}
        (ensure-preview! store (if (:staging seen)
                                 (edge/staging-view seen)
                                 (stg/staging-view store)))]
    (if-not (and parent live?)
      v
      (let [check (governance/staging-merge-check store parent staging base)]
        (assoc v
               :check       check
               :old         (governance/authored-target check)
               :staging-rev (:rev (world/basis (world/at store staging))))))))

(defn commit-staged!
  [store seen {:keys [run-fn guard] :as callbacks}]
  (let [{:keys [parent staging live? rows check old staging-rev] :as reads}
        (commit-reads store seen)
        state (if live? :staged :empty)]
    (cond
      (nil? parent)
      (refusal/refuse! :refusal.actions/commit-staged-no-branch {})

      (not (lc/allowed? stg/lifecycle :commit state))
      {:status :clean :committed 0 :results []}

      :else
      (let [_       (stg/decide! store :commit nil state)
            preview (governance/preview-of check)
            store   (cond-> store
                      guard (guard (:diff preview) (delay (stg/enriched-rows store reads rows))))]
        (if (policy/unclean? (:status preview))
          (assoc preview :committed 0 :results [])
          (let [plan (merge-plan store reads)
                tx   (assoc (:tx plan)
                            :prior   {:authored old}
                            :retires [{:branch staging :expect-rev staging-rev}])
                run  (land! store parent tx (summary-of rows)
                            (sysds/readable (:parent-world check)) run-fn)]
            (after-commit! (ctx/at store parent) callbacks old tx)
            (landed rows run)))))))

(defn plan-publish
  [{:keys [store branch] :as req} calls]
  (run! action/refuse-sys! (map :action calls))
  (let [branch-store  (ctx/at store branch)
        old  (or (world/authored branch-store) {})
        {:keys [rows unit installed]}
        (plan-requests (held-run (request/request-on
                                  branch-store (assoc (select-keys req [:provenance])
                                                      :branch branch :schema nil)))
                       (:provenance req) (sysds/as-staging-schema old) calls)
        full (some-> installed ri/ensure-actions)]
    (when full
      (judge-merged! (kept/verdict (kept/db branch-store branch) full old))
      (backing/validate-external-sources! full))
    (action/guard-tx-vector! (:ops unit))
    (assoc (governance/plan {:parent branch :old old :full full :staged-rows [] :applied unit
                             :actor (grants/acting-for store)})
           :rows rows)))

(defn publish-requests!
  [{:keys [store branch] :as req} calls {:keys [run-fn guard] :as callbacks}]
  (let [{:keys [old full rows tx]} (plan-publish req calls)
        branch-store (ctx/at store branch)
        store  (cond-> store
                 guard (guard (governance/touched-rows old (or full old))
                              (delay (->> (map-indexed (fn [i r] (assoc r :seq (- -1 i))) rows)
                                          (stg/enrich-rows branch-store (sysds/as-staging-schema
                                                                         (or full old)))))))
        run (land! store branch tx (summary-of rows) nil run-fn)]
    (after-commit! (ctx/at store branch) callbacks old tx)
    (landed rows run)))
