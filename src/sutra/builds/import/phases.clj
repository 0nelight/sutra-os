(ns sutra.builds.import.phases
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.effects :as effects]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.control :as ctl]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.changes :as changes]
            [sutra.catalog.lookup :as lookup]
            [sutra.projects.api :as projects]
            [sutra.foundation.encoding :as enc]
            [sutra.builds.ports :as ports]
            [sutra.builds.plan.dag :as dag]
            [sutra.builds.import.changelog :as changelog]
            [sutra.builds.import.nodes :as import-nodes]
            [sutra.builds.import.reconcile :as reconcile]
            [sutra.builds.ledger :as ledger]
            [sutra.builds.ledger.jobs :as jl]
            [sutra.builds.run :as runs]
            [sutra.types.naming :as naming]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn- submit-partitioned!
  [store schema decls rid->name cancel-check]
  (let [by-group (group-by changes/publish-group decls)
        _        (when-let [rows (get by-group nil)]
                   (anomaly/raise! :fault "not a publish decl on the import write path"
                                   {:decl (first rows)}))
        rank     (zipmap changes/group-order (range))
        groups   (sort-by (fn [[{:keys [kind verb]} _]]
                            (rank [kind verb] (count changes/group-order)))
                          by-group)]
    (reduce (fn [acc [{:keys [kind verb type] :as group} rows]]
              (try
                (update acc :interpret-ms +
                        (:interpret-ms
                         (ports/publish-index! store schema (vec rows) group cancel-check)))
                (catch InterruptedException t (throw t))
                (catch Exception t
                  (when (effects/cancellation t) (throw t))
                  (update acc :failures conj
                          {:kind kind :verb verb :type type :count (count rows)
                           :fn (keyword (str (name verb) "-"
                                             (name (rid/resolve-rid->name rid->name type))))
                           :error (runs/describe-throwable schema t)}))))
            {:failures [] :interpret-ms 0}
            groups)))

(defn- expand-import
  [{:keys [schema] :as env} import state0]
  (when-not import
    (refusal/refuse! :refusal.builds/no-import-block {}))
  (let [state (dag/run-dag! env (import-nodes/nodes-of schema import) state0)
        {:keys [obj-decls link-decls pk-index rows conn-pairs restriction-failures withheld
                landed]}
        (import-nodes/without-failed-types schema (:acc state) (:failures state))]
    (cond-> {:noted      (:noted state)
             :decls      (into (import-nodes/coalesce-object-decls obj-decls) link-decls)
             :withheld   withheld
             :pk-index   pk-index
             :rows       rows
             :landed     landed
             :conn-pairs conn-pairs}
      (seq restriction-failures) (assoc :restriction-failures restriction-failures)
      (seq (:failures state))    (assoc :job-failure (dag/job-failure (:failures state))))))

(defn- media->blob-decls
  [schema media]
  (when-let [blob-rid (lookup/rid-of schema :object-types :blob)]
    (let [seen (volatile! #{})]
      (into []
            (keep (fn [{:keys [hash mime size]}]
                    (when-not (@seen hash)
                      (vswap! seen conj hash)
                      (changes/create-object (enc/pk-uuid :blob hash) blob-rid
                                             {:pk hash :hash hash :mime mime :size size}))))
            media))))

(defn- dag-env
  [env]
  (select-keys env
               [:store :build-branch :schema :datasources :options :run-id :code-env :pa
                :row-cache :work]))

(defn- producers-phase
  [{:keys [schema node-scope] :as env} state]
  (let [r (dag/run-dag! (dag-env env) (dag/scoped-nodes schema node-scope)
                        (select-keys state [:noted]))]
    {:state (assoc state
                   :noted        (:noted r)
                   :stage-writes (:facts r)
                   :carried      (select-keys r (keys dag/no-job-failures)))}))

(defn- minted
  [decls]
  (let [creates (fn [kind] (count (filter #(and (= kind (:kind %)) (= :create (:verb %))) decls)))]
    {:objects (creates :objects) :links (creates :links)}))

(defn- computed
  [schema state {:keys [noted decls pk-index rows conn-pairs restriction-failures job-failure
                        withheld landed]} media]
  (let [decls     (into (vec decls) (media->blob-decls schema media))
        rid->name (sch/rid-name-index schema)]
    (cond-> (assoc state
                   :noted          noted
                   :decls          decls
                   :withheld       withheld
                   :rows           rows
                   :landed         landed
                   :conn-pairs     conn-pairs
                   :rid->name      rid->name
                   :pk-index       pk-index
                   :pk-index-named (into {} (for [[r m] pk-index]
                                              [(rid/resolve-rid->name rid->name r) m]))
                   :counts (minted decls))
      (seq restriction-failures) (update :publish-failures into restriction-failures)
      job-failure                (assoc :job-failure job-failure))))

(defn- compute-phase
  [{:keys [schema plan media-sink] :as env} state]
  {:state (computed schema state
                    (expand-import (dag-env env) (:import plan)
                                   (assoc (:carried state) :noted (:noted state)))
                    @media-sink)})

(defn- indexed-backing
  [schema ot]
  (let [{:keys [dataset authored?]} (backing/object-backing schema ot)
        tbl (some-> dataset naming/dataset-table)]
    (when (and tbl (not authored?) (not= tbl (naming/obj-table ot))) tbl)))

(defn- changes-since
  [store schema rows v0 full-refresh?]
  (when (and (seq rows) (not full-refresh?))
    (let [v1 (dsets/current-version store)]
      (reduce
       (fn [acc [ot obj-rows]]
         (let [tbl       (indexed-backing schema ot)
               signature (some->> tbl (dsets/content-signature store))
               delta     (when tbl
                           (try (dsets/window-delta store tbl v0 v1)
                                (catch clojure.lang.ExceptionInfo _ nil)))]
           (cond-> acc
             delta (-> (assoc-in [:changed ot]
                                 (into (changelog/unindexed-ids
                                        store ot (:dataset (backing/object-backing schema ot))
                                        obj-rows)
                                       (map str)
                                       (:touched delta)))
                       (assoc-in [:removals ot] {:signature signature
                                                 :removed   (:removed delta)})))))
       {:changed {} :removals {}}
       rows))))

(defn- publish-plan
  [{:keys [decls pk-index]} changed]
  {:decls       (changelog/publish-changes decls changed)
   :import-keys (changelog/changed-keys pk-index changed)})

(defn- unpublished-after
  [u fails]
  (reduce (fn [u {:keys [kind verb count]}]
            (cond-> u (= :create verb) (update kind + count)))
          u fails))

(defn- published
  [schema build-low {:keys [pk-index] :as state} decls written fails interp-ms]
  (let [failed-types (into #{} (comp (filter #(= :objects (:kind %))) (map :type)) fails)
        failed-ds    (into #{} (keep #(:dataset (backing/object-backing schema %))) failed-types)
        state        (assoc state
                            :counts (minted decls)
                            :object-writes
                            (into [] (remove (comp failed-ds :dataset-rid)) written))]
    {:state    (cond-> state
                 (seq fails) (-> (update :publish-failures into fails)
                                 (update :unpublished unpublished-after fails)))
     :effects  [{:io :retract-outputs :v0 build-low :datasets (seq failed-ds)}
                {:io :index-times :types (remove failed-types (keys pk-index))}]
     :observed (into [{:event :step :phase :publish/interpret :ms interp-ms}]
                     (for [{:keys [fn count error]} fails]
                       {:event :publish/failed :fn fn :count count :error error}))}))

(defn- publish-phase
  [{:keys [store schema sub cancel-at run-id options build-low]}
   {:keys [rows landed noted] :as state}]
  (let [written (sub :publish/backing
                     (fn []
                       (reconcile/write-object-tables! store schema run-id (:snapshot-run noted)
                                                       landed rows)))
        {:keys [changed removals]}
        (sub :publish/changes
             (fn [] (changes-since store schema rows build-low (:full-refresh options))))
        {:keys [decls import-keys]} (publish-plan state changed)
        _       (sub :publish/import-keys (fn [] (projects/write-import-keys! store import-keys)))
        cancel-check (cancel-at :publish)
        {fails :failures interp-ms :interpret-ms}
        (sub :publish/submit
             (fn [] (submit-partitioned! store schema decls (:rid->name state) cancel-check)))]
    (published schema build-low (assoc state :known-removals removals)
               decls written fails interp-ms)))

(defn- materialize-phase
  [{:keys [store schema build-branch build-low sub cancel-at run-id]}
   {:keys [rows conn-pairs noted job-failure object-writes known-removals] :as state}]
  (let [cancel-check (cancel-at :materialize-tables)
        w      (into (vec object-writes)
                     (sub :materialize/transform-tables
                          (fn [] (reconcile/write-link-tables! store schema run-id conn-pairs))))
        _      (cancel-check)
        swept  (sub :materialize/index-reconcile
                    (fn []
                      (into
                       (reconcile/reconcile-object-tables! store build-branch schema rows build-low
                                                           (:snapshot-run noted) known-removals)
                       (reconcile/reconcile-link-tables!
                        store build-branch schema conn-pairs build-low))))]
    (cancel-check)
    {:state   (assoc state :backing-writes w :index-report swept)
     :effects (concat (jl/terminal-job-effects
                       noted run-id build-branch
                       (remove (comp (or (:failed-t-kws (ex-data job-failure)) #{})
                                     :transform-rid)
                               w)
                               (ctl/stamp store))
                      [(ledger/skip-unstarted-jobs run-id
                                                   (ctl/stamp store))])}))

(def phases
  [{:phase :producers          :run producers-phase}
   {:phase :compute            :run compute-phase}
   {:phase :publish            :run publish-phase}
   {:phase :materialize-tables :run materialize-phase}])
