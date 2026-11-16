(ns sutra.builds.plan.dag
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.attempt :as attempt]
            [sutra.foundation.profile.scope :as profile-scope]
            [sutra.foundation.settings :as settings]
            [sutra.sources.api :as sources]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.grants :as grants]
            [sutra.store.access.scope :as scope]
            [sutra.kit.effects :as effects]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.transforms :as transforms]
            [clojure.string :as str]
            [sutra.builds.ledger.facts :as facts]
            [sutra.builds.plan.freshness :as fresh]
            [sutra.builds.plan.expectations :as expect]
            [sutra.builds.run.exports :as exports]
            [sutra.builds.run.media-sync :as media-sync]
            [sutra.builds.import.nodes :as import-nodes]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.catalog.lookup :as lookup]
            [sutra.builds.ledger.jobs :as jl]
            [sutra.store.control :as ctl]
            [sutra.builds.plan :as plan]
            [sutra.builds.plan.producers :as producers]
            [sutra.kit.err :as err]
            [sutra.builds.run.monitor :as bm]
            [sutra.builds.ledger :as ledger]
            [sutra.types.languages :as langs]
            [sutra.types.link-side :as link-side]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn- job-status [store run-id jr]
  (some #(when (= (str jr) (str (or (:job_rid %) (:job-rid %)))) (:status %))
        (ledger/jobs-of-run store run-id)))

(defn- stamped
  [r jr commit]
  (update r :facts (fn [fs] (mapv #(assoc % :job-rid jr :snapshot-id commit) fs))))

(defn- consumed-cursors
  [notes]
  (keep (fn [[k _ v]] (when (= :consumed-cursors k) v)) notes))

(defn- attempt-end
  [schema build-branch t-kw jr v0 {:keys [r e commit]} notes noted at]
  (if e
    {:retract (when v0 (ledger/declared-outputs schema t-kw))}
    (let [{:keys [facts rows-out]} r
          consumed (consumed-cursors notes)
          finish (merge (facts/job-facts noted t-kw at)
                        {:job-rid     jr
                         :outputs     (map :dataset-rid facts)
                         :snapshot-id commit
                         :rows-out    (or rows-out (reduce + 0 (keep :written facts)))}
                        (when v0 {:consumed-cursors consumed}))
          snapshot? (some (fn [[k _ v]] (and (= :snapshot-run k) (= t-kw v))) notes)]
      {:ledger              (filterv some?
                                     [(ledger/finish-job finish)
                                      (when v0
                                        (ledger/advance-on-complete jr build-branch consumed))
                                      (when snapshot?
                                        (ledger/snapshot-served jr build-branch t-kw))])
       :retract-unless-done (when v0 (keep :dataset-rid facts))})))

(defn- input-sigs-notes
  [store schema noted t-kw]
  (let [t0 (System/nanoTime)]
    (when-let [note (fresh/input-sigs-note store schema noted t-kw)]
      [note [:job-spans t-kw [{:name "input-sigs" :at-ms 0
                               :ms (quot (- (System/nanoTime) t0) 1000000)}]]])))

(defn- with-job!
  [{:keys [store build-branch schema run-id snapshot-requested]} noted t-kw attempt run]
  (let [jr    (when t-kw (ledger/job-rid run-id t-kw))
        v0    (when (and jr (:datasets store)) (dsets/current-version store))
        notes (atom (if (contains? snapshot-requested t-kw) [[:snapshot-run nil t-kw]] []))
        out   (try (when jr
                     (jl/ledger! store
                                 [(ledger/open-job
                                   (assoc (jl/open-job-facts build-branch run-id t-kw attempt
                                                             (ctl/stamp store))
                                          :snapshot-id v0))])
                     (swap! notes into (input-sigs-notes store schema noted t-kw)))
                   (let [r      (run notes)
                         commit (when (seq (:facts r)) (dsets/current-version store))]
                     {:commit commit :r (stamped r jr commit)})
                   (catch Throwable e {:e e}))
        noted (facts/fold-notes noted @notes)
        {:keys [ledger retract-unless-done retract]}
        (when jr
          (attempt-end schema build-branch t-kw jr v0 out @notes noted (ctl/stamp store)))]
    (when (seq ledger) (jl/ledger! store ledger))
    (when (and (seq retract-unless-done) (not= "COMPLETED" (job-status store run-id jr)))
      (ledger/retract-outputs! store v0 retract-unless-done))
    (when (seq retract) (ledger/retract-outputs! store v0 retract))
    (assoc out :noted noted)))

(defn- parallel-run!
  [thunks]
  (let [n (min (long (settings/value :build-parallelism)) (count thunks))]
    (if (<= n 1)
      (mapv (fn [t] (t)) thunks)
      (let [pool (java.util.concurrent.Executors/newFixedThreadPool n)]
        (try
          (->> thunks
               (mapv (fn [t]
                       (.submit pool ^java.util.concurrent.Callable
                                (bound-fn* (profile-scope/joined t)))))
               (mapv (fn [^java.util.concurrent.Future f] (.get f))))
          (finally (.shutdown pool)))))))

(defn- level-code-computes
  [obj-nodes]
  (->> obj-nodes
       (mapcat (comp plan/entries :spec))
       (keep #(get-in % [:from :compute]))
       (filter #(langs/code-language? (:language %)))
       (reduce (fn [m c] (assoc m (:transform-name c) c)) {})
       vals))

(defn- source-pool
  [connect datasources]
  (let [sources    (atom {})
        source-for (fn [ds-key]
                     (inputs/memo! sources ds-key
                                   #(connect (or (get datasources ds-key)
                                                 (refusal/refuse! :refusal.builds/unknown-datasource
                                                                  {:datasource ds-key})))))]
    [source-for (fn [] (doseq [s (vals @sources)] (sources/close! s)))]))

(defn- classify-node-failure
  [e]
  (cond
    (not (instance? Exception e)) {:disposition :rethrow :why :jvm-error}
    (instance? InterruptedException e) {:disposition :rethrow :why :interrupted}
    (effects/cancellation e)      {:disposition :rethrow :why :cancelled}
    (:violations (ex-data e))     {:disposition :restriction
                                   :violations      (:violations (ex-data e))
                                   :violation-count (:violation-count (ex-data e))}
    :else                         {:disposition :failure}))

(defn- job-env
  [env state notes]
  (update env :ctx assoc :notes notes :staged (get-in state [:acc :staged])))

(defn- prewarm-level!
  [{:keys [row-cache] :as env} nodes state]
  (let [todo  (remove #(contains? @row-cache [::inputs/code (:transform-name %)])
                      (level-code-computes nodes))
        ready (into [] (keep (fn [c]
                               (let [notes (atom [])
                                     ctx   (:ctx (job-env env state notes))]
                                 (attempt/or-answer nil
                                   [ctx (inputs/resolve-code-compute ctx c) notes]))))
                    todo)
        noted (parallel-run!
               (mapv (fn [[ctx r notes]]
                       (fn []
                         (let [t-kw (:transform-name (:compute r))
                               t0   (System/nanoTime)]
                           (attempt/or-answer nil
                             (swap! row-cache assoc [::inputs/code t-kw]
                                    (inputs/run-resolved-compute! ctx r))
                             (inputs/note! ctx :job-spans
                                           (lookup/rid-of (:schema ctx) :transforms t-kw)
                                           [{:name "compute" :at-ms 0
                                             :ms (quot (- (System/nanoTime) t0) 1000000)}])
                             @notes))))
                     ready))]
    (update state :noted facts/fold-notes (apply concat noted))))

(defn- writeback-seen
  [store prior t]
  (let [c (sub/type-clock store t)]
    (if (and prior (= c (:clock prior)))
      prior
      {:clock c :index (import-nodes/writeback-index t (sub/read-type store t))})))

(defn- seed-level-writeback!
  [{:keys [store]} nodes {:keys [acc] :as state}]
  (let [links (into {} (map (juxt :key :spec)) nodes)
        seen  (into {} (for [t (import-nodes/writeback-endpoint-types
                                links (:pks (import-nodes/endpoints acc)))]
                         [t (writeback-seen store (get-in state [::writeback-seen t]) t)]))]
    (-> state
        (update ::writeback-seen merge seen)
        (assoc-in [:acc :writeback-index] (update-vals seen :index)))))

(defn- producer-nodes
  [schema in]
  (producers/producer-nodes schema (in :datasets) (in :materializations)))

(defn- export-nodes [schema in] (exports/export-nodes schema (in :exports)))

(defn- media-sync-nodes [schema in] (media-sync/nodes schema (in :media-syncs)))

(def node-kinds
  (array-map
   :producer   {:rank 0 :prepare prewarm-level! :job #'producers/producer-job
                :nodes producer-nodes}
   :object     {:rank 0 :prepare prewarm-level! :job #'import-nodes/object-job}
   :supplement {:rank 1 :prepare prewarm-level! :job #'import-nodes/supplement-job}
   :link       {:rank 2 :prepare seed-level-writeback! :job #'import-nodes/link-job}
   :export     {:rank 3 :prepare prewarm-level! :job #'exports/export-job :nodes export-nodes}
   :media-sync {:rank 0 :prepare prewarm-level! :job #'media-sync/media-sync-job
                :nodes media-sync-nodes}))

(defn scoped-nodes
  [schema node-scope]
  (let [in (partial plan/in-node-scope node-scope)]
    (mapcat (fn [[_ {:keys [nodes]}]] (when nodes (nodes schema in))) node-kinds)))

(defn- levels-of
  [graph nodes]
  (let [lvl-of    (into {} (for [[i lvl] (map-indexed vector (:levels graph)), tk lvl]
                             [tk i]))
        level-key (fn [n] (or (:level n) (get lvl-of (:t-kw n) Long/MAX_VALUE)))]
    (->> nodes (sort-by level-key) (partition-by level-key) (mapv vec))))

(defn- level-groups
  [level]
  (sort-by (comp :rank node-kinds key) (group-by :kind level)))

(defn- runnable
  [doomed level]
  (remove #(contains? doomed (:t-kw %)) level))

(defn- doomed-by
  [schema graph {:keys [kind key t-kw]}]
  (cond-> (disj (transforms/downstream-of graph [t-kw]) t-kw)
    (= :object kind)
    (into (for [[lk spec] (:link-types schema)
                :when (some #{key} (link-side/ends spec))]
            (:producer (backing/backing-of schema :link-type lk))))))

(defn- failure-of
  [schema graph run-id abort? state {:keys [t-kw kind key withholds] :as node} nm e at]
  (let [kids (disj (doomed-by schema graph node) nil)]
    {:state  (-> state
                 (update :failures conj
                         {:job t-kw :name nm :error e :aborted kids :kind kind :key key
                          :withholds withholds})
                 (update :doomed into kids))
     :ledger (cond-> []
               (seq kids) (conj (ledger/abort-jobs run-id (map #(ledger/job-rid run-id %) kids)
                                                   at (str "aborted — depends on failed job "
                                                           t-kw)))
               abort?     (conj (ledger/abort-jobs run-id nil at
                                                   (str "aborted — job " (name nm)
                                                        " failed and :abort-on-failure is set"))))
     :line   (format (if abort?
                       "  ⚠ job FAILED %s (abort-on-failure — every other job is cancelled): %s"
                       "  ⚠ job FAILED %s (isolated — independent jobs continue): %s")
                     (name nm) (ex-message e))}))

(defn- fail-node!
  [{:keys [store schema graph options run-id]} state node nm e]
  (let [abort? (boolean (:abort-on-failure options))
        {:keys [state ledger line]}
        (failure-of schema graph run-id abort? state node nm e (ctl/stamp store))]
    (println line)
    (run! #(jl/ledger! store [%]) ledger)
    (when abort?
      (throw (ex-info (str "job failed and :abort-on-failure is set: " (name nm))
                      {:failed-jobs [nm] :jobs-attributed? true :aborted-non-dependents true}
                      e)))
    state))

(def ^:private transient-sql-states
  #{"08" "40" "53" "57P01" "57P02" "57P03" "55P03"})

(defn retryable-failure?
  [^Throwable e]
  (boolean
   (some (fn [^Throwable t]
           (let [d (ex-data t)]
             (or (true? (:retryable d))
                 (contains? #{429 502 503 504} (:status d))
                 (instance? Throwable (:cause d))
                 (instance? java.net.ConnectException t)
                 (instance? java.net.SocketTimeoutException t)
                 (instance? java.net.http.HttpTimeoutException t)
                 (instance? java.sql.SQLTransientException t)
                 (instance? java.sql.SQLRecoverableException t)
                 (when-let [code (and (instance? java.sql.SQLException t)
                                      (.getSQLState ^java.sql.SQLException t))]
                   (or (contains? transient-sql-states code)
                       (contains? transient-sql-states (subs code 0 (min 2 (count code)))))))))
         (err/causes e))))

(defn- sleep-backoff!
  [progress ms]
  (let [deadline (+ (System/currentTimeMillis) (long ms))]
    (loop []
      (bm/check-cancelled! progress {:phase :retry-backoff})
      (let [left (- deadline (System/currentTimeMillis))]
        (when (pos? left)
          (Thread/sleep (long (min left 250)))
          (recur))))))

(defn- note-retry!
  [{:keys [store pa build-branch run-id options]} t-kw nm attempt e]
  (println (format "  ↻ job %s attempt %d failed (retryable), attempt %d in %dms: %s"
                   (name nm) attempt (inc attempt) (long (:retry-backoff-ms options))
                   (ex-message e)))
  (when t-kw
    (jl/ledger! store
                [(ledger/open-job (jl/open-job-facts build-branch run-id t-kw (inc attempt)
                                                    (ctl/stamp store)))]))
  (sleep-backoff! pa (:retry-backoff-ms options)))

(defn- verdict
  [kind attempt attempts e]
  (if (nil? e)
    :done
    (let [d (:disposition (classify-node-failure e))
          d (if (and (= :producer kind) (= :restriction d)) :failure d)]
      (if (and (= :failure d) (< attempt attempts) (retryable-failure? e)) :retry d))))

(defn- succeeded
  [state r]
  (-> state
      (update :facts into (:facts r))
      (update :acc facts/fold-delta (:delta r))))

(defn- restricted
  [state {:keys [t-kw]} nm e]
  (let [{:keys [violations violation-count]} (ex-data e)]
    (update-in state [:acc :restriction-failures] conj
               {:fn nm :t-kw t-kw :count violation-count
                :error (ex-message e) :violations violations})))

(defn- run-node!
  [{:keys [rid->name options] :as env} state {:keys [kind t-kw key] :as node}]
  (let [nm       (or (rid/resolve-rid->name rid->name key) key)
        attempts (inc (long (:retry-count options)))
        job      (:job (node-kinds kind))]
    (loop [attempt 1 state state]
      (let [{:keys [r e noted]} (with-job! env (:noted state) t-kw attempt
                                  #(let [env (job-env env state %)]
                                     (when t-kw (expect/pre-conditions! (:ctx env) t-kw))
                                     (job env (:acc state) node)))
            state   (assoc state :noted noted)
            outcome (verdict kind attempt attempts e)]
        (case outcome
          :done  (succeeded state r)
          :retry (do (note-retry! env t-kw nm attempt e)
                     (recur (inc attempt) state))
          (do (jl/record-job-failure! env noted t-kw e)
              (case outcome
                :rethrow     (throw e)
                :restriction (do (println
                                  (format (str "  ⚠ data restrictions REJECTED %s (isolated — "
                                               "build continued): %d violation(s)")
                                          (name nm) (count (:violations (ex-data e)))))
                                 (restricted state node nm e))
                :failure     (fail-node! env state node nm e))))))))

(def no-job-failures
  {:doomed #{} :failures []})

(defn job-failure
  [fs]
  (when (seq fs)
    (ex-info
     (str (count fs) " job(s) failed: " (str/join ", " (map (comp name :name) fs))
          (when-let [n (seq (mapcat :aborted fs))]
            (str " — " (count (distinct n)) " dependent job(s) aborted")))
     {:failed-jobs      (mapv :name fs)
      :failed-t-kws     (into #{} (keep :job) fs)
      :aborted-jobs     (vec (distinct (mapcat :aborted fs)))
      :jobs-attributed? true}
     (:error (first fs)))))

(defn- run-env
  [{:keys [store build-branch schema datasources options run-id code-env pa row-cache]}
   source-for trace?]
  (let [ctx {:store store :schema schema :datasources datasources :source-for source-for
             :run-id run-id :build-branch build-branch
             :code-env code-env
             :row-cache (or row-cache (atom {})) :mat-cache (atom {})
             :full-refresh? (boolean (:full-refresh options))
             :snapshot-requested (ledger/snapshot-requests store build-branch)
             :run-setup (:run-setup options)
             :trace? trace?}]
    (assoc ctx
           :graph (transforms/build-graph schema) :options options :pa pa
           :build-branch build-branch :sql-cache (atom {}) :ctx ctx
           :rid->name (sch/rid-name-index schema))))

(defn- start-state
  [state0 {:keys [fresh-rows fresh-pk-index]}]
  (assoc (merge {:noted (facts/empty-facts false) :facts []} no-job-failures state0)
         :acc (assoc (facts/start facts/deltas false)
                     :fresh-rows (or fresh-rows {})
                     :fresh-pk-index (or fresh-pk-index {}))))

(defn- run-level!
  [{:keys [store schema run-id] :as env} freshness state level]
  (let [admitted (->> (fresh/settle-level! store schema run-id freshness level)
                      (runnable (:doomed state)))]
    (jl/pend-level! store run-id admitted)
    (reduce (fn [state [kind nodes]]
              (let [prepare (:prepare (node-kinds kind))]
                (reduce #(run-node! env %1 %2)
                        (cond->> state prepare (prepare env nodes))
                        nodes)))
            state
            (level-groups admitted))))

(defn run-dag!
  [{:keys [store build-branch schema datasources options run-id] :as args} nodes state0]
  (let [[source-for close-all!] (source-pool (scope/connect (grants/source-authority store))
                                             datasources)
        env       (run-env args source-for (contains? (:noted state0) :trace))
        graph     (:graph env)
        {:keys [runs?] :as freshness}
        (fresh/freshness store build-branch run-id schema graph nodes options)]
    (try
      (reduce #(run-level! env freshness %1 %2)
              (start-state state0 freshness)
              (levels-of graph (filter runs? nodes)))
      (finally
        (close-all!)))))
