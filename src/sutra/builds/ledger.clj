(ns sutra.builds.ledger
  (:require [sutra.store.control :as ctl]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.outputs :as outputs]
            [sutra.types.naming :as naming]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.foundation.encoding :as enc]
            [sutra.store.storage.command :as cmd]
            [sutra.store.api :as api]
            [sutra.store.memo :as memo]
            [sutra.store.tx-vector :as tv]
            [sutra.store.world :as world]
            [sutra.types.rid :as rid]
            [sutra.kit.noted :as noted])
  (:import (java.util UUID)))

(set! *warn-on-reflection* true)

(defn- attr [col] (keyword (str/replace (name col) "_" "-")))
(defn- sql-keys [m] (some-> m (update-keys #(keyword (str/replace (name %) "-" "_")))))

(defn job-rid
  [run-id transform-key]
  (rid/new-rid :job (enc/transform-row-id :job [(str run-id) (str transform-key)])))

(defn request-snapshot!
  [store {:keys [transform-rid dataset-rid branch requested-by message]}]
  (ctl/commit! store [[:upsert :snapshot-requests
                       [{:branch (str branch) :transform-rid (str transform-rid)
                         :dataset-rid (some-> dataset-rid str) :requested-by requested-by
                         :message message}]]]))

(defn snapshot-requests
  [store branch]
  (into #{} (map :transform-rid) (ctl/list store :snapshot-requests {:branch (str branch)})))

(defn snapshot-served
  [job-rid branch transform-rid]
  {:op :ledger/snapshot-served :job-rid job-rid :branch (str branch)
   :transform-rid (str transform-rid)})

(defn latest-txn
  [store dataset-rid branch]
  (some-> (first (ctl/list store :ledger-txns {:dataset-rid dataset-rid :branch branch}
                           {:order [[:started-at :desc]] :limit 1}))
          (select-keys [:txn-rid :snapshot-id])
          sql-keys))

(defn- txn-row
  [t prev {:keys [dataset-rid branch run-id job-rid snapshot-id type status
                  schema-commit row-count row-delta started-at ended-at]}]
  {:internal-uuid (str (UUID/randomUUID)) :txn-rid t :dataset-rid dataset-rid
   :branch branch :run-id (some-> run-id str) :job-rid job-rid :snapshot-id snapshot-id
   :prev-txn-rid prev :type (some-> type name) :status (some-> status name)
   :schema-commit schema-commit :row-count row-count :row-delta row-delta
   :started-at started-at :ended-at ended-at})

(defn record-txn!
  [store {:keys [dataset-rid branch prev-txn-rid] :as txn}]
  (let [t    (rid/new-rid :transaction (random-uuid))
        prev (or prev-txn-rid (:txn_rid (latest-txn store dataset-rid branch)))]
    (ctl/commit! store [[:insert :ledger-txns [(txn-row t prev txn)]]])
    t))

(defn list-transactions
  [store dataset-rid branch limit]
  (mapv sql-keys (ctl/list store :ledger-txns {:dataset-rid dataset-rid :branch branch}
                           {:order [[:started-at :desc]] :limit limit})))

(defn- edge-id [consumer-rid input-dataset-rid branch]
  (enc/transform-row-id :dataset-consumption [consumer-rid input-dataset-rid branch]))

(defn- pointer-row
  [{:keys [consumer-rid input-dataset-rid branch last-txn-rid last-snapshot-id updated-at]}]
  {:internal-uuid (edge-id consumer-rid input-dataset-rid branch)
   :consumer-rid consumer-rid :input-dataset-rid input-dataset-rid :branch branch
   :last-txn-rid last-txn-rid :last-snapshot-id last-snapshot-id :updated-at updated-at})

(defn advance-pointer!
  [store pointer]
  (ctl/commit! store [[:upsert :ledger-consumption [(pointer-row pointer)]]]))

(defn- delete-pointers!
  [store k value branch]
  (first (ctl/commit! store [[:delete :ledger-consumption
                              (cond-> {k value} branch (assoc :branch branch))]])))

(defn reset-consumer!
  ([store consumer-rid] (reset-consumer! store consumer-rid nil))
  ([store consumer-rid branch] (delete-pointers! store :consumer-rid consumer-rid branch)))

(defn consumer-pointer
  [store consumer-rid input-dataset-rid branch]
  (some-> (ctl/get store :ledger-consumption
                   {:internal-uuid (edge-id consumer-rid input-dataset-rid branch)})
          (select-keys [:last-txn-rid :last-snapshot-id])
          sql-keys))

(defn consumption-of
  [store consumer-rid branch]
  (mapv #(sql-keys (select-keys % [:input-dataset-rid :last-txn-rid :last-snapshot-id :updated-at]))
        (ctl/list store :ledger-consumption {:consumer-rid consumer-rid :branch branch}
                  {:order [[:updated-at :desc]]})))

(defn read-boundary
  [store consumer-rid input-dataset-rid branch]
  {:low  (:last_snapshot_id (consumer-pointer store consumer-rid input-dataset-rid branch))
   :high (dsets/current-version store)})

(defn added-changed
  [store consumer-rid input-dataset-rid branch]
  (let [{:keys [low high]} (read-boundary store consumer-rid input-dataset-rid branch)]
    (cond
      (nil? high) []
      (nil? low)  :bootstrap
      (not (dsets/addressable? low)) :bootstrap
      (dsets/below-horizon? store (naming/dataset-table input-dataset-rid) low) :bootstrap
      (not (dsets/only-added? store (naming/dataset-table input-dataset-rid) low high)) :bootstrap
      :else       (dsets/added-values store (naming/dataset-table input-dataset-rid) low high
                                      nil))))

(defn classify
  [{:keys [incremental? bootstrap? upsert? written deleted]}]
  (let [written (or written 0) deleted (or deleted 0)]
    {:type      (cond (or (not incremental?) bootstrap?) :SNAPSHOT
                      (pos? deleted)                     :DELETE
                      upsert?                            :UPDATE
                      :else                              :APPEND)
     :row-delta (+ written deleted)}))

(defn- build-records
  [{:keys [branch run-id schema-commit writes consumed at snapshot-id counts new-rid]} latest-of]
  (let [latest-of (memoize latest-of)
        {:keys [txns last]}
        (reduce (fn [acc {:keys [dataset-rid job-rid] :as w}]
                  (let [{:keys [type row-delta]} (classify w)
                        t (new-rid)]
                    (-> acc
                        (update :txns conj
                                (txn-row t
                                         (or (get-in acc [:last dataset-rid])
                                             (latest-of dataset-rid))
                                         {:dataset-rid dataset-rid :branch branch :run-id run-id
                                          :job-rid job-rid :snapshot-id snapshot-id :type type
                                          :status :COMMITTED :schema-commit schema-commit
                                          :row-count (get counts dataset-rid) :row-delta row-delta
                                          :started-at at :ended-at at}))
                        (assoc-in [:last dataset-rid] t))))
                {:txns [] :last {}}
                writes)]
    {:txns     txns
     :pointers (mapv (fn [{:keys [consumer-rid input-dataset-rid last-snapshot-id]}]
                       (pointer-row {:consumer-rid consumer-rid :input-dataset-rid input-dataset-rid
                                     :branch branch
                                     :last-txn-rid
                                     (or (get last input-dataset-rid)
                                         (latest-of input-dataset-rid))
                                     :last-snapshot-id (or last-snapshot-id snapshot-id)
                                     :updated-at at}))
                     consumed)}))

(defn- record-entries
  [store {:keys [txns pointers]}]
  (into (mapv #(ctl/entry store :ledger-txns %) txns)
        (for [row pointers
              :let [{:keys [key value]} (ctl/entry store :ledger-consumption row)
                    old (ctl/get store :ledger-consumption
                                 {:internal-uuid (:internal-uuid value)})]]
          {:coll (:coll (ctl/decl :ledger-consumption)) :key key :value value
           :expect (or old api/absent)})))

(defn commit-transactions!
  [store {:keys [branch writes] :as build}]
  (let [built (build-records (assoc build
                                    :snapshot-id (str (dsets/current-version store))
                                    :counts
                                    (into {}
                                          (map (fn [{:keys [dataset-rid]}]
                                                 [dataset-rid
                                                  (dsets/table-count store dataset-rid)]))
                                          writes)
                                    :new-rid #(rid/new-rid :transaction (random-uuid)))
                             #(:txn_rid (latest-txn store % branch)))]
    (when-let [entries (not-empty (record-entries store built))]
      (world/commit! (world/at store ctl/trunk)
                     {:records  entries
                      :at-head? true
                      :actor    {:principal "sutra.builds.ledger"}}))
    nil))

(defn record-edit-transactions!
  [store {:keys [branch snapshot-id at ops run-id actor]}]
  (doseq [[otype otype-ops] (group-by :type (filter tv/object-write? ops))
          :when otype]
    (record-txn! store {:dataset-rid otype :branch branch :run-id run-id
                        :job-rid (job-rid run-id (str "user-edit" (some->> actor (str ":"))))
                        :snapshot-id snapshot-id
                        :type (tv/ledger-type otype-ops) :status :COMMITTED
                        :row-count (sub/stored-count store otype)
                        :row-delta (count otype-ops)
                        :started-at at :ended-at at})))

(def job-states
  #{:WAITING :RUN_PENDING :RUNNING :ABORT_PENDING :COMPLETED :FAILED :DID_NOT_RUN :ABORTED})

(def legacy-job-states
  {"SKIPPED" "DID_NOT_RUN" "SUCCEEDED" "COMPLETED"})

(defn read-job-state
  [status]
  (let [s (str status)]
    (get legacy-job-states s s)))

(defn job-state
  [state]
  (name (or (job-states state)
            (throw (ex-info (str "unknown job state " (pr-str state))
                            {:state state :known job-states})))))

(defn- job-pk [job-rid]
  (enc/transform-row-id :job-row [job-rid]))

(def ^:const max-job-log-chars
  20000)

(defn clamp-log
  [s]
  (let [s (some-> s str str/trim not-empty)]
    (cond
      (nil? s) nil
      (<= (count s) max-job-log-chars) s
      :else (str (subs s 0 max-job-log-chars) "\n… [log truncated at "
                 max-job-log-chars " chars]"))))

(def ^:const max-job-spans
  2000)

(defn clamp-spans
  [spans]
  (when (seq spans)
    (pr-str (if (<= (count spans) max-job-spans)
              (vec spans)
              (let [open (filter :open spans)
                    slow (take (- max-job-spans (count open))
                               (sort-by #(- (long (or (:ms %) 0))) (remove :open spans)))]
                (vec (sort-by #(long (or (:at-ms %) 0)) (concat open slow))))))))

(def ^:private job-columns
  [[:run_id           :run-id           #(some-> % str)]
   [:transform_rid    :transform-rid    #(some-> % str)]
   [:branch           :branch]
   [:attempt          :attempt]
   [:started_at       :started-at]
   [:ended_at         :ended-at]
   [:snapshot_id      :snapshot-id]
   [:outputs          :outputs          #(pr-str (vec %))]
   [:rows_in          :rows-in]
   [:rows_out         :rows-out]
   [:code_sha         :code-sha]
   [:jobspec_id       :jobspec-id]
   [:fingerprint      :fingerprint      identity :coalesce]
   [:pending_observed :pending-observed identity :coalesce]
   [:job_log          :job-log          clamp-log :coalesce]
   [:job_spans        :job-spans        clamp-spans :coalesce]
   [:consumed_cursors :consumed-cursors #(some-> (seq %) vec pr-str) :coalesce]
   [:error            :error            #(some-> % str)]])

(def job-transitions
  {:WAITING   {:sets [] :on-conflict :keep}
   :RUNNING   {:sets [:attempt :started_at :snapshot_id]
               :nulls [:error :ended_at :job_log :job_spans]
               :from #{:WAITING :RUN_PENDING :RUNNING :FAILED}}
   :COMPLETED {:sets [:snapshot_id :outputs :rows_in :rows_out :code_sha
                      :pending_observed :job_log :job_spans :consumed_cursors :ended_at]
               :nulls [:error]
               :from #{:WAITING :RUN_PENDING :RUNNING :ABORT_PENDING :COMPLETED}}
   :FAILED    {:sets [:error :job_log :job_spans :ended_at]
               :from #{:WAITING :RUN_PENDING :RUNNING :ABORT_PENDING :COMPLETED :FAILED}}})

(def job-sweeps
  {:RUN_PENDING   {:from [:WAITING]}
   :ABORT_PENDING {:from [:RUN_PENDING :RUNNING]}
   :DID_NOT_RUN   {:from [:WAITING :RUN_PENDING] :sets [:ended_at :error]}
   :ABORTED       {:from [:WAITING :RUN_PENDING :RUNNING :ABORT_PENDING] :sets [:ended_at :error]}})

(defn- column-row [col]
  (or (some #(when (= col (first %)) %) job-columns)
      (throw (ex-info (str "not a jobs column " (pr-str col)) {:col col}))))

(defn- column-value [facts col]
  (let [[_ k enc] (column-row col)]
    ((or enc identity) (get facts k))))

(defn- coalesce? [col] (= :coalesce (nth (column-row col) 3 nil)))

(defn- blocked?
  [{:keys [from]} old]
  (not (contains? (into #{} (map job-state) from) (read-job-state (:status old)))))

(defn- moved
  [state facts old]
  (let [{:keys [sets nulls]} (job-transitions state)]
    (merge old
           {:status (job-state state)}
           (into {} (for [c sets] [(attr c) (if (coalesce? c)
                                              (if (some? (column-value facts c))
                                                (column-value facts c) (get old (attr c)))
                                              (column-value facts c))]))
           (into {} (for [c nulls] [(attr c) nil])))))

(defn- inserted
  [state {:keys [job-rid] :as facts} k]
  (let [{:keys [sets]} (job-transitions state)]
    (into (assoc k :job-rid job-rid :status (job-state state))
          (for [[c fk] job-columns :when (or (some #{c} sets) (contains? facts fk))]
            [(attr c) (column-value facts c)]))))

(defn- publish-step?
  [state old]
  (and (= :COMPLETED state)
       (= (job-state :COMPLETED) (read-job-state (:status old)))
       (:ended-at old)))

(defn job-after
  [state {:keys [job-rid run-id] :as facts} old]
  (let [t (job-transitions state)
        k {:internal-uuid (job-pk job-rid)}]
    (cond
      (nil? old)                  (when run-id (inserted state facts k))
      (= :keep (:on-conflict t))  nil
      (blocked? t old)            nil
      (publish-step? state old)   (assoc (moved state facts old) :ended-at (:ended-at old))
      :else                       (moved state facts old))))

(defmethod cmd/plan :job/transition
  [store {:keys [state] {:keys [job-rid] :as facts} :facts} at]
  (ctl/swapping store :ledger-jobs {:internal-uuid (job-pk job-rid)}
                #(job-after state facts %)
                at))

(defmethod cmd/plan :job/sweep
  [store {:keys [state run-id facts job-rids]} at]
  (let [{:keys [from sets]} (job-sweeps state)]
    (ctl/updating store :ledger-jobs
                  (cond-> {:run-id (str run-id) :status (into #{} (map job-state) from)}
                    (seq job-rids) (assoc :job-rid (set job-rids)))
                  (into {:status (job-state state)}
                        (for [c sets] [(attr c) (column-value facts c)]))
                  at)))

(defn- transition [state facts] {:op :job/transition :state state :facts facts})

(defn advance-on-complete
  [job-rid branch consumed]
  (when (seq consumed)
    {:op :ledger/advance :job-rid job-rid :branch branch :consumed (vec consumed)}))

(defmethod cmd/plan :ledger/advance
  [store {:keys [job-rid branch consumed]} at]
  (if (= "COMPLETED"
         (read-job-state (:status (ctl/get store :ledger-jobs {:internal-uuid (job-pk job-rid)}))))
    {:records (vec (for [{:keys [consumer-rid input-dataset-rid last-snapshot-id]} consumed
                         :let
                         [row (pointer-row
                               {:consumer-rid consumer-rid
                                :input-dataset-rid input-dataset-rid
                                :branch branch :last-snapshot-id last-snapshot-id
                                :last-txn-rid
                                (:txn-rid (latest-txn store input-dataset-rid branch))
                                :updated-at at})
                          old (ctl/get store :ledger-consumption
                                       {:internal-uuid (:internal-uuid row)})]]
                     (assoc (ctl/entry store :ledger-consumption row) :expect (or old api/absent))))
     :result true}
    {:result false}))

(defmethod cmd/plan :ledger/snapshot-served
  [store {:keys [job-rid branch transform-rid]} _]
  (if (= "COMPLETED"
         (read-job-state (:status (ctl/get store :ledger-jobs {:internal-uuid (job-pk job-rid)}))))
    (ctl/deleting store :snapshot-requests {:branch branch :transform-rid transform-rid})
    {:result false}))

(defn declared-outputs
  [schema t]
  (when-let [spec (and t (get-in schema [:transforms t]))]
    (keep #(when (= :dataset (:kind %)) (:dataset %)) (outputs/outputs-of spec))))

(defn retract-outputs!
  [store v0 ds-rids]
  (doseq [ds (distinct ds-rids)
          :let [sig (some-> (dsets/content-signature store (naming/dataset-table ds)) parse-long)
                v0  (dsets/snapshot-id v0)]
          :when (and v0 sig (> sig (long v0)))]
    (noted/quietly (str "ledger: retract the attempt's writes to " ds)
                     (dsets/rollback-to! store ds (str v0)))))

(defn- sweep [state run-id facts job-rids]
  {:op :job/sweep :state state :run-id run-id :facts facts :job-rids job-rids})

(defn seed-jobs
  [{:keys [run-id transform-rids branch jobspec-ids]}]
  (into [] (comp (distinct) (filter some?)
                 (map #(transition :WAITING
                                   (cond-> {:job-rid (job-rid run-id %) :run-id run-id
                                            :transform-rid % :branch branch :attempt 1}
                                     (get jobspec-ids %) (assoc :jobspec-id (get jobspec-ids %))))))
        transform-rids))

(defn open-job
  [{:keys [at attempt] :as facts}]
  (transition :RUNNING (assoc facts :attempt (or attempt 1) :started-at at)))

(defn abort-pending-jobs
  [run-id]
  (sweep :ABORT_PENDING run-id {} nil))

(defn finish-job
  [{:keys [at run-id] :as facts}]
  (transition :COMPLETED (cond-> (assoc facts :ended-at at)
                           run-id (assoc :attempt 1 :started-at at))))

(defn fail-job
  [{:keys [at run-id] :as facts}]
  (transition :FAILED (cond-> (assoc facts :ended-at at)
                        run-id (assoc :attempt 1 :started-at at))))

(defn pend-jobs
  [run-id job-rids]
  (when (seq job-rids)
    (sweep :RUN_PENDING run-id {} job-rids)))

(defn skip-unstarted-jobs
  ([run-id at] (skip-unstarted-jobs run-id at nil nil))
  ([run-id at job-rids reason]
   (sweep :DID_NOT_RUN run-id {:ended-at at :error reason} (seq job-rids))))

(defn abort-jobs
  [run-id job-rids at reason]
  (sweep :ABORTED run-id {:ended-at at :error reason} (seq job-rids)))

(defn failed-job-count
  [store run-id]
  (ctl/count store :ledger-jobs {:run-id (str run-id) :status (job-state :FAILED)}))

(defn settle-failed-run
  [failed-jobs {:keys [run-id branch error at attributed?]}]
  [(when (and (not attributed?) (zero? failed-jobs))
     (fail-job {:job-rid (job-rid run-id :build) :run-id run-id :transform-rid nil
                :branch branch :error error :at at}))
   (abort-jobs run-id nil at "aborted — the build failed before this job ran")])

(defn- recency [r] (or (:ended-at r) (:started-at r) ""))

(defn- newest-per
  [k rows]
  (reduce (fn [m r]
            (let [v (some-> (get r k) str not-empty)]
              (cond-> m
                (and v (or (not (contains? m v)) (pos? (compare (recency r) (recency (get m v))))))
                (assoc v r))))
          {} rows))

(defn- read-states [rows] (mapv #(update % :status read-job-state) rows))

(defn- newest-rows [store t where by]
  (ctl/list store t where {:pick {:by by :max [:ended-at :started-at]}}))

(defn last-observed-pending
  [store branch]
  (into {}
        (map (fn [[t {:keys [pending-observed run-id ended-at]}]]
               [t {:pending (long pending-observed) :run (str run-id) :at (str ended-at)}]))
        (newest-per :transform-rid
                    (newest-rows store :ledger-jobs
                                 {:branch branch :pending-observed :records/present}
                                 [:transform-rid]))))

(defn read-spans
  [stored]
  (when (seq (str stored))
    (try (edn/read-string (str stored))
         (catch RuntimeException e
           (println (str "[ledger] stored spans do not read: " (ex-message e)))
           nil))))

(defn jobs-of-run
  [store run-id]
  (read-states
   (mapv #(-> (sql-keys (select-keys %
                                     [:job-rid :transform-rid :status :attempt :snapshot-id :rows-in
                                      :rows-out
                                      :pending-observed :code-sha :jobspec-id :outputs :error
                                      :job-log
                                      :job-spans :started-at
                                      :ended-at]))
              (update :job_spans read-spans))
         (ctl/list store :ledger-jobs {:run-id (str run-id)} {:order [[:started-at :desc]]}))))

(defonce ^:private by-clock-memo (atom {}))
(def ^:private by-clock-spec {::jobs {:cap 32} ::txns {:cap 32}})

(defn- by-clock
  ([table store branch f] (by-clock table nil store branch f))
  ([table shape store branch f]
   (if-let [c (ctl/written store table)]
     (memo/through by-clock-memo by-clock-spec (if (= table :ledger-jobs) ::jobs ::txns)
                   [(:db store) (str branch) shape c] f)
     (f))))

(defn- job-of-version
  [store dataset-rid branch snapshot-id]
  (when-let [jr (some #(when (= (str snapshot-id) (str (:snapshot-id %))) (:job-rid %))
                      (ctl/list store :ledger-txns {:dataset-rid (str dataset-rid)
                                                    :branch (str branch)}
                                {:order [[:started-at :desc]] :limit 500}))]
    (ctl/get store :ledger-jobs {:internal-uuid (job-pk jr)})))

(defn jobspec-of-version
  [store dataset-rid branch snapshot-id]
  (some-> (job-of-version store dataset-rid branch snapshot-id) :jobspec-id str))

(defn rewind-producer!
  [store {:keys [transform-rid dataset-rid branch snapshot-id at] :or {at (ctl/stamp store)}}]
  (when (seq (consumption-of store transform-rid branch))
    (if-let [stored (:consumed-cursors (job-of-version store dataset-rid branch snapshot-id))]
      (do (reset-consumer! store transform-rid branch)
          (doseq [{:keys [input-dataset-rid last-snapshot-id]} (edn/read-string (str stored))]
            (advance-pointer! store {:consumer-rid transform-rid
                                     :input-dataset-rid input-dataset-rid
                                     :branch branch :last-snapshot-id last-snapshot-id
                                     :updated-at at}))
          :rewound)
      (do (request-snapshot! store {:transform-rid transform-rid :dataset-rid dataset-rid
                                    :branch branch :requested-by "rollback"
                                    :message (str "rolled back to " snapshot-id)})
          :snapshot-requested))))

(defn last-jobs-per-transform
  [store branch]
  (by-clock
   :ledger-jobs store branch
   (fn []
     (let [rows (read-states (newest-rows store :ledger-jobs {:branch branch}
                                          [:transform-rid :status]))
           pick (fn [rs] (update-vals (newest-per :transform-rid rs)
                                      #(select-keys % [:status :rows-out :error :run-id
                                                       :code-sha :jobspec-id :started-at
                                                       :ended-at])))]
       {:any    (pick rows)
        :ok     (pick (filter #(= "COMPLETED" (:status %)) rows))
        :failed (pick (filter #(= "FAILED" (:status %)) rows))}))))

(defn- last-job-per-transform*
  [store branch]
  (update-vals (newest-per :transform-rid
                           (newest-rows store :ledger-jobs {:branch branch} [:transform-rid]))
               #(-> (select-keys %
                                 [:transform-rid :status :attempt :run-id :rows-in :rows-out :error
                                  :job-log :code-sha :jobspec-id :snapshot-id :started-at
                                  :ended-at])
                    sql-keys
                    (update :status read-job-state))))

(defn last-job-per-transform
  [store branch]
  (by-clock :ledger-jobs ::last store branch #(last-job-per-transform* store branch)))

(defmethod cmd/plan :job/fingerprint
  [store {:keys [run-id job-rid fingerprint]} at]
  (ctl/updating store :ledger-jobs
                {:job-rid (str job-rid) :run-id (str run-id) :status (job-state :COMPLETED)}
                {:fingerprint fingerprint}
                at))

(defn stamp-fingerprints
  [run-id fps]
  (mapv (fn [[job-rid fp]] {:op :job/fingerprint :run-id run-id :job-rid job-rid :fingerprint fp})
        fps))

(defn last-fingerprints
  [store branch]
  (update-vals (newest-per :transform-rid
                           (newest-rows store :ledger-jobs
                                        {:branch      branch
                                         :fingerprint :records/present
                                         :status      #{(job-state :COMPLETED) "SUCCEEDED"}}
                                        [:transform-rid]))
               #(sql-keys (select-keys % [:transform-rid :fingerprint :run-id]))))

(defn- last-txn-per-dataset*
  [store branch]
  (let [where    {:branch branch}
        newest   (newest-per :dataset-rid (newest-rows store :ledger-txns where [:dataset-rid]))
        first-of (newest-per :dataset-rid
                             (ctl/list store :ledger-txns where
                                       {:pick {:by [:dataset-rid] :min [:started-at :ended-at]}}))
        counts   (ctl/counts store :ledger-txns where :dataset-rid)]
    (into {}
          (for [[d newest] newest
                :let [oldest (get first-of d)]]
            [d
             (assoc (sql-keys (select-keys newest
                                           [:dataset-rid :type :status :row-count :row-delta :run-id
                                            :job-rid :snapshot-id :started-at :ended-at]))
                    :created (or (:started-at oldest) (:ended-at oldest) "")
                    :txns (get counts d))]))))

(defn last-txn-per-dataset
  [store branch]
  (by-clock :ledger-txns store branch #(last-txn-per-dataset* store branch)))
