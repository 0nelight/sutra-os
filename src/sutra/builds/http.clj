(ns sutra.builds.http
  (:require [sutra.catalog.grants :as grants]
            [sutra.store.control :as ctl]
            [sutra.builds.ledger.report :as report]
            [sutra.builds.plan.preview :as transform-preview]
            [sutra.builds.plan.unit-tests :as unit-tests]
            [sutra.builds.plan.inputs :as inputs]
            [clojure.string :as str]
            [sutra.objects.api :as objects]
            [sutra.builds.run.queue :as builds]
            [sutra.builds.import :as importer]
            [sutra.builds.import.sources :as import-sources]
            [sutra.builds.plan :as plan]
            [sutra.builds.run.reindex :as reindex]
            [sutra.builds.plan.virtual :as virtual]
            [sutra.sources.api :as sources]
            [sutra.types.virtual :as vtypes]
            [sutra.types.granular :as granular]
            [sutra.builds.run :as runs]
            [sutra.builds.last-updated :as last-updated]
            [sutra.builds.ledger :as ledger]
            [sutra.builds.import.changelog :as changelog]
            [sutra.builds.http.files :as files]
            [sutra.catalog.backing :as backing]
            [sutra.store.world :as world]
            [sutra.builds.run.lock :as lock]
            [sutra.llm.api :as llm]
            [sutra.builds.run.monitor :as bm]
            [sutra.foundation.live-trace :as live]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.nodes :as nodes]
            [sutra.catalog.project-scope :as scope]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.store.storage.command :as cmd]
            [sutra.store.access.scope :as ctx]
            [sutra.projects.api :as projects]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.dataset-rows :as dsrows]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.http.endpoint :as ep]
            [sutra.foundation.ops :as op]
            [sutra.http.responses :refer [ok fault]]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)
(defn- llm-window
  [{:keys [today since until caller model run errors limit]}]
  (let [utoday (str (java.time.LocalDate/now java.time.ZoneOffset/UTC))]
    {:since   (if today utoday since)
     :until   (if today utoday until)
     :caller  caller
     :model   model
     :run     run
     :errors? (boolean errors)
     :limit   limit}))

(def llm-cost
  (ep/handler :llm-cost
              {:call (fn [{:keys [store branch schema per calls] :as a}]
                       (let [per (or per "email")]
                         (llm/cost (ctx/at store branch) schema
                                   (assoc (llm-window a)
                                          :calls? (boolean calls)
                                          :per per
                                          :per-count
                                          (get-in
                                           (objects/object-counts
                                            (ctx/at store branch) schema)
                                           [:by-type (keyword per)])))))}))

(def llm-audit
  (ep/handler :llm-audit
              {:respond (fn [{:keys [store branch schema request] :as a}]
                          (if request
                            (if-let [row (llm/trace (ctx/at store branch) schema request)]
                              (ok {:call row})
                              (refusal/refuse! :refusal.builds/no-llm-call {:request-id request}))
                            (ok (llm/audit (ctx/at store branch) schema (llm-window a)))))}))

(def reindex-poll
  (ep/handler :import-poll
              {:call #(reindex/reindex-if-changed! (:store %) (:schema %) (:branch %))}))

(defn- sees-all?
  [auth]
  (or (nil? auth)
      (let [ops (grants/operations auth)] (or (= grants/all ops) (contains? ops :read)))))

(defn- readable-transform?
  [auth schema t]
  (let [rid (or (when (contains? (:transforms schema) t) t)
                (some->> t str not-empty keyword (lookup/rid-of schema :transforms)))]
    (boolean (and rid (scope/may? auth :read (get-in schema [:transforms rid :project]))))))

(defn run-for
  [auth schema {:keys [actor targets] :as run}]
  (let [own?     (or (nil? auth)
                     (= grants/all (grants/operations auth))
                     (and actor (= (str actor) (str (grants/current-user-id auth)))))
        readable (filterv #(readable-transform? auth schema %) targets)]
    (cond
      own?                  run
      (or (sees-all? auth)
          (and (seq targets) (= (count readable) (count targets))))
      (dissoc run :inputs)
      :else                 (cond-> (dissoc run :inputs :error-message :progress)
                              (some? targets) (assoc :targets readable)))))

(defn visible-jobs
  [auth schema jobs]
  (if (sees-all? auth)
    jobs
    (filterv #(readable-transform? auth schema (or (:transform_rid %) (:transform-rid %))) jobs)))

(def list-runs
  (ep/handler :runs
              {:call (fn [{:keys [store branch schema auth kind hours limit brief]}]
                       {:runs
                        (cond->> (mapv #(run-for auth schema %)
                                       (runs/list-recent (ctx/at store branch) limit kind
                                                         (some-> hours (* 3600))))
                          brief (mapv #(dissoc % :inputs-hash :progress :targets)))
                        :window {:hours hours :limit limit}})}))

(defn- requested-run
  [store id]
  (let [run (builds/requested-run store id)]
    (cond-> run
      (= "running" (:status run)) (merge (lock/queue-place store id)))))

(def get-run
  (ep/handler :run
              {:call #(some->> (or (runs/get-run (ctx/at (:store %) (:branch %)) (:id %))
                                   (requested-run (:store %) (:id %)))
                               (run-for (:auth %) (:schema %)))
               :or-404 :refusal.builds/no-run}))

(def active-builds
  (ep/handler :builds-active
              {:call (fn [{:keys [store]}] (bm/active-builds store))
               :wrap :builds}))

(def live-trace
  (ep/handler :live {:call #(live/since (:since %))}))

(defn- catalog-ref
  [schema raw kinds]
  (when-let [s (not-empty raw)]
    (if (rid/rid? s)
      s
      (let [k (keyword s)]
        (some #(lookup/rid-of schema % k) kinds)))))

(defn- object-type-producer
  [schema k]
  (let [ot-rid (or (lookup/rid-of schema :object-types k) (lookup/rid-of schema :objects k))
        want   (cond-> #{k} ot-rid (conj ot-rid))]
    (some (fn [[t-rid t]]
            (some (fn [out]
                    (when (and (= :object-type (:kind out))
                               (contains? want (:object-type out)))
                      {:dataset (:dataset out) :producer t-rid}))
                  (outputs/outputs-of t)))
          (:transforms schema))))

(def dataset-transactions
  (ep/handler :catalog-transactions
              {:respond
               (fn [{:keys [store branch schema dataset limit auth]}]
                 (let [direct (catalog-ref schema dataset [:datasets :transforms])
                       store     (ctx/at store branch)
                       txns   (when direct (ledger/list-transactions store direct branch limit))]
                   (cond
                     (seq txns) (ok {:transactions txns})
                     :else
                     (if-let [{ds :dataset :keys [producer]}
                              (object-type-producer schema (some-> dataset keyword))]
                       (let [pnm (or (rid/resolve-rid->name (sch/rid-name-index schema) producer)
                                     producer)
                             ot  (lookup/rid-of schema :object-types (keyword dataset))]
                         (grants/authorize-read! auth (get-in schema [:object-types ot]) ot)
                         (ok {:transactions (ledger/list-transactions store ds branch limit)
                              :via {:object-type dataset
                                    :producer (if (keyword? pnm) (name pnm) (str pnm))}}))
                       (if direct
                         (ok {:transactions []})
                         (refusal/refuse! :refusal.http/unknown-dataset {:dataset dataset}))))))}))

(def dataset-consumption
  (ep/handler :catalog-consumption
              {:respond
               (fn [{:keys [store branch schema transform]}]
                 (let [t-rid (catalog-ref schema transform [:transforms :datasets])]
                   (if (nil? t-rid)
                     (refusal/refuse! :refusal.builds/unknown-transform {:transform transform})
                     (ok {:consumption
                          (ledger/consumption-of (ctx/at store branch) t-rid branch)}))))}))

(def dataset-delta
  (ep/handler :catalog-delta
              {:respond
               (fn [{:keys [store branch schema transform input]}]
                 (let [t-rid  (catalog-ref schema transform [:transforms :datasets])
                       in-rid (catalog-ref schema input [:datasets :transforms])]
                   (if (or (nil? t-rid) (nil? in-rid))
                     (refusal/refuse! :refusal.builds/unknown-transform-or-input
                                      {:transform transform :input input})
                     (let [store   (ctx/at store branch)
                           rng  (ledger/read-boundary store t-rid in-rid branch)
                           rows (ledger/added-changed store t-rid in-rid branch)]
                       (ok {:delta (merge rng (if (= rows :bootstrap)
                                                {:bootstrap true}
                                                {:added-modified (count rows)}))})))))}))

(def cancel-run
  (ep/handler :run-cancel
              {:call (fn [{:keys [store branch id force]}]
                       (if force
                         (let [reaped (bm/force-fail! store branch id)]
                           (when reaped
                             (runs/settle-failed-run!
                              (ctx/at store branch) []
                              {:run-id id :branch branch :at (ctl/stamp store)
                               :error "force-reaped: hard-stopped"}))
                           {:run-id id :cancelled false :reaped (boolean reaped) :forced true})
                         (if (or (builds/cancel-queued! store id)
                                 (lock/cancel-queued! store id))
                           (do (live/emit! {:kind :build/dequeued :level :warn :run-id id})
                               {:run-id id :cancelled true :dequeued true})
                           (let [live?   (or (bm/request-cancel! id)
                                             (bm/request-cancel-elsewhere! store id))
                                 pending (when live?
                                           (first
                                            (cmd/execute!
                                             (ctx/at store branch)
                                             [(ledger/abort-pending-jobs id)])))
                                 reaped  (when-not live? (pos? (runs/reap-orphans! store 0)))]
                             (live/emit! {:kind :build/cancel-requested :level :warn :run-id id
                                          :jobs (or pending 0)})
                             {:run-id id :cancelled (boolean live?) :reaped (boolean reaped)
                              :abort-pending (or pending 0)}))))}))

(def retry-run
  (ep/handler :run-retry
              {:call (fn [{:keys [store branch id]}]
                       (let [{:keys [run-id counts]} (importer/retry-run! store branch id)]
                         {:retry-of id :run-id run-id :counts counts}))}))

(def run-jobs
  (ep/handler :run-jobs
              {:call (fn [{:keys [store branch schema auth id]}]
                       (visible-jobs auth schema (ledger/jobs-of-run (ctx/at store branch) id)))
               :wrap :jobs}))

(def preview-transform
  (ep/handler :preview-transform
              {:path {:name :kw}
               :respond
               (fn [{:keys [store schema branch name limit sample param output]}]
                 (let [overrides (into {}
                                       (keep (fn [s]
                                               (let [i (.indexOf (str s) "=")]
                                                 (when (pos? i)
                                                   [(keyword (subs (str s) 0 i))
                                                    (subs (str s) (inc i))]))))
                                       param)
                       {:keys [rows count error language input-caps input-sample]}
                       (transform-preview/preview-transform!
                        (ctx/at store branch) schema name
                        (cond-> {:limit limit :sample sample :output output}
                          (seq overrides) (assoc :params overrides)))]
                   (if (and error (empty? rows) (str/starts-with? (str error) "no transform named"))
                     (refusal/refuse! :refusal.builds/preview-no-transform {:transform name}
                                      {:error error})
                     (ok {:transform name :language language
                          :rows (vec rows) :count count :error error
                          :input-sample input-sample
                          :input-caps (vec input-caps)}))))}))

(def test-transform
  (ep/handler :test-transform
              {:path {:name :kw}
               :respond
               (fn [{:keys [store schema branch name]}]
                 (if-not (second (inputs/transform-entry schema name))
                   (refusal/refuse! :refusal.builds/no-transform-named {:transform name}
                                    {:transform-name (clojure.core/name name)})
                   (ok (unit-tests/run-tests (ctx/at store branch) schema name))))}))

(defn- forecast-ledger
  [store]
  {:observed      (ledger/last-observed-pending store (:branch store))
   :added-changed (fn [t-rid rid] (ledger/added-changed store t-rid rid (:branch store)))
   :transactions  (fn [rid] (ledger/list-transactions store rid (:branch store) 500))})

(def build-estimate
  (ep/handler :build-estimate
              {:call (fn [{:keys [store branch schema only from strategy days calls-per-row]}]
                       (let [{:keys [refusal scope]} (plan/build-plan
                                                      schema {:only only :from from
                                                              :strategy (some-> strategy keyword)})
                             _ (when refusal
                                 (refusal/refuse! :refusal.builds/build-scope
                                                  (or (:data refusal) {})
                                                  {:error (:error refusal)}))]
                         (assoc (let [store (ctx/at store branch)]
                                  (llm/estimate store schema (:build scope)
                                                {:days days :calls-per-row calls-per-row
                                                 :ledger (forecast-ledger store)}))
                                :scope (merge {:only (vec only)}
                                              (plan/scope-preview schema scope)))))}))

(def get-import-plan
  (ep/handler :import-plan
              {:call (fn [{:keys [schema]}]
                       (let [{:keys [plan unresolved]}
                             (plan/derive-plan-leniently schema)]
                         (cond-> {:plan plan}
                           (seq unresolved) (assoc :unresolved unresolved))))}))

(defn- merge-plan!
  [store branch f]
  (let [store  (ctx/at store branch)
        cur (or (projects/read-import-plan store) {:objects {} :links {}})
        nxt (f cur)]
    (projects/write-import-plan! store nxt)
    nxt))

(def upsert-object-import
  (ep/handler :import-objects-of
              {:path {:type :kw}
               :call (fn [{:keys [store branch type body]}]
                       (let [{:keys [from primary-key column-map transforms]} body
                             entry (cond-> {:from        (update from :datasource ->kw)
                                            :primary-key (->kw primary-key)
                                            :column-map
                                            (into {}
                                                  (for [[a c] column-map] [(->kw a) (->kw c)]))}
                                     (seq transforms) (assoc :transforms (vec transforms)))]
                         {:plan (merge-plan! store branch #(assoc-in % [:objects type] entry))}))}))

(def delete-object-import
  (ep/handler :import-objects-of
              {:path {:type :kw}
               :call (fn [{:keys [store branch type]}]
                       {:plan (merge-plan! store branch #(update % :objects dissoc type))})}))

(def upsert-link-import
  (ep/handler :import-conns-of
              {:path {:type :kw}
               :call (fn [{:keys [store branch type body]}]
                       (let [{:keys [from from-type from-key-column to-type to-key-column]} body
                             entry {:from            (update from :datasource ->kw)
                                    :from-type       (->kw from-type)
                                    :from-key-column (->kw from-key-column)
                                    :to-type         (->kw to-type)
                                    :to-key-column   (->kw to-key-column)}]
                         {:plan (merge-plan! store branch #(assoc-in % [:links type] entry))}))}))

(def delete-link-import
  (ep/handler :import-conns-of
              {:path {:type :kw}
               :call (fn [{:keys [store branch type]}]
                       {:plan (merge-plan! store branch #(update % :links dissoc type))})}))

(defn- preset-of
  [{:keys [only strategy from trace full profile]} options]
  (cond-> {}
    (seq only)                (assoc :only-types only)
    strategy                  (assoc :build-strategy (keyword strategy))
    (seq from)                (assoc :build-from from)
    trace                     (assoc :trace? true)
    profile                   (assoc :profile? true)
    true                      (merge options)
    full                      (assoc :full-refresh true)))

(defn- authorize-build!
  [auth schema targets]
  (if (seq (:only targets))
    (doseq [t (:jobs (plan/build-plan schema targets))]
      (scope/require! auth :update (get-in schema [:transforms t :project])
                      (str "Transform " (name t))))
    (scope/require! auth :update nil "Der Build")))

(defn run-import
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch
    auth :sutra/auth}]
  (let [only     (:only body-params)
        strategy (some-> (:strategy body-params) keyword)
        from     (:from body-params)
        refusal  (plan/build-scope-refusal schema
                                           {:only only :strategy strategy :from from})
        _        (when-not refusal
                   (authorize-build! auth schema {:only only :strategy strategy :from from}))
        options  (importer/build-options body-params)]
    (cond
      refusal
      (refusal/refuse! (:code refusal) (:data refusal) (:params refusal))

      (not (import-sources/read-spec (ctx/at store branch)))
      (refusal/refuse! :refusal.builds/no-import-plan {:branch branch})

      (:detach body-params)
      (ok {:run-id     (:run-id (importer/build! store branch
                                                 (preset-of body-params options) :detach))
           :status     "started"
           :branch     branch
           :started-at (ctl/clock store)})

      :else
      (let [{:keys [counts skipped run-id snapshot-id timings phase-timings trace scope
                    index-reconcile publish-failures]}
            (importer/install-from-preset! store branch (preset-of body-params options))]
        (ok (cond-> {:counts           counts
                     :run-id           run-id
                     :snapshot-id snapshot-id
                     :branch           branch
                     :ran-at           (ctl/clock store)}
              (seq timings)       (assoc :timings
                                         (mapv (fn [t]
                                                 (assoc t :name
                                                        (or
                                                         (lookup/name-of
                                                          schema :transforms (:transform t))
                                                         (:transform t))))
                                               timings))
              (seq phase-timings) (assoc :phase-timings phase-timings)
              scope               (assoc :scope scope)
              index-reconcile     (assoc :index-reconcile index-reconcile)
              (seq trace)         (assoc :trace (report/resolve-trace-names schema trace))
              (seq skipped)       (assoc :skipped skipped)
              (seq publish-failures) (assoc :publish-failures publish-failures)))))))

(def import-status
  (ep/handler :import-status
              {:call (fn [{:keys [store branch schema]}]
                       (let [store'   (ctx/at store branch)
                             named?   #(lookup/name-of schema :object-types (first %))
                             counts   (into {} (comp (map (juxt :object-type :n)) (filter named?))
                                            (projects/import-key-counts store'))
                             times    (into {} (filter named?) (projects/index-times store'))
                             per-type (into {}
                                            (for [t (distinct (concat (keys counts) (keys times)))]
                                              [(lookup/name-of schema :object-types t)
                                               {:count (get counts t 0) :last-at (get times t)}]))]
                         {:per-type per-type
                          :total    (reduce + 0 (vals counts))
                          :last-at  (last (sort (vals times)))}))}))

(defn data-freshness
  [{:keys [body-params] store :sutra/store schema :sutra/schema branch :sutra/branch
    auth :sutra/auth}]
  (ok {:updates (last-updated/of-items (ctx/at store branch) schema auth (:items body-params))}))

(def list-datasets
  (ep/handler :datasets {:call (fn [{:keys [schema]}] {:datasets (or (:datasets schema) {})})}))

(def get-dataset (ep/entry :dataset-by :datasets :refusal.builds/dataset-not-found))

(defn force-dataset-snapshot
  [{:keys [path-params body-params] store :sutra/store schema :sutra/schema branch :sutra/branch
    auth :sutra/auth}]
  (let [nm  (:name path-params)
        rid (lookup/rid-of schema :datasets (keyword nm))]
    (cond
      (nil? (get-in schema [:datasets rid]))
      (refusal/refuse! :refusal.http/unknown-dataset {:name nm})

      (nil? (nodes/producer-of schema rid))
      (refusal/refuse! :refusal.builds/no-producer {:name nm})

      :else
      (let [t (nodes/producer-of schema rid)]
        (ledger/request-snapshot! store {:transform-rid t :dataset-rid rid :branch branch
                                         :requested-by (some-> (grants/current-user-id auth) str)
                                         :message (:message body-params)})
        (ok {:dataset nm :queued true :transform (str t)
             :effect "the next build of this transform runs as a snapshot"})))))

(defn write-dataset-rows
  [{:keys [path-params body-params] store :sutra/store schema :sutra/schema branch :sutra/branch}]
  (let [nm  (:name path-params)
        rid (lookup/rid-of schema :datasets (keyword nm))
        ds  (get-in schema [:datasets rid])]
    (if (nil? ds)
      (refusal/refuse! :refusal.http/unknown-dataset {:name nm})
      (let [rows    (:rows body-params)
            mode    (keyword (or (:mode body-params) :append))
            key-col (some-> (:key body-params) ->kw)]
        (if-let [{:keys [message data]} (dsrows/write-problem rid ds nm rows mode key-col)]
          (refusal/refuse! :refusal.builds/rows-refused (or data {}) {:message message})
          (let [store (ctx/at store branch)]
            (dsets/ensure-datasets! store {rid (:columns ds)})
            (dsets/write-rows! store rid (if (= mode :snapshot) :snapshot :append)
                               (dsrows/records nm key-col rows))
            (ok {:dataset nm :written (count rows)
                 :mode mode :key key-col :branch branch})))))))

(defn- restricted-view-rows
  [store schema ds as-of]
  (let [{x :dataset policy :policy} (:restricted-view ds)
        backing (if (contains? (:datasets schema) x) x (lookup/rid-of schema :datasets x))]
    (granular/restricted-rows policy (grants/read-rights store)
                              (dsets/preview-rows store backing as-of
                                                  (get-in schema [:datasets backing :columns])))))

(def dataset-preview
  (ep/handler :dataset-preview
              {:respond (fn [{:keys [store branch schema id as-of]}]
                          (let [ds (get-in schema [:datasets id])]
                            (cond
                              (nil? ds)
                              (refusal/refuse! :refusal.http/unknown-dataset {:id id})

                              (and (vtypes/virtual? ds) as-of)
                              (refusal/refuse! :refusal.builds/virtual-no-versions
                                               {:id id :as-of as-of})

                              :else
                              (try
                                (cond
                                  (= :restricted-view (:kind ds))
                                  (ok {:dataset id :kind :restricted-view :as-of as-of
                                       :rows (restricted-view-rows (ctx/at store branch)
                                                                   schema ds as-of)})

                                  (vtypes/virtual? ds)
                                  (ok {:dataset id :kind :virtual :columns (:columns ds)
                                       :virtual (:virtual ds)
                                       :rows (vec (map-indexed (fn [i r] {:id i :values r})
                                                               (virtual/rows
                                                                (ctx/at store branch) schema ds
                                                                sources/max-rows)))})

                                  :else
                                  (ok {:dataset id :kind (:kind ds) :columns (:columns ds)
                                       :as-of as-of
                                       :rows
                                       (dsets/preview-rows
                                        (ctx/at store branch) id as-of (:columns ds))}))
                                (catch InterruptedException e (throw e))
                                (catch Exception e
                                  (fault e {:id id :as-of as-of}))))))}))

(def list-sources
  (ep/handler :sources
              {:call (fn [{:keys [store schema]}]
                       {:sources (import-sources/sources-with-consumers store schema)})}))

(def transform-history
  (ep/handler :transform-history
              {:path {:name :kw}
               :call (fn [{:keys [store branch name]}]
                       {:transform name :runs (runs/list-recent (ctx/at store branch) 50 nil)})}))

(def reindex-object-type
  (ep/handler :object-type-reindex
              {:respond
               (fn [{:keys [store branch schema type force]}]
                 (let [rid    (lookup/->type-rid schema :object-types (->kw type))]
                   (if-let [{:keys [dataset]} (and rid (backing/object-backing schema rid))]
                     (let [delta      (changelog/object-removals (ctx/at store branch) rid dataset
                                                                 {:propagate-empty? true})
                           candidates (:removed delta)]
                       (if (and (seq candidates) (not force))
                         (refusal/refuse! :refusal.builds/reindex-would-delete
                                          {:object-type type
                                           :would-delete (count candidates)
                                           :sample (vec (take 10 candidates))})
                         (let [deleted (do (world/index! (world/at store branch)
                                                         (mapv (fn [id]
                                                                 {:op :delete-object :type rid
                                                                  :payload {:id (str id)}})
                                                               candidates))
                                           (count candidates))]
                           (ok {:object-type type :deleted (or deleted 0)}))))
                     (refusal/refuse! :refusal.builds/no-backing-to-reindex {:type type}))))}))

(def object-type-edit-history
  (ep/handler :object-type-edit-history
              {:respond
               (fn [{:keys [store branch schema type limit pk token sort kinds editors props
                            timeline auth]}]
                 (if-let [rid (lookup/->type-rid schema :object-types (->kw type))]
                   (let [store (ctx/at store branch)
                         {:keys [history total next-token]}
                         (objects/edit-history-page store rid
                                                    {:limit limit :pk pk :token token :sort sort
                                                     :kinds kinds :editors editors :props props
                                                     :authority auth})]
                     (ok (cond-> {:object-type     type
                                  :history         history
                                  :total           total
                                  :next-page-token next-token}
                           (and timeline pk)
                           (assoc :timeline (objects/edit-timeline store schema rid
                                                                   {:pk pk :authority auth}))
                           (nil? pk)
                           (assoc :transactions (ledger/list-transactions store rid branch limit)
                                  :provenance (world/edit-provenance (world/at store branch) rid
                                                                     {:authority auth})))))
                   (refusal/refuse! :refusal.http/unknown-object-type {:type type})))}))

(def object-type-drop-edits
  (ep/handler :object-type-drop-edits
              {:respond
               (fn [{:keys [store branch schema type]}]
                 (let [rid    (lookup/->type-rid schema :object-types (->kw type))]
                   (if-let [{:keys [dataset]} (and rid (backing/object-backing schema rid))]
                     (let [res (let [store      (ctx/at store branch)
                                     cleared (count (world/edited (world/at store branch) rid))
                                     _       (world/forget-edits! (world/at store branch) [rid])
                                     delta   (changelog/object-removals store rid dataset
                                                                        {:propagate-empty? true})
                                     swept   (count (:removed delta))
                                     _       (world/index! (world/at store branch)
                                                           (mapv (fn [id]
                                                                   {:op :delete-object :type rid
                                                                    :payload {:id (str id)}})
                                                                 (:removed delta)))]
                                 {:edits-cleared cleared :rows-swept (or swept 0)})]
                       (ok (assoc res :object-type type)))
                     (refusal/refuse! :refusal.builds/no-backing-to-reset {:type type}))))}))

(op/defop :builds/schema-plan [{:keys [store schema]} _]
  (let [{p :plan :keys [unresolved]} (plan/derive-plan-leniently schema)]
    {:import-plan (plan/sources-only p)
     :sources     (import-sources/sources-with-consumers store schema)
     :unresolved  unresolved}))

(def module
  {:name        :conductor
   :contributes {:http/routes
                 {:build-estimate       {:get build-estimate}
                  :data-freshness       {:post data-freshness}
                  :object-type-drop-edits   {:post object-type-drop-edits}
                  :object-type-edit-history {:get object-type-edit-history}
                  :object-type-reindex      {:post reindex-object-type}
                  :transform-history        {:get transform-history}
                  :builds-active        {:get active-builds}
                  :catalog-consumption  {:get dataset-consumption}
                  :catalog-delta        {:get dataset-delta}
                  :catalog-transactions {:get dataset-transactions}
                  :dataset-by           {:get get-dataset}
                  :sources              {:get list-sources}
                  :dataset-preview      {:get dataset-preview}
                  :dataset-force-snapshot {:post force-dataset-snapshot}
                  :dataset-write        {:post write-dataset-rows}
                  :dataset-export       {:get files/dataset-export}
                  :dataset-import       {:post files/dataset-import}
                  :datasets             {:get list-datasets}
                  :import-conns-of      {:post upsert-link-import :delete delete-link-import}
                  :import-objects-of    {:post upsert-object-import :delete delete-object-import}
                  :import-plan          {:get get-import-plan}
                  :import-poll          {:post reindex-poll}
                  :import-run           {:post run-import}
                  :import-run-by        {:get get-run}
                  :import-runs          {:get list-runs}
                  :import-status        {:get import-status}
                  :ledger-consumption   {:get dataset-consumption}
                  :ledger-delta         {:get dataset-delta}
                  :ledger-transactions  {:get dataset-transactions}
                  :live                 {:get live-trace}
                  :llm-audit            {:get llm-audit}
                  :llm-cost             {:get llm-cost}
                  :preview-transform    {:get preview-transform}
                  :test-transform       {:get test-transform}
                  :run                  {:get get-run}
                  :run-cancel           {:post cancel-run}
                  :run-jobs             {:get run-jobs}
                  :run-retry            {:post retry-run}
                  :runs                 {:get list-runs}}}})
