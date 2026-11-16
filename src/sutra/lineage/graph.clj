(ns sutra.lineage.graph
  (:require [sutra.lineage.ports :as ports]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.nodes :as nodes]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.store.access.scope :as ctx]
            [sutra.kit.time :as time]
            [clojure.string :as str]
            [clojure.set]
            [sutra.parser.impact :as impact]
            [sutra.types.kinds :as kinds]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.transform-io :as tio]
            [sutra.types.virtual :as vtypes]
            [sutra.viz.lineage-projection :as lp]
            [sutra.builds.api :as builds]
            [sutra.llm.api :as llm]
            [sutra.schedules.api :as schedules]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.lookup :as lookup]
            [sutra.projects.api :as projects]
            [sutra.capps.catalog :as cat]
            [sutra.capps.refs :as refs]
            [sutra.sources.api :as sources]))

(set! *warn-on-reflection* true)

(def ^:private node-key lp/status-key)

(def ^:private nm rid/->name)

(defn- node-of
  [idx r fallback-kind]
  (if-let [section (sch/section-of idx r)]
    {:kind (rid/kinds-key->tag section) :name (nm (sch/name-of idx r))}
    {:kind fallback-kind :name (or (nm r) (nm fallback-kind))}))

(defn- ref->node
  [idx side ref]
  (let [k  (:kind ref)
        at (get-in kinds/kinds [k side :at] k)
        r  (get ref at)]
    (if (= k :language-model)
      {:kind k :name (nm (:model ref))}
      (cond-> (merge {:rid r} (node-of idx r k) (select-keys ref [:table :query?]))
        (not= at k) (assoc :kind k)))))

(def job-status
  {"COMPLETED"     "succeeded"
   "RUN_PENDING"   "waiting"
   "FAILED"        "failed"
   "RUNNING"       "running"
   "ABORT_PENDING" "running"
   "WAITING"       "waiting"
   "DID_NOT_RUN"   "skipped"
   "ABORTED"       "aborted"})

(defn- ->inst
  [s]
  (when-let [s (some-> s str not-empty)]
    (time/->epoch-ms s)))

(defn job->build
  [{:keys [status attempt run_id rows_in rows_out error job_log
           code_sha snapshot_id started_at ended_at]}]
  (let [t0 (->inst started_at) t1 (->inst ended_at)]
    {:build-status (get job-status (builds/read-job-state status) "never")
     :last-build   (or (not-empty (str ended_at)) (not-empty (str started_at)))
     :started-at   (not-empty (str started_at))
     :duration-ms  (when (and t0 t1 (>= t1 t0)) (- t1 t0))
     :rows-out     rows_out
     :rows-in      rows_in
     :attempt      attempt
     :run-id       (some-> run_id str)
     :code-sha     (some-> code_sha str not-empty)
     :commit       (some-> snapshot_id str not-empty)
     :error        (some-> error str not-empty)
     :log?         (boolean (some-> job_log str not-empty))}))

(defn txn->data
  [{:keys [type row_count row_delta created txns snapshot_id ended_at]}]
  {:rows          row_count
   :row-delta     row_delta
   :created-at    (not-empty (str created))
   :transactions  (some-> txns long)
   :last-write    (some-> type str not-empty)
   :last-write-at (not-empty (str ended_at))
   :commit        (some-> snapshot_id str not-empty)})

(defn producers
  [schema]
  (let [idx (sch/index schema)]
    (reduce (fn [m [rid t]]
              (reduce (fn [m ref]
                        (let [{:keys [kind name]} (ref->node idx :output-ref ref)]
                          (cond-> m
                            (and kind name)
                            (update (node-key kind name) (fnil conj #{}) rid))))
                      m (impact/transform-outputs t)))
            {} (:transforms schema))))

(defn- sum-usage
  [parts]
  (let [n (fn [k] (reduce + 0 (keep #(get % k) parts)))
        costs (keep :cost parts)]
    {:attempted    (n :attempted)
     :successful   (n :successful)
     :rate-limited (n :rate-limited)
     :failed       (n :failed)
     :prompt-tokens (n :prompt-tokens)
     :completion-tokens (n :completion-tokens)
     :tokens       (n :tokens)
     :cost         (when (seq costs) (reduce + 0.0 costs))
     :currency     (or (some :currency parts) "USD")
     :models       (vec (sort (distinct (mapcat :models parts))))}))

(defn spread-usage
  [schema usage]
  (let [prod (producers schema)]
    (reduce-kv
     (fn [acc k rids]
       (let [parts (keep #(get usage
                               (lp/status-key :transform
                                              (nm (:transform-name (get (:transforms schema) %)))))
                         rids)]
         (cond-> acc (seq parts) (assoc k (sum-usage parts)))))
     usage prod)))

(defn build-index
  [schema jobs txns]
  (let [prod    (producers schema)
        newest  (fn [builds]
                  (->> builds (sort-by #(or (:last-build %) "")) last))
        from-producers
        (fn [k] (when-let [rids (seq (get prod k))]
                  (some->> rids
                           (keep #(get jobs (str %)))
                           seq
                           (map job->build)
                           newest)))
        ->tx (into {} (for [[rid t] txns] [(str rid) (txn->data t)]))]
    (reduce
     (fn [acc [kind rid node-name]]
       (let [k (node-key kind node-name)
             build (if (= :transform kind)
                     (some-> (get jobs (str rid)) job->build)
                     (from-producers k))
             data  (when (= :dataset kind) (get ->tx (str rid)))
             merged (when (or build data)
                      (let [m (merge data build)]
                        (cond-> m (nil? (:rows m)) (assoc :rows (:rows-out build)))))]
         (cond-> acc merged (assoc k merged))))
     {}
     (concat (for [[rid v] (:transforms schema)]   [:transform rid (:transform-name v)])
             (for [[rid v] (:datasets schema)]     [:dataset rid (:dataset-name v)])
             (for [[rid v] (:object-types schema)] [:object-type rid (:object-type-name v)])))))

(defn schedule-counter
  [schedules]
  (let [targets (fn [s] (into #{}
                              (comp (map str/trim) (remove str/blank?))
                              (str/split (str (:target s)) #",")))
        by-name (reduce (fn [m s]
                          (reduce (fn [m t] (update m t (fnil inc 0)))
                                  m (targets s)))
                        {} schedules)]
    (fn [k]
      (let [slash (.indexOf ^String k "/")]
        (get by-name (if (neg? slash) k (subs k (inc slash))) 0)))))

(defn status
  [store sch branch]
  (let [rs      (builds/list-recent (ctx/at store branch) 200 nil)
        succ    (filter #(= "succeeded" (:status %)) rs)
        latest  (fn [kind] (->> succ (filter #(= kind (:kind %))) first :finished-at))
        last-build  (latest :import)
        last-logic  (latest :publish)
        [jobs txns] (let [store (ctx/at store branch)]
                      [(builds/last-job-per-transform store branch)
                       (builds/last-txn-per-dataset store branch)])
        builds   (build-index sch jobs txns)
        usage    (spread-usage sch (llm/usage-by-node (ctx/at store branch) sch))
        sched-of (schedule-counter (schedules/list-all store branch))
        hsum     (ports/node-health store branch)
        health-of (fn [kind nm]
                    (get hsum [(name kind) (name nm)]))
        derived-node
        (fn [kind nm]
          (let [k (node-key kind nm)
                b (get builds k)]
            [k (cond-> {:build-status (:build-status b "never")
                        :last-build   (:last-build b)
                        :schedules    (sched-of k)
                        :health       (health-of kind nm)}
                 b                (merge (dissoc b :build-status :last-build))
                 (get usage k)    (assoc :model-usage (get usage k)))]))
        trashed  (into #{} (keep (fn [[ref a]] (when (:trashed a) (str ref))))
                       (projects/read-kompass-annotations (ctx/at store branch)))
        trashed? (fn [ref] (contains? trashed (str ref)))
        idx      (sch/index sch)
        syncs    (frequencies (keep #(some->> (:dataset %) (sch/name-of idx) nm)
                                    (vals (:exports sch))))
        mark     (fn [[k m] ref] [k (cond-> m (trashed? ref) (assoc :trashed? true))])
        ots (for [[rid v] (:object-types sch)]
              (mark (derived-node :object-type (:object-type-name v)) rid))
        txs (for [v (vals (:transforms sch))]
              (derived-node :transform (:transform-name v)))
        dss (for [[rid v] (:datasets sch)
                  :let [nm* (nm (:dataset-name v))]]
              (-> (if (vtypes/virtual? v)
                    [(node-key :dataset (:dataset-name v))
                     {:build-status nil :out-of-date? false :last-build nil
                      :virtual (select-keys (:virtual v) [:source :table])
                      :schedules (sched-of (node-key :dataset (:dataset-name v)))
                      :health (health-of :dataset (:dataset-name v))}]
                    (derived-node :dataset (:dataset-name v)))
                  (mark rid)
                  (update 1 #(cond-> % (get syncs nm*) (assoc :syncs (get syncs nm*))))))
        bare (fn [kind nm* rid] (mark [(node-key kind nm*) {}] rid))
        apps (for [[rid v] (:applications sch)]
               (bare :application (or (:application-name v) rid) rid))
        acts (for [[rid v] (:actions sch)]
               (bare :action (or (:action-name v) rid) rid))
        src-health (fn [spec nm]
                     (let [conn (sources/spec-health spec)]
                       (if (= "misconfigured" (:status conn))
                         {:status "failed" :kind "connection"
                          :code (:code conn) :reason (:message conn)}
                         (health-of :datasource nm))))
        srcs (for [v (vals (:sources sch))
                   :let [nm (:source-name v) k (node-key :datasource nm)]]
               (mark [k {:build-status nil :out-of-date? false :last-build nil
                         :schedules (sched-of k) :health (src-health v nm)}]
                     (str "datasource/" (name nm))))]
    {:last-build        last-build
     :last-logic-change last-logic
     :nodes (into {} (concat ots txs srcs dss apps acts))}))

(defn- inputs-of
  [t]
  (mapv (fn [in]
          (cond-> in
            (= :datasource (:kind in))
            (merge (select-keys (:source t) [:table])
                   (when (seq (:query (:source t))) {:query? true}))))
        (remove #(kinds/is? :call-input? (:kind %)) (tio/inputs-of t))))

(defn edges
  [schema]
  (let [idx (sch/index schema)]
    (->> (:transforms schema)
         (map (fn [[_ t]]
                (cond-> {:name    (nm (:transform-name t))
                         :inputs  (mapv #(ref->node idx :input-ref %) (inputs-of t))
                         :outputs (mapv #(ref->node idx :output-ref %)
                                       (impact/transform-outputs t))}
                  (seq (:external-systems t))
                  (assoc :external-systems (mapv nm (:external-systems t))))))
         (sort-by :name)
         vec)))

(defn anchored
  [edges node]
  (let [want (nm node)
        hit? (fn [n] (= want (str (:name n))))]
    {:node want
     :produced-by (vec (for [e edges :when (some hit? (:outputs e))] (:name e)))
     :consumed-by (vec (for [e edges :when (some hit? (:inputs e))] (:name e)))}))

(defn- authored-backings
  [schema]
  (let [idx (sch/index schema)
        ->n (fn [r] (nm (or (sch/name-of idx r) r)))]
    (for [[ot spec] (:object-types schema)
          :let [ds (or (get-in spec [:backing :dataset]) (:dataset spec))]
          :when (and ds (not= :object-backing (get-in schema [:datasets ds :kind])))]
      {:object-type (->n ot) :dataset (->n ds)})))

(defn- backing-edges
  [schema want]
  (let [bs     (authored-backings schema)
        dss    (vec (for [b bs :when (= want (:object-type b))] (:dataset b)))
        types  (vec (sort (for [b bs :when (= want (:dataset b))] (:object-type b))))
        vds    (first (for [[_ ds] (:datasets schema)
                            :when (and (vtypes/virtual? ds) (= want (nm (:dataset-name ds))))]
                        ds))]
    (cond-> {}
      (seq dss)   (assoc :backed-by (first dss))
      (seq types) (assoc :backs types)
      vds         (assoc :reads-source
                         {:source (nm (vtypes/source-name vds)) :table (vtypes/table vds)}))))

(defn action-edits
  [schema]
  (let [idx (sch/index schema)]
    (into {} (for [[an a] (lookup/actions-by-name schema)]
               [(->kw an) (into #{} (for [[k t] (impact/action-targets a)
                                          :when (= :object-type k)]
                                      (->kw (or (sch/name-of idx t) t))))]))))

(defn app-uses
  [schema]
  (let [idx   (sch/index schema)
        ->ot  (fn [t] (->kw (or (sch/name-of idx t) t)))
        tname (fn [t] (some-> (sch/name-of idx (str t)) ->kw))
        ots   (into #{} (map (comp ->kw :object-type-name)) (vals (:object-types schema)))]
    (into {} (for [[k a] (:applications schema)
                   :let [lifted (refs/lift-application a tname)
                         used (->> (concat (cat/scope-tokens lifted :type)
                                           (cat/referenced-types (:layout lifted)))
                                   (remove nil?)
                                   (map ->ot)
                                   (filter #(contains? ots %))
                                   set)]
                   :when (seq used)]
               [(->kw (or (:application-name a) k)) used]))))

(defn graph
  ([schema] (graph schema nil))
  ([schema node]
   (let [es (edges schema)]
     (if-let [want (nm node)]
       (if-let [self (first (filter #(= want (str (:name %))) es))]
         {:transform want :inputs (:inputs self) :outputs (:outputs self)}
         (merge (anchored es want) (backing-edges schema want)))
       {:transforms   es
        :action-edits (action-edits schema)
        :app-uses     (app-uses schema)
        :backings     (vec (authored-backings schema))}))))

(defn- backing-object-types
  [schema ds-rid]
  (when ds-rid
    (let [idx (sch/index schema)
          ds  (nm (sch/name-of idx ds-rid))]
      (vec (distinct
            (concat
             (for [[_ t] (:transforms schema)
                   out   (impact/transform-outputs t)
                   :when (and (= :object-type (:kind out))
                              (= ds-rid (:dataset out)))
                   :let  [n (nm (sch/name-of idx (:object-type out)))]
                   :when n]
               n)
             (for [b (authored-backings schema) :when (= ds (:dataset b))]
               (:object-type b))))))))

(defn- rid-of-node
  [schema kind nm*]
  (when-let [section (rid/tag->kinds-key (keyword kind))]
    (sch/rid-of (sch/index schema) section (keyword nm*))))

(defn related
  [schema kind node]
  (let [want (nm node)
        k    (keyword (or kind :object-type))
        es   (edges schema)
        hit? (fn [n] (= want (str (:name n))))
        edge-items
        (concat
         (for [e es :when (some hit? (:outputs e))]
           {:kind "transform" :name (:name e) :via "produces"})
         (for [e es :when (some hit? (:inputs e))]
           {:kind "transform" :name (:name e) :via "consumes"}))
        self (when (= :transform k) (first (filter #(= want (str (:name %))) es)))
        own  (when self
               (concat (for [i (:inputs self)]
                         {:kind (nm (:kind i)) :name (nm (:name i)) :via "reads"})
                       (for [o (:outputs self)]
                         {:kind (nm (:kind o)) :name (nm (:name o)) :via "writes"})))
        dep  (when (= :object-type k) (impact/dependents-of schema want))
        backed-by (when (= :object-type k) (:backed-by (backing-edges schema want)))
        type-items
        (concat (for [l (:links dep)]  {:kind "link-type"   :name (nm (:link l))   :via "links"})
                (for [a (:actions dep)] {:kind "action"      :name (nm (:action a)) :via "edits"})
                (for [a (:apps dep)]
                  {:kind "application" :name (nm (:app a))    :via "surfaces"}))
        ds-items
        (if backed-by
          [{:kind "dataset" :name backed-by :via "backed-by"}]
          (when (= :dataset k)
            (for [ot (backing-object-types schema (rid-of-node schema :dataset want))]
              {:kind "object-type" :name ot :via "backs"})))
        items (->> (concat own edge-items type-items ds-items)
                   (remove #(and (= want (:name %)) (= (nm k) (:kind %))))
                   distinct
                   (sort-by (juxt :kind :name :via))
                   vec)]
    {:node want :kind (nm k) :items items :total (count items)}))

(defn watch-candidates
  [edges target]
  (let [want       (nm target)
        watchable? #(kinds/is? :watchable? %)
        self       (first (filter #(= want (str (:name %))) edges))
        inputs     (if self
                     (:inputs self)
                     (mapcat :inputs
                             (filter #(some (fn [o] (= want (str (:name o)))) (:outputs %))
                                     edges)))]
    (->> inputs (filter (comp watchable? :kind)) distinct vec)))

(defn dependents
  [schema type]
  (let [me (nm type)]
    (or (impact/dependents-of schema me)
        {:object-type me :links [] :transforms [] :actions [] :apps [] :total 0})))

(defn- node-table-or-throw [schema node-kind node-key]
  (or (nodes/node->table schema node-kind node-key)
      (refusal/refuse! :refusal.lineage/no-rollback-table
                       {:node-kind node-kind :node-key node-key})))

(defn- version-token-or-throw
  [commit]
  (let [s (str commit)]
    (when-not (and (seq s) (or (parse-long s) (re-matches #"[0-9a-z]{8,64}" s)))
      (refusal/refuse! :refusal.lineage/rollback-needs-version {:commit commit}))
    s))

(defn- rollback-unsupported!
  [schema node-kind node-key]
  (let [backing (when (= "object-type" (name node-kind))
                  (some->> (lookup/rid-of schema :object-types (keyword node-key))
                           (backing/authored-backing-dataset schema)
                           (#(get-in schema [:datasets % :dataset-name]))
                           name))]
    (if (= "object-type" (name node-kind))
      (refusal/refuse! :refusal.lineage/rollback-object-type
                       {:node-kind node-kind :node-key node-key :backing backing}
                       {:backing-text (if backing (str " " backing) "")})
      (refusal/refuse! :refusal.lineage/rollback-unsupported
                       {:node-kind node-kind :node-key node-key :backing backing}))))

(defn rollback!
  [store schema branch {:keys [node-kind node-key commit]}]
  (let [token (version-token-or-throw commit)
        tbl   (node-table-or-throw schema node-kind node-key)
        dsrs  (or (nodes/node->datasets schema node-kind node-key)
                  (rollback-unsupported! schema node-kind node-key))
        named (fn [ds] (some-> (get-in schema [:datasets ds :dataset-name]) name))
        warn  (first (keep #(builds/rollback-check (ctx/at store branch)
                                                   (get-in schema [:datasets %])
                                                   % (or (named %) node-key) token)
                           dsrs))]
    (builds/with-run!
      [[store _] (ctx/at store branch)
       {:kind :rollback :inputs {:node-kind node-kind :node-key node-key :commit commit}}]
      (let [results (mapv #(dsets/rollback-to! store % token) dsrs)
            {:keys [rows-before rows-after]} (first results)
            rewound (into {}
                          (keep (fn [t]
                                  (when-let [r (builds/rewind-producer!
                                                store
                                                {:transform-rid t :branch branch
                                                 :dataset-rid (first dsrs) :snapshot-id token})]
                                    [t r])))
                          (distinct (keep #(nodes/producer-of schema %) dsrs)))]
        {:summary (cond-> {:table tbl :commit commit
                           :rows-before rows-before :rows-after rows-after}
                    (next dsrs)    (assoc :outputs (mapv named dsrs))
                    (seq rewound)  (assoc :producers rewound)
                    warn           (assoc :warning warn))}))))
