(ns sutra.health.checks
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.catalog.nodes :as nodes]
            [sutra.store.control :as ctl]
            [sutra.repos.api :as repos]
            [sutra.types.column :as column]
            [clojure.string :as str]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.lookup :as lookup]
            [sutra.builds.api :as builds]
            [sutra.store.access.datasets :as dsets]
            [sutra.types.health-metric :as hm]
            [sutra.types.opid :as opid]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.store.access.substrate :as sub]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.kit.time :as ktime]
            [sutra.store.control.cadence :as cadence]
            [sutra.ref.value :as rv]
            [sutra.types.witness :as wit]
            [sutra.objects.api :as objects]
            [sutra.health.ports :as ports]))

(set! *warn-on-reflection* true)
(defn- grade* [n {:keys [op value]}]
  (let [ok? (case (keyword op)
              :>  (and n (> n value))
              :>= (and n (>= n value))
              :<  (and n (< n value))
              :<= (and n (<= n value))
              :=  (and n (= n value))
              :nonzero (and n (not (zero? n)))
              (some? n))]
    (if ok? "passed" "failed")))

(defn grade
  [v expects]
  (grade* (if (number? v) v (try (Long/parseLong (str v)) (catch NumberFormatException _ nil)))
          expects))

(defn grade-num
  [v expects]
  (grade* (if (number? v) v (try (Double/parseDouble (str v))
                                 (catch NumberFormatException _ nil)))
          expects))

(defn- rider-bound [rider r]
  (some (fn [[[op k d] last?]]
          (let [v (or (get r k) d)]
            (when (or (some? v) last?) [op v])))
        (map vector rider (concat (repeat (dec (count rider)) false) [true]))))

(defn rider->check
  [{:keys [name target on kind severity column within-hours min max comparison query paused
           measure against tolerance]}]
  (let [[op value] (rider-bound (:rider (hm/kind kind))
                                {:within-hours within-hours :min min :max max :tolerance tolerance})
        expects (cond-> {:severity (or severity :moderate)}
                  (some? op)  (assoc :op op :value value)
                  column      (assoc :column column)
                  comparison  (assoc :comparison comparison)
                  measure     (assoc :measure measure)
                  against     (assoc :against against))]
    {:name      name
     :node-kind (or on :object-type)
     :node-key  (some-> target clojure.core/name)
     :kind      (clojure.core/name kind)
     :query     (when (= kind :custom) query)
     :expects   expects
     :enabled   (not (true? paused))}))

(defn- row->check [row]
  (-> (dissoc row :r-seq :r-status :r-value :r-detail :r-run-at)
      (assoc :severity (get-in row [:expects :severity]))))

(defn- joined->latest [row]
  (when (:r-seq row)
    {:seq (:r-seq row) :check-id (:id row) :status (:r-status row)
     :value (:r-value row) :detail (:r-detail row) :run-at (:r-run-at row)}))

(defn health-checks
  [store branch]
  (let [b       (or branch ctl/trunk)
        latest  (->> (ctl/list store :health-check-results {:branch b})
                     (group-by :check-id)
                     (into {}
                           (map (fn [[k rs]]
                                  [k
                                   (last
                                    (sort-by
                                     (juxt #(some-> ^java.util.Date (:run-at %) .getTime) :seq)
                                     rs))]))))]
    (mapv (fn [c] (let [x (clojure.core/get latest (:id c))]
                    (cond-> c x (assoc :r-seq (:seq x) :r-status (:status x) :r-value (:value x)
                                       :r-detail (:detail x) :r-run-at (:run-at x)))))
          (ctl/list store :health-checks {:branch b}))))

(defn delete-health-check! [store branch id]
  (ctl/delete! store :health-check-results {:branch branch :check-id id})
  (ctl/delete! store :health-checks {:branch branch :id id})
  nil)

(defn list-all [store branch]
  (mapv #(assoc (row->check %) :latest (joined->latest %))
        (health-checks store branch)))

(defn results [store branch check-id]
  (ctl/list store :health-check-results
            {:branch branch :check-id check-id} {:limit 50}))

(defn upsert!
  [store branch {:keys [id name node-kind node-key kind query expects enabled]}]
  (let [id (or id (opid/name-id :health-check name))]
    (row->check
     (cadence/upsert! store branch "health_checks"
                      {:id id :name name :node-kind (clojure.core/name node-kind)
                       :node-key (clojure.core/name node-key)
                       :kind (or (some-> kind clojure.core/name) "row-count")
                       :query query :expects expects
                       :enabled (boolean (if (nil? enabled) true enabled))}))))

(defn expectation-records
  [store {:keys [branch check result]}]
  (let [{:keys [id name node-key expects]} check
        {:keys [status value detail]}     result]
    (into (:records (ctl/upserting store :health-checks
                                   [{:branch branch :id id :name name :node-kind "transform"
                                     :node-key node-key :kind "expectation" :query nil
                                     :enabled true :expects expects}]
                                   (ctl/clock store)))
          [(ctl/entry store :health-check-results
                      {:branch branch :check-id id :status status :value value
                       :detail detail})])))

(defn delete! [store branch id]
  (delete-health-check! store branch id)
  {:deleted id})

(defn- safe-select?
  [q]
  (and (string? q)
       (re-matches #"(?is)\s*select\b.*" q)
       (not (re-find #";" q))
       (not (re-find #"(?i)\b(insert|update|delete|drop|alter|create|call|grant|truncate)\b" q))))

(defn- node->object-type
  [schema node-kind node-key]
  (when (= :object-type (keyword node-kind))
    (lookup/rid-of schema :object-types (keyword node-key))))

(defn- object-metric
  [store rid col metric]
  (if (= :row-count (keyword metric))
    (sub/stored-count store rid)
    (let [rows (sub/read-type (ctx/as-system store) rid nil)
          k    (when col (keyword (str/replace (str col) "_" "-")))]
      (hm/of-rows metric rows (when col (map #(or (rv/value % (keyword col)) (rv/value % k)) rows))
                  col (ctl/instant store)))))

(def ^:private node-ref-re
  #"\{\{\s*(?:(object-type|transform|dataset)\s*:)?\s*([^{}\s:]+)\s*\}\}")

(defn resolve-query
  [schema q land]
  (if-not (string? q)
    {:query q :tables [] :landed []}
    (let [seen   (volatile! [])
          landed (volatile! [])
          q'     (str/replace q node-ref-re
                              (fn [[whole kind nm]]
                                (let [kind (or kind "object-type")
                                      ot   (when (= "object-type" kind)
                                             (lookup/rid-of schema :object-types (keyword nm)))
                                      t    (or (if ot
                                                 (doto (land ot) (->> (vswap! landed conj)))
                                                 (nodes/node->table schema kind nm))
                                               (refusal/refuse! :refusal.health/unknown-node
                                                                {:ref whole :node nm :kind kind}))]
                                  (vswap! seen conj t)
                                  t)))]
      {:query q' :tables @seen :landed @landed})))

(defn- measure-node
  [store schema node-kind node-key col metric]
  (let [dsr (nodes/node->dataset schema node-kind node-key)
        ot  (node->object-type schema node-kind node-key)]
    (cond
      dsr   (dsets/scalar-metric store dsr col metric)
      ot    (object-metric store ot col metric)
      :else (anomaly/raise! :fault (str "no table for " (name node-kind) " " node-key) {}))))

(defn- reconcile
  [store schema chk col]
  (let [{:keys [against measure]} (:expects chk)
        metric (if (= "sum" (some-> measure name)) :sum :row-count)
        b-kind (some-> (:on against) name)
        b-key  (some-> (:target against) name)
        b-col  (some-> (or (:column against) (:column (:expects chk))) column/attr->col name)
        label  (fn [k nm c] (str k " " nm " " (if (= :sum metric) (str "sum(" c ")") "count")))]
    (cond
      (not (and b-kind b-key))
      {:status "failed" :detail "a reconcile check needs :against {:on … :target …}"}

      (and (= :sum metric) (not (and col b-col)))
      {:status "failed"
       :detail "a :sum reconcile needs a :column (and :against :column when it differs)"}

      :else
      (let [a (measure-node store schema (:node-kind chk) (:node-key chk) col metric)
            b (measure-node store schema b-kind b-key b-col metric)
            d (Math/abs (- (double (or a 0)) (double (or b 0))))]
        {:status (grade-num d (merge {:op :<= :value 0} (select-keys (:expects chk) [:op :value])))
         :value  (str d)
         :detail (str (label (:node-kind chk) (:node-key chk) col) " = " a " vs "
                      (label b-kind b-key b-col) " = " b " — difference " d)}))))

(defn- run-check-now!
  [store schema branch check-id chk kind]
  (let [{:keys [status value detail]}
        (try
          (let [tbl      (nodes/node->table   schema (:node-kind chk) (:node-key chk))
                dsr      (nodes/node->dataset schema (:node-kind chk) (:node-key chk))
                col      (some-> (:column (:expects chk)) column/attr->col clojure.core/name)
                q        (:query chk)
                {:keys [pending? column? float? custom? reconcile?] :as row} (hm/kind kind)
                grade-fn (if float? grade-num grade)
                graded   (fn [v] {:status (grade-fn v (or (:expects chk) {:op :nonzero}))
                                  :value  (str v)})]
            (cond
              reconcile?
              (reconcile store schema chk col)

              pending?
              {:status "warning"
               :detail (str kind " check: evaluator pending (declaration only)")}

              (nil? tbl)
              {:status "failed" :detail "no table for node"}

              custom?
              (if-not (safe-select? q)
                {:status "failed" :detail "query is not a single read-only SELECT"}
                (if dsr
                  (builds/with-landing store schema
                    (fn [land]
                      (let [{q' :query :keys [landed]} (resolve-query schema q land)]
                        (graded (dsets/scalar-query store dsr q' landed)))))
                  {:status "failed"
                   :detail (str "a custom SQL check reads a dataset or a transform's output;"
                                " object types are checked by kind (row-count, sum, …),"
                                " never by SQL against their storage")}))

              (nil? (:sql row))
              {:status "failed" :detail (:message (hm/unknown kind))}

              (and column? (nil? col))
              {:status "failed" :detail "check missing a required :column"}

              :else
              (graded (measure-node store schema (:node-kind chk) (:node-key chk) col kind))))
          (catch InterruptedException e (throw e))
          (catch Exception t {:status "failed" :detail (.getMessage t)}))]
    (ctl/insert! store :health-check-results
                 {:branch branch :check-id check-id :status status
                  :value value :detail detail})
    (ports/result-recorded! store branch check-id)
    {:check-id check-id :status status :value value :detail detail}))

(defn- latest-result [store branch check-id]
  (last (sort-by :seq (results store branch check-id))))

(defn run-check!
  [store schema branch check-id]
  (let [store   (ctx/at store branch)
        chk  (some-> (ctl/get store :health-checks
                              {:branch branch :id check-id})
                     row->check)
        kind (:kind chk)]
    (if (:build? (hm/kind kind))
      (merge {:check-id check-id :status "warning"
              :detail "a data expectation runs in its transform's build — rebuild it"}
             (some-> (latest-result store branch check-id)
                     (select-keys [:check-id :status :value :detail])))
      (run-check-now! store schema branch check-id chk kind))))

(defn- drift-counts
  [store schema]
  (sub/with-read
    store
    (fn [store]
      (into {}
            (keep (fn [[rid _]]
                    (when-let [{:keys [incremental?]} (backing/object-backing schema rid)]
                      (when-not incremental?
                        [rid (long (sub/stored-count store rid))]))))
            (:object-types schema)))))

(defn- scan-index-drift!
  [store schema branch]
  (let [store     (ctx/at store branch)
        counts (drift-counts store schema)]
    (vec
     (keep
      (fn [[rid v]]
        (try
          (let [nm (name (:object-type-name v))]
            (when-let [{:keys [dataset incremental?]} (backing/object-backing schema rid)]
              (when-not incremental?
                (let [oc    (long (get counts rid 0))
                      tc    (long (dsets/table-count store dataset))
                      orphans (when-not (= oc tc)
                                (count (:removed (builds/object-removals
                                                  store rid dataset
                                                  {:propagate-empty? true}))))
                      missing (when (< oc tc) (- tc oc))
                      orphans (or orphans 0)
                      ds-nm   (or (some-> (lookup/name-of schema :datasets dataset) name)
                                  (str dataset))
                      counts  (str "index-drift compares ROW COUNTS only: " oc " " nm
                                   " object(s) vs " tc " row(s) in its backing dataset " ds-nm
                                   " — it says nothing about the values those rows carry"
                                   " (declare a :reconcile health check for a sum)")]
                  {:check-id (str "index-drift:" nm)
                   :status   (if (and (zero? orphans) (nil? missing)) "passed" "failed")
                   :value    (str (if (pos? orphans) orphans (or missing 0)))
                   :objects  oc
                   :backing  tc
                   :dataset  ds-nm
                   :detail   (cond
                               (pos? orphans)
                               (str counts "; "
                                    orphans " instance(s) with no producer backing and no"
                                    " edit provenance; `reindex " nm "` would drop them")
                               missing
                               (str counts "; at least "
                                    missing " row(s) the producer backs are NOT indexed;"
                                    " the publish that should have landed them did not"
                                    " (check the build's `publish-failures` via `run <id>`),"
                                    " then `run-import --only " nm "` republishes")
                               (not= oc tc)
                               (str counts "; the " (- oc tc)
                                    " extra object(s) all carry edit provenance")
                               :else counts)}))))
          (catch InterruptedException e (throw e))
          (catch Exception e
            {:check-id (str "index-drift:" (name (:object-type-name v)))
             :status   "failed"
             :detail   (str "index-drift scan failed: " (ex-message e))})))
      (:object-types schema)))))

(def ^:private projection-lag-grace-ms
  3600000)

(defn- projection-lag
  [anchors tips now-ms]
  (into []
        (keep (fn [{:keys [repo-rid branch tip-seq]}]
                (when (branch-kinds/projectable-branch? branch)
                  (let [{:keys [rev at]} (:last (get tips (str branch)))
                        behind (when (and rev tip-seq) (- (long rev) (long tip-seq)))]
                    (when (and behind (pos? behind))
                      {:pair (str repo-rid "/" branch) :behind behind
                       :waited-ms (when-let [i (some-> at ktime/->instant)]
                                    (- now-ms (.toEpochMilli i)))})))))
        anchors))

(defn projection-lag-verdict
  [lagging]
  (let [overdue (filterv #(> (long (or (:waited-ms %) 0)) projection-lag-grace-ms) lagging)
        worst   (reduce max 0 (map :behind lagging))]
    {:check-id "git-projection-lag"
     :status   (if (seq overdue) "failed" "passed")
     :value    (str worst)
     :detail   (if (seq overdue)
                 (str "the git projection has stood behind the installed ontology for over an"
                      " hour on " (count overdue) " repository/branch pair(s): "
                      (str/join "; " (for [{:keys [pair behind]} (take 5 overdue)]
                                       (str pair " — " behind " schema commit(s) behind")))
                      ". A clone there serves a tree older than the ontology, every push from"
                      " it re-checks what the instance has already moved past, and a push that"
                      " does go through RETRACTS what the tree is missing. Project the branch"
                      " again; if that cannot catch up, rebuild the repository from the CAS")
                 (if (seq lagging)
                   (str "every projection anchor is current or catching up (" worst
                        " schema commit(s) behind at most, none for over an hour)")
                   "every projection anchor is level with its branch"))}))

(defn- projection-lag-check
  [store anchors]
  (projection-lag-verdict
   (projection-lag anchors
                   (world/tips store (into [] (comp (map :branch) (distinct)) anchors))
                   (ctl/millis store))))

(defn- scan-git-projection!
  [store]
  (let [anchors (repos/repo-anchors store)
        stuck   (into []
                      (keep (fn [{:keys [repo-rid branch tip-sha]}]
                              (try
                                (when-let [ref (and tip-sha
                                                    (branch-kinds/projectable-branch? branch)
                                                    (repos/branch->ref branch))]
                                  (let [actual (repos/ref-sha (ctx/repos) repo-rid ref)]
                                    (cond
                                      (= actual tip-sha) nil
                                      (and (nil? actual)
                                           (nil? (repos/commit-info (ctx/repos) repo-rid tip-sha)))
                                      (str repo-rid "/" branch " — recorded tip " tip-sha
                                           " has neither a ref nor its objects")
                                      (nil? actual) nil
                                      (let [repos (ctx/repos)]
                                        (or (repos/descends-from? repos repo-rid actual tip-sha)
                                            (repos/descends-from? repos repo-rid tip-sha actual)))
                                      nil
                                      :else
                                      (str repo-rid "/" branch " — ref " actual
                                           " and recorded tip " tip-sha
                                           " share no ancestry"))))
                                (catch InterruptedException e (throw e))
                                (catch Exception e
                                  (str repo-rid "/" branch " — could not be read: "
                                       (ex-message e)))))
                            anchors))]
    [{:check-id "git-projection"
      :status   (if (seq stuck) "failed" "passed")
      :value    (str (count stuck))
      :detail   (if (seq stuck)
                  (str "the git projection is stuck for " (count stuck)
                       " repository/branch pair(s): " (str/join "; " (take 5 stuck))
                       ". A clone serves a tree that no longer matches the installed"
                       " ontology, and pushing that tree back RETRACTS what it is"
                       " missing — rebuild the repository from the CAS before anyone"
                       " authors on it")
                  "every projection anchor agrees with its ref")}
     (projection-lag-check store anchors)]))

(defn- scan-fold-gaps!
  [store schema branch]
  (let [store (ctx/at (ctx/as-system store) branch)]
    (vec
     (keep
      (fn [[rid a e]]
        (let [tn   (name (or (get-in schema [:object-types rid :object-type-name]) rid))
              id   (str "fold-gap:" tn "." (name a))
              over (str/join ", " (distinct (map (comp name :over) (wit/questions a e))))]
          (try
            (when-let [n (objects/fold-gaps store schema rid a e)]
              {:check-id id
               :status   (if (zero? n) "passed" "failed")
               :value    (str n)
               :detail   (if (zero? n)
                           (str tn "." (name a) ": jede Zeile, deren Fold über " over
                                " etwas erreicht, faltet auch etwas")
                           (str n " von " tn " lesen " (name a) " als 0, obwohl ihr Fold"
                                " über " over " Zeilen erreicht — dort ist der Wert"
                                " unbeantwortet, nicht 0"))})
            (catch Exception ex
              {:check-id id :status "failed"
               :detail (str "fold-gap scan failed: " (ex-message ex))}))))
      (objects/folded-properties schema)))))

(defn run-all! [store schema branch]
  (let [ids (into [] (comp (remove (comp :build? hm/kind :kind)) (map :id))
                  (ctl/list store :health-checks {:branch branch :enabled true}))]
    (-> (mapv #(run-check! store schema branch %) ids)
        (into (scan-index-drift! store schema branch))
        (into (scan-fold-gaps!  store schema branch))
        (into (when (contains? #{nil ctl/trunk} branch)
                (scan-git-projection! store))))))

(defn- severity-of [chk]
  (let [s (:severity chk)] (cond (keyword? s) (name s) (seq (str s)) (str s) :else nil)))

(defn- expectation-of
  [chk]
  (let [{:keys [op value]} (:expects chk)]
    (when (and op (some? value))
      (str (if (keyword? op) (name op) op) " " value))))

(defn join-results
  [results checks]
  (let [by-id (coll/index-by :id checks)
        rank  {"critical" 0 "high" 1 "moderate" 2 "low" 3}
        joined (for [r results
                     :let [chk (get by-id (:check-id r))]]
                 {:check    (or (:name chk) (:check-id r))
                  :status   (:status r)
                  :severity (or (severity-of chk) "—")
                  :value    (str (:value r))
                  :expects  (expectation-of chk)
                  :detail   (not-empty (str (:detail r)))})]
    {:pass    (count (filter #(= "passed" (:status %)) results))
     :total   (count results)
     :failing (->> joined
                   (remove #(= "passed" (:status %)))
                   (sort-by (juxt #(get rank (:severity %) 9) :check))
                   vec)}))

(defn operator-summary
  [{:keys [results checks active-builds last-run schedules]}]
  (let [{:keys [pass total failing] :as chk} (join-results results checks)
        scheds (vec (sort-by (juxt :paused :name) schedules))
        armed  (remove :paused scheds)
        red?   (or (boolean (some #(= "critical" (:severity %)) failing))
                   (= "failed" (:status last-run)))
        amber? (or (seq failing) (empty? armed))
        verdict (cond red? "red" amber? "yellow" :else "green")]
    {:ok      (= "green" verdict)
     :verdict verdict
     :checks  (assoc chk :pass pass :total total)
     :build   {:running (mapv #(select-keys % [:run-id :branch :phase :started :owner :remote?])
                              active-builds)
               :last    (when last-run (select-keys last-run [:id :kind :status :started-at]))}
     :schedules {:armed (count armed) :total (count scheds) :rows scheds}}))

(defn summary-by-node
  [store branch]
  (let [checks (list-all store branch)
        rank   {"failed" 3 "warning" 2 "passed" 1 nil 0}]
    (reduce (fn [acc {:keys [node-kind node-key latest]}]
              (let [k [node-kind node-key]
                    status (:status latest)
                    cur (get acc k {:status nil :checks 0 :failed 0})]
                (assoc acc k
                       {:status (if (> (rank status 0) (rank (:status cur) 0)) status (:status cur))
                        :checks (inc (:checks cur))
                        :failed (+ (:failed cur) (if (= "failed" status) 1 0))})))
            {} checks)))
