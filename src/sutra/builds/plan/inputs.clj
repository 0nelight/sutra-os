(ns sutra.builds.plan.inputs
  (:require [sutra.kit.noted :as noted]
            [sutra.kit.refusal :as refusal]
            [sutra.sources.api :as sources]
            [sutra.builds.ports :as ports]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.grants :as grants]
            [sutra.types.value-types :as vt]
            [sutra.store.access.row-shape :as row-shape]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.outputs :as catalog-outputs]
            [sutra.catalog.transforms :as transforms]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.llm-models :as llm-models]
            [sutra.media.api :as media]
            [sutra.types.naming :as naming]
            [clojure.string :as str]
            [sutra.store.storage.instance-config :as ic]
            [sutra.builds.import.materialize :as mat]
            [sutra.builds.plan :as plan]
            [sutra.builds.plan.virtual :as virtual]
            [sutra.kit.digest :as digest]
            [sutra.builds.ledger :as ledger]
            [sutra.types.kinds :as kinds]
            [sutra.types.table :as tbl]
            [sutra.types.languages :as langs]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.builds.plan.outputs :as outputs]
            [sutra.types.file-sync :as file-sync]
            [sutra.foundation.settings :as settings]))

(set! *warn-on-reflection* true)

(defn- from->query
  [{:keys [query table]}]
  (cond
    query query
    table (str "SELECT * FROM " table)
    :else (refusal/refuse! :refusal.builds/import-from-incomplete {})))

(defn- filtered-files
  [table filters rows]
  (if (seq filters)
    (let [relative #(file-sync/relative-path table (:path %))
          files    (map #(assoc % :path (relative %)) rows)
          taken    (into #{} (map :path) (:files (file-sync/select filters files)))]
      (filterv #(taken (relative %)) rows))
    rows))

(defn select-rows [source {:keys [query table filters] :as from}]
  (if (and table (not query) (not (sources/speaks-sql? source)))
    (filtered-files table filters (sources/scan source table))
    (sources/read source (from->query from) (::params from))))

(defn- source-cache-key
  [{:keys [datasource query table filters] :as from}]
  [datasource (or query (str "TABLE:" table)) filters (::params from)])

(defn memo!
  [cache k f]
  (or (get @cache k)
      (let [v (f)]
        (swap! cache assoc k v)
        v)))

(defn- cached-select-rows
  [cache source from]
  (memo! cache (source-cache-key from) #(select-rows source from)))

(defn- resolve-external-systems
  [schema compute]
  (if-let [es (seq (:external-systems compute))]
    (let [by-name (into {} (mapcat (fn [[k v]]
                                     (cond-> [[k v]]
                                       (:source-name v) (conj [(:source-name v) v]))))
                        (:sources schema))]
      (assoc compute :external-systems
             (mapv (fn [sid]
                     (let [spec (get by-name sid)]
                       (cond-> {:source-id sid}
                         spec (assoc :spec spec))))
                   es)))
    compute))

(defn- output-enums
  [schema compute]
  (if (not= :object-type (get-in compute [:output :kind]))
    {}
    (let [ot (get-in compute [:output :object-type])
          al (vt/resolve-value-types (get-in schema [:object-types ot :attribute-list])
                                     (:value-types schema))]
      (reduce (fn [m {:keys [attribute-name opts]}]
                (if-let [one-of (get-in opts [:constraints :one-of])]
                  (assoc m (name attribute-name) (vec one-of))
                  m))
              {} al))))

(defn note!
  ([ctx k v] (note! ctx k nil v))
  ([{:keys [notes]} k at v]
   (some-> notes (swap! conj [k at v]))
   nil))

(defn- consumer-of
  [schema compute]
  (lookup/rid-of schema :transforms (:transform-name compute)))

(defn- noted?
  [{:keys [schema notes]} k t]
  (let [consumer (consumer-of schema t)]
    (boolean (some (fn [[nk _ v]] (and (= k nk) (= consumer v))) (some-> notes deref)))))

(defn snapshot-run?
  [{:keys [schema row-cache snapshot-requested] :as ctx} t]
  (or (noted? ctx :snapshot-run t)
      (contains? snapshot-requested (consumer-of schema t))
      (contains? (some-> row-cache deref) [::snapshot-run (consumer-of schema t)])))

(defn check-strict-append!
  [{:keys [store full-refresh?] :as ctx} t ds ids]
  (when (and (transforms/strict-append? t) (not full-refresh?) (not (snapshot-run? ctx t)))
    (when-let [hit (seq (dsets/overwritten-ids store ds ids))]
      (throw (ex-info (str "transform " (pr-str (:transform-name t)) " declares strict_append and"
                           " would overwrite " (count hit) " existing row(s)")
                      {:transform (:transform-name t) :dataset ds :overwrites (count hit)})))))

(defn- snapshot-run!
  [{:keys [schema snapshot-requested] :as ctx} compute]
  (when (and (transforms/require-incremental? compute)
             (not (contains? snapshot-requested (consumer-of schema compute))))
    (throw (ex-info (str "transform " (pr-str (:transform-name compute))
                         " declares require_incremental and cannot run incrementally: an input"
                         " overwrote or removed rows. Declare it in static_inputs, or bump"
                         " semantic_version to ask for the snapshot.")
                    {:transform (:transform-name compute)})))
  (when-let [cache (:row-cache ctx)]
    (swap! cache assoc [::snapshot-run (consumer-of schema compute)] true))
  (note! ctx :snapshot-run (consumer-of schema compute)))

(defn- added-read-window!
  [{:keys [store schema] :as ctx} compute input-dataset-rid branch table-name]
  (let [consumer-rid (consumer-of schema compute)
        {:keys [low high]} (ledger/read-boundary store consumer-rid input-dataset-rid branch)
        first-read? (or (not (dsets/addressable? low)) (nil? high))
        swept?      (and (not first-read?) (dsets/below-horizon? store table-name low))
        low         (if (and swept? (transforms/allow-retention? compute))
                      (dsets/retained-floor store table-name)
                      low)
        lost?       (and swept? (not (transforms/allow-retention? compute)))
        forced?     (contains? (:snapshot-requested ctx) consumer-rid)
        only-added? (if (transforms/allow-retention? compute)
                      dsets/only-added-past-retention?
                      dsets/only-added?)
        was-empty?  (and (not first-read?) (not lost?) (not forced?)
                         (dsets/empty-at? store table-name low high))
        ended?      (and (not first-read?) (not was-empty?)
                         (or forced? lost? (not (only-added? store table-name low high))))
        bootstrap?  (or first-read? was-empty? ended?)]
    (when ended? (snapshot-run! ctx compute))
    (when high
      (note! ctx :consumed-cursors
             {:consumer-rid consumer-rid :input-dataset-rid input-dataset-rid
              :last-snapshot-id high}))
    (when bootstrap? (note! ctx :bootstrapped consumer-rid))
    {:low low :high high :bootstrap? bootstrap?}))

(defn- noted-rows
  [sink consumer-rid input-dataset-rid extra rows]
  (note! sink :rows-in consumer-rid (count rows))
  (note! sink :trace (assoc extra :kind :dataset-read :consumer-rid consumer-rid
                            :input-dataset-rid input-dataset-rid :rows (count rows)))
  rows)

(defn- dataset-values
  [store schema t]
  (dsets/row-values store t (row-shape/array-keys store schema t)))

(defn- incremental-dataset-rows
  [{:keys [store schema] :as ctx} compute input-dataset-rid branch table-name]
  (let [{:keys [low high
                bootstrap?]} (added-read-window! ctx compute input-dataset-rid branch
                                                 table-name)]
    (noted-rows (select-keys ctx [:notes]) (consumer-of schema compute) input-dataset-rid
                {:branch branch :incremental? true :low low :high high
                 :path (if bootstrap? :bootstrap-full-read :delta-added)}
                (if bootstrap?
                  (dataset-values store schema table-name)
                  (dsets/added-values store table-name low high
                                      (row-shape/array-keys store schema table-name))))))

(defn- input-dataset-rows
  [{:keys [store schema] :as ctx} compute alias ds-rid tbl]
  (if (and (transforms/added-read? compute) (not (transforms/snapshot-input? compute alias)))
    (incremental-dataset-rows ctx compute ds-rid (:branch store) tbl)
    (noted-rows (select-keys ctx [:notes]) (consumer-of schema compute) ds-rid
                {:incremental? false :path :full-read}
                (dataset-values store schema tbl))))

(defn- virtual-input-ended!
  [{:keys [store schema] :as ctx} compute alias ds-rid]
  (when (and alias (transforms/snapshot-ends-increment? compute alias))
    (let [consumer (consumer-of schema compute)]
      (when (dsets/addressable? (:low (ledger/read-boundary store consumer ds-rid (:branch store))))
        (snapshot-run! ctx compute))
      (note! ctx :bootstrapped consumer))))

(defn- virtual-input-rows
  [{:keys [store schema] :as ctx} compute alias ds-rid vds]
  (let [consumer (consumer-of schema compute)]
    (virtual-input-ended! ctx compute alias ds-rid)
    (noted-rows (select-keys ctx [:notes]) consumer ds-rid {:incremental? false :path :virtual-scan}
                (virtual/rows (ctx/connect (grants/source-authority store)) schema vds))))

(defn- stored-object-rows [reads schema ot]
  (mapv :values (mat/object-rows reads schema ot)))

(defn- object-input-moved?
  [store ot low]
  (or (not (dsets/addressable? low))
      (sub/changed-since? store ot (dsets/snapshot-id low))))

(defn- object-type-input-rows
  [{:keys [store schema] :as ctx} compute alias in]
  (let [ot       (:object-type in)
        staged   (get (:staged ctx) ot)
        consumer (consumer-of schema compute)
        ot-rid   (lookup/rid-of schema :object-types ot)]
    (if staged
      (mapv :values (mat/staged-object-rows store schema ot staged))
      (do (when (transforms/snapshot-ends-increment? compute alias)
            (let [low (:low (ledger/read-boundary store consumer ot-rid (:branch store)))]
              (when (object-input-moved? store ot low)
                (when (dsets/addressable? low) (snapshot-run! ctx compute))
                (note! ctx :bootstrapped consumer))))
          (noted-rows (select-keys ctx [:notes]) consumer ot-rid
                      {:incremental? false :path :object-snapshot}
                      (stored-object-rows (sub/reader store) schema ot))))))

(defn- sql-added-map
  [{:keys [store schema] :as ctx} compute]
  (when (transforms/added-read? compute)
    (let [branch (:branch store)]
      (reduce-kv
       (fn [m alias in]
         (if-let [ds-rid (and (kinds/added-read-kind? (:kind in))
                              (not (transforms/snapshot-input? compute alias))
                              (not (virtual/spec-of schema (:dataset in)))
                              (catalog-outputs/input-dataset-rid schema in))]
           (let [{:keys [low high bootstrap?]}
                 (added-read-window! ctx compute ds-rid branch (naming/dataset-table ds-rid))]
             (cond-> m (not bootstrap?) (assoc alias {:low low :high high})))
           m))
       {} (:inputs compute)))))

(defn- input-column-aliases
  [schema in]
  (not-empty
   (naming/column-aliases
    (case (kinds/physical (:kind in))
      :landed        (outputs/attr-names (get-in schema [:object-types (:object-type in)]))
      :dataset-table (map :attribute-name
                          (get-in schema [:datasets
                                          (lookup/rid-of schema :datasets (:dataset in))
                                          :columns]))
      nil))))

(defn- input-derived-names
  [schema in]
  (let [ot (case (kinds/physical (:kind in))
             :landed (:object-type in)
             :dataset-table
             (let [ds (get-in schema [:datasets (lookup/rid-of schema :datasets (:dataset in))])]
               (when (= :materialization (:kind ds)) (:object-type ds)))
             nil)]
    (when ot
      (not-empty
       (into []
             (mapcat (fn [[a]] [(name a) (name (naming/read-key a))]))
             (mat/derived-attrs
              (get-in schema [:object-types (or (lookup/rid-of schema :object-types ot) ot)])))))))

(defn- datasource-input
  [{:keys [store datasources mat-cache]} _ _ in]
  (let [spec (get datasources (:datasource in))]
    {:kind :datasource
     :path (or ((ports/foreign-datasources store :path)
                (grants/source-authority store) spec
                #(memo! mat-cache [:landed (:datasource in)] %))
               (refusal/refuse! (if (:external-transform spec)
                                  :refusal.builds/datasource-input-external
                                  :refusal.builds/datasource-input-unknown-type)
                                {:datasource (:datasource in) :type (:type spec)
                                 :external-transform (boolean (:external-transform spec))}
                                {:source    (name (:datasource in))
                                 :type-name (name (:type spec))}))}))

(def ^:private whole-rows
  (tbl/per-key "plan/inputs whole-rows" (tbl/where kinds/kinds :whole-rows?)
               {:dataset     (fn [{:keys [store schema]} in]
                               (let [ds-rid (lookup/rid-of schema :datasets (:dataset in))]
                                 (if-let [vds (virtual/spec-of schema ds-rid)]
                                   (virtual/rows (ctx/connect (grants/source-authority store))
                                                 schema vds)
                                   (dataset-values store schema (naming/dataset-table ds-rid)))))
                :transform   (fn [{:keys [store schema]} in]
                               (dataset-values store schema
                                               (naming/trans-table
                                                (catalog-outputs/transform-input-key schema in))))
                :object-type (fn [{:keys [store schema staged]} in]
                               (if-let [ds (get staged (:object-type in))]
                                 (dataset-values store schema (naming/dataset-table ds))
                                 (stored-object-rows (sub/reader store) schema
                                                     (:object-type in))))}))

(defn whole-input-rows
  [ctx in]
  (let [k (keyword (:kind in))]
    (if-let [read-all (get whole-rows k)]
      (read-all ctx in)
      (throw (ex-info (str "no whole rows of an input of kind " (pr-str k)) {:input in})))))

(defn- transform-input
  [{:keys [store schema]} _ _ in]
  {:kind :transform
   :rows (dataset-values store schema
                         (naming/trans-table (catalog-outputs/transform-input-key schema in)))})

(defn- object-type-input
  [ctx compute alias in]
  {:kind :object-type :rows (object-type-input-rows ctx compute alias in)})

(defn- dataset-input
  [{:keys [schema] :as ctx} compute alias in]
  (let [ds-rid (lookup/rid-of schema :datasets (:dataset in))]
    {:kind :dataset
     :rows (if-let [vds (virtual/spec-of schema ds-rid)]
             (virtual-input-rows ctx compute alias ds-rid vds)
             (input-dataset-rows ctx compute alias ds-rid (naming/dataset-table ds-rid)))}))

(defn- media-set-input
  [{:keys [schema] :as ctx} compute alias in]
  (let [{:keys [rows]} (dataset-input ctx compute alias in)]
    (media/media-set-entry schema (lookup/rid-of schema :datasets (:dataset in)) rows)))

(defn- media-set-items-input
  [{:keys [store schema]} _compute _alias {:keys [media-set]}]
  (media/set-items-entry store schema media-set))

(def ^:private input-entries
  {:datasource     datasource-input
   :transform      transform-input
   :object-type    object-type-input
   :dataset        dataset-input
   :media-set      media-set-input
   :media-set-items media-set-items-input
   :language-model (fn [ctx _ _ in] (media/language-model-entry (:schema ctx) in))})

(defn- resolve-compute-inputs
  [{:keys [schema] :as ctx} compute]
  (reduce-kv
   (fn [acc alias in]
     (let [entry!  (or (input-entries (:kind in))
                       (refusal/refuse! :refusal.builds/unknown-input-kind
                                        {:alias alias :input in}
                                        {:kind    (pr-str (:kind in))
                                         :handled (str/join ", " (sort (keys input-entries)))}))
           aliases (input-column-aliases schema in)
           derived (input-derived-names schema in)
           entry   (entry! ctx compute alias in)]
       (assoc acc alias (cond-> entry
                          aliases (assoc :aliases aliases)
                          derived (assoc :derived-names derived)))))
   {}
   (:inputs compute)))

(defn- model-input-grants
  [compute]
  (into [] (comp (map val)
                 (filter llm-models/language-model-input?)
                 (keep :source))
        (:inputs compute)))

(defn- with-model-sources
  [schema compute]
  (cond-> compute
    (map? (:inputs compute))
    (update :inputs update-vals #(llm-models/with-source schema %))))

(defn- derive-external-systems
  [schema compute]
  (let [compute (with-model-sources schema compute)
        derived (model-input-grants compute)]
    (cond-> compute
      (seq derived)
      (assoc :external-systems
             (into [] (distinct) (concat (:external-systems compute) derived))))))

(defn transform-entry
  [schema k]
  (let [t-kw (if (get-in schema [:transforms k])
               k
               (rid/resolve-name->rid (sch/name-index schema) #{:transforms} k))]
    [t-kw (get-in schema [:transforms t-kw])]))

(defn select-output
  [result output-name]
  (if-let [outs (:outputs result)]
    (cond
      output-name (or (get outs (keyword output-name))
                      (refusal/refuse! :refusal.builds/no-such-output
                                       {:want output-name :have (vec (keys outs))}))
      (= 1 (count outs)) (val (first outs))
      :else (refusal/refuse! :refusal.builds/output-name-needed
                             {:have (vec (keys outs))}
                             {:names (str/join ", " (map name (keys outs)))}))
    (:rows result)))

(defn- run-code-compute!
  [store schema compute opts]
  (let [lang (:language compute)]
    ((ports/compute-runner store :run-code!) lang
                                       (if ((ports/compute-runner store :egress?) lang)
                                         (resolve-external-systems schema
                                                                   (derive-external-systems
                                                                    schema compute))
                                         (with-model-sources schema compute))
                                       opts)))

(defn run-code-on-inputs!
  [store env schema transform-kw rows-of output-name]
  (let [t (second (transform-entry schema transform-kw))]
    (when-not (langs/code-language? (:language t))
      (refusal/refuse! :refusal.builds/given-rows-need-code
                       {:transform transform-kw :language (:language t)}))
    (let [c      (plan/compute t transform-kw :code-run)
          inputs (reduce-kv (fn [m alias in]
                              (assoc m alias (if (llm-models/language-model-input? in)
                                               (media/language-model-entry schema in)
                                               {:kind :object-type
                                                :rows (vec (rows-of alias))})))
                            {} (:inputs t))]
      (select-output
       (run-code-compute! store schema c
                          (assoc ((ports/compute-runner store :prepare-inputs)
                                  (:language t) inputs nil)
                                 :output-enums (output-enums schema c)
                                 :blobs (:blobs env)
                                 :lm-service (:lm-service env)))
       output-name))))

(defn resolve-code-compute
  ([ctx compute] (resolve-code-compute ctx compute nil))
  ([{:keys [store schema] :as ctx} compute {:keys [prev-table]}]
   (let [resolved  (resolve-compute-inputs ctx compute)
         prev-out  (or (:output compute)
                       (first (:outputs compute)))
         prev-tbl  (or prev-table (some-> prev-out mat/output-table))
         snapshot? (snapshot-run? ctx compute)
         consumer  (consumer-of schema compute)
         prev-rows (when (and (:incremental compute) prev-tbl (not snapshot?))
                     (try (if (= :object-type (:kind prev-out))
                            (mapv :values
                                  (mat/column-rows (sub/read-type store (:object-type prev-out))))
                            (dataset-values store schema prev-tbl))
                          (catch InterruptedException e (throw e))
                          (catch Exception e (noted/note "previous output" e) nil)))]
     (when (:incremental compute)
       (note! ctx :trace {:kind :prev-output
                          :consumer-rid (consumer-of schema compute)
                          :prev-out (or (:object-type prev-out) (:dataset prev-out) prev-table)
                          :done-rows (count prev-rows)}))
     (assoc ((ports/compute-runner store :prepare-inputs) (:language compute) resolved prev-rows)
            :compute (cond-> compute
                       (and snapshot? (settings/value :refuse-capped-replace))
                       (assoc :refuse-cap? true)
                       (and (:incremental compute) (:notes ctx) (not (:full-refresh? ctx))
                            (not snapshot?) (not (noted? ctx :bootstrapped compute))
                            (or (seq prev-rows)
                                (seq (ledger/consumption-of store consumer (:branch store)))))
                       (assoc :incremental-run? true))
            :output-enums (output-enums schema compute)))))

(defn code-env
  [store]
  {:blobs (ctx/blobs) :lm-service (delay (ic/lm-service-config store))})

(defn run-resolved-compute!
  [{:keys [store schema trace? run-id code-env] :as ctx} {:keys [compute] :as resolved}]
  (let [consumer (consumer-of schema compute)
        result   (run-code-compute! store schema (cond-> compute run-id (assoc :build-job? true))
                                    (merge (dissoc resolved :compute ::caps)
                                           code-env
                                           {:run-id run-id
                                            :trace? (boolean trace?)}))]
    (note! ctx :code-sha (:transform-name compute) (some-> (:code compute) digest/sha256-hex))
    (when (and run-id (seq (:media-set-items result)))
      (media/record-output-items! store schema compute (:media-set-items result)))
    (when-let [events (seq (:trace result))]
      (note! ctx :trace {:kind :code :consumer-rid consumer :events (vec events)}))
    (when (false? (:incremental-complete result))
      (note! ctx :incomplete consumer))
    (when-let [n (:pending-observed result)]
      (note! ctx :pending-observed consumer (long n)))
    (let [{:keys [spans log]} ((ports/compute-runner store :spans) (:log result))]
      (note! ctx :job-log consumer (str/join "\n" log))
      (note! ctx :job-spans consumer (not-empty spans)))
    (dissoc result :trace :incremental-complete :pending-observed :log)))

(defn- source-inputs
  [compute]
  (for [[_ in] (:inputs compute) :when (= :datasource (:kind in))] in))

(defn- virtual-inputs
  [schema compute]
  (for [[alias in] (:inputs compute)
        :let [vds (when (= :dataset (:kind in)) (virtual/spec-of schema (:dataset in)))]
        :when vds]
    [alias vds]))

(defn- sql-job-line [compute]
  (str "sql " (if (:incremental compute) "(incremental)" "(full)")
       " · inputs " (pr-str (vec (keys (:inputs compute))))
       "\n" (str/trim (str (:query compute)))))

(defn- unrunnable [compute]
  (refusal/refusal :refusal.builds/language-not-runnable
                   {:transform (:transform-name compute) :language (:language compute)}
                   {:known (pr-str langs/compute-language-names)}))

(defn- run-sql-compute!
  [{:keys [store schema datasources mat-cache] :as ctx} compute prev-table]
  (let [inc? (:incremental compute)]
    (doseq [in (source-inputs compute)]
      (mat/materialize-source! store schema datasources mat-cache in))
    (doseq [[alias vds] (virtual-inputs schema compute)]
      (virtual-input-ended! ctx compute alias
                            (lookup/rid-of schema :datasets
                                           (:dataset (get (:inputs compute) alias))))
      (mat/stage-virtual! store mat-cache vds
                          #(virtual/rows (ctx/connect (grants/source-authority store)) schema
                                         vds)))
    (note! ctx :job-log (consumer-of schema compute) (sql-job-line compute))
    (let [added (when inc? (sql-added-map ctx compute))]
      {:rows (mat/run-transform!
              store schema
              (cond-> (assoc compute ::mat/staged (:staged ctx))
                (and inc? prev-table)     (assoc ::mat/prev-table prev-table)
                (snapshot-run? ctx compute) (assoc ::mat/prev-empty? true)
                inc?                      (assoc ::mat/added added)))})))

(defn run-compute!
  [ctx compute {:keys [prev-table]}]
  (cond
    (langs/code-language? (:language compute))
    (run-resolved-compute! ctx (resolve-code-compute ctx compute {:prev-table prev-table}))

    (langs/query-language? (:language compute)) (run-sql-compute! ctx compute prev-table)
    :else                        (throw (unrunnable compute))))

(defn read-source-rows
  [{:keys [store source-for row-cache schema] :as ctx} from output-name]
  (if-let [compute (:compute from)]
    (if (langs/code-language? (:language compute))
      (select-output (memo! row-cache [::code (:transform-name compute)]
                            #(run-compute! ctx compute nil))
                     output-name)
      (select-output
       (run-compute! ctx compute
                     {:prev-table (when (= :object-type (get-in compute [:output :kind]))
                                    [:object-type (get-in compute [:output :object-type])])})
       output-name))
    (if-let [ds-ref (:dataset from)]
      (let [ds-rid (or (lookup/rid-of schema :datasets ds-ref) ds-ref)
            tbl    (naming/dataset-table ds-rid)
            vds    (virtual/spec-of schema ds-rid)]
        (cond
          vds              (if-let [consumer (:consumer from)]
                             (virtual-input-rows ctx consumer nil ds-rid vds)
                             (virtual/rows (ctx/connect (grants/source-authority store)) schema
                                           vds))
          (:consumer from) (input-dataset-rows ctx (:consumer from) nil ds-rid tbl)
          :else            (dataset-values store schema tbl)))
      (cached-select-rows row-cache (source-for (:datasource from)) from))))
