(ns sutra.authoring.install.findings.backing
  (:require [clojure.string :as str]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.live :as live]
            [sutra.authoring.install.findings :as fnd]
            [sutra.authoring.install.findings.sql :as sql-findings]
            [sutra.kit.coerce :refer [->name-kw]]
            [sutra.types.attribute :as attribute]
            [sutra.types.backing :as backing]
            [sutra.types.column :as column]
            [sutra.types.rid :as rid]
            [sutra.types.sql-traps :as traps]
            [sutra.types.transform-io :as tio]
            [sutra.types.virtual :as virtual]))

(set! *warn-on-reflection* true)

(defn declared-bindings
  [schema]
  (concat
   (for [[k spec] (:datasets schema) :when (:external-source spec)]
     [:dataset k :external-source])
   (for [[k spec] (:datasets schema)
         :when (and (= :materialization (:kind spec)) (:object-type spec))]
     [:dataset k :materialization])
   (for [[k spec] (:objects schema)
         :let [via (cond (:external-source spec) :external-source
                         (seq (:external-sources spec)) :external-sources)]
         :when via]
     [:object-type k via])
   (for [[k _from _to _card opts] (:links schema) :when (:external-source opts)]
     [:link-type k :external-source])))

(defn- ref-str [kind k] (str (name kind) " " (pr-str k)))

(defn- kw-name [x] (some-> x rid/->name name keyword))

(defn- in-before?
  [before [kind v]]
  (if-let [kk (get rid/tag->kinds-key kind)]
    (live/holds? (get before kk) v)
    true))

(def binding-dropped
  {:find (fn [{:keys [text after removed]}]
           (for [[kind k via] (declared-bindings text)
                 :let [ref [kind k]]
                 :when (and (empty? (live/lookup (:producers after) ref)) (not (removed ref)))]
             {:rule :binding-dropped :entity-class kind :entity k :via via}))
   :says {:binding-dropped
          (fn [{kind :entity-class k :entity :keys [via]}]
            (str "The .ent declares " via " on " (ref-str kind k)
                 " but the plan stages no transform that produces it — the"
                 " binding was dropped between the authored file and the"
                 " staged changes. Installing this would leave " (ref-str kind k)
                 " with no producer, and the next build would treat its"
                 " input as empty. This is a planner bug, not an authoring"
                 " one: report it rather than working around it."))}})

(defn- producer-removed-finding
  [{:keys [after removed]} backed? ref producers]
  (when (and (seq producers)
             (empty? (live/lookup (:producers after) ref))
             (not (removed ref))
             (not (backed? ref)))
    (cond-> {:rule :producer-removed :entity-class (first ref) :entity (second ref)
             :was (vec (sort producers))}
      (= :dataset (first ref))
      (assoc :rule :dataset-no-longer-built :severity :warn))))

(defn- changed-producers-removed
  [{:keys [text before] :as d}]
  (let [backed? (fn [[kind v]]
                  (and (= :object-type kind)
                       (boolean
                        (or (:backing (get (:objects text) v))
                            (and (:backing (live/lookup (:object-types before) v))
                                 (not (contains? (:objects text) (->name-kw v))))))))
        refs    (distinct (for [t (sort-by str (diff/changed d :transform))
                                o (tio/outputs-of (live/lookup (:transforms before) t))
                                :let [ref (diff/output-ref o)]
                                :when ref]
                            ref))]
    (for [ref refs
          :when (in-before? before ref)
          :let [f (producer-removed-finding d backed? ref
                                                    (live/lookup (:producers before) ref))]
          :when f]
      f)))

(def producer-removed
  {:find (fn [d] (lazy-seq (changed-producers-removed d)))
   :says {:dataset-no-longer-built
          (fn [{k :entity :keys [was]}]
            (str "dataset " (pr-str k) " loses its last producer " (pr-str was)
                 " and stays as it is: readable, no longer built (\"no JobSpec"
                 " on master\"). Delete it in a later install when nothing reads it."))
          :producer-removed
          (fn [{kind :entity-class k :entity :keys [was]}]
            (str (ref-str kind k) " is live and produced by "
                 (pr-str was)
                 ", and this install would remove its last producer while"
                 " keeping the entity — no build would reach it again, and the"
                 " rows that producer wrote would stay served with nothing"
                 " behind them. Either stage a replacement producer in the same"
                 " .ent, delete the entity too, or — to keep the type and fill it"
                 " through Actions from now on — back it by its own dataset in"
                 " the ontology first: `:backing {:dataset <upload-dataset>"
                 " :primary-key <pk> :replaces " (pr-str (first was)) "}`"
                 " (an object's state is its source rows plus every edit made"
                 " on it — the next build keeps what Actions created and"
                 " retires what only the old producer carried). Taking the type"
                 " apart: leave it out of the ontology FIRST (its producers are"
                 " no blockers there), then drop them here."))}})

(def sourceless-root
  {:find (fn [d]
           (for [[t-kw args] (diff/staged d :transform)
                 :when (backing/sourceless-root? args)]
             {:rule :root-transform-sourceless :entity-class :transform :entity t-kw
              :datasource (get-in args [:input :datasource])}))
   :says {:root-transform-sourceless
          (fn [{t-kw :entity :keys [datasource]}]
            (str "Transform " (pr-str t-kw) " is rooted on datasource "
                 (pr-str datasource)
                 " and names neither :source.table nor :source.query — it"
                 " says WHERE to read from, not WHAT. No plan can be"
                 " derived from such a root, so every reader of the"
                 " backing it produces fails, not just its own build."
                 " Name the table (or the query) on the :external-source"
                 " that mints it."))}})

(defn- writers
  [{:keys [transforms]}]
  (reduce-kv (fn [m t spec]
               (reduce (fn [m o] (if-let [d (kw-name (:dataset o))]
                                   (update m d (fnil conj #{}) (kw-name t))
                                   m))
                       m (tio/outputs-of spec)))
             {} transforms))

(defn- writers-of
  [{:keys [after] :as d}]
  (let [tx (:transforms after)]
    (live/looked (fn [ds]
                   (let [ws (into #{} (comp (filter (fn [t] (some #(= ds (kw-name (:dataset %)))
                                                                  (tio/outputs-of
                                                                   (live/lookup tx t)))))
                                            (map kw-name))
                                  (diff/dataset-writers d ds))]
                     (when (seq ws) [ws])))
                 (delay (writers (update after :transforms live/whole))))))

(def virtual-dataset
  {:find (fn [{:keys [text before] :as d}]
           (let [sources (live/merged (:sources before) (:sources text))
                 writers (writers-of d)]
             (for [[k spec] (:datasets text)
                   :when (virtual/virtual? spec)
                   :let [src (virtual/source-name spec)
                         ds  (kw-name k)]
                   p (virtual/problems (live/select sources [src])
                                       (live/select writers [ds]) ds spec)]
               (merge {:rule :virtual-dataset-invalid :entity-class :dataset :entity k} p))))
   :says
   {:virtual-dataset-invalid
    (fn [{k :entity :keys [problem source type table columns column transforms]}]
      (str "Virtual dataset " (pr-str k) ": "
           (case problem
             :unknown-source       (str "names source " (pr-str source)
                                        ", which no :sources entry declares.")
             :source-cannot-scan   (str "reads source " (pr-str source)
                                        " (:" (some-> type name) "), which cannot scan a"
                                        " table in place — only a source whose profile has"
                                        " :scan (a database, a CSV, a REST resource, SFTP)"
                                        " backs a virtual dataset. Land it through an"
                                        " :external-source instead.")
             :unsafe-table         (str ":table " (pr-str table) " is not a plain identifier"
                                        " ([A-Za-z_][A-Za-z0-9_]*); the source would refuse"
                                        " every read.")
             :key-not-a-column     (str ":primary-key names " (pr-str columns) ", which its"
                                        " :columns do not declare — a read projects to"
                                        " :columns, so the key would never come back.")
             :detect-not-a-column  (str ":detect :column " (pr-str column)
                                        " is not among its :columns —"
                                        " the probe would read a column no consumer sees.")
             :written-by-transform (str "is written by " (pr-str transforms) ". A virtual"
                                        " dataset holds no rows: write the source table"
                                        " through a :writeback action, or give the transform"
                                        " its own dataset."))))}})

(defn- sql-producers-of
  [transforms ot]
  (for [[t-nm t] transforms
        :when (sql-findings/sql-transform? t)
        o     (tio/outputs-of t)
        :when (and (= :object-type (:kind o))
                   (= ot (->name-kw (:object-type o))))]
    [t-nm t o]))

(defn- col-of [a] (str/lower-case (name (column/attr->col a))))

(defn- sql-producers
  [d ot]
  (let [tx (get-in d [:after :transforms])]
    (sql-producers-of (into (sorted-map-by #(compare (str %1) (str %2)))
                            (map (juxt identity #(live/lookup tx %)))
                            (diff/producing d :object-type ot))
                      ot)))

(defn- fed-attributes
  [d obj ot]
  (into {} (for [[t-nm t o] (sql-producers d ot)
                 :let [cm (merge (:column-map t) (:column-map o))
                       by-col (into {} (map (fn [[a c]] [(str/lower-case (name c)) a])) cm)]
                 col (traps/select-output-names (:query t))
                 :let [a (or (get by-col col)
                             (some #(when (= col (col-of %)) %) (diff/attr-names-of obj)))]
                 :when a]
             [a t-nm])))

(defn- decimal-type? [t] (and (vector? t) (= :decimal (first t))))

(defn- column-types
  [{:keys [text before after] :as d} ds]
  (let [spec (or (get-in text [:datasets ds]) (live/lookup (:datasets before) ds))
        tx   (:transforms after)
        cols (or (seq (:columns spec))
                 (some (fn [k]
                         (let [t (live/lookup tx k)]
                           (when (= ds (get-in t [:output :dataset]))
                             (seq (:output-columns t)))))
                       (sort-by str (diff/dataset-writers d ds))))]
    (into {} (map (fn [c] (if (map? c)
                            [(name (:attribute-name c)) (:attribute-type c)]
                            [(name (first c)) (second c)])))
          cols)))

(def primary-key-dropped
  {:find (fn [{:keys [before after] :as d}]
           (for [[ot args] (diff/staged d :object-type)
                 :when (not= :merge (:reconcile-mode args))
                 :let  [had  (diff/attr-names-of (live/lookup (:object-types before) ot))
                        has  (diff/attr-names-of (live/lookup (:object-types after) ot))
                        gone (into (sorted-set)
                                   (filter #(and (contains? had %) (not (contains? has %))))
                                   (diff/pk-attrs d :after ot))]
                 :when (seq gone)]
             {:rule :primary-key-attribute-dropped :entity-class :object-type :entity ot
              :dropped (vec gone)}))
   :says {:primary-key-attribute-dropped
          (fn [{:keys [entity dropped]}]
            (str "Object type " (name entity) " is declared without "
                 (str/join ", " (map pr-str dropped)) ", its primary key. An object-type"
                 " declaration REPLACES the live attribute list (default :reconcile-mode"
                 " :replace), so installing this drops the key property while its"
                 " producer still mints every id from it. Keep the attribute in the"
                 " declaration, or add :reconcile-mode :merge if the file means to add"
                 " to the type rather than restate it."))}})

(def edit-only
  {:find (fn [{:keys [after] :as d}]
           (for [ot    (keys (diff/staged d :object-type))
                 :let  [obj (live/lookup (:object-types after) ot)
                        pk  (diff/pk-attrs d :after ot)
                        fed (fed-attributes d obj ot)]
                 {:keys [attribute-name opts]} (:attribute-list obj)
                 :when (:edit-only opts)
                 :let  [why (cond
                              (contains? pk attribute-name) [:primary-key]
                              (:derived opts)               [:derived]
                              (get fed attribute-name)      [:fed (get fed attribute-name)])]
                 :when why]
             {:rule :edit-only-backed :entity-class :object-type :entity ot
              :property attribute-name ::fnd/ctx {:why why}}))
   :says {:edit-only-backed
          (fn [{ot :entity :keys [property] {[why producer] :why} ::fnd/ctx}]
            (str "Property " (pr-str property) " on " (name ot) " is :edit-only and "
                 (case why
                   :primary-key "is the primary key, which must be backed by a column"
                   :derived     "is :derived, which is computed and never stored"
                   :fed         (str "is fed by the column transform " (pr-str producer)
                                     " emits"))
                 ". its values come only from edits. Drop :edit-only to"
                 " map it, or stop backing it."))}})

(defn- unmapped-finding
  [objs fk-col? t-nm t o]
  (let [ot       (->name-kw (:object-type o))
        expected (into #{"is_deleted"}
                       (map #(col-of (->name-kw %)))
                       (concat (:names (attribute/of (live/lookup objs ot)))
                               (vals (:column-map t)) (vals (:column-map o))
                               (keep identity [(:primary-key t) (:primary-key o)])))
        unmapped (remove #(or (expected %) (fk-col? %)) (traps/select-output-names (:query t)))]
    (when (seq unmapped)
      {:rule :supplement-column-unmapped :severity :warn
       :entity-class :transform :entity t-nm
       :columns (vec unmapped) ::fnd/ctx {:object-type ot}})))

(defn- staged-supplements-unmapped
  [{{objs :object-types tx :transforms} :after :as d}]
  (for [t-nm (sort-by str (keys (diff/staged d :transform)))
        :let [t (live/lookup tx t-nm)]
        :when (sql-findings/sql-transform? t)
        o (tio/outputs-of t)
        :when (and (= :object-type (:kind o)) (= :supplement (:role o))
                   (live/holds? objs (->name-kw (:object-type o))))
        :let [f (unmapped-finding objs #(diff/fk-col? d %) t-nm t o)]
        :when f]
    f))

(def supplement-unmapped
  {:find (fn [d] (lazy-seq (staged-supplements-unmapped d)))
   :says {:supplement-column-unmapped
          (fn [{t-nm :entity unmapped :columns {ot :object-type} ::fnd/ctx}]
            (str "Supplement " (name t-nm) " emits " (str/join ", " (map pr-str unmapped))
                 ", which no attribute of " (name ot) " maps — the import drops "
                 (if (next unmapped) "them" "it") " and the build still reports its rows."
                 " Declare the attribute on " (name ot) " in the ontology, or stop"
                 " selecting the column."))}})

(defn- precision-mismatches
  [d ot spec]
  (let [{:keys [dataset column-map]} (:backing spec)]
    (when dataset
      (let [cols (column-types d dataset)]
        (for [{:keys [attribute-name attribute-type]} (:attribute-list spec)
              :when (decimal-type? attribute-type)
              :let [col (name (get column-map attribute-name attribute-name))
                    ct  (get cols col)]
              :when (and (decimal-type? ct)
                         (not= (vec (rest attribute-type)) (vec (rest ct))))]
          {:rule :decimal-precision-mismatch :entity-class :object-type :entity ot
           :attribute attribute-name :declared attribute-type :column col
           :column-type ct :dataset dataset})))))

(defn- touched-datasets
  [d]
  (into (set (map name (keys (diff/staged d :dataset))))
        (for [t (keys (diff/staged d :transform))
              o (tio/outputs-of (live/lookup (get-in d [:after :transforms]) t))
              :let [ds (:dataset o)]
              :when ds]
          (name ds))))

(defn precision-touched
  [d]
  (let [dss (touched-datasets d)]
    (set (concat (keys (diff/staged d :object-type))
                 (mapcat #(diff/backed-by d %) dss)))))

(def decimal-precision
  {:find (fn [{:keys [after] :as d}]
           (for [ot (sort-by str (precision-touched d))
                 :let [spec (live/lookup (:object-types after) ot)]
                 :when spec
                 f (precision-mismatches d ot spec)]
             f))
   :says {:decimal-precision-mismatch
          (fn [{:keys [entity attribute declared column column-type dataset]}]
            (str "Property " (name attribute) " of " (name entity) " declares "
                 (pr-str declared) ", but the column " column " of " (name dataset)
                 " that backs it is " (pr-str column-type)
                 ". Precision and scale must match the backing column: declare "
                 (pr-str column-type) " on the property, or change the column."))}})
