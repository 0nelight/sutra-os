(ns sutra.builds.plan
  (:require [clojure.set :as set]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.foundation.contracts :as ct]
            [sutra.kit.tree :as tree]
            [sutra.catalog.backing :as catalog-backing]
            [sutra.catalog.outputs :as catalog-outputs]
            [sutra.catalog.retirement :as retirement]
            [sutra.catalog.transforms :as transforms]
            [sutra.catalog.values :as values]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.world-rules :as rules]
            [sutra.parser.params :as params]
            [sutra.types.backing :as backing]
            [sutra.types.entities :as ents]
            [sutra.types.kinds :as kinds]
            [sutra.types.languages :as langs]
            [sutra.types.rid :as rid]
            [sutra.types.table :as tbl]
            [sutra.types.value-types :as vt]
            [sutra.builds.plan.outputs :as outputs]))

(set! *warn-on-reflection* true)

(def plan-only-fields
  #{:board :code-path :column-map :deprecation :description :done-key :fk-attribute :from-key-column
    :function-name :ignore-inherited :input :max-per-run :ops :output-columns :pipeline
    :primary-key :project :prompt-key :rationale :releases :repository :returns
    :semantic-version
    :source :sources :spec-source :status :strategy :tests :to-key-column :version})

(def compute-fields
  (into [] (remove plan-only-fields) (sort (keys (:transforms ents/fields)))))

(def compute-modes
  {:plan #{}

   :code-run #{:query :incremental :outputs}

   :sql-run #{:modules :external-systems :incremental :output :outputs :profile}

   :join-source #{:incremental :output :outputs}})

(defn compute
  [t transform-name mode]
  (let [omitted (or (get compute-modes mode)
                    (refusal/refuse! :refusal.builds/unknown-compute-mode
                                     {:mode mode :known (set (keys compute-modes))}))]
    (assoc (select-keys (params/substitute-transform t) (remove omitted compute-fields))
           :transform-name transform-name)))

(defn value-type-head
  [to]
  (if (and (vector? to) (= :decimal (first to))) :decimal to))

(defn structured-type?
  [t]
  (boolean (and (vector? t) (#{:array :struct} (first t)))))

(defn- derive-casts
  [attribute-list]
  (vec (for [{:keys [attribute-name attribute-type]} attribute-list
             :when (or (vt/auto-cast? (value-type-head attribute-type))
                       (structured-type? attribute-type))]
         {:op :cast :column attribute-name :to attribute-type})))

(def ^:private chain-ends
  (tbl/per-key "builds/plan chain-ends" kinds/kinds
               {:datasource
                (fn [_ cur-k cur-t in]
                  (let [src (:source cur-t)]
                    (when (backing/sourceless-root? cur-t)
                      (refusal/refuse! :refusal.builds/root-transform-no-source
                                       {:transform cur-k}))
                    {:from (cond-> {:datasource (:datasource in)}
                             (seq (:table src)) (assoc :table (:table src))
                             (seq (:query src)) (assoc :query (:query src))
                             (seq (:filters src)) (assoc :filters (:filters src))
                             (:incremental src) (assoc :incremental (:incremental src)))}))

                #{:dataset :object-type}
                (fn [_ _ cur-t in]
                  {:from {(:kind in) (kinds/input-ref-val in)
                          :consumer  (select-keys cur-t [:transform-name :incremental])}})

                :transform
                (fn [ts cur-k _ in]
                  (let [next-k (:transform in)]
                    [::next next-k
                     (or (get ts next-k)
                         (refusal/refuse! :refusal.builds/chained-transform-not-found
                                          {:from cur-k :to next-k}))]))

                #{:link-type :media-set :media-set-items :media :language-model :export :scalar}
                nil}))

(defn walk-chain
  [ts term-k term-t]
  (cond
    (langs/compute-language? (:language term-t))
    {:from {:compute (compute term-t term-k :plan)}}
    :else
    (loop [seen #{} cur-k term-k cur-t term-t accum-ops []]
      (when (contains? seen cur-k)
        (refusal/refuse! :refusal.builds/transform-chain-cycle {:seen seen :at cur-k}))
      (let [in    (:input cur-t)
            accum (into (vec (or (:ops cur-t) [])) accum-ops)
            end   (tbl/on chain-ends (:kind in) ts cur-k cur-t in)]
        (cond
          (vector? end) (let [[_ next-k next-t] end]
                          (recur (conj seen cur-k) next-k next-t accum))
          end           (cond-> end (seq accum) (assoc :transforms accum))
          :else         (refusal/refuse! :refusal.builds/input-kind-invalid
                                         {:transform cur-k :input in}
                                         {:kinds (pr-str (sort kinds/source-input-kinds))}))))))

(defn- resolve-one-backing
  [schema obj-kw [term-k term-t output]]
  (let [chain   (walk-chain (:transforms schema) term-k term-t)
        derived (derive-casts (get-in schema [:object-types obj-kw :attribute-list]))]
    (cond-> chain
      (:primary-key output) (assoc :primary-key (:primary-key output))
      (:column-map output) (assoc :column-map (:column-map output))
      (:edit-conflict output) (assoc :edit-conflict (:edit-conflict output))
      (seq derived) (update :transforms #(into (vec derived) (or % [])))
      :always (assoc :output-name (:name output) :role (:role output :primary)))))

(defn- resolve-authored-backing
  [schema obj-kw {:keys [dataset primary-key column-map]}]
  (let [derived (derive-casts (get-in schema [:object-types obj-kw :attribute-list]))]
    (cond-> {:from {:dataset dataset} :role :primary}
      primary-key   (assoc :primary-key primary-key)
      column-map    (assoc :column-map column-map)
      (seq derived) (assoc :transforms (vec derived)))))

(defn resolve-source-for-object-type
  [schema obj-kw]
  (let [backing (catalog-backing/object-backing schema obj-kw)]
    (cond
      (:authored? backing)
      (when-not (get-in schema [:datasets (:dataset backing) :stream])
        (resolve-authored-backing schema obj-kw backing))

      :else
      (when-let [prim (catalog-outputs/primary-output-for schema :object-type obj-kw)]
        (resolve-one-backing schema obj-kw prim)))))

(defn- resolve-backings
  [schema obj-kw supplement?]
  (vec (for [[_ _ output :as trip] (catalog-outputs/find-outputs-for schema :object-type obj-kw)
             :when (= supplement? (= :supplement (:role output)))]
         (resolve-one-backing schema obj-kw trip))))

(defn resolve-source-for-link-type
  [schema conn-kw]
  (when-let [[term-k term-t output] (catalog-outputs/find-output-for schema :link-type conn-kw)]
    (let [conn  (get-in schema [:link-types conn-kw])
          chain (fn [& kvs]
                  (merge (walk-chain (:transforms schema) term-k term-t)
                         {:from-type   (:from-object-type conn)
                          :to-type     (:to-object-type conn)
                          :output-name (:name output)}
                         (apply hash-map kvs)))
          join  (fn [& kvs]
                  (apply chain :strategy :join-table
                         :from-key-column (:from-key-column output)
                         :to-key-column (:to-key-column output)
                         kvs))]
      (cond
        (= :resolve-by-shared-property (:strategy term-t))
        (chain :strategy        :resolve-by-shared-property
               :from-key-column (:from-key-column output)
               :match-column    (:match-column output)
               :shared-property (:shared-property output))

        (langs/compute-language? (:language term-t))
        (join)

        (= :polymorphic (:strategy term-t))
        {:strategy    :polymorphic
         :output-name (:name output)
         :sources     (mapv (fn [s]
                              {:from            (if (langs/compute-language? (:language s))
                                                  {:compute
                                                   (compute s
                                                            (keyword
                                                             (str (name (:name output)) "-"
                                                                  (name (:from-type s))))
                                                            :join-source)}
                                                  (cond-> {:datasource (:datasource s)}
                                                    (seq (:table s)) (assoc :table (:table s))
                                                    (seq (:query s)) (assoc :query (:query s))))
                               :from-type       (:from-type s)
                               :from-key-column (:from-key-column s)
                               :to-type         (or (:to-type s) (:to-object-type conn))
                               :to-key-column   (:to-key-column s)})
                            (:sources term-t))}

        :else
        (case (kinds/link-backing (get-in term-t [:input :kind]))
          :join-table
          (join)

          :endpoint-fk
          (let [fk-side-type (get-in term-t [:input :object-type])
                fk-side (cond
                          (= fk-side-type (:from-object-type conn)) :from
                          (= fk-side-type (:to-object-type conn)) :to
                          :else
                          (refusal/refuse! :refusal.builds/endpoint-fk-not-endpoint
                                          {:link conn-kw
                                           :fk-side-type fk-side-type
                                           :endpoints [(:from-object-type conn)
                                                       (:to-object-type conn)]}))]
            (cond-> {:strategy :endpoint-fk
                     :from-type (:from-object-type conn)
                     :to-type (:to-object-type conn)
                     :fk-side fk-side
                     :fk-attribute (:fk-attribute output)
                     :cardinality (:cardinality conn)
                     :output-name (:name output)}
              (:on-null output) (assoc :on-null (:on-null output))
              (:on-unresolved output) (assoc :on-unresolved (:on-unresolved output))
              (:on-duplicate output) (assoc :on-duplicate (:on-duplicate output)))))))))

(defn entries
  [entry]
  (if (vector? entry) entry [entry]))

(defn- checked-object-backing
  [entry]
  (if (vector? entry)
    (mapv #(ct/check! :sutra.foundation.contracts/object-backing %) entry)
    (ct/check! :sutra.foundation.contracts/object-backing entry)))

(defn- plan-of
  [schema guard]
  {:objects     (into {}
                      (for [[k _] (:object-types schema)
                            :let [e (guard :object-type k
                                           (fn []
                                             (when-let [entry
                                                        (if (and
                                                             (outputs/resolved-object-type?
                                                              schema k)
                                                             (not
                                                              (:authored?
                                                               (catalog-backing/object-backing schema k))))
                                                          (let [srcs
                                                                (resolve-backings
                                                                 schema k false)]
                                                            (if (> (count srcs) 1) srcs
                                                                (first srcs)))
                                                          (resolve-source-for-object-type
                                                           schema k))]
                                               (checked-object-backing entry))))]
                            :when e]
                        [k e]))
   :supplements (into {}
                      (for [[k _] (:object-types schema)
                            :let [e (guard :supplement k
                                           (fn []
                                             (when-let [supps
                                                        (seq (resolve-backings schema k true))]
                                               (mapv
                                                #(ct/check!
                                                  :sutra.foundation.contracts/object-backing %)
                                                supps))))]
                            :when e]
                        [k e]))
   :links       (into {}
                      (for [[k _] (:link-types schema)
                            :let [e (guard :link-type k
                                           (fn []
                                             (when-let [entry
                                                        (resolve-source-for-link-type schema k)]
                                               (ct/check! :sutra.foundation.contracts/link-backing
                                                          entry))))]
                            :when e]
                        [k e]))})

(defn derive-plan-from-schema
  [schema]
  (plan-of schema (fn [_ _ f] (f))))

(defn- entity-source
  [entry]
  (cond-> {}
    (:datasource entry)
    (assoc :datasource (:datasource entry))
    (get-in entry [:from :datasource])
    (assoc :from {:datasource (get-in entry [:from :datasource])})
    (get-in entry [:join-table :datasource])
    (assoc :join-table {:datasource (get-in entry [:join-table :datasource])})))

(defn sources-only
  [plan]
  (update-vals plan #(update-vals % entity-source)))

(defn derive-plan-leniently
  [schema]
  (let [bad (volatile! [])]
    {:plan       (plan-of schema (fn [kind k f]
                                   (try (f)
                                        (catch Exception e
                                          (vswap! bad conj {:kind  kind
                                                            :name  k
                                                            :error (ex-message e)})
                                          nil))))
     :unresolved @bad}))

(defn dataset-output-producers
  [schema]
  (for [[t-kw t] (:transforms schema)
        :let [ds-outs (filter #(= :dataset (:kind %)) (catalog-outputs/outputs-of t))]
        :when (seq ds-outs)]
    [t-kw t ds-outs]))

(defn- resource-key [schema section x] (some-> (or (lookup/rid-of schema section x) x) str))

(defn- object-type-keys
  [schema ot]
  (let [r (or (lookup/rid-of schema :object-types ot) ot)]
    [(str r) (some->> (:dataset (catalog-backing/object-backing schema r)) (resource-key schema :datasets))]))

(def ^:private written-keys
  (tbl/per-key "builds/plan written-keys" kinds/output-kinds
               {:object-type (fn [schema _ k] (object-type-keys schema k))
                :export      (fn [schema section k]
                               [(outputs/external-key (resource-key schema section k))])
                #{:link-type :dataset :media-set}
                (fn [schema section k] [(resource-key schema section k)])}))

(defn output-keys
  [schema t-kw t]
  (cond-> (vec (mapcat (fn [o]
                         (when-let [[section k] (outputs/output-ref o)]
                           (tbl/on written-keys (:kind o) schema section k)))
                       (catalog-outputs/outputs-of t)))
    (:output-columns t) (conj (str t-kw))))

(defn input-key
  [schema in]
  (if (= :transform (:kind in))
    (str (catalog-outputs/transform-input-key schema in))
    (when-let [sec (kinds/input-names-section in)]
      (resource-key schema sec (kinds/input-ref-val in)))))

(defn build-footprint
  [schema t-kws {:keys [objects links supplements]}]
  (let [canon (fn [section x] (resource-key schema section x))
        ts    (keep #(when-let [t (get-in schema [:transforms %])] [% t]) t-kws)
        clean #(into #{} (remove nil?) %)]
    {:writes (clean (concat (mapcat (fn [[k t]] (output-keys schema k t)) ts)
                            (mapcat #(object-type-keys schema %)
                                    (concat (keys objects) (keys supplements)))
                            (map #(canon :link-types %) (keys links))))
     :reads  (clean (concat (mapcat (fn [[_ t]] (map #(input-key schema %) (vals (:inputs t)))) ts)
                            (mapcat (fn [l] [(canon :object-types (:from-type l))
                                             (canon :object-types (:to-type l))])
                                    (vals links))))}))

(defn resolve-scope-names
  [schema {:keys [transforms object-types datasets unresolved strategy build]}]
  (let [nm  (fn [kind rid] (or (lookup/name-of schema kind rid) rid))
        nms (fn [kind xs] (vec (sort-by str (map #(nm kind %) xs))))]
    {:strategy     (or strategy :default)
     :targets      (nms :transforms transforms)
     :build        (nms :transforms build)
     :jobs         (count build)
     :object-types (nms :object-types object-types)
     :datasets     (nms :datasets datasets)
     :unresolved   (vec (sort-by str unresolved))}))

(defn- upstream-intermediates
  [schema target-transforms]
  (let [ts       (:transforms schema)
        t-inputs (fn [t-kw]
                   (for [[_ in] (:inputs (get ts t-kw))
                         :when (= :transform (:kind in))]
                     (:transform in)))]
    (set (tree/reach t-inputs (mapcat t-inputs target-transforms)))))

(defn- scope-import
  [import-block target-obj-types]
  (let [tset (set target-obj-types)]
    {:objects     (select-keys (:objects import-block) tset)
     :supplements (select-keys (:supplements import-block) tset)
     :links       (into {} (filter (fn [[_ spec]]
                                     (or (tset (:from-type spec)) (tset (:to-type spec))))
                                   (:links import-block)))}))

(defn- canon
  [x]
  (when (some? x)
    (-> (if (keyword? x) (name x) (str x)) str/lower-case (str/replace "_" "-"))))

(defn- name-index
  [schema section]
  (into {} (for [[rid spec] (get schema section)
                 :let [nm (get spec (rid/name-field-of section))]
                 :when nm]
             [(canon nm) rid])))

(defn- object-type-rid
  [schema]
  (let [idx (name-index schema :object-types)]
    (fn [x] (or (when (contains? (:object-types schema) x) x) (get idx (canon x))))))

(def ^:private filled-types
  (tbl/per-key "builds/plan filled-types" kinds/output-kinds
               {:object-type (fn [_ ->rid o] (some-> (->rid (:object-type o)) vector))
                :dataset     (fn [schema _ o]
                               (catalog-backing/types-backed-by schema (:dataset o)))
                #{:link-type :media-set :export} nil}))

(defn- produced-types
  [schema ->rid t]
  (into #{} (mapcat #(tbl/on filled-types (:kind %) schema ->rid %))
        (catalog-outputs/outputs-of t)))

(defn resolve-build-targets
  [schema names]
  (let [ts       (:transforms schema)
        dss      (:datasets schema)
        xf-idx   (name-index schema :transforms)
        ds-idx   (name-index schema :datasets)
        exp-idx  (name-index schema :exports)
        exp->xf  (into {} (for [[k t] ts
                                :let [e (some :export (catalog-outputs/outputs-of t))]
                                :when e]
                            [e k]))
        ->rid    (object-type-rid schema)
        xf-obj   (into {} (for [[k t] ts
                                :let [rids (produced-types schema ->rid t)]
                                :when (seq rids)]
                            [k rids]))
        obj->xfs (reduce-kv (fn [m k rids]
                              (reduce #(update %1 %2 (fnil conj #{}) k) m rids))
                            {} xf-obj)
        add-xf   (fn [acc k] (-> acc
                                 (update :transforms conj k)
                                 (update :object-types into (get xf-obj k))))]
    (reduce
     (fn [acc n]
       (let [c  (canon n)
             xf (or (when (contains? ts n) n)  (get xf-idx c))
             ot (->rid n)
             ds (or (when (contains? dss n) n) (get ds-idx c))
             ex (or (when (contains? (:exports schema) n) n) (get exp-idx c))]
         (cond
           xf (add-xf acc xf)
           ot (reduce add-xf (update acc :object-types conj ot) (get obj->xfs ot))
           ds (reduce add-xf (-> acc
                                 (update :datasets conj ds)
                                 (update :object-types into (catalog-backing/types-backed-by schema ds)))
                      (catalog-outputs/dataset-producers schema ds))
           ex (if-let [k (exp->xf ex)]
                (add-xf acc k)
                (update acc :unresolved conj n))
           :else (update acc :unresolved conj n))))
     {:transforms #{} :object-types #{} :datasets #{} :unresolved #{}}
     names)))

(defn link-only-scope-violations
  [schema only-types]
  (let [ot-name (fn [rid] (or (:object-type-name (get-in schema [:object-types rid])) rid))
        by-name (into {} (for [[_ t] (:transforms schema)
                               :let [nm (:transform-name t)] :when nm]
                           [nm t]))
        viol (for [n     only-types
                   :let  [t (get by-name (keyword n))]
                   :when t
                   :let  [kinds (set (map :kind (catalog-outputs/outputs-of t)))]
                   :when (and (contains? kinds :link-type)
                              (not (contains? kinds :object-type)))
                   :let  [lt  (some #(when (= :link-type (:kind %)) (:link-type %))
                                    (catalog-outputs/outputs-of t))
                          ltd (lookup/conn-type schema lt)]]
               {:name (keyword n)
                :from (some-> ltd lookup/from-type ot-name)
                :to   (some-> ltd lookup/to-type ot-name)})]
    (seq viol)))

(defn fk-link-transform?
  [schema t-kw]
  (let [outs (catalog-outputs/outputs-of (get-in schema [:transforms t-kw]))]
    (and (seq outs)
         (every? #(and (= :link-type (:kind %))
                       (rules/fk-link-spec schema (:link-type %)))
                 outs))))

(def build-strategies
  #{:ancestors :between :selected})

(defn readers-of
  [schema {:keys [object-types datasets]}]
  (let [nodes (into (into #{} (map #(vector :object-types %)) object-types)
                    (map #(vector :datasets %)) datasets)]
    (into #{}
          (keep (fn [[k t]]
                  (when (some #(some-> % kinds/input-target nodes)
                              (cons (:input t) (vals (:inputs t))))
                    k)))
          (:transforms schema))))

(defn build-scope
  ([schema targets strategy from] (build-scope schema targets strategy from #{}))
  ([schema targets strategy from readers]
   (let [ts     (:transforms schema)
         known  (fn [ks] (into #{} (filter #(contains? ts %)) ks))
         target (known targets)]
     (case strategy
       :ancestors (transforms/upstream-closure schema target)
       :between   (let [boundary (known from)]
                    (-> (set/intersection (transforms/upstream-closure schema target)
                                          (transforms/downstream-closure
                                           schema (into boundary (known readers))))
                        (set/difference boundary)))
       :selected  target
       (into target (upstream-intermediates schema target))))))

(defn- with-output
  [k]
  (fn [schema t-kws]
    (let [ts (:transforms schema)]
      (into #{} (filter #(some k (catalog-outputs/outputs-of (get ts %)))) t-kws))))

(def scope-kinds
  {:materializations {:node    :producer
                      :selects (fn [schema t-kws]
                                 (into #{} (filter #(get-in schema [:transforms % :output-columns]))
                                       t-kws))}
   :datasets         {:node    :producer
                      :selects (fn [schema t-kws]
                                 (set/intersection
                                  (set t-kws)
                                  (into #{} (map first) (dataset-output-producers schema))))}
   :exports          {:node :export :selects (with-output :export)}
   :media-syncs      {:node :media-sync :selects (with-output :media-set)}})

(defn- job-scope
  [schema t-kws]
  {:node-scope (into {} (for [[k {:keys [selects]}] scope-kinds] [k (selects schema t-kws)]))})

(defn in-node-scope
  [node-scope kind]
  (if node-scope #(contains? (get node-scope kind) %) (constantly true)))

(defn scoped-import-plan
  ([schema import-block0 only-types] (scoped-import-plan schema import-block0 only-types nil))
  ([schema import-block0 only-types {:keys [strategy from]}]
   (if-not (seq only-types)
     {:import import-block0}
     (let [{:keys [transforms object-types] :as tgt} (resolve-build-targets schema only-types)
           from-tgt  (when (seq from) (resolve-build-targets schema from))
           from-t    (:transforms from-tgt)
           closure   (into #{} (remove #(fk-link-transform? schema %))
                           (build-scope schema transforms strategy from-t
                                        (if from-tgt (readers-of schema from-tgt) #{})))
           obj-rids  (if (#{:ancestors :between} strategy)
                       (into object-types (:object-types (resolve-build-targets schema closure)))
                       object-types)]
       (merge {:import (scope-import import-block0 obj-rids)
               :scope  (assoc tgt :strategy (or strategy :default) :build closure)}
              (job-scope schema closure))))))

(defn scope-preview
  [schema scope]
  (let [all    (set (keys (:transforms schema)))
        scope' (or scope {:transforms all :build all :strategy :full})
        build  (set (:build scope'))
        nm     (fn [k] (or (lookup/name-of schema :transforms k) k))]
    (assoc (resolve-scope-names schema scope')
           :levels (into []
                         (comp (map (fn [lvl]
                                      (vec (sort-by str (map nm (filter build lvl))))))
                               (remove empty?))
                         (transforms/topo-levels schema)))))

(defn scope->only-types
  [schema target scope]
  (when (and (string? target) (not (str/blank? target)))
    (let [tkws (into #{} (comp (map str/trim) (remove str/blank?) (map keyword))
                     (str/split target #","))]
      (if-not (#{:with-dependencies :downstream} scope)
        tkws
        (let [ts         (:transforms schema)
              ->rid      (object-type-rid schema)
              target-xfs (:transforms (resolve-build-targets schema tkws))
              closure    (if (= scope :downstream)
                           (transforms/downstream-closure schema target-xfs)
                           (transforms/upstream-closure schema target-xfs))
              type-name  (fn [r] (or (get-in schema [:object-types r :object-type-name])
                                     (when (keyword? r) r)))]
          (into tkws (mapcat (fn [xf]
                               (let [t (get ts xf)]
                                 (or (seq (keep type-name (produced-types schema ->rid t)))
                                     (some-> (:transform-name t) vector)))))
                closure))))))

(defn schedule-targets
  [schema {:keys [target scope from]}]
  (let [split #(into #{} (comp (map str/trim) (remove str/blank?) (map keyword))
                     (str/split (str %) #","))]
    (if (and (#{:connecting :between} scope) (seq from) (seq target))
      {:only (split target) :strategy :between :from (split from)}
      {:only (scope->only-types schema target scope)})))

(defn- missing-inputs
  [schema t-kws]
  (let [orphan? (set (keys (retirement/orphaned-producers schema)))]
    (for [t-kw t-kws
          :when (not (orphan? t-kw))
          :let [t (get-in schema [:transforms t-kw])]
          :when t
          [alias in] (:inputs t)
          :let [section (kinds/input-names-section in)
                ref     (kinds/input-ref-val in)]
          :when (if (= :transform (:kind in))
                  (not (contains? (:transforms schema) (:transform in)))
                  (and section (some? ref) (nil? (lookup/rid-of schema section ref))))]
      {:transform (or (:transform-name t) t-kw) :input alias :kind (:kind in)
       :ref (or ref (:transform in))})))

(defn- build-loop
  [schema t-kws]
  (let [built (set t-kws)]
    (first (filter #(or (some built (:cycle %)) (built (:transform %)))
                   (transforms/loop-problems schema)))))

(defn- refused-plan
  [code data params]
  {:code code :data data :params params :error (refusal/message code (merge data params))})

(defn resolution-refusal
  [schema t-kws]
  (if-let [problem (build-loop schema t-kws)]
    (let [[code data] (transforms/loop-refusal #(or (lookup/name-of schema %1 %2) %2) problem)]
      (refused-plan :refusal.builds/resolution-failed (assoc data :loop code)
                    {:why (refusal/message code data)}))
    (when-let [gone (seq (missing-inputs schema t-kws))]
      (refused-plan :refusal.builds/inputs-missing
                    {:missing-inputs
                     (mapv #(update-vals % (fn [v] (if (keyword? v) (name v) (str v)))) gone)}
                    {:list (str/join "; " (map (fn [{:keys [transform input kind ref]}]
                                                 (str (name transform) "." (name input) " → "
                                                      (name kind) " " (pr-str ref)))
                                               gone))}))))

(defn- scope-refusal
  [schema {:keys [only strategy from]} scoped]
  (let [unres (fn [names] (seq (:unresolved (resolve-build-targets schema names))))
        ot-name (fn [rid] (some-> (get-in schema [:object-types rid :object-type-name]) name))
        by-name (name-index schema :transforms)
        fk-xfs  (for [n only
                      :let [k (get by-name (canon n))]
                      :when (and k (fk-link-transform? schema k))]
                  [n (keep #(rules/fk-link-spec schema (:link-type %))
                           (catalog-outputs/outputs-of (get-in schema [:transforms k])))])
        link-hint (fn [names]
                    (if-let [l (seq (filter #(lookup/rid-of schema :link-types (keyword (name %)))
                                            names))]
                      (str " (" (str/join ", " (map name l)) " is a link-type — "
                           "a link has no dataset to build; scope its endpoint "
                           "object-types instead)")
                      ""))
        nothing (delay (let [{:keys [import scope]} @scoped]
                         (when (and (empty? (:build scope)) (every? empty? (vals import)))
                           scope)))]
    (cond
      (and strategy (not (contains? build-strategies strategy)))
      (refused-plan :refusal.builds/unknown-strategy {:strategy strategy}
                    {:strategies (str/join ", " (sort (map name build-strategies)))})

      (and (= :between strategy) (empty? from))
      (refused-plan :refusal.builds/between-needs-from {:strategy strategy} nil)

      (and (seq from) (not= :between strategy))
      (refused-plan :refusal.builds/from-needs-between {:strategy strategy :from from} nil)

      (and (seq only) (unres only))
      (refused-plan :refusal.builds/unknown-targets {:unresolved (vec (unres only))}
                    {:names (str/join ", " (map name (unres only)))
                     :hint  (link-hint (unres only))})

      (and (seq from) (unres from))
      (refused-plan :refusal.builds/unknown-from {:unresolved (vec (unres from))}
                    {:names (str/join ", " (map name (unres from)))})

      (seq fk-xfs)
      (let [[n specs] (first fk-xfs)
            {:keys [fk-type pk-type fk-attribute]} (first specs)]
        (refused-plan :refusal.builds/fk-link-not-a-job
                      {:fk-link-transform (name n) :fk-type (ot-name fk-type)
                       :pk-type (ot-name pk-type)}
                      {:fk-attribute (name fk-attribute)}))

      (and (seq only) (link-only-scope-violations schema only))
      (let [v (first (link-only-scope-violations schema only))]
        (refused-plan :refusal.builds/link-only-scope {:link-only-transform (:name v)}
                      {:link (name (:name v))
                       :endpoints (str/join "," (remove nil? [(some-> (:from v) name)
                                                              (some-> (:to v) name)]))}))

      (and (seq only) @nothing)
      (let [ots (->> (:object-types @nothing)
                     (keep #(some-> (get-in schema [:object-types % :object-type-name]) name))
                     sort vec)]
        (refused-plan :refusal.builds/empty-build-scope
                      {:empty-build-scope (mapv name only) :object-types ots}
                      {:scope    (str/join ", " (map name only))
                       :produced (if (seq ots) (str/join ", " ots) "it")}))

      :else
      (resolution-refusal schema (if (seq only)
                                   (:build (:scope @scoped))
                                   (keys (:transforms schema)))))))

(defn build-scope-refusal
  [schema {:keys [only strategy from] :as targets}]
  (scope-refusal schema targets
                 (delay (scoped-import-plan schema (:plan (derive-plan-leniently schema))
                                            only {:strategy strategy :from from}))))

(defn build-plan
  [schema {:keys [only transforms strategy from] :as targets}]
  (let [import0 (derive-plan-from-schema schema)
        scoped  (if (seq transforms)
                  (assoc (job-scope schema transforms) :import import0)
                  (scoped-import-plan schema import0 only {:strategy strategy :from from}))
        jobs    (vec (or (seq (:build (:scope scoped))) (seq transforms)
                         (keys (:transforms schema))))]
    (merge scoped
           (build-footprint schema jobs (:import scoped))
           {:datasources (values/sources->datasources schema)
            :jobs        jobs
            :refusal     (if (seq transforms)
                           (resolution-refusal schema jobs)
                           (scope-refusal schema targets (delay scoped)))})))
