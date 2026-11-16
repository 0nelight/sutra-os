(ns sutra.objects.views
  (:require [sutra.kit.coll :as coll]
            [malli.core :as m]
            [sutra.catalog.object-ref :as oref]
            [clojure.string :as str]
            [sutra.catalog.interfaces :as interfaces]
            [sutra.catalog.link-ends :as link-ends]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.derived :as sd]
            [sutra.types.world-rules :as rules]
            [sutra.types.expr :as expr]
            [sutra.foundation.profile :as prof]
            [sutra.store.access.substrate :as sub]
            [sutra.store.access.timeseries :as ats]
            [sutra.objects.planner :as planner]
            [sutra.objects.saved-sets :as temp-sets]
            [sutra.objects.explorer :as explorer]
            [sutra.types.aggregate :as agg]
            [sutra.types.object-set :as os]
            [sutra.ref.objectset :as ros]
            [sutra.types.schema :as sch]
            [sutra.types.query :as q]
            [sutra.types.rid :as rid]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.refusal :as refusal]
            [sutra.types.link-side :as ls]
            [sutra.types.schema-view :as sv]
            [sutra.types.derive-graph :as dg]
            [sutra.types.witness :as wit]
            [sutra.objects.plan-run :as plan-run]))

(set! *warn-on-reflection* true)

(defn- type-rid [schema nm]
  (let [idx (sch/index schema)]
    (or (sch/rid-of idx :object-types nm)
        (sch/rid-of idx :interfaces nm)
        (refusal/refuse! :refusal.objects/unknown-type {:type nm}))))

(defn- side-name? [schema rid k]
  (boolean (some #(= k (:api-name %)) (ls/outgoing schema rid))))

(defn- attr-types
  [schema rid]
  (:types (lookup/attrs-index schema rid)))

(defn- coerce-where-val
  [attr-type op raw]
  (cond
    (not (string? raw))                          raw
    (q/text-ops op)                              raw
    (= :boolean attr-type)
    (case (str/lower-case raw)
      ("true" "t" "1" "yes" "y" "wahr" "ja")  true
      ("false" "f" "0" "no" "n" "falsch" "nein") false
      raw)
    (= :integer attr-type)                       (or (parse-long raw) raw)
    (or (= :decimal attr-type)
        (and (vector? attr-type) (= :decimal (first attr-type))))
    (or (parse-double raw) raw)
    :else                                        raw))

(defn- attr-preds
  [types attr m]
  (when-not (map? m)
    (refusal/refuse! :refusal.objects/filter-not-a-map {:attr attr :filter m}))
  (when (empty? m)
    (refusal/refuse! :refusal.objects/filter-empty {:attr attr}))
  (mapv (fn [[op val]]
          (when-not (q/surface-ops op)
            (refusal/refuse! :refusal.objects/filter-unknown-op
                             {:op op :attr attr}
                             {:known (str/join ", " (sort (map str q/surface-ops)))}))
          [op attr (coerce-where-val (get types attr) op val)])
        m))

(defn- conjoin
  [preds]
  (case (count preds)
    0 nil
    1 (first preds)
    (into [:and] preds)))

(defn- identity-preds
  [m]
  (when-not (and (map? m) (= [:=] (keys m)))
    (refusal/refuse! :refusal.objects/filter-id-equality {:attr :id :filter m}))
  (let [v (get m :=)]
    [[:id-in (if (coll? v) (mapv str v) [(str v)])]]))

(defn- id-attr?
  [schema rid]
  (boolean (some #(= :id (:attribute-name %)) (lookup/attrs schema rid))))

(declare translate-where)

(defn- junction-pred
  [schema root-rid op parts]
  (let [preds (mapv (fn [w]
                      (let [{:keys [wheres hops]} (translate-where schema root-rid w)]
                        (when (seq hops)
                          (refusal/refuse! :refusal.objects/op-own-attributes {:where w} {:op op}))
                        (conjoin wheres)))
                    parts)]
    (if (= :or op)
      (when (not-any? nil? preds) (into [:or] preds))
      (conjoin (into [] (remove nil?) preds)))))

(declare text-attrs)

(defn- ordered-attr [order-by] (when (not= q/relevance order-by) order-by))

(defn- translate-where
  [schema root-rid where]
  (reduce
   (fn [acc [k v]]
     (cond
       (= :matches-tokens-in k)
       (let [[attrs text] v
             named        (set attrs)]
         (update acc :wheres conj
                 [:matches-tokens-in
                  (filterv named (text-attrs schema root-rid))
                  text]))

       (and (#{:and :or} k) (vector? v))
       (if-let [p (junction-pred schema root-rid k v)]
         (update acc :wheres conj p)
         acc)

       (side-name? schema root-rid k)
       (let [far (:far-type (ls/resolve-side schema root-rid k))]
         (update acc :hops conj
                 (os/match-hop (ls/resolve-side schema root-rid k)
                               (conjoin (into []
                                              (mapcat (fn [[attr m]]
                                                        (if (and (= :id attr)
                                                                 (not (and far
                                                                           (id-attr? schema far))))
                                                          (identity-preds m)
                                                          (attr-preds {} attr m))))
                                              v)))))

       (and (= :id k) (not (id-attr? schema root-rid)))
       (update acc :wheres into (identity-preds v))

       :else
       (update acc :wheres into (attr-preds (attr-types schema root-rid) k v))))
   {:wheres [] :hops []}
   where))

(declare derived-names)

(defn- text-attrs
  [schema rid]
  (let [dnames (derived-names schema rid)]
    (into []
          (keep (fn [[a t]] (when (and (#{:string :text} t) (not (dnames a))) a)))
          (sort-by key (attr-types schema rid)))))

(defn- keyword-pred
  [schema rid kw]
  [:matches-tokens (text-attrs schema rid) (str/trim kw)])

(defn- build-object-set
  [schema {:keys [find ids where keywords order-by sort-dir limit offset]} select]
  (let [root-rid (type-rid schema find)
        {:keys [wheres hops]} (translate-where schema root-rid (or where {}))
        wheres   (cond-> wheres
                   (some? ids) (conj [:id-in (vec ids)])
                   (not (str/blank? keywords)) (conj (keyword-pred schema root-rid keywords)))]
    (os/qt->ir (cond-> {:root root-rid}
                 (seq wheres)            (assoc :where (conjoin wheres))
                 order-by                (assoc :order-by order-by)
                 (and order-by sort-dir) (assoc :sort-dir sort-dir)
                 (some? limit)           (assoc :limit limit)
                 (some? offset)          (assoc :offset offset)
                 select                  (assoc :select select))
               #(os/intersect-hops % hops))))

(defn- temporary-ir
  [store {:keys [object-set]}]
  (cond
    (nil? object-set) nil
    (os/temporary-rid? object-set)
    (:ir (temp-sets/fetch store object-set (grants/acting-user (grants/actor store))))
    (explorer/exploration-rid? object-set) (explorer/definition store object-set)))

(defn- saved-set-ir
  [auth schema ident args temporary]
  (if (or (os/temporary-rid? ident) (explorer/exploration-rid? ident))
    (or temporary
        (refusal/refuse! :refusal.objects/temporary-set-gone {:object-set ident}))
    (let [spec (os/by-name (:object-sets schema) (if (string? ident) ident (keyword ident)))]
      (when-not spec
        (refusal/refuse! :refusal.objects/unknown-object-set {:object-set ident}))
      (grants/authorize-read! auth spec (:id spec))
      (ros/instantiate (os/definition spec) (or args {})))))

(defn- own-shaping?
  [ir]
  (boolean (some #(#{:order :page} (:op %)) (os/nodes ir))))

(defn- refine-set
  [schema root-rid ir where]
  (let [{:keys [wheres hops]} (translate-where schema root-rid (or where {}))]
    (os/intersect-hops (if (seq wheres) {:op :filter :where (conjoin wheres) :in ir} ir)
                       hops)))

(defn- around-hops
  [schema ir hops]
  (reduce (fn [in {:keys [link]}]
            (let [side (ls/resolve-side schema (os/result-type schema in) link)]
              {:op :search-around :link (:link side) :dir (:dir side) :in in}))
          ir
          hops))

(defn- shape-around
  [ir {:keys [order-by sort-dir limit offset]}]
  (cond-> ir
    order-by      (as-> $ {:op :order :order-by order-by :sort-dir sort-dir :in $})
    (some? limit) (as-> $ {:op :page :limit limit :offset offset :in $})))

(defn- aggregate-set
  [ir aggs group-by shaping]
  (let [strip (fn strip [n] (if (#{:order :page :project} (:op n)) (strip (:in n)) n))]
    (shape-around (cond-> {:op :aggregate :aggs aggs :in (strip ir)}
                    (seq group-by) (assoc :group-by (vec group-by)))
                  (or shaping {}))))

(defn- count-set
  [ir]
  (aggregate-set ir :count nil nil))

(defn- side-edges
  [reads {:keys [link dir]} ids]
  (let [[end near far] (if (= :to dir)
                         [:from :from-object-id :to-object-id]
                         [:to :to-object-id :from-object-id])]
    [near far (sub/links reads link {end (mapv str ids)})]))

(defn- connected-index
  [reads side root-ids]
  (let [roots (into #{} (map str) root-ids)]
    (if (empty? roots)
      {}
      (let [[near far edges] (side-edges reads side roots)
            by-near (group-by near edges)
            idx     (coll/index-by :id (sub/objects reads (:far-type side)
                                       (vec (distinct (map far edges)))))]
        (into {} (map (fn [id] [id (into [] (keep idx) (map far (get by-near id)))])) roots)))))

(defn- traverse-fn-for-roots
  [reads schema root-type conn-refs root-ids]
  (let [side-of (memoize #(ls/resolve-side schema root-type %))
        cache   (into {} (for [c conn-refs] [c (connected-index reads (side-of c) root-ids)]))]
    (fn [_state object-id conn object-type]
      (let [oid (str object-id)
            m   (get cache conn)]
        (if (and (= object-type root-type) (contains? m oid))
          (get m oid)
          (get (connected-index reads (ls/resolve-side schema object-type conn) [oid]) oid []))))))

(defn- attach-search-arounds
  [reads schema root-rid rows hops]
  (reduce
   (fn [rows rel]
     (let [[conn-name attrs] (first rel)
           by-root (connected-index reads (ls/resolve-side schema root-rid conn-name)
                                    (map :id rows))]
       (mapv (fn [row]
               (assoc-in row [:values conn-name]
                         (mapv #(select-keys (:values %) attrs) (get by-root (str (:id row)) []))))
             rows)))
   rows
   hops))

(defn- derived-attrs
  [schema rid]
  (:derived (lookup/attrs-index schema rid)))

(defn- derived-names
  [schema rid]
  (into #{} (map first) (derived-attrs schema rid)))

(defn- derived-plan
  ([schema rid want] (derived-plan schema rid want nil))
  ([schema rid want extra]
   (expr/eval-order (concat (derived-attrs schema rid) extra)
                    (when want (into (set want) (map first) extra)))))

(defn- witnessed-plan
  ([schema rid want] (witnessed-plan schema rid want nil))
  ([schema rid want extra]
   (let [declared (derived-plan schema rid want extra)
         {:keys [questions pairs]} (wit/plan declared)]
     [(into declared pairs) questions])))

(defn- planned-answers
  [reads schema rid rows derived]
  (let [read-ir #(hash-map :op :static :root rid :ids (vec %))]
    (if (= (read-ir []) (sd/plan schema (read-ir []) derived))
      {}
      (into {}
            (comp (mapcat #(sub/select reads (sd/plan schema (read-ir %) derived)))
                  (map (juxt (comp str :id) :values)))
            (partition-all 500 (mapv (comp str :id) rows))))))

(declare attach-derived*)

(defn- type-label
  [schema rid]
  (name (or (get-in schema [:object-types rid :object-type-name])
            (get-in schema [:interfaces rid :interface-name])
            rid)))

(defn- derive-reads
  [schema]
  [(fn [T] (derived-attrs schema T))
   (fn [T handle]
     (try (:far-type (ls/resolve-side schema T handle))
          (catch clojure.lang.ExceptionInfo _ nil)))])

(defn- derive-cycles
  [schema]
  (sch/column schema ::derive-cycles
              (fn [W]
                (let [W           (sch/indexed W)
                      [d-of f-of] (derive-reads W)]
                  (dg/cycle-index
                   (dg/graph (concat (keys (:object-types W)) (keys (:interfaces W)))
                             d-of f-of))))))

(defn- refuse-cycle!
  [schema chain]
  (refusal/refuse! :refusal.objects/derive-cycle
                   {:cycle (mapv (fn [[T a]] {:object-type (type-label schema T) :attribute a})
                                 chain)}
                   {:why (dg/cycle-message #(str (type-label schema (first %)) "."
                                                 (name (second %)))
                                           chain)}))

(defn- derive-cap
  []
  (:max-derive-objects planner/default-caps))

(defn- refuse-chain-size!
  [schema rid {:keys [handle far attrs]} loaded]
  (refusal/refuse! :refusal.objects/derive-cap
                   {:cap         :max-derive-objects
                    :limit       (derive-cap)
                    :actual      loaded
                    :object-type (type-label schema rid)
                    :link        handle
                    :far-type    (type-label schema far)
                    :attributes  (vec attrs)}
                   {:link-name (name handle) :attrs (str/join "/" (map name attrs))}))

(defn- refuse-nested-side!
  [schema rid {:keys [near handle far attrs]}]
  (refusal/refuse! :refusal.objects/nested-derived-fold
                   {:object-type (type-label schema rid)
                    :near-type   (type-label schema near)
                    :link        handle
                    :far-type    (type-label schema far)
                    :attributes  (vec attrs)}
                   {:link-name (name handle) :attrs (str/join "/" (map name attrs))}))

(defn- above-store
  [schema rid derived]
  (sd/leftovers schema {:op :static :root rid :ids []} derived))

(defn chain-of
  [schema rid derived]
  (let [[d-of f-of] (derive-reads schema)
        {:keys [levels sides nodes]} (dg/chain-plan d-of f-of rid (above-store schema rid derived))]
    (when (pos? levels)
      {:levels levels
       :sides  (vec (sort (for [[T h] sides] (str (type-label schema T) "." (name h)))))
       :properties (vec (sort (for [[T a] nodes] (str (type-label schema T) "." (name a)))))})))

(defn- approve-chain!
  [schema rid derived]
  (let [on-cycle (derive-cycles schema)]
    (when (seq on-cycle)
      (when-let [chain (some (fn [[a _]] (get on-cycle [rid a])) derived)]
        (refuse-cycle! schema chain)))))

(defn- far-overrides
  [reads schema rid rows derived]
  (let [[d-of f-of] (derive-reads schema)
        needs (dg/far-needs d-of f-of rid derived)]
    (when (seq needs)
      (let [ids (mapv :id rows)]
        (into {}
              (for [{:keys [near handle far attrs] :as step} needs]
                (do
                  (when-not (= near rid) (refuse-nested-side! schema rid step))
                  (let [ci   (connected-index reads (ls/resolve-side schema rid handle) ids)
                        flat (vals (coll/index-by :id (mapcat val ci)))]
                    (when (> (count flat) (derive-cap))
                      (refuse-chain-size! schema rid step (count flat)))
                    (let [dm (coll/index-by :id (attach-derived* reads schema far (vec flat)
                                                                 attrs nil nil))]
                      [[rid handle]
                       (update-vals ci (fn [nbrs] (mapv #(get dm (:id %) %) nbrs)))])))))))))

(defn- attach-derived*
  [reads schema rid rows want extra answers]
  (let [[derived questions] (witnessed-plan schema rid want extra)
        dropped (let [kept (into #{} (map first) derived)]
                  (into [] (remove kept) (derived-names schema rid)))
        rows    (cond->> rows
                  (seq dropped)
                  (mapv (fn [row] (update row :values #(apply dissoc % dropped)))))]
    (if (or (empty? derived) (empty? rows))
      rows
      (let [ir      {:op :static :root rid :ids []}
            answers (or answers (planned-answers reads schema rid rows derived))
            left    (above-store schema rid derived)
            ovr     (far-overrides reads schema rid rows left)
            refs    (into #{} (mapcat (fn [[_ ex]] (expr/link-refs ex))) left)
            tf      (when (seq refs)
                      (traverse-fn-for-roots reads schema rid refs (mapv :id rows)))
            tf*     (if (seq ovr)
                      (fn [state oid conn otype]
                        (or (get-in ovr [[otype conn] (str oid)])
                            (when tf (tf state oid conn otype))))
                      tf)
            advisory (into #{} (comp (mapcat (juxt :seen :used)) (remove nil?)) questions)
            read    (sd/reader schema ir derived advisory)
            eval*   (fn [r V ex]
                      (expr/eval V ex
                                 {:state nil :object-id (:id r) :object-type rid
                                  :traverse-fn tf*}))]
        (mapv #(wit/settle (read % (get answers (str (:id %))) eval*) questions) rows)))))

(defn- attach-approved
  [reads schema rid rows want extra answers]
  (approve-chain! schema rid (derived-plan schema rid want extra))
  (attach-derived* reads schema rid rows want extra answers))

(defn attach-derived
  ([reads schema rid rows] (attach-approved reads schema rid rows nil nil nil))
  ([reads schema rid rows want] (attach-approved reads schema rid rows want nil nil))
  ([reads schema rid rows want extra] (attach-approved reads schema rid rows want extra nil)))

(defn- attach-read-derived
  [reads schema rid rows want]
  (attach-approved reads schema rid rows want nil
                   (into {} (map (juxt (comp str :id) :values)) rows)))

(defn- agg-attr-names
  [aggs group-by]
  (if-let [n (agg/normalize {:aggs aggs :group-by group-by})]
    (into #{} (keep :attr) (concat (:aggs n) (:group-by n)))
    #{}))

(defn- hop-result-rid
  [schema root-rid hops]
  (reduce (fn [rid {:keys [link]}]
            (:far-type (ls/resolve-side schema rid link)))
          root-rid
          hops))

(defn- safe-select
  [schema root-rid show-attrs]
  (let [real (into #{} (map :attribute-name) (lookup/attrs schema root-rid))
        want (into [] (filter real) show-attrs)]
    (when (seq want)
      (let [inputs (into #{} (comp (mapcat (fn [[_ ex]] (expr/field-refs ex)))
                                   (filter real))
                         (derived-attrs schema root-rid))]
        (into want (remove (set want)) inputs)))))

(defn- with-title-order
  [schema {:keys [find order-by] :as spec}]
  (if (= q/title order-by)
    (if-let [tk (when find (get-in schema [:object-types (type-rid schema find) :title-key]))]
      (assoc spec :order-by tk)
      (dissoc spec :order-by :sort-dir))
    spec))

(defn- read-root!
  [auth schema temporary {:keys [find object-set object-set-args]}]
  (let [set-ir   (when object-set
                   (saved-set-ir auth schema object-set object-set-args temporary))
        root-rid (if set-ir
                   (or (:root (os/source set-ir))
                       (when find (type-rid schema find))
                       (refusal/refuse! :refusal.objects/set-without-type {:object-set object-set}))
                   (type-rid schema find))]
    (grants/authorize-read! auth (get-in schema [:object-types root-rid]) root-rid)
    {:set-ir set-ir :root-rid root-rid}))

(defn- check-read-spec!
  [{:keys [around group-by aggs object-set order-by limit offset]} set-ir]
  (doseq [h around
          :when (and (map? h) (contains? h :dir))]
    (refusal/refuse! :refusal.objects/around-with-dir {:around h}))
  (when (and (seq group-by) (nil? aggs))
    (refusal/refuse! :refusal.objects/group-by-without-aggs {:group-by (vec group-by)}))
  (when (and set-ir (empty? around) (own-shaping? set-ir)
             (or order-by (some? limit) (some? offset)))
    (refusal/refuse! :refusal.objects/set-ordering-layered {:object-set object-set})))

(defn- check-stated!
  [schema lowered]
  (when-let [[T a] (sd/unstated schema lowered)]
    (refusal/refuse! :refusal.objects/derived-not-computable
                     {:attribute a :type T}
                     {:attr-name (name a)
                      :type-name (name (or (get-in schema [:object-types T :object-type-name])
                                           T))})))

(defn- wanted-attrs
  [{:keys [want order-by aggs group-by]} shown]
  (let [extra (concat (when (ordered-attr order-by) [order-by])
                      (agg-attr-names aggs group-by))]
    (or (some-> want set not-empty (into extra))
        (when (seq shown) (into shown extra)))))

(defn- shaped-ir
  [schema {:keys [where order-by sort-dir limit offset aggs group-by] :as spec}
   {:keys [set-ir root-rid hops sel]}]
  (let [hops? (boolean (seq hops))
        agg?  (some? aggs)
        page  {:order-by order-by :sort-dir sort-dir :limit limit :offset offset}
        base  (cond-> spec
                (or hops? agg?) (dissoc :limit :offset :order-by :sort-dir))]
    (as-> (if set-ir
            (refine-set schema root-rid set-ir where)
            (build-object-set schema base (when-not hops? sel))) $
      (if (and set-ir (not hops?))
        (cond-> (shape-around $ page)
          sel (as-> % {:op :project :select sel :in %}))
        $)
      (if hops? (around-hops schema $ hops) $)
      (if (and hops? (not agg?)) (shape-around $ page) $)
      (if agg? (aggregate-set $ aggs group-by page) $))))

(defn- plan-read
  [schema {:keys [show around aggs group-by] :as spec} {:keys [set-ir root-rid]}]
  (let [hops       (mapv (fn [h] {:link (if (map? h) (:link h) h)}) around)
        hops?      (boolean (seq hops))
        result-rid (if hops? (hop-result-rid schema root-rid hops) root-rid)
        show-attrs (into [] (filter keyword?) show)
        want       (wanted-attrs spec (set show-attrs))
        sel        (safe-select schema root-rid show-attrs)
        ir         (shaped-ir schema spec {:set-ir set-ir :root-rid root-rid
                                           :hops hops :sel sel})
        lowered    (sd/plan schema ir [])]
    (check-stated! schema lowered)
    {:ir lowered
     :read-ir (sd/plan schema ir (first (witnessed-plan schema result-rid want)))
     :root-rid root-rid :result-rid result-rid
     :derived {:compute (mapv first (derived-plan schema result-rid want)) :want want}
     :hops? hops? :show-hops (into [] (filter map?) show)
     :aggregate (when (some? aggs) (agg/normalize {:aggs aggs :group-by group-by}))}))

(defn- read-shape
  [auth schema temporary spec]
  (let [spec (with-title-order schema spec)
        root (read-root! auth schema temporary spec)]
    (check-read-spec! spec (:set-ir root))
    (plan-read schema spec root)))

(defn object-set-ir
  [store schema spec]
  {:ir (:ir (read-shape (grants/read-rights store) schema (temporary-ir store spec) spec))})

(defn- row-count
  [store root]
  (try (some-> (first (sub/query store (assoc (sub/query-request nil {:op :aggregate :aggs :count
                                                                      :in {:op :scan :root root}})
                                              :authority grants/system-authority)))
               :metrics :count long)
       (catch clojure.lang.ExceptionInfo _ nil)))

(defn- plan-opts
  [store schema {:keys [accuracy engine explain?]}]
  (let [memo (atom {})]
    (cond-> {:authority (grants/read-rights store)
             :exact-only? (not explain?)
             :stats-fn (fn [root]
                         (when (get-in schema [:object-types root])
                           (if (contains? @memo root)
                             (get @memo root)
                             (let [n (row-count store root)] (swap! memo assoc root n) n))))}
      accuracy (assoc :accuracy (keyword accuracy))
      engine   (assoc :engine (keyword engine)))))

(defn materialize
  [store schema ir]
  (let [plan (planner/plan schema ir (plan-opts store schema nil))
        rows (plan-run/execute (sub/reader store) plan)]
    (cond-> {:op :static :ids (mapv :id rows)}
      (:root plan) (assoc :root (:root plan)))))

(defn- result-name
  [schema result-rid {:keys [find]} hops?]
  (if (or hops? (nil? find))
    (or (get-in schema [:object-types result-rid :object-type-name])
        (get-in schema [:interfaces result-rid :interface-name])
        result-rid)
    find))

(defn- answered
  [reads explain schema spec {:keys [ir read-ir hops? aggregate derived show-hops]} opts]
  (let [{:keys [count? count-only? explain?]} spec
        plan       (planner/plan schema read-ir opts)
        result-rid (:root plan)
        type-name  (result-name schema result-rid spec hops?)]
    (cond
      explain?
      (cond-> (explain plan)
        (seq (:compute derived))
        (assoc :above-substrate
               (let [chain (chain-of schema result-rid
                                     (derived-plan schema result-rid (:want derived)))]
                 (cond-> {:properties (:compute derived)}
                   chain (assoc :chain chain)))))

      aggregate
      {:object-type type-name
       :keys (agg/result-key-groups aggregate)
       :aggregations (plan-run/execute reads plan)}

      count-only?
      {:object-type type-name
       :count (-> (plan-run/execute reads (planner/plan schema (count-set ir) opts))
                  first :metrics :count)}

      :else
      (let [rows (plan-run/execute reads plan)
            rows (attach-search-arounds reads schema result-rid rows show-hops)
            rows (attach-read-derived reads schema result-rid rows (:want derived))]
        (cond-> {:object-type type-name :rows rows}
          count? (assoc :count
                        (-> (plan-run/execute reads (planner/plan schema (count-set ir) opts))
                            first :metrics :count)))))))

(def object-query-answer
  [:or
   [:map [:object-type some?] [:rows [:sequential [:map [:id some?] [:values map?]]]]
    [:count {:optional true} nat-int?]]
   [:map [:object-type some?] [:count nat-int?]]
   [:map [:object-type some?]
    [:keys [:map [:group sequential?] [:metrics sequential?]]]
    [:aggregations [:sequential [:map [:group map?] [:metrics map?]]]]]
   [:map [:store any?] [:above-substrate {:optional true} map?]]])

(def ^:private object-query-answer? (m/validator object-query-answer))

(defn run-object-query
  [store schema spec]
  (let [answer (prof/with-mark :Read {:find (str (:find spec)) :around (str (:around spec))}
                 (answered (sub/reader store) #(plan-run/explain store %) schema spec
                           (read-shape (grants/read-rights store) schema
                                       (temporary-ir store spec) spec)
                           (plan-opts store schema spec)))]
    (when-not (object-query-answer? answer)
      (anomaly/raise! :fault "run-object-query answers outside object-query-answer"
                      {:explain (m/explain object-query-answer answer)}))
    answer))

(defn connected
  [reads schema rid side root-ids]
  (update-vals (connected-index reads (ls/resolve-side schema rid side) root-ids)
               (fn [far] (mapv :id far))))

(defn folded-properties
  [schema]
  (into []
        (for [rid   (keys (:object-types schema))
              [a e] (derived-attrs schema rid)
              :when (seq (wit/folds e))]
          [rid a e])))

(defn fold-gaps
  [store schema rid a e]
  (let [qs    (wit/questions a e)
        prs   (wit/pairs qs)
        cols  (sd/stated-columns schema rid prs)
        preds (into [] (keep (fn [{:keys [seen used]}]
                               (let [s (cols seen) u (cols used)]
                                 (when (and s u) [:and [:> s 0] [:= u 0]]))))
                    qs)]
    (when (seq preds)
      (let [ir {:op :filter
                :where (if (= 1 (count preds)) (first preds) (into [:or] preds))
                :in (sd/plan schema {:op :scan :root rid} prs)}]
        (-> (plan-run/execute (sub/reader store)
                              (planner/plan schema (count-set ir) (plan-opts store schema nil)))
            first :metrics :count)))))

(defn object-query
  [store schema]
  (fn [spec] (run-object-query store schema spec)))

(defn resolve-object-ref
  [store schema rid raw]
  (let [find (or (get-in schema [:object-types rid :object-type-name])
                 (get-in schema [:interfaces rid :interface-name])
                 rid)
        read (fn [spec] (:rows (run-object-query store schema (merge {:find find} spec))))]
    (oref/resolve-ref schema rid raw
                      (fn [{[k v] :key id :id}]
                        (if k (read {:where {k {:= v}} :limit 2}) (read {:ids [id] :limit 1}))))))

(def ^:private series-object-keys
  #{:find :ids :where :around :object-set :object-set-args
    :order-by :sort-dir :limit :offset :accuracy :engine})

(def ^:private series-own-keys
  #{:property :from :until :bucket :rollup})

(defn- guard-series-spec!
  [spec]
  (let [stray (remove (into series-object-keys series-own-keys) (keys spec))]
    (when (seq stray)
      (refusal/refuse! :refusal.objects/series-unknown-keys
                       {:unknown (vec (sort stray))}
                       {:object-keys (vec (sort series-object-keys))
                        :series-keys (vec (sort series-own-keys))}))
    spec))

(defn run-series-query
  [store schema spec]
  (guard-series-spec! spec)
  (let [obj-spec (select-keys spec series-object-keys)
        {:keys [object-type rows]} (run-object-query store schema obj-spec)
        prop      (:property spec)
        root-rid  (lookup/rid-of schema :object-types object-type)
        title-key (get-in schema [:object-types root-rid :title-key])
        by-series (reduce (fn [acc {:keys [id values]}]
                            (if-let [sid (not-empty (str (get values prop)))]
                              (update acc sid (fnil conj [])
                                      (cond-> {:id id}
                                        title-key (assoc :label (get values title-key))))
                              acc))
                          {}
                          rows)
        fetched   (ats/fetch store schema object-type
                             (assoc (select-keys spec [:property :from :until :bucket :rollup])
                                    :series-ids (vec (keys by-series))))]
    (-> fetched
        (assoc :object-type object-type
               :objects (clojure.core/count rows)
               :series (mapv (fn [s] (assoc s :objects (get by-series (:series-id s) [])))
                             (:series fetched))))))

(defn implementers
  [schema]
  (into {}
        (for [iface (keys (:interfaces schema))]
          [iface (vec (sort (rules/implementers schema iface)))])))

(defn endpoint-types
  [schema]
  (let [schema (sch/indexed schema)
        lts    (:link-types schema)
        ifs    (:interfaces schema)
        bare   (into {} (map (fn [L] [L {:from #{} :to #{}}])) (keys lts))]
    (reduce-kv
     (fn [m T ot]
       (let [sat (into #{T} (filter #(contains? ifs %))
                       (interfaces/effective-implements schema ot))]
         (reduce (fn [m L]
                   (let [lt (get lts L)]
                     (cond-> m
                       (sat (:from-object-type lt)) (update-in [L :from] conj T)
                       (sat (:to-object-type lt))   (update-in [L :to] conj T))))
                 m (link-ends/reaching schema T))))
     bare (:object-types schema))))

(defn- conns-vec [schema]
  (vec (for [[k v] (:link-types schema)]
         {:name (or (:link-type-name v) k)
          :from (:from-object-type v)
          :to   (:to-object-type v)
          :cardinality (:cardinality v)})))

(defn matrix-index
  [schema]
  (let [ep        (endpoint-types schema)
        ot-set    (set (keys (:object-types schema)))
        all-conns (conns-vec schema)
        from-set  (fn [cn] (get-in ep [cn :from]))
        to-set    (fn [cn] (get-in ep [cn :to]))

        owner-options
        (vec (sort (distinct
                    (for [c all-conns
                          :when (= (:cardinality c) :one-to-many)
                          ot    (from-set (:name c))
                          :when (contains? ot-set ot)]
                      ot))))

        outgoing-otm-by-owner
        (into {} (for [ot ot-set
                       :let [outs (vec (for [c all-conns
                                             :when (and (= (:cardinality c) :one-to-many)
                                                        (contains? (from-set (:name c)) ot))]
                                         {:name (:name c) :child-type (:to c)}))]
                       :when (seq outs)]
                   [ot outs]))

        pair->conns
        (reduce
         (fn [acc c]
           (let [fs (from-set (:name c))
                 ts (to-set   (:name c))]
             (reduce
              (fn [m [a b]] (update m [a b] (fnil conj []) c))
              acc
              (concat (for [a fs b ts :when (and (contains? ot-set a) (contains? ot-set b))] [a b])
                      (for [a ts b fs :when (and (contains? ot-set a) (contains? ot-set b))]
                        [a b])))))
         {} all-conns)

        cell-edges-by-pair
        (into {}
              (for [[pair edges] pair->conns
                    :let [uniq  (vec (distinct edges))
                          mtos  (filter #(= (:cardinality %) :many-to-one) uniq)
                          marker (when (= 1 (count mtos)) (:name (first mtos)))]]
                [pair {:edges uniq :marker marker}]))

        peer-edges-by-child
        (into {}
              (for [ot ot-set
                    :let [self (vec (distinct
                                     (for [c all-conns
                                           :when (and (#{:one-to-one :many-to-one} (:cardinality c))
                                                      (contains? (from-set (:name c)) ot)
                                                      (contains? (to-set   (:name c)) ot))]
                                       c)))]
                    :when (seq self)]
                [ot self]))

        all-children-conns
        (distinct (for [[_ outs] outgoing-otm-by-owner
                        {:keys [name child-type]} outs]
                    [child-type name]))

        other-options-by-child-conn
        (into {}
              (for [[child cc] all-children-conns
                    :let [candidates
                          (vec (distinct
                                (for [c all-conns
                                      :when (not= (:name c) cc)
                                      peer (concat (when (contains? (from-set (:name c)) child)
                                                     (to-set (:name c)))
                                                   (when (contains? (to-set (:name c)) child)
                                                     (from-set (:name c))))
                                      :when (and (contains? ot-set peer)
                                                 (not= peer child))]
                                  peer)))]]
                [[child cc] candidates]))]
    {:owner-options              owner-options
     :outgoing-otm-by-owner      outgoing-otm-by-owner
     :cell-edges-by-pair         cell-edges-by-pair
     :peer-edges-by-child        peer-edges-by-child
     :other-options-by-child-conn other-options-by-child-conn}))

(defn derive-all
  [schema]
  (let [s (if (rid/rid-keyed? schema) (rid/name-keyed schema) schema)]
    {:implementers     (implementers s)
     :endpoint-types   (endpoint-types s)
     :matrix           (matrix-index s)
     :app-object-types (sv/app-object-types s)}))
