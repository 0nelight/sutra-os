(ns sutra.parser.sugar
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.tree :as tree]
            [sutra.types.refs :as refs]
            [sutra.types.column :as column]
            [sutra.parser.spec :as spec]
            [sutra.parser.resolve :as res]
            [sutra.types.attribute :as attribute]
            [sutra.types.backing :as backing]
            [sutra.types.entities :as ents]
            [sutra.types.forms :as forms]
            [sutra.types.languages :as langs]
            [sutra.types.link-side :as ls]
            [sutra.types.membership :as member]
            [sutra.types.value-types :as vt]
            [sutra.types.query :as q]
            [sutra.types.submission :as csub]
            [sutra.capps.derived :as cd]
            [clojure.set :as set]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn lower-display-name
  [spec]
  (if (string? (:name spec))
    (-> spec (dissoc :name) (assoc :display-name (:name spec)))
    spec))

(defn raise-display-name
  [m]
  (if-let [dn (:display-name m)]
    (-> m (dissoc :display-name) (assoc :name dn))
    m))

(def name-sugar-sections
  (into #{} (for [[section fields] ents/fields
                  :when (= :name-sugar (get-in fields [:display-name :author-form]))]
              section)))

(defn raise-name-sugar
  [section m]
  (cond-> m (and (map? m) (contains? name-sugar-sections section)) raise-display-name))

(defn rollup->expr
  [{:keys [agg where] :as rollup}]
  (let [op (or (q/op-of-agg agg)
               (anomaly/coded! :refusal.dialect/unknown-rollup :incorrect
                 "unknown rollup :agg {agg:pr} — known: {known:pr}"
                 {:agg agg :known q/aggs}))]
    (cond-> (into [op] (map rollup) (q/link-slots op))
      (and where (q/link-takes-where? op)) (conj where))))

(defn derived-property->expr
  [spec]
  (case (cd/kind-of spec)
    :aggregation (rollup->expr spec)
    :column-math (:expression spec)
    nil))

(defn- lower-attr-rollup
  [item]
  (if (and (vector? item) (= 3 (count item)) (get-in item [2 :derived :rollup]))
    (let [[nm tp opts] item
          rollup (get-in opts [:derived :rollup])]
      [nm tp (update opts :derived #(-> % (dissoc :rollup)
                                        (assoc :expression (rollup->expr rollup))))])
    item))

(defn- lower-section
  [schema section f]
  (cond-> schema
    (get schema section)
    (update section #(reduce-kv (fn [m k v] (assoc m k (f v))) {} %))))

(defn lower-rollups
  [schema]
  (lower-section schema :objects
                 (fn [obj]
                   (cond-> obj
                     (:attributes obj) (update :attributes #(mapv lower-attr-rollup %))))))

(def ^:private incremental-defaults {:read-mode :added :write-mode :modify})

(defn- lower-incremental-one
  [t]
  (let [i (:incremental t)]
    (cond
      (true? i) (assoc t :incremental incremental-defaults)
      (map? i)  (assoc t :incremental (merge incremental-defaults i))
      :else     t)))

(defn lower-incremental
  [schema]
  (reduce #(lower-section %1 %2 lower-incremental-one) schema [:transforms :functions]))

(def ^:private scalar-type-set
  (into #{:number} vt/authorable-types))

(defn- compatible-field-type?
  [ft vt]
  (= ft (:type vt)))

(defn- lower-attr-vtype
  [vts item]
  (if (and (vector? item) (>= (count item) 2))
    (let [[nm tp & more] item
          opts   (when (map? (first more)) (first more))
          vt-ref (:value-type opts)]
      (cond
        (and (keyword? tp) (contains? vts tp))
        (let [vt (get vts tp)]
          [nm (:type vt) (assoc (or opts {}) :value-type tp)])

        vt-ref
        (let [vt (get vts vt-ref)]
          (when (and vt (not (compatible-field-type? tp vt)))
            (anomaly/coded! :refusal.dialect/field-type-vs-value-type :incorrect
              "Attribute {attribute} declares field type {field-type:pr} but value-type {value-type} has base {value-type-base:pr} — a value-type wraps its base type, it does not change storage. Align the field type with the value-type's base (or the reverse)."
              {:attribute nm :field-type tp
               :value-type vt-ref :value-type-base (:type vt)}))
          (cond-> [nm tp] (seq opts) (conj opts)))

        (and (keyword? tp) (scalar-type-set tp)) item

        (keyword? tp)
        (anomaly/coded! :refusal.dialect/unknown-attribute-type :incorrect
          "Unknown type {type} on attribute {attribute} — not a scalar value-type and not a declared :value-types entry {known-value-types}"
          {:attribute nm :type tp
           :known-value-types (vec (sort (keys vts)))})

        :else item))
    item))

(defn- whole-of
  [m]
  (if (instance? clojure.lang.IDeref m) @m m))

(defn- lower-shared-vtype
  [vts sp-kw {vt-ref :value-type tp :type :as sp}]
  (if-not vt-ref
    sp
    (let [vt (get vts vt-ref)]
      (cond
        (and vt tp (not (compatible-field-type? tp vt)))
        (anomaly/coded! :refusal.dialect/shared-property-type-vs-value-type :incorrect
          "Shared property {shared-property} declares :type {type:pr} but value-type {value-type} has base {value-type-base:pr} — a value-type wraps its base type, it does not change storage. Drop :type (it follows the value-type) or align it."
          {:shared-property sp-kw :type tp
           :value-type vt-ref :value-type-base (:type vt)})

        vt  (assoc sp :type (:type vt))

        tp  sp

        :else
        (anomaly/coded! :refusal.dialect/shared-property-value-type-undeclared :incorrect
          "Shared property {shared-property} names :value-type {value-type}, which is not a declared :value-types entry {known-value-types} — declare it, or give the shared property its own :type when the value type lives in another file."
          {:shared-property sp-kw :value-type vt-ref
           :known-value-types (vec (sort (keys (whole-of vts))))})))))

(defn lower-shared-value-types
  [schema]
  (if-not (some :value-type (vals (:shared-properties schema)))
    schema
    (let [vts (:value-types schema)]
      (update schema :shared-properties
              #(reduce-kv (fn [m k sp] (assoc m k (lower-shared-vtype vts k sp))) {} %)))))

(defn lower-value-types
  [schema]
  (if (and (not (::lent-vts schema)) (empty? (:value-types schema)))
    schema
    (let [vts      (:value-types schema)
          lower-l  #(mapv (partial lower-attr-vtype vts) %)
          lower-in (fn [m k] (cond-> m (get m k) (update k lower-l)))]
      (-> schema
          (lower-section :objects #(lower-in % :attributes))
          (lower-section :interfaces #(-> % (lower-in :property-list)
                                          (lower-in :properties)))))))

(defn- coerce-value-type
  [t]
  (if (sequential? t) (vec t) (keyword t)))

(defn lower-shared-attr
  [shared a]
  (if (sequential? a)
    (let [[n t opts] a
          pair [(keyword n) (coerce-value-type t)]]
      (if (map? opts) (conj pair opts) pair))
    (attribute/shared-attr a (get shared (keyword a)))))

(defn- relift-value-types
  [attrs vts]
  (mapv (fn [item]
          (let [[nm tp opts] (when (vector? item) item)
                vt-name      (:value-type opts)
                vt           (get vts vt-name)]
            (if (and vt (= tp (:type vt)))
              (let [opts' (dissoc opts :value-type)]
                (cond-> [nm vt-name] (seq opts') (conj opts')))
              item)))
        attrs))

(defn- relift-shared
  [attrs sps]
  (mapv (fn [item]
          (let [nm (when (vector? item) (first item))
                sp (get sps nm)]
            (if (and sp (= item (attribute/shared-attr nm sp))) nm item)))
        attrs))

(defn raise-attrs [attr-list vts sps]
  (relift-value-types (relift-shared (mapv attribute/->tuple attr-list) sps) vts))

(defn- link-backing
  [link]
  (when (and (vector? link) (>= (count link) 5) (map? (nth link 4)))
    (let [opts    (nth link 4)
          present (filterv opts [:fk :join :fk-resolve])]
      (when (> (count present) 1)
        (anomaly/coded! :refusal.dialect/link-backings :incorrect
          "Link {link} carries {backings:pr} — a link has exactly one backing. Use :fk for a foreign-key edge (no join table), :join for a many-to-many join-table edge, or :fk-resolve to compute a non-PK match into an FK first."
          {:link (nth link 0) :backings present}))
      (first present))))

(defn inferred-fk-side
  [card]
  (case card
    :many-to-one :from
    :one-to-many :to
    nil))

(defn- fk->transform
  [link-name from to card fk]
  (let [m    (if (keyword? fk) {:attribute fk} fk)
        side (or (:side m)
                 (inferred-fk-side card)
                 (case card
                   :one-to-one
                   (anomaly/coded! :refusal.dialect/fk-one-to-one :incorrect
                     "Link {link} is :one-to-one — neither end is the `many` side, so :fk cannot infer which one carries the key. Name it: {:fk {:attribute … :side :from|:to}}."
                     {:link link-name :cardinality card})
                   :many-to-many
                   (anomaly/coded! :refusal.dialect/fk-many-to-many :incorrect
                     "Link {link} is :many-to-many — a foreign key cannot express it (there is nowhere to put a single key)."
                     {:link link-name :cardinality card})))]
    (-> (backing/fk-producer link-name (case side :from from :to to)
                             (assoc m :fk-attribute (:attribute m)))
        (dissoc :transform-name))))

(defn- join->transform
  [link-name from to card join]
  (when-not (= :many-to-many card)
    (anomaly/coded! :refusal.dialect/join-on-fk-cardinality :incorrect
      "Link {link} is {cardinality} but declares a :join (a join-table edge backing). a {cardinality} edge is an object-type foreign key — use {:fk …} (the FK sits on the many side). If the edge really is many-to-many, set its :cardinality to :many-to-many."
      {:link link-name :cardinality card}))
  (let [{:keys [on from-key to-key from-pk to-pk where]} join
        fk   (or from-key (first on))
        tk   (or to-key (second on))
        col  #(name (column/attr->col %))
        fjc  (col fk), tjc (col tk), fpk (col from-pk), tpk (col to-pk)
        guard (case (or where :none)
                :both-non-empty (str "src." fjc " <> '' AND dst." tjc " <> '' AND ")
                :from-non-empty (str "src." fjc " <> '' AND ")
                :to-non-empty   (str "dst." tjc " <> '' AND ")
                :none           "")
        q    (str "SELECT src." fpk " AS from_id, dst." tpk " AS to_id "
                  "FROM src JOIN dst ON " guard "src." fjc " = dst." tjc)]
    {:language langs/default-query-language
     :query    q
     :inputs   {:src {:kind :object-type :object-type from}
                :dst {:kind :object-type :object-type to}}
     :output   {:kind :link-type :link-type link-name
                :from-key-column :from_id :to-key-column :to_id}}))

(defn- fk-resolve->constructs
  [link-name from to card
   {:keys [key as language query code code-file function fk-type side
           on-null on-unresolved on-duplicate]}]
  (let [side* (or side (inferred-fk-side card)
                  (anomaly/coded! :refusal.dialect/fk-resolve-side :incorrect
                    "Link {link} {:fk-resolve} on {cardinality} must name the FK :side (:from|:to) — neither end is unambiguously the `many` side."
                    {:link link-name :cardinality card}))
        fk-side-type (case side* :from from :to to)
        pk-side-type (case side* :from to :to from)
        supp (cond-> {:language (or language langs/default-query-language)
                      :inputs {fk-side-type {:kind :object-type :object-type fk-side-type}
                               pk-side-type {:kind :object-type :object-type pk-side-type}}
                      :output {:kind :object-type :object-type fk-side-type :role :supplement
                               :name (str "resolve-" (name link-name))
                               :primary-key key}}
               query     (assoc :query query)
               code      (assoc :code code)
               code-file (assoc :code-file code-file)
               function  (assoc :function function))
        link (fk->transform link-name from to card
                            (cond-> {:attribute as :side side*}
                              on-null       (assoc :on-null on-null)
                              on-unresolved (assoc :on-unresolved on-unresolved)
                              on-duplicate  (assoc :on-duplicate on-duplicate)))]
    {:resolve supp :link link
     :fk-side-type fk-side-type :fk-attr as :fk-type (or fk-type :string)}))

(defn- attr-declared?
  [attrs attr-kw]
  (boolean (some #(= attr-kw (res/attr-name %)) attrs)))

(defn- implements-of
  [schema ot]
  (set (tree/reach #(get-in schema [:interfaces % :extends])
                   (get-in schema [:objects ot :implements]))))

(defn- constraint-hits
  [schema owner api card target]
  (when api
    (let [target-ifs (implements-of schema target)]
      (vec (for [i (sort (implements-of schema owner))
                 c (get-in schema [:interfaces i :link-constraints])
                 :when (and (= api (:link-name c))
                            (= card (or (ls/constraint-cardinality (:cardinality c)) card))
                            (or (= target (:to-object-type c))
                                (contains? target-ifs (:to-interface c))))]
             {:interface i :constraint api})))))

(defn- attr-link->link
  [schema from attr {:keys [to as reverse strict satisfies] :as l}]
  (let [fk        (cond-> {:attribute attr}
                    (not strict) (assoc :on-null :skip :on-unresolved :skip))
        prose     (select-keys l [:forward-name :reverse-name :description])
        vorwaerts (or satisfies (constraint-hits schema from as :one to))
        rueckwaerts (when-not (seq vorwaerts)
                      (seq (constraint-hits schema to reverse :many from)))]
    (if rueckwaerts
      [(or (:name l) (keyword (str (name to) "-" (name reverse))))
       to from :one-to-many
       (merge {:fk fk :forward-api-name reverse :reverse-api-name as
               :satisfies (vec rueckwaerts)}
              (set/rename-keys prose {:forward-name :reverse-name
                                      :reverse-name :forward-name}))]
      [(or (:name l) (keyword (str (name from) "-" (name as))))
       from to :many-to-one
       (cond-> (merge {:fk fk :forward-api-name as} prose)
         reverse           (assoc :reverse-api-name reverse)
         (seq vorwaerts)   (assoc :satisfies (vec vorwaerts)))])))

(defn lower-attr-links
  [schema]
  (let [links (for [[ot {:keys [attributes]}] (:objects schema)
                    item attributes
                    :when (and (vector? item) (= 3 (count item)) (:link (nth item 2)))
                    :let [l (:link (nth item 2))]
                    one (if (map? l) [l] l)]
                (attr-link->link schema ot (first item) one))
        vorhanden (set (map first (:links schema)))]
    (if (empty? links)
      schema
      (do
        (doseq [[nm] links :when (vorhanden nm)]
          (anomaly/coded! :refusal.dialect/link-declared-twice :incorrect
            "Link {link} steht in :links UND entsteht aus einem Attribut-:link — eine Beziehung wird an einer Stelle deklariert."
            {:link nm}))
        (let [doppelt (->> links (map first) frequencies (filter #(< 1 (val %))) (map key))]
          (when (seq doppelt)
            (anomaly/coded! :refusal.dialect/attribute-link-names-twice :incorrect
              "Attribut-:link ergibt denselben Link-Namen mehrfach: {links:pr} — gib :name an."
              {:links (vec doppelt)})))
        (-> schema
            (update :objects
                    (fn [objs]
                      (update-vals objs
                                   (fn [o]
                                     (cond-> o
                                       (:attributes o)
                                       (update :attributes
                                               (fn [as]
                                                 (mapv (fn [item]
                                                         (if (and (vector? item) (= 3 (count item))
                                                                  (:link (nth item 2)))
                                                           (let [o* (dissoc (nth item 2) :link)]
                                                             (if (seq o*) (assoc item 2 o*)
                                                                 (subvec item 0 2)))
                                                           item))
                                                       as))))))))
            (update :links (fnil into []) links))))))

(defn lower-links
  [schema]
  (if-not (some link-backing (:links schema))
    schema
    (let [own (fn [opts t]
                (cond-> t
                  (:project opts)          (assoc :project (:project opts))
                  (:ignore-inherited opts) (assoc :ignore-inherited (:ignore-inherited opts))))
          taken? (fn [tk] (contains? (:transforms schema) tk))
          acc (reduce
               (fn [a link]
                 (if-let [backing (link-backing link)]
                   (let [[nm from to card opts] link
                         tk (backing/link-producer-name nm)]
                     (when (taken? tk)
                       (anomaly/coded! :refusal.dialect/link-backing-and-producer :incorrect
                         "Link {link} has a {backing} but a transform {transform} is also declared — a link may carry an inline backing OR a hand-written producer, not both."
                         {:link nm :transform tk :backing backing}))
                     (case backing
                       :fk   (assoc-in a [:transforms tk]
                                       (own opts (fk->transform nm from to card (:fk opts))))
                       :join (assoc-in a [:transforms tk]
                                       (own opts (join->transform nm from to card (:join opts))))
                       :fk-resolve
                       (let [rtk (keyword (str "resolve-" (name nm)))
                             _   (when (taken? rtk)
                                   (anomaly/coded!
                                     :refusal.dialect/fk-resolve-transform-taken :incorrect
                                     "Link {link} {:fk-resolve} would generate transform {transform} but it is already declared."
                                     {:link nm :transform rtk}))
                             {:keys [resolve link fk-side-type fk-attr fk-type]}
                             (fk-resolve->constructs nm from to card (:fk-resolve opts))]
                         (-> a
                             (assoc-in [:transforms tk]  (own opts link))
                             (assoc-in [:transforms rtk] (own opts resolve))
                             (update-in [:attrs fk-side-type] (fnil conj []) [fk-attr fk-type])))))
                   a))
               {:transforms {} :attrs {}} (:links schema))
          strip (fn [link]
                  (if-let [backing (link-backing link)]
                    (let [opts (dissoc (nth link 4) backing)]
                      (if (seq opts) (assoc link 4 opts) (subvec link 0 4)))
                    link))
          add-attrs (fn [sch]
                      (reduce-kv
                       (fn [s ot items]
                         (if (get-in s [:objects ot])
                           (reduce (fn [s [an _at :as item]]
                                     (cond-> s
                                       (not
                                        (attr-declared? (get-in s [:objects ot :attributes]) an))
                                       (update-in [:objects ot :attributes] (fnil conj []) item)))
                                   s items)
                           (assoc-in s [:objects ot]
                                     {:reconcile-mode :merge :attributes (vec items)})))
                       sch (:attrs acc)))]
      (-> schema
          (update :links #(mapv strip %))
          (update :transforms merge (:transforms acc))
          add-attrs))))

(defn raise-fk-backing
  [from to card tx]
  (let [{:keys [fk-attribute on-null on-unresolved on-duplicate]} (:output tx)]
    (when (and tx (backing/endpoint-fk-producer? tx))
      (let [side (let [ot (get-in tx [:input :object-type])]
                   (cond (= ot from) :from
                         (= ot to)   :to))
            m    (cond-> {}
                   (and side (not= side (inferred-fk-side card)))
                   (assoc :side side)
                   on-null       (assoc :on-null       on-null)
                   on-unresolved (assoc :on-unresolved on-unresolved)
                   on-duplicate  (assoc :on-duplicate  on-duplicate))]
        (if (seq m) (assoc m :attribute fk-attribute) fk-attribute)))))

(defn- action-log-cfg
  [action]
  (when-let [al (:action-log action)]
    (if (map? al) al {})))

(def ^:private log-stamps
  [[:action-rid :string {:name "Action RID" :render-hints [:searchable]}]
   [:action-type-rid :string {:name "Action Type RID"}]
   [:action-type-version :integer {:name "Action Type Version"}]
   [:timestamp :timestamp {:name "Zeitstempel" :render-hints [:sortable]}]])

(defn- action-log-object
  [an cfg edited-type arrays pattrs]
  (cond-> {:plural      (str "[LOG] " (name an))
           :visibility  :hidden
           :symbol      "\uD83E\uDDFE"
           :title       :log-id
           :match-keys  [:log-id]
           :primary-key :log-id
           :attributes  [[:log-id  :string {:name "Submission" :render-hints [:identifier]}]
                         [:action  :string {:name "Action"}]
                         [:at      :string {:name "Zeitpunkt" :render-hints [:sortable]}]
                         [:user-id :string {:name "Benutzer" :render-hints [:searchable]}]]}
    true           (update :attributes into log-stamps)
    edited-type    (update :attributes conj
                           [:edited edited-type
                            {:name         "Bearbeitetes Objekt"
                             :description  "Prim\u00E4rschl\u00FCssel des Objekts, auf das die Regel zielt; bei einer Massen-Action leer \u2014 alle stehen unter :edited-objects."
                             :render-hints [:searchable]}])
    (seq arrays)   (update :attributes into
                           (for [{:keys [attr ot pk-type]} arrays]
                             [attr [:array pk-type]
                              {:name        (str "Bearbeitete Objekte (" (name ot) ")")
                               :description (str "Prim\u00E4rschl\u00FCssel ALLER " (name ot)
                                                 "-Objekte, die diese Submission "
                                                 "bearbeitet hat.")}]))
    (seq pattrs)   (update :attributes into
                           (for [{:keys [attr type label]} pattrs]
                             [attr type {:name        label
                                         :description "Wert beim Absenden."}]))
    (:project cfg) (assoc :project (:project cfg))
    (:status cfg)  (assoc :status (:status cfg))))

(defn- action-log-pk
  [sch ot]
  (let [o (merge (get-in sch [::live ot]) (get-in sch [:objects ot]))]
    (or (:primary-key o) (first (:match-keys o)) (:title o))))

(defn- action-log-edited-expr
  [r pk]
  (when pk
    (case (:op r)
      :create (get (:values r) pk)
      (:update :delete-object)
      (let [t (:target r)]
        (when (and (vector? t) (= :get (first t))) [:get (second t) pk]))
      nil)))

(defn- action-log-params
  [params]
  (merge {:log-id {:type :string :prefill :uuid         :exposure :hidden}
          :log-at {:type :string :prefill :now          :exposure :hidden}
          :log-by {:type :string :prefill :current-user :exposure :hidden}}
         params))

(defn- lower-set-step
  [an step]
  (if-let [ks (:set step)]
    (do (when-not (and (vector? ks) (every? keyword? ks))
          (anomaly/coded! :refusal.dialect/action-set-shape :incorrect
            "action {action}: :set takes a vector of property names, got {set:pr}"
            {:action an :set ks}))
        (-> step
            (dissoc :set)
            (assoc :values (merge (into {} (map (fn [k] [k [:get k]])) ks) (:values step)))))
    step))

(declare property-base-type)

(defn- known-property-type
  [sch ot prop]
  (or (property-base-type sch ot prop)
      (property-base-type (assoc sch :objects (::live sch)) ot prop)))

(defn- lower-action-sugar*
  [an act]
  (let [steps   (refs/steps (:rules act))
        set-ks  (distinct (mapcat (fn [s] (remove #(contains? (:values s) %) (:set s))) steps))
        rules   (refs/update-steps #(lower-set-step an %) (:rules act))
        implied (into {} (map (fn [k] [k {}])) set-ks)
        params  (reduce-kv (fn [m p spec]
                             (assoc m p (cond-> spec (false? (:required spec)) (dissoc :required))))
                           {} (merge implied (:params act)))]
    (cond-> act
      (:rules act)   (assoc :rules rules)
      (seq params)   (assoc :params params))))

(defn- lower-action-sugar
  [schema]
  (cond-> schema
    (:actions schema)
    (update :actions #(reduce-kv (fn [m an act] (assoc m an (lower-action-sugar* an act))) {} %))))

(def ^:private context-sources
  {[:now] :now [:current-user] :current-user})

(defn- property-base-type
  [schema ot prop]
  (let [a (some #(when (= prop (res/attr-name %)) %)
                (get-in schema [:objects ot :attributes]))]
    (cond
      (vector? a)  (second a)
      (keyword? a) (get-in schema [:shared-properties a :type]))))

(defn- lower-context-values*
  [schema an act]
  (let [used  (volatile! #{})
        step  (fn [s]
                (if-not (map? (:values s))
                  s
                  (update s :values
                          (fn [vs]
                            (reduce-kv
                             (fn [m prop v]
                               (if-let [src (context-sources v)]
                                 (let [p (csub/context-param
                                          src (property-base-type schema (:object-type s) prop))]
                                   (vswap! used conj p)
                                   (assoc m prop [:get p]))
                                 (assoc m prop v)))
                             {} vs)))))
        rules (refs/update-steps step (:rules act))]
    (if (empty? @used)
      act
      (reduce (fn [a p]
                (let [want (get-in csub/implicit-context-params [p :spec])
                      have (get-in a [:params p])]
                  (when (and have (not= have want))
                    (anomaly/coded! :refusal.dialect/action-param-reserved :incorrect
                      "action {action}: parameter {param} is reserved for [:now]/[:current-user] in rule values — rename yours"
                      {:action an :param p}))
                  (assoc-in a [:params p] want)))
              (assoc act :rules rules)
              @used))))

(defn- lower-context-values
  [schema]
  (cond-> schema
    (:actions schema)
    (update :actions
            #(reduce-kv (fn [m an act] (assoc m an (lower-context-values* schema an act))) {} %))))

(def ^:private edit-ops
  #{:create :update :delete-object})

(defn edited-objects-attr?
  [a]
  (str/starts-with? (name a) "edited-objects"))

(defn- action-log-arrays
  [sch tn rules]
  (let [ots   (->> (refs/steps rules)
                   (filter #(edit-ops (:op %)))
                   (keep :object-type)
                   (remove #{tn})
                   distinct
                   (filter #(get-in sch [:objects %])))
        one?  (= 1 (count ots))]
    (vec (for [ot ots
               :let [pk (action-log-pk sch ot)]
               :when pk]
           {:ot      ot
            :pk-type (or (known-property-type sch ot pk) :string)
            :attr    (if one? :edited-objects (keyword (str "edited-objects-" (name ot))))
            :link    (keyword (str (name tn) "-edited" (when-not one? (str "-" (name ot)))))}))))

(declare param-derivations)

(def ^:private log-own-attrs
  (into #{:log-id :action :at :user-id :edited} (map first) log-stamps))

(defn- action-log-param-attrs
  [sch an action]
  (let [derivs (param-derivations sch action)
        refuse (fn [p why] (anomaly/coded! :refusal.dialect/action-log-param :incorrect
                             "action {action-name}: :action-log :params {param-name} — {why}"
                             {:action an :param p}
                             {:action-name (name an) :param-name (name p) :why why}))]
    (vec (for [p (:params (action-log-cfg action))
               :let [spec (get-in action [:params p])
                     t    (or (:type spec) (:type (get derivs p)) :string)
                     ot   (when (get-in sch [:objects t]) t)
                     pk   (when ot (or (get-in sch [:objects ot :primary-key])
                                       (action-log-pk sch ot)))]]
           (cond
             (nil? spec)
             (refuse p "the action has no such parameter")
             (or (log-own-attrs p) (edited-objects-attr? p))
             (refuse p "collides with a property every log carries — rename the parameter")
             (and ot (:multiple spec))
             (refuse p (str "an object parameter with several values has no single key "
                            "to keep"))
             (and ot (nil? pk))
             (refuse p (str (name ot) " names no primary key to keep"))
             (and (nil? ot) (not (scalar-type-set t)))
             (refuse p (str "type " (pr-str t) " is no scalar a log property can hold"))
             :else
             {:attr  p
              :type  (if ot (or (property-base-type sch ot pk) :string) t)
              :value (if ot [:get p pk] [:get p])
              :label (or (:name spec) (name p))})))))

(defn- action-log-value-attrs
  [sch an action]
  (let [derivs (param-derivations sch action)
        refuse (fn [a why] (anomaly/coded! :refusal.dialect/action-log-value :incorrect
                             "action {action-name}: :action-log :values {attr-name} — {why}"
                             {:action an :attr a}
                             {:action-name (name an) :attr-name (name a) :why why}))]
    (vec (for [[a v] (:values (action-log-cfg action))
               :let [get? (and (vector? v) (= :get (first v)))
                     [_ p prop] (when get? v)
                     spec (when get? (get-in action [:params p]))
                     pt   (when get? (or (:type spec) (:type (get derivs p))))
                     t    (cond
                            (or (log-own-attrs a) (edited-objects-attr? a))
                            (refuse a "collides with a property every log carries")
                            (and get? (nil? spec)) (refuse a "the action has no such parameter")
                            prop (or (known-property-type sch pt prop)
                                     (refuse a (str (pr-str pt) " has no property " (name prop))))
                            get? (or pt :string)
                            (or (nil? v) (string? v)) :string
                            (boolean? v) :boolean
                            (number? v) :double
                            :else (refuse a "is no parameter, property or constant"))]]
           {:attr a :type t :value v :label (name a)}))))

(defn- lower-action-logs
  [schema]
  (reduce-kv
   (fn [sch an action]
     (if-let [cfg (action-log-cfg action)]
       (let [tn      (or (:object-type cfg) (keyword (str (name an) "-log")))
             has?    (fn [rules] (some #(and (= :create (:op %)) (= tn (:object-type %))) rules))
             edits   (remove #(and (= :create (:op %)) (= tn (:object-type %)))
                             (:rules action))
             ots     (distinct (keep :object-type edits))
             ot      (when (= 1 (count ots)) (first ots))
             pk      (when ot (action-log-pk sch ot))
             off?    (false? (:edited cfg))
             expr    (when-not off?
                       (some #(when (= ot (:object-type %)) (action-log-edited-expr % pk)) edits))
             pk-type (when expr (or (known-property-type sch tn :edited)
                                    (known-property-type sch ot pk)
                                    :string))
             eigen?  (nil? (get-in sch [:objects tn]))
             attrs   (get-in sch [:objects tn :attributes])
             edited  (if (spec/function-rule action)
                       (for [{t :object-type} (:edits action) :when t] {:op :update :object-type t})
                       edits)
             arrays  (when-not off?
                       (filterv #(or eigen? (attr-declared? attrs (:attr %)))
                                (action-log-arrays sch tn edited)))
             legacy  (when (and expr (not eigen?) (empty? arrays) (attr-declared? attrs :edited))
                       [{:ot ot :attr :edited :link (keyword (str (name tn) "-edited"))}])
             declared (fn [link] (some #(when (= link (first %)) %) (:links sch)))
             beside  (fn [{:keys [link attr] :as a}]
                       (let [l (declared link)]
                         (cond-> a
                           (and l (not= attr (get-in (peek l) [:fk :attribute])))
                           (assoc :link (keyword (str (name link) "-objects"))))))
             links   (remove (comp declared :link) (map beside (concat arrays legacy)))
             pattrs  (filterv #(or eigen? (attr-declared? attrs (:attr %)))
                              (into (action-log-param-attrs sch an action)
                                    (action-log-value-attrs sch an action)))
             rule    {:op :create :object-type tn
                      :values (cond-> (into {:log-id  [:get :log-id]
                                             :action  (name an)
                                             :at      [:get :log-at]
                                             :user-id [:get :log-by]}
                                            (map (juxt :attr :value))
                                            pattrs)
                                expr (assoc :edited expr))}]
         (cond-> sch
           eigen?
           (assoc-in [:objects tn] (action-log-object an cfg pk-type arrays pattrs))

           (seq links)
           (update :links (fnil into [])
                   (for [{:keys [ot attr link]} links]
                     [link tn ot :many-to-one
                      (cond-> {:description
                               (str "[LOG] " (name an) " → die bearbeiteten " (name ot) ".")
                               :fk {:attribute attr :on-unresolved :skip}}
                        (:project cfg) (assoc :project (:project cfg)))]))

           true
           (update-in [:actions an :params] action-log-params)

           (not (has? (get-in sch [:actions an :rules])))
           (update-in [:actions an :rules] (fnil conj []) rule)))
       sch))
   schema (:actions schema)))

(defn- keyed-create?
  [schema {:keys [object-type values]}]
  (let [pk (get-in schema [:objects (keyword (name (or object-type ""))) :primary-key])]
    (and (keyword? pk) (map? values) (contains? values pk))))

(defn- lower-create-ids
  [schema]
  (cond-> schema
    (:actions schema)
    (update :actions
            (fn [acts]
              (reduce-kv
               (fn [m nm act]
                 (assoc m nm
                        (if (empty? (:rules act))
                          act
                          (reduce
                           (fn [a [i r]]
                             (if (and (= :create (:op r)) (not (contains? r :id))
                                      (not (keyed-create? schema r)))
                               (let [p (csub/implicit-create-id i)]
                                 (-> a
                                     (assoc-in [:rules i :id] [:get p])
                                     (update :params
                                             #(merge {p {:type :string :prefill :uuid
                                                         :exposure :hidden}} %))))
                               a))
                           (update act :rules vec)
                           (map-indexed vector (:rules act))))))
               {} acts)))))

(defn raise-create-ids
  [act]
  (let [gen? (fn [id] (and (vector? id) (= 2 (count id)) (= :get (keyword (first id)))
                           (csub/implicit-create-id? (keyword (second id)))))
        ctx  (fn [v] (when (and (vector? v) (= 2 (count v)) (= :get (keyword (first v))))
                       (get-in csub/implicit-context-params [(keyword (second v)) :source])))
        ctx-values (fn [r] (cond-> r
                             (map? (:values r))
                             (update :values update-vals #(if-let [src (ctx %)] [src] %))))]
    (cond-> act
      (seq (:rules act))
      (update :rules
              #(refs/update-steps (fn [r] (cond-> (ctx-values r) (gen? (:id r)) (dissoc :id))) %))

      (:params act)
      (update :params #(reduce-kv (fn [m k v] (if (csub/implicit-param? k) m (assoc m k v)))
                                  (empty %) %))

      (and (seq (:params act)) (every? csub/implicit-param? (keys (:params act))))
      (dissoc :params))))

(defn writes-of-action
  [rules]
  (let [collect (fn [rs]
                  (reduce (fn [m {:keys [object-type values]}]
                            (reduce-kv (fn [m prop v]
                                         (let [p (cond (keyword? v) v
                                                       (and (vector? v) (= :get (first v)))
                                                       (second v))]
                                           (cond-> m p (update p (fnil conj #{})
                                                               [object-type prop]))))
                                       m (or values {})))
                          {} rs))
        log?    #(= [:get :log-id] (get-in % [:values :log-id]))
        written (filter :values rules)]
    (merge (collect (filter log? written)) (collect (remove log? written)))))

(defn- written-attr
  [schema ot prop]
  (let [find-in (fn [attrs] (some (fn [a] (when (and (vector? a) (= prop (first a))) a)) attrs))]
    (or (find-in (get-in schema [:objects ot :attributes]))
        (some #(find-in (get-in schema [:interfaces % :property-list]))
              (sort (implements-of schema ot))))))

(defn- attr-derivation
  [schema ot prop]
  (let [a (written-attr schema ot prop)
        t (some-> (second a) csub/param-type-of)
        n (:name (get a 2))]
    (when (or t n) (cond-> {} t (assoc :type t) n (assoc :name n)))))

(defn- read-derivations
  [schema act]
  (let [params  (:params act)
        current (or (some (fn [[_ s]] (when (= :current-object (some-> (:prefill s) keyword))
                                        (:type s)))
                          params)
                    (:on act))]
    (into {}
          (for [[p spec] params
                :let  [d (when (map? spec) (:default spec))]
                :when (and (map? d) (keyword? (:property d)))
                :let  [src (if-let [f (:from d)] (get-in params [(keyword f) :type]) current)
                       ot  (some-> src keyword)
                       dv  (when (get-in schema [:objects ot])
                             (attr-derivation schema ot (:property d)))]
                :when dv]
            [p dv]))))

(defn param-derivations
  [schema act]
  (merge (read-derivations schema act)
         (into {}
               (for [[p tgt] (writes-of-action (:rules act))
                     :when (= 1 (count tgt))
                     :let  [[ot prop] (first tgt)
                            dv (attr-derivation schema ot prop)]
                     :when dv]
                 [p dv]))))

(defn- derive-param
  [spec d]
  (cond-> spec
    (and (not (contains? spec :type)) (:type d))
    (assoc :type (:type d))
    (and (not (contains? spec :name)) (:name d) (not= :hidden (:exposure spec)))
    (assoc :name (:name d))))

(defn lower-param-types
  [schema]
  (cond-> schema
    (:actions schema)
    (update :actions
            (fn [acts]
              (reduce-kv
               (fn [m nm act]
                 (let [writes (writes-of-action (:rules act))
                       derivs (param-derivations schema act)]
                   (assoc m nm
                          (cond-> act
                            (:params act)
                            (update :params
                                    (fn [ps]
                                      (reduce-kv
                                       (fn [ps p spec]
                                         (let [spec (derive-param spec (get derivs p))]
                                           (if (contains? spec :type)
                                             (assoc ps p spec)
                                             (let [tgt (get writes p)
                                                   n   (count tgt)]
                                               (anomaly/coded!
                                                 :refusal.dialect/param-type-underivable :incorrect
                                                 "action {action}: parameter {param} has no :type and none can be derived — it writes {writes-text}. where there is none, say it."
                                                 {:action nm :param p :writes tgt}
                                                 {:writes-text (if (zero? n)
                                                                 "no property"
                                                                 (str n " properties"))})))))
                                       ps ps)))))))
               {} acts)))))

(defn- lower-object-view-cards
  [schema]
  (cond-> schema
    (map? (:applications schema))
    (update :applications update-vals
            (fn [a] (cond-> a
                      (and (map? a) (:form-factor a))
                      (update :layout (requiring-resolve 'sutra.capps.layout/lower-view-body)))))))

(defn- log-type-of [an act] (forms/log-type-of an act))

(defn derive-projects
  ([schema] (derive-projects schema {}))
  ([schema known]
   (reduce (fn [s tag]
             (let [lens (forms/author s known)]
               (reduce (fn [s [nm spec :as entry]]
                         (if-let [p (and (map? spec) (not (:project spec))
                                         (member/derived tag entry lens))]
                           (forms/assoc-project s tag nm p)
                           s))
                       s
                       ((:entries lens) tag))))
           schema
           member/order)))

(def ^:private passes
  [{:id :projects          :run #'derive-projects :derived true}
   {:id :object-view-cards :run #'lower-object-view-cards}
   {:id :attr-links        :run #'lower-attr-links :generates #{:links}}
   {:id :shared-values     :run #'lower-shared-value-types}
   {:id :action-sugar      :run #'lower-action-sugar}
   {:id :context-values    :run #'lower-context-values}
   {:id :action-logs       :run #'lower-action-logs :generates #{:objects :links}}
   {:id :create-ids        :run #'lower-create-ids}
   {:id :value-types       :run #'lower-value-types}
   {:id :param-types       :run #'lower-param-types}
   {:id :links             :run #'lower-links :generates #{:transforms}}
   {:id :rollups           :run #'lower-rollups}
   {:id :incremental       :run #'lower-incremental}])

(defn- run-passes
  [passes schema]
  (let [derived (filterv :derived passes)]
    (reduce (fn [s {:keys [run generates]}]
              (cond-> (run s)
                generates (as-> s' (reduce (fn [s'' d] ((:run d) s'')) s' derived))))
            schema
            passes)))

(defn lower-schema
  [schema]
  (run-passes passes schema))

(defn- log-types
  [schema]
  (for [[an a] (:actions schema) :when (:action-log a)] (log-type-of an a)))

(defn- lent-value-types
  [live-vts text-vts]
  (let [whole (delay (merge (apply dissoc (whole-of live-vts) (keys text-vts)) text-vts))]
    (reify
      clojure.lang.ILookup
      (valAt [this k] (.valAt this k nil))
      (valAt [_ k nf] (if-let [e (find text-vts k)] (val e) (get live-vts k nf)))
      clojure.lang.Associative
      (containsKey [_ k] (or (contains? text-vts k) (contains? live-vts k)))
      (entryAt [_ k] (or (find text-vts k) (find live-vts k)))
      clojure.lang.Seqable
      (seq [_] (seq @whole))
      clojure.lang.IDeref
      (deref [_] @whole))))

(defn lower-schema-against
  ([schema live] (lower-schema-against schema live {}))
  ([schema live live-vts] (lower-schema-against schema live live-vts (boolean (seq live-vts))))
  ([schema live live-vts lend-vts?]
   (let [project  #(or (:project (get live %)) %2)
         schema   (derive-projects schema (reify clojure.lang.ILookup
                                            (valAt [_ k] (project k nil))
                                            (valAt [_ k nf] (project k nf))))
         declared (set (keys (:objects schema)))
         used     (into #{} (comp (mapcat #(concat (keep :object-type (refs/steps (:rules %)))
                                                   (keep :object-type (:edits %))
                                                   (keep :type (vals (:params %)))))
                                  (filter keyword?))
                        (vals (:actions schema)))
         wanted   (remove declared used)
         context  (select-keys live wanted)
         text-vts (:value-types schema)
         lent     (cond-> schema
                    (seq context) (update :objects merge context)
                    lend-vts?     (assoc :value-types (lent-value-types live-vts text-vts)
                                         ::lent-vts true)
                    true          (assoc ::live (select-keys live
                                                             (concat (filter declared used)
                                                                     (log-types schema)))))
         lowered  (dissoc (lower-schema lent) ::live ::lent-vts)]
     (cond-> lowered
       (seq context) (update :objects #(apply dissoc % (keys context)))
       (and lend-vts? (contains? schema :value-types))
       (assoc :value-types (select-keys (:value-types lowered) (keys text-vts)))
       (and lend-vts? (not (contains? schema :value-types))) (dissoc :value-types)))))

(def ^:private stamp-attr? (into #{} (map first) log-stamps))

(defn- lower-action-part
  [schema]
  (try (-> schema lower-action-sugar lower-context-values lower-action-logs lower-create-ids
           lower-param-types
           (update :actions update-vals #(dissoc % :action-log))
           (update :objects
                   #(some-> % (update-vals
                               (fn [o]
                                 (cond-> (dissoc o :visibility)
                                   (:attributes o)
                                   (update :attributes
                                           (fn [as]
                                             (vec (remove (fn [a] (stamp-attr? (res/attr-name a)))
                                                          as))))))))))
       (catch clojure.lang.ExceptionInfo _ ::unlowerable)))

(defn- log-link?
  [tn]
  (let [base (str (name tn) "-edited")]
    (fn [l] (let [n (name l)] (or (= n base) (str/starts-with? n (str base "-")))))))

(defn- action-slice
  [body an]
  (let [act (get-in body [:actions an])
        tn  (log-type-of an act)
        ots (into #{tn} (keep :object-type) (refs/steps (:rules act)))
        ln? (log-link? tn)]
    (cond-> {:actions {an act}
             :objects (select-keys (:objects body) ots)
             :links   (filterv #(ln? (first %)) (:links body))}
      (::live body) (assoc ::live (select-keys (::live body)
                                               (into ots (keep :type) (vals (:params act))))))))

(defn- raise-log
  [body an]
  (let [act (get-in body [:actions an])
        tn  (log-type-of an act)
        obj (get-in body [:objects tn])
        ln? (log-link? tn)
        log-rule? #(and (= :create (:op %)) (= tn (:object-type %)))]
    (if-not (and obj (some log-rule? (:rules act)))
      body
      (let [edited? (some #(or (= :edited %) (edited-objects-attr? %))
                          (map res/attr-name (:attributes obj)))
            params  (:params (:action-log act))
            kept    (apply dissoc (:values (some #(when (log-rule? %) %) (:rules act)))
                           (concat log-own-attrs params))
            cfg     (cond-> (if (map? (:action-log act)) (:action-log act) {})
                      (:project obj)                  (assoc :project (:project obj))
                      (:status obj)                   (assoc :status (:status obj))
                      (not edited?)                   (assoc :edited false)
                      (seq kept)                      (assoc :values kept)
                      (not= tn (keyword (str (name an) "-log"))) (assoc :object-type tn))]
        (-> body
            (update :objects dissoc tn)
            (cond-> (:links body)
              (update :links #(vec (remove (fn [l]
                                             (and (ln? (first l))
                                                  (some-> (get-in (peek l) [:fk :attribute])
                                                          edited-objects-attr?)))
                                           %))))
            (assoc-in [:actions an]
                      (-> act
                          (assoc :action-log (if (seq cfg) cfg true))
                          (update :rules #(vec (remove log-rule? %)))
                          (update :params dissoc :log-id :log-at :log-by))))))))

(defn- raise-set
  [body an]
  (if-not (seq (get-in body [:actions an :rules]))
    body
    (update-in body [:actions an :rules]
               #(mapv (fn [r]
                        (let [same (sort (keep (fn [[k v]] (when (= v [:get k]) k)) (:values r)))]
                          (if (empty? same)
                            r
                            (let [vals (apply dissoc (:values r) same)]
                              (cond-> (assoc r :set (into (vec (:set r)) same))
                                (seq vals)  (assoc :values vals)
                                (empty? vals) (dissoc :values))))))
                      %))))

(defn- raise-params
  [body an]
  (let [slice  (lower-action-sugar (action-slice body an))
        derivs (param-derivations slice (get-in slice [:actions an]))
        set-ks (set (mapcat :set (refs/steps (get-in body [:actions an :rules]))))]
    (update-in body [:actions an :params]
               (fn [ps]
                 (reduce-kv
                  (fn [m p spec]
                    (let [d    (get derivs p)
                          spec (cond-> spec
                                 (false? (:required spec))                   (dissoc :required)
                                 (and (:type d) (= (:type d) (:type spec)))  (dissoc :type)
                                 (and (:name d) (= (:name d) (:name spec))
                                      (not= :hidden (:exposure spec)))       (dissoc :name))]
                      (if (and (empty? spec) (set-ks p)) m (assoc m p spec))))
                  {} ps)))))

(defn- drop-empty-params [body an]
  (cond-> body
    (and (contains? (get-in body [:actions an]) :params)
         (empty? (get-in body [:actions an :params])))
    (update-in [:actions an] dissoc :params)))

(defn- raise-slice
  [slice an]
  (reduce (fn [slice step]
            (let [cand (drop-empty-params (step slice an) an)]
              (if (and (not= cand slice)
                       (= (lower-action-part (action-slice slice an))
                          (lower-action-part (action-slice cand an))))
                cand
                slice)))
          slice
          [raise-log raise-set raise-params]))

(def ^:private raised-slices (atom {}))

(defn- raised-slice [slice an]
  (let [k [an slice]]
    (or (get @raised-slices k)
        (let [v (raise-slice slice an)]
          (swap! raised-slices #(assoc (if (< (count %) 8192) % {}) k v))
          v))))

(defn raise-actions
  ([body live] (dissoc (raise-actions (cond-> body (seq live) (assoc ::live live))) ::live))
  ([body]
   (reduce
    (fn [body an]
      (let [slice  (action-slice body an)
            slice' (raised-slice slice an)]
        (if (identical? slice slice')
          body
          (let [gone-objects (remove (set (keys (:objects slice'))) (keys (:objects slice)))
                gone-links   (set (remove (set (:links slice')) (:links slice)))]
            (cond-> (assoc-in body [:actions an] (get-in slice' [:actions an]))
              (seq gone-objects) (update :objects #(apply dissoc % gone-objects))
              (seq gone-links)   (update :links #(vec (remove gone-links %))))))))
    body
    (sort (keys (:actions body))))))
