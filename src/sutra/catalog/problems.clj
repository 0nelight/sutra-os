(ns sutra.catalog.problems
  (:require [sutra.kit.coll :as coll]
            [sutra.types.authority :as authority]
            [sutra.types.finding :as finding]
            [sutra.types.ontologies :as ontologies]
            [sutra.catalog.streaming :as streaming]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.link-side :as ls]
            [sutra.types.attribute :as attribute]
            [sutra.types.entities :as ents]
            [sutra.types.kinds :as kinds]
            [sutra.types.media :as mt]
            [sutra.types.object-set :as os]
            [sutra.parser.spec :as spec]
            [sutra.parser.lint :as lint]
            [sutra.parser.board :as board]
            [sutra.types.expr :as expr]
            [clojure.string :as str]
            [sutra.ref.install :as ri]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.backing-checks :as backing-checks]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.structure :as structure]
            [sutra.catalog.transforms :as transforms]))

(set! *warn-on-reflection* true)

(defn link-type-problem
  [schema ct-kw ct]
  (try
   (let [ots (:object-types schema)
         ifs (:interfaces schema)
         ept? (fn [k] (or (contains? ots k) (contains? ifs k)))]
     (cond
       (not (ept? (:from-object-type ct)))
       (str "from-endpoint " (pr-str (:from-object-type ct))
            " is not a declared object-type/interface")
       (not (ept? (:to-object-type ct)))
       (str "to-endpoint " (pr-str (:to-object-type ct))
            " is not a declared object-type/interface")
       (not (contains? outputs/valid-cardinalities-all (:cardinality ct)))
       (str "invalid cardinality " (pr-str (:cardinality ct)))
       :else
       (if-let [[_ trans output] (outputs/find-output-for schema :link-type ct-kw)]
         (case (:kind (backing/link-backing schema ct-kw))
           :polymorphic
           (when-not (every? #(contains? ots (:from-type %)) (:sources trans))
             "a polymorphic sub-source names an undeclared :from-type")
           :resolve-by-shared-property
           (str "link " (pr-str ct-kw)
                " is backed by :resolve-by-shared-property, which resolves the target on"
                " a non-primary-key shared property. Compute the foreign key upstream in"
                " the source pipeline and declare the link `{:fk …}`, or use"
                " `{:fk-resolve …}` sugar.")
           :join-table
           (when-not (= :many-to-many (:cardinality ct))
             (str "link " (pr-str ct-kw) " is " (pr-str (:cardinality ct))
                  " but is backed by a " (name (:language trans))
                  " transform that materialises an edge-pair join table"
                  " (:from-key-column/:to-key-column). a " (pr-str (:cardinality ct))
                  " link is an object-type foreign key."
                  " Move the match into an FK attribute on the many side and declare"
                  " it `{:fk …}`, or — if the edge really is many-to-many — set"
                  " :cardinality :many-to-many."))
           :fk (backing/endpoint-fk-problem schema ct-kw ct trans output)

           :source-provided
           (when-not (= :join-table (kinds/link-backing (get-in trans [:input :kind])))
             (str "link backing transform has an unrecognized input kind "
                  (pr-str (get-in trans [:input :kind])))))
         (backing-checks/unbacked-link ct-kw))))
   (catch Exception e (str "validation error: " (ex-message e)))))

(defn side-name-problems
  [schema]
  (for [{:keys [type api-name sides]} (ls/name-conflicts schema)]
    {:link   (:link (first sides))
     :reason (str "object type " (pr-str type) " reaches " (count sides)
                  " link sides named " (pr-str api-name)
                  " — a side's API name must be unique per object type. A self-link needs"
                  " :forward-api-name and :reverse-api-name that differ.")}))

(defn- explain-link-types
  [schema]
  (let [schema (sch/indexed schema)]
    (into (vec (for [[ct-kw ct] (:link-types schema)
                     :let [r (link-type-problem schema ct-kw ct)]
                     :when r]
                 {:link ct-kw :reason r}))
          (side-name-problems schema))))

(defn- target-ref
  [ots cts tgt]
  (let [ot (:target-object-type tgt)
        lt (:target-link-type tgt)]
    (cond
      ot (when (contains? ots ot) [:object-type ot])
      lt (when (contains? cts lt) [:link-type lt])
      (or (keyword? tgt) (string? tgt))
      (cond (contains? ots tgt) [:object-type tgt]
            (contains? cts tgt) [:link-type tgt]))))

(defn explain-actions
  [schema]
  (let [ots (lookup/names-and-rids schema :object-types)
        cts (lookup/names-and-rids schema :link-types)
        its (lookup/names-and-rids schema :interfaces)]
    (vec
     (for [[k action] (or (:actions schema) {})
           :let [nm (or (:action-name action) k)
                 tgt (:target action)
                 derived (set (spec/derive-edits action))
                 gründe
                 (remove
                  nil?
                  (concat
                   [(when (and tgt (nil? (target-ref ots cts tgt)))
                      (cond
                        (:target-object-type tgt)
                        (str ":target-object-type " (pr-str (:target-object-type tgt))
                             " is not a declared object-type")
                        (:target-link-type tgt)
                        (str ":target-link-type " (pr-str (:target-link-type tgt))
                             " is not a declared link-type")
                        (or (keyword? tgt) (string? tgt))
                        (str ":target " (pr-str tgt)
                             " is neither a declared object-type nor a link-type")
                        :else (str ":target " (pr-str tgt) " is not a target shape")))]
                   (for [[pn {t :type}] (:params action)
                         :when (and (keyword? t)
                                    (not (spec/scalar-value-types t))
                                    (not (contains? ots t))
                                    (not (contains? cts t))
                                    (not (contains? its t)))]
                     (str "param " pn " has type " t
                          ", which is neither a storable value type nor a declared type"))
                   (when-not (spec/function-rule action)
                     (for [e (:edits action) :when (not (contains? derived e))]
                       (str "declares the edit " (pr-str e)
                            " but no rule performs it (rules mutate " (pr-str (vec derived)) ")")))
                   [(spec/function-rule-problem action)]
                   [(spec/function-deps-problem action)]
                   (functions/unresolvable-function-deps schema action)
                   (map :message (expr/rule-value-problems action))
                   (for [[i c] (map-indexed vector (:submission-criteria action))
                         {:keys [kind binding ops]} (expr/criterion-problems c)]
                     (case kind
                       :pred (str "submission criterion " i " uses "
                                  (str/join ", " (map pr-str ops))
                                  " in its :pred, which the expression evaluator has no"
                                  " meaning for — it would refuse EVERY submission with a"
                                  " server error instead of this criterion's message")
                       :test (str "submission criterion " i " binds " (pr-str binding)
                                  " to an expression starting with " (pr-str (first ops))
                                  ", and a rule argument only speaks "
                                  (str/join ", " (map pr-str (sort expr/binding-ref-ops)))
                                  " — anything else is read as a LITERAL, so the predicate"
                                  " compares the expression to itself")))))]
           :when (seq gründe)]
       {:action nm :reason (str/join "; " gründe)}))))

(defn- collect-slots
  [v]
  (cond
    (os/param? v) #{(second v)}
    (map? v) (reduce into #{} (map collect-slots (vals v)))
    (coll? v) (reduce into #{} (map collect-slots v))
    :else #{}))

(defn object-set-ok?
  [schema spec]
  (let [refs     (structure/object-set-refs spec)
        declared (into #{} (map #(if (map? %) (:name %) %)) (or (:param-slots spec) []))]
    (boolean
     (and (seq (:roots refs))
          (every? #(or (contains? (:object-types schema) %) (contains? (:interfaces schema) %))
                  (:roots refs))
          (every? #(contains? (:link-types schema) %) (:links refs))
          (every? declared (collect-slots (os/definition (dissoc spec :param-slots))))))))

(defn- board-security-problems
  [schema]
  (for [[t-kw t] (:transforms schema)
        {:keys [removes]} (some-> (:board t) board/security-problems)]
    {:transform (or (:transform-name t) t-kw)
     :reason    (str "removes " (pr-str removes) " on its output but does not allow changes"
                     " to security markings — set :security-approvals true on the board,"
                     " which keeps every change of it on a reviewed proposal")}))

(defn transform-problems [schema]
  (-> (transforms/explain-transform-refs schema)
      (into (transforms/explain-transform-collisions schema))
      (into (transforms/explain-orphaned-supplements schema))
      (into (board-security-problems schema))))

(defn- media-set-source-problems
  [schema]
  (for [[_ ms] (:media-sets schema)
        :let [nm  (:media-set-name ms)
              src (get-in schema [:sources (:source ms)])
              why (cond
                    (and (or (:virtual ms) (:sync ms)) (nil? (:source ms)))
                    "names no :source"
                    (and (:source ms) (nil? src))
                    (str "names the source " (pr-str (:source ms)) ", which does not exist")
                    (and (:virtual ms)
                         (seq (remove (set (:markings ms)) (:markings src))))
                    "lacks markings its source carries, and would show its files to anyone")]
        :when why]
    {:entity-class :media-sets :entity nm :message (str "Media set " (pr-str nm) " " why)}))

(defn- undeclared-media-sets
  [schema]
  (let [known (into #{} (keep (comp :media-set-name val)) (:media-sets schema))]
    (vec
     (for [[kk nf] [[:object-types :object-type-name] [:datasets :dataset-name]]
           [_rid spec] (get schema kk)
           a (mt/media-attributes (attribute/rows spec))
           :let [ms (mt/set-of a)]
           :when (and ms (not (contains? known ms)))]
       {:entity-class kk :entity (get spec nf)
        :attribute (:attribute-name a) :media-set ms}))))

(def ^:private wording
  {:names-unique?
   {:key :duplicate-names
    :finding (fn [rows]
               (for [{:keys [entity-class entity rids]} rows]
                 {:entity-class entity-class :entity entity
                  :message (str (name entity-class) " declares the name " (pr-str entity) " "
                                (count rids) " times: " (pr-str rids))}))}

   :no-dangling-refs?
   {:key :dangling-references
    :finding (fn [rows]
               (for [{:keys [in entity ref targets]} rows]
                 {:entity-class in :entity entity
                  :message (str "references " (pr-str ref) ", which no "
                                (pr-str (vec (sort targets))) " entry declares")}))}

   :link-types-valid?
   {:problems explain-link-types :key :link-type-errors :entity-class :link-types
    :finding (fn [rows]
               (for [{:keys [link reason]} rows]
                 {:entity link :message (str reason)}))}

   :actions-valid?
   {:problems explain-actions :key :action-errors :entity-class :actions
    :finding (fn [rows]
               (for [{:keys [action reason]} rows]
                 {:entity action :message (str reason)}))}

   :object-sets-valid?
   {:ok? #(every? (fn [[_ spec]] (object-set-ok? % spec)) (:object-sets %))
    :entity-class :object-sets}

   :media-set-sources?
   {:problems media-set-source-problems :key :media-set-source-problems
    :finding identity}

   :media-sets-declared?
   {:problems undeclared-media-sets :key :undeclared-media-sets
    :finding (fn [rows]
               (for [{:keys [entity-class entity attribute media-set]} rows]
                 {:entity-class entity-class :entity entity
                  :message (str "property " (pr-str attribute) " holds media in set "
                                (pr-str media-set) ", which no :media-sets entry declares"
                                " — a media reference without its set has no resource to"
                                " govern it")}))}

   :transforms-valid?
   {:problems transform-problems
    :key :transform-errors :entity-class :transforms
    :finding (fn [rows]
               (for [{:keys [transform reason]} rows]
                 {:entity transform :message (str reason)}))}

   :models-registered?
   {:key :unregistered-models :entity-class :transforms
    :finding (fn [rows]
               (for [{:keys [entity model]} rows]
                 {:entity entity
                  :message (str "names language model " (pr-str model)
                                ", which the model register does not hold")}))}

   :compute-graph-valid?
   {:entity-class :transforms
    :finding (fn [rows]
               (for [{:keys [entity input cycle?]} rows]
                 {:entity entity
                  :message (if cycle?
                             "these transforms read each other in a cycle"
                             (str "reads " (pr-str input) ", which does not resolve"
                                  " (or is also what it writes)"))}))}

   :shared-property-value-types?
   {:key :shared-property-value-type-errors :entity-class :shared-properties
    :finding (fn [rows]
               (for [{:keys [entity value-type type base]} rows]
                 {:entity entity
                  :message (if (nil? base)
                             (str ":value-type " (pr-str value-type)
                                  " names no declared value type")
                             (str ":type " (pr-str type) " is not the base " (pr-str base)
                                  " of its value type " (pr-str value-type)
                                  " — a value type wraps its base, it does not change storage"))}))}

   :title-keys-valid?
   {:key :title-key-errors :entity-class :object-types
    :finding (fn [rows]
               (for [{:keys [entity title-key available]} rows]
                 {:entity entity
                  :where [:object-types entity :title-key]
                  :message (str ":title " (pr-str title-key)
                                " names no property of this type — declared: "
                                (pr-str available))}))}

   :columns-valid?
   {:key :column-errors :entity-class :object-types
    :finding (fn [rows]
               (for [{:keys [problem entity column attributes]} rows]
                 {:entity entity
                  :message (str (name problem) " on column " (pr-str (str column))
                                " folded from " (pr-str attributes))}))}

   :lint-gate-clean?
   {:problems #(lint/gate-violations % (spec/vocabulary))
    :key :lint-gate-violations
    :finding identity}

   :implements-known?
   {:key :unknown-implements :entity-class :object-types
    :finding (fn [rows]
               (for [{:keys [entity interface]} rows]
                 {:entity entity
                  :message (str "implements " (pr-str interface)
                                ", which no interface declares")}))}

   :property-superset?
   {:key :property-superset-violations :entity-class :object-types
    :finding (fn [rows]
               (for [{:keys [entity interface attribute want got]} rows]
                 {:entity entity
                  :message (str "implements " (pr-str interface) " but property "
                                (pr-str attribute) " is "
                                (if got (str (pr-str got) ", not " (pr-str want))
                                    (str "missing (interface declares " (pr-str want) ")")))}))}

   :marking-constraints-met?
   {:key :marking-constraint-violations
    :problems authority/constraint-violations
    :finding (fn [rows]
               (for [{:keys [resource project marking reason]} rows]
                 {:entity resource
                  :message (str "carries marking " (pr-str marking)
                                (if (= :forbidden reason)
                                  ", which its project forbids"
                                  ", which its project does not allow")
                                " (project " (pr-str project) ") — markings travel with the"
                                " data: remove it at the source, or change the project's constraint")}))}

   :project-classification-met?
   {:key :project-classification-missing :entity-class :projects
    :problems authority/unclassified-projects
    :finding (fn [rows]
               (for [{:keys [project category]} rows]
                 {:entity project
                  :message (str "carries no " (pr-str category) " marking, which a marking of"
                                " that category requires of every project (:required-on-projects)"
                                " — mark the project, its space or its organization")}))}

   :ontology-references-local?
   {:key :cross-ontology-references
    :problems ontologies/cross-ontology-references
    :finding (fn [rows]
               (for [{:keys [section resource ontology references in]} rows]
                 {:entity-class section :entity resource
                  :message (str "references " (pr-str references) " of ontology " (pr-str in)
                                " from ontology " (pr-str ontology) " — a reference stays"
                                " inside its space's ontology or reaches the default one")}))}

   :project-organizations-in-space?
   {:key :project-organizations-outside-space :entity-class :projects
    :problems ontologies/organization-violations
    :finding (fn [rows]
               (for [{:keys [project space organizations]} rows]
                 {:entity project
                  :message (str "is shared with " (pr-str organizations) ", which its space "
                                (pr-str space) " is not restricted by — a project's"
                                " organizations are a subset of its space's")}))}

   :stream-types-unedited?
   {:key :stream-type-edits :entity-class :actions
    :problems streaming/edit-violations
    :finding (fn [rows]
               (for [{:keys [action object-type]} rows]
                 {:entity action
                  :message (str "edits " (pr-str object-type) ", which a stream backs — user"
                                " edits are not supported on streaming object types. Set"
                                " :direct true on its :backing (a direct datasource takes edits"
                                " beside the stream), or publish the change into the stream")}))}

   :stream-types-single-datasource?
   {:key :stream-type-supplements :entity-class :transforms
    :problems streaming/multi-datasource-violations
    :finding (fn [rows]
               (for [{:keys [transform object-type]} rows]
                 {:entity transform
                  :message (str "supplements " (pr-str object-type) ", which a stream backs —"
                                " multi-datasource object types are not supported on streams;"
                                " give the supplement a type of its own and link it")}))}

   :stream-backing-settings-valid?
   {:key :stream-backing-settings :entity-class :object-types
    :problems streaming/setting-violations
    :finding (fn [rows]
               (for [{:keys [object-type reason]} rows]
                 {:entity object-type :message reason}))}

   :stream-derivations-valid?
   {:key :stream-derivations :entity-class :datasets
    :problems streaming/derive-violations
    :finding (fn [rows]
               (for [{:keys [dataset reason]} rows]
                 {:entity dataset :message reason}))}

   :primary-organization-unique?
   {:key :primary-organization-conflicts :entity-class :organizations
    :problems ontologies/primary-conflicts
    :finding (fn [rows]
               (for [{:keys [principal organizations]} rows]
                 {:entity (first organizations)
                  :message (str (pr-str principal) " is a member of " (pr-str organizations)
                                " — one primary organization each; name it in the others'"
                                " :guests")}))}

   :link-constraints-met?
   {:key :link-constraint-violations :entity-class :object-types
    :finding (fn [rows]
               (for [{:keys [object-type interface link-name reason expected]} rows]
                 {:entity object-type
                  :message (str "interface " (pr-str interface) " constraint "
                                (pr-str link-name) ": " (name reason)
                                " — expected " (pr-str expected))}))}

   :interface-properties-agree?
   {:key :cross-interface-property-conflicts :entity-class :object-types
    :finding (fn [rows]
               (for [{:keys [entity attribute by-interface]} rows]
                 {:entity entity
                  :message (str "its interfaces disagree on property " (pr-str attribute)
                                ": " (pr-str by-interface))}))}

   :interface-links-agree?
   {:key :cross-interface-link-conflicts :entity-class :object-types
    :finding (fn [rows]
               (for [{:keys [entity link-name by-interface]} rows]
                 {:entity entity
                  :message (str "its interfaces disagree on link " (pr-str link-name)
                                ": " (pr-str by-interface))}))}

   :extends-known?
   {:key :extends-unknown :entity-class :interfaces
    :finding (fn [rows]
               (for [{:keys [entity parent]} rows]
                 {:entity entity
                  :message (str "extends " (pr-str parent)
                                ", which no interface declares")}))}

   :extends-acyclic?
   {:key :extends-cycles :entity-class :interfaces
    :finding (fn [rows]
               (for [{:keys [entity]} rows]
                 {:entity entity :message "its :extends chain contains itself"}))}})

(def ^:private gates (mapv #(merge % (wording (:id %))) ri/gates))

(defn frame-problems
  [schema]
  (vec (concat
        (for [k (structure/unknown-section-keys schema)]
          {:entity-class k :section-error "not a canonical schema section"})
        (for [{cls :section, required? :required?} ents/entities
              :let [m (get schema cls)]
              :when (if required? (not (map? m)) (and (some? m) (not (map? m))))]
          {:entity-class cls :section-error (str "section must be a map, got " (pr-str m))}))))

(def ^:private entity-of (coll/index-by :section ents/entities))

(defn entry-problem
  [cls k v]
  (when-let [{key-kind :key id-match? :id-match?} (entity-of cls)]
    (let [bad-key (case key-kind
                    :rid     (when-not (rid/well-formed-for? cls k)
                               (str "key is not a well-formed " (name cls) " RID"))
                    :keyword (when-not (keyword? k) "key must be a keyword")
                    :string  (when-not (string? k) "key must be a string"))
          bad-id  (when (and id-match? (not= k (:id v)))
                    (str "entity :id " (pr-str (:id v)) " ≠ its map key " (pr-str k)))
          shape   (or (spec/explain-entity cls v)
                      (when-not ((or (structure/value-checks cls) (spec/validators cls)) v)
                        [(str "does not match the declared shape for " cls)]))]
      (when (or bad-key bad-id shape)
        (cond-> {:entity-class cls :key k}
          (get v (rid/name-field-of cls)) (assoc :name (get v (rid/name-field-of cls)))
          bad-key (assoc :key-error bad-key)
          bad-id  (assoc :id-error bad-id)
          shape   (assoc :shape-error shape))))))

(defn- structural-problems [schema]
  (into (frame-problems schema)
        (for [{cls :section} ents/entities
              [k v] (get schema cls)
              :let [row (entry-problem cls k v)]
              :when row]
          row)))

(defn- shape-parts
  [e]
  (if (map? e) (sort-by (comp str key) e) e))

(defn structural-finding
  [{:keys [key section-error key-error id-error shape-error] nm :name :as row}]
  (finding/finding
   :schema/structure :error nil (or nm key)
   (assoc row :message (str/join "; " (remove nil? [section-error key-error id-error
                                                    (some->> shape-error shape-parts
                                                             (str/join "; "))])))))

(defn structural-findings
  [schema]
  (map structural-finding (structural-problems schema)))

(def gate-problems
  (into {}
        (for [{:keys [id problems ok?]} gates
              :when (not (contains? ri/stated-gates id))
              :when (not= :lint-gate-clean? id)]
          [id (fn [schema]
                (if problems
                  (mapv (fn [r] {::row r}) (problems schema))
                  (if (ok? schema) [] [{}])))])))

(def ^:private gate-by-rule (coll/index-by :rule gates))

(defn word
  [{:keys [rule severity] :as p}]
  (if-let [{:keys [id finding entity-class]} (gate-by-rule rule)]
    (let [row (if (contains? p ::row) (::row p) (dissoc p :rule :severity :gate))
          w   (if (and finding (not= {} row))
                (first (finding [row]))
                {:message (str (name id) " failed")})]
      (into (finding/finding rule (or severity :error) entity-class nil) w))
    (dissoc p :gate)))

(defn lint-key [f] [(:rule f) (str (:entity f))])

(def ^:private structural-keys
  [:entity-class :key :name :section-error :key-error :id-error :shape-error])

(defn reasons
  [findings]
  (let [by-gate    (group-by :gate findings)
        rows       (fn [fs]
                     (mapv #(if (contains? % ::row) (::row %) (dissoc % :rule :severity :gate)) fs))
        lint-rows  (mapv #(dissoc % :gate) (by-gate :lint-gate-clean?))
        structural (into [] (comp (filter #(= :schema/structure (:rule %)))
                                  (map #(into {} (remove (comp nil? val))
                                              (select-keys % structural-keys))))
                         findings)
        relational (into []
                         (comp (filter (comp seq by-gate :id)) (remove #(false? (:relational? %)))
                               (map :id))
                         ri/gates)
        details    (into {} (for [{:keys [id key]} gates
                                  :when key
                                  :let [fs (by-gate id)]
                                  :when (seq fs)]
                              [key (if (= :lint-gate-clean? id) lint-rows (rows fs))]))]
    (not-empty
     (cond-> details
       (seq structural) (assoc :structural structural)
       (seq relational) (assoc :relational-checks-failed relational)))))

(defn eval-pred
  [vals p] (expr/pred vals p))

(def empty-state {:objects {} :links {} :action-log []})
