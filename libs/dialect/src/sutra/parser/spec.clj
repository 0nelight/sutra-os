(ns sutra.parser.spec
  (:require [sutra.kit.attempt :as attempt]
            [sutra.types.refs :as refs]
            [sutra.kit.system :as sys]
            [sutra.types.rid :as rid]
            [sutra.types.source :as source]
            [sutra.types.entities :as ents]
            [sutra.types.transform-io :as tio]
            [sutra.types.expectations :as xp]
            [sutra.types.unit-tests :as unit-tests]
            [sutra.types.kinds :as kinds]
            [sutra.types.languages :as langs]
            [sutra.types.semver :as semver]
            [sutra.types.value-types :as vt]
            [sutra.types.run-limits :as limits]
            [sutra.parser.params :as params]
            [sutra.types.submission :as csub]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [malli.util :as mu]))

(set! *warn-on-reflection* true)

(def scalar-value-types
  vt/storable-types)

(def ValueType
  [:schema {:registry
            {::vt [:or
                   (into [:enum] scalar-value-types)
                   [:tuple [:= :decimal] :int :int]
                   [:tuple [:= :vector] :int]
                   [:tuple [:= :array] [:ref ::vt]]
                   [:tuple [:= :struct] [:map-of :keyword [:ref ::vt]]]]}}
   ::vt])

(def Cardinality
  [:enum :one-to-one :one-to-many :many-to-one :many-to-many])

(def ConstraintCardinality
  (into [:enum :one :many] (rest Cardinality)))

(def RefId
  [:or :keyword [:and :string [:fn rid/rid?]]])

(def ref-id-val?
  (m/validator RefId))

(def Attribute
  [:map {:closed false}
   [:attribute-name keyword?]
   [:attribute-type ValueType]
   [:opts {:optional true} [:maybe map?]]])

(def AttrList [:vector Attribute])

(defn- canonical-type [{:keys [in coerce type]}]
  (cond
    in                                          :any
    (= :kw coerce)                              :keyword
    (fn? type)                                  type
    (#{:string :boolean :keyword :int} type)    type
    (and (vector? type) (= :enum (first type))) type
    :else                                       :any))

(defn- canonical-schema [{:keys [section fields key refs]}]
  (let [ref-keys (into #{} (map (comp first :at)) refs)]
    (into [:map {:closed false}]
          (concat (when (= :rid key) [[(ents/name-field section) keyword?]])
                  (for [[k {:keys [as author-only] :as rule}] fields :when (not author-only)]
                    [(or as k) {:optional true}
                     [:maybe (if (ref-keys k) :any (canonical-type rule))]])))))

(def ObjectType
  (mu/merge
   (canonical-schema (ents/by-section :object-types))
   [:map {:closed false}
    [:attribute-list AttrList]
    [:implements      {:optional true} [:maybe [:vector RefId]]]
    [:match-keys      {:optional true} [:maybe [:vector :keyword]]]
    [:dataset         {:optional true} [:maybe RefId]]
    [:backing         {:optional true} [:maybe [:map {:closed false}
                                                [:dataset RefId]]]]
    [:external-source {:optional true} any?]
    [:checks          {:optional true} [:maybe [:vector any?]]]
    [:semantic-search {:optional true} [:maybe [:map {:closed false}
                                                [:property :keyword]]]]]))

(def ConnectionType
  [:map {:closed false}
   [:link-type-name keyword?]
   [:from-object-type     RefId]
   [:to-object-type       RefId]
   [:cardinality          Cardinality]
   [:external-source      {:optional true} any?]
   [:satisfies {:optional true}
    [:maybe [:vector [:map {:closed false}
                      [:interface RefId]
                      [:constraint keyword?]]]]]])

(def LinkConstraint
  [:and
   [:map {:closed false}
    [:link-name   keyword?]
    [:cardinality ConstraintCardinality]
    [:required    {:optional true} boolean?]]
   [:fn {:error/message ":to-object-type or :to-interface required (ref-id)"}
    (fn [{:keys [to-object-type to-interface]}]
      (or (ref-id-val? to-object-type) (ref-id-val? to-interface)))]])

(def InterfaceType
  [:map {:closed false}
   [:interface-name keyword?]
   [:extends          {:optional true} [:maybe [:vector RefId]]]
   [:display-name     {:optional true} [:maybe string?]]
   [:description      {:optional true} [:maybe string?]]
   [:status           {:optional true} [:maybe keyword?]]
   [:searchable       {:optional true} [:maybe boolean?]]
   [:property-list    AttrList]
   [:link-constraints {:optional true} [:maybe [:vector LinkConstraint]]]])

(def ^:private attr-valid? (m/validator Attribute))

(defn- input-ref-ok?
  [{:keys [kind] :as in}]
  (if-let [{:keys [at shape]} (kinds/input-ref kind)]
    (case shape
      :keyword (keyword? (get in at))
      :model   (and (or (nil? (:source in)) (keyword? (:source in)))
                    (string? (:model in)))
      (ref-id-val? (get in at)))
    (kinds/valueless-input? kind)))

(defn- valid-transform-input?
  [in]
  (and (map? in)
       (kinds/source-input? (:kind in))
       (input-ref-ok? in)))

(defn- valid-compute-input?
  [in]
  (and (map? in)
       (input-ref-ok? in)
       (or (not= :datasource (:kind in))
           (string? (:table in))
           (string? (:query in)))))

(def ^:private output-knobs
  (set tio/backing-fields))

(def ^:private map-or-nil? (some-fn nil? map?))
(def ^:private kw-or-nil?  (some-fn nil? keyword?))

(defn- knobs-clear?
  [m allowed]
  (every? #(nil? (get m %)) (remove allowed output-knobs)))

(defn- edit-conflict-or-nil?
  [m]
  (or (nil? m)
      (and (map? m)
           (contains? #{:apply-user-edits :apply-most-recent} (:strategy m))
           (kw-or-nil? (:timestamp-attr m))
           (empty? (dissoc m :strategy :timestamp-attr)))))

(def ^:private output-shapes
  {[:source :object-type] [{:object-type ref-id-val? :primary-key keyword?
                            :column-map map-or-nil? :edit-conflict edit-conflict-or-nil?}]
   [:source :dataset]     [{:dataset ref-id-val? :primary-key kw-or-nil?
                            :column-map map-or-nil?}]
   [:object :dataset]     [{:dataset ref-id-val? :column-map map-or-nil?}]
   [:source :link-type]   [{:link-type ref-id-val? :from-key-column keyword?
                            :to-key-column keyword?
                            :match-column nil? :shared-property nil?}
                           {:link-type ref-id-val? :from-key-column keyword?
                            :match-column keyword? :shared-property keyword?}]
   [:object :link-type]   [{:link-type ref-id-val? :fk-attribute keyword?}]
   [:source :export]      [{:export ref-id-val?}]
   [:source :media-set]   [{:media-set ref-id-val?}]})

(defn- producer-side
  [in-kind]
  (cond (or (kinds/source-rows? in-kind) (langs/compute-language? in-kind)) :source
        (= :object-type in-kind) :object))

(defn- holds?
  [ok?]
  (attempt/or-answer false (ok?)))

(defn- failing
  [checks]
  (vec (for [[msg ok?] checks :when (not (holds? ok?))] msg)))

(defn- stray-knobs
  [m allowed]
  (filter #(some? (get m %)) (remove allowed output-knobs)))

(defn- shape-misses
  [shape m]
  (concat (for [[k pred] shape :when (not (pred (get m k)))]
            (if (nil? (get m k)) (str (pr-str k) " is missing")
                (str (pr-str k) " " (pr-str (get m k)) " is not valid here")))
          (for [k (stray-knobs m shape)] (str (pr-str k) " does not belong here"))))

(defn- output-entry-problems
  [in-kind out]
  (let [shapes (get output-shapes [(producer-side in-kind) (:kind out)])
        what   (str "output " (some-> (:name out) name) " (" (some-> (:kind out) name) ")")]
    (cond
      (not (map? out))             [(str "an output is a map, not " (pr-str out))]
      (not (keyword? (:name out))) [(str "an output needs a keyword :name: " (pr-str out))]
      (empty? shapes)              [(str what ": a " (some-> in-kind name)
                                         " transform does not write this kind of output")]
      :else
      (let [misses (map #(shape-misses % out) shapes)]
        (if (some empty? misses)
          []
          [(str what ": " (str/join ", " (apply min-key count misses)))])))))

(defn- valid-output-entry? [in-kind out]
  (empty? (output-entry-problems in-kind out)))

(defn- valid-compute-inputs-map?
  ([inputs] (valid-compute-inputs-map? inputs false))
  ([inputs external?]
   (if (and external? (or (nil? inputs) (and (map? inputs) (empty? inputs))))
     true
     (and (map? inputs)
          (seq inputs)
          (every? (fn [[k v]] (and (keyword? k) (valid-compute-input? v)))
                  inputs)))))

(def ^:private inputs-message
  ":inputs must map keyword aliases to compute inputs (a :datasource one names :table or :query)")

(defn- code-problems
  [t]
  (failing [[":transform-name must be a keyword" #(keyword? (:transform-name t))]
            [":code must be a string" #(string? (:code t))]
            [inputs-message
             #(valid-compute-inputs-map? (:inputs t) (boolean (or (seq (:external-systems t))
                                                                  (seq (:parameters t)))))]
            [":ops belongs to a datasource transform, not a code one" #(nil? (:ops t))]]))

(defn- sql-problems
  [t]
  (failing [[":transform-name must be a keyword" #(keyword? (:transform-name t))]
            [":query must be a string" #(string? (:query t))]
            [inputs-message
             #(valid-compute-inputs-map? (:inputs t) (boolean (seq (:external-systems t))))]
            [":ops belongs to a datasource transform, not a :sql one" #(nil? (:ops t))]
            [":code does not belong on a :sql transform — its body is :query" #(nil? (:code t))]]))

(defn- query-function-spec?
  [t]
  (some? (:function-name t)))

(defn output-mode-count [t]
  (let [outs (:outputs t)]
    (count (filter some? [(:output t) (when (seqable? outs) (seq outs)) (:output-columns t)]))))

(defn- knob-message
  [t allowed]
  (str (str/join ", " (map pr-str (stray-knobs t allowed)))
       " does not belong on the transform with this output form"))

(defn- output-problems
  [in-kind t]
  (let [{:keys [output outputs output-columns incremental]} t]
    (cond
      (and (langs/compute-language? in-kind) (query-function-spec? t))
      []

      (not= 1 (output-mode-count t))
      ["exactly one of :output / :outputs / :output-columns is declared"]

      output-columns
      (failing [[":output-columns must be a vector of attributes"
                 #(and (vector? output-columns) (every? attr-valid? output-columns))]
                [":output-columns is written by a source-side transform only"
                 #(= :source (producer-side in-kind))]
                [(if incremental
                   "incremental :output-columns needs a keyword :primary-key"
                   ":output-columns takes a :primary-key only with :incremental")
                 #(if incremental (keyword? (:primary-key t)) (nil? (:primary-key t)))]
                [(knob-message t #{:primary-key}) #(knobs-clear? t #{:primary-key})]])

      (and (seqable? outputs) (seq outputs))
      (into (failing [[":outputs must be a vector" #(vector? outputs)]
                      [":outputs names an output twice"
                       #(= (count outputs) (count (set (map :name outputs))))]
                      ["an object-type input feeds exactly one output"
                       #(or (not= :object-type in-kind) (= 1 (count outputs)))]
                      [(knob-message t #{}) #(knobs-clear? t #{})]])
            (when (sequential? outputs) (mapcat #(output-entry-problems in-kind %) outputs)))

      output
      (if-let [out (attempt/silently (first (tio/outputs-of t)))]
        (output-entry-problems in-kind out)
        [(str ":output " (pr-str output) " is no output")]))))

(defn- valid-polymorphic-source? [s]
  (and (map? s)
       (ref-id-val? (:from-type s))
       (keyword? (:from-key-column s))
       (keyword? (:to-key-column s))
       (or (nil? (:to-type s)) (ref-id-val? (:to-type s)))
       (if (langs/compute-language? (:language s))
         (and (map? (:inputs s))
              (string? (get s (langs/body-field (:language s)))))
         (and (keyword? (:datasource s))
              (or (string? (:table s)) (string? (:query s)))))
       (or (nil? (:transforms s))
           (and (vector? (:transforms s))
                (every? #(keyword? (:op %)) (:transforms s))))))

(defn- valid-polymorphic-conn-transform? [t]
  (and (keyword? (:transform-name t))
       (vector? (:sources t)) (seq (:sources t))
       (every? valid-polymorphic-source? (:sources t))
       (let [out (:output t)]
         (and (map? out)
              (= :link-type (:kind out))
              (ref-id-val? (:link-type out))))
       (nil? (:input t)) (nil? (:source t))
       (nil? (:from-key-column t)) (nil? (:to-key-column t))
       (nil? (:outputs t)) (nil? (:output-columns t))))

(defn transform-kind
  [t]
  (let [lang (:language t)]
    (cond
      (langs/query-language? lang)   :compute/sql
      (langs/compute-language? lang) (keyword "compute" (name lang))
      (= :polymorphic (:strategy t)) :polymorphic
      :else                          :source)))

(defn- source-problems
  [t]
  (failing [[":transform-name must be a keyword" #(keyword? (:transform-name t))]
            [(str ":input " (pr-str (:input t)) " is no valid source input")
             #(valid-transform-input? (:input t))]
            [":source belongs to a :datasource input and names its :table or :query"
             #(or (nil? (:source t))
                  (and (= :datasource (get-in t [:input :kind]))
                       (map? (:source t))
                       (or (string? (:table (:source t)))
                           (string? (:query (:source t))))))]
            [":ops must be a vector of steps, each with a keyword :op"
             #(or (nil? (:ops t))
                  (and (vector? (:ops t)) (every? (fn [o] (keyword? (:op o))) (:ops t))))]]))

(defn shape-problems
  [t]
  (case (transform-kind t)
    (:compute/python :compute/clojure) (into (code-problems t) (output-problems (:language t) t))
    :compute/sql (into (sql-problems t) (output-problems :sql t))
    :polymorphic (failing [[(str "a polymorphic link transform needs :sources, each with"
                                 " :from-type, :from-key-column and :to-key-column, and one"
                                 " :link-type :output — and nothing of a single-source one")
                            #(valid-polymorphic-conn-transform? t)]])
    :source      (into (source-problems t) (output-problems (get-in t [:input :kind]) t))))

(defn- valid-shape?
  [t]
  (empty? (shape-problems t)))

(def ^:private CodeTransformShape
  [:map {:closed false}
   [:transform-name keyword?]
   [:code string?]
   [:inputs {:optional true} [:maybe [:map-of keyword? :any]]]])

(def ^:private TransformShape
  [:multi {:dispatch transform-kind}
   [:compute/python  CodeTransformShape]
   [:compute/clojure CodeTransformShape]
   [:compute/sql    [:map {:closed false}
                     [:transform-name keyword?]
                     [:query string?]
                     [:inputs {:optional true} [:maybe [:map-of keyword? :any]]]
                     [:parameters {:optional true}
                      [:maybe [:map-of keyword?
                               [:map {:closed false}
                                [:type [:enum :string :number :boolean]]]]]]]]
   [:polymorphic    [:map {:closed false}
                     [:transform-name keyword?]
                     [:sources [:vector :any]]]]
   [:source         [:map {:closed false}
                     [:transform-name keyword?]
                     [:input [:map {:closed false} [:kind keyword?]]]]]])

(defn- compute-lang? [t]
  (langs/compute-language? (:language t)))

(defn- stale-backing [t]
  (filter #(let [v (get t %)] (if (= % :ops) (seq v) (some? v))) [:input :source :ops]))

(defn- bad-input-kinds [t]
  (when (map? (:inputs t))
    (->> (vals (:inputs t)) (map :kind) (remove kinds/compute-input?) distinct)))

(defn- param-errors [t]
  (params/errors (or (:transform-name t) :transform) t))

(def ^:private transform-rules
  [{:when #(and (query-function-spec? %) (langs/query-language? (:language %)))
    :msg  (fn [_]
            (str "a Function runs USER CODE at apply time, and :sql is not a code language: "
                 "its :inputs resolve to database views over BUILT tables, which an action step "
                 "does not have. Declare :language :python or :clojure, or author it under "
                 ":transforms, where a build materializes it."))}
   {:when #(and (query-function-spec? %) (pos? (output-mode-count %)))
    :gate compute-lang?
    :msg  #(str "a Function declares NO output — it returns its rows to whoever invoked it "
                "(:invoke-function binds them), and found " (output-mode-count %)
                ". An output would make it "
                "the target's BACKING transform, scheduled and materialized by every build. "
                "Drop the output, or author it under :transforms to keep it.")}
   {:when #(and (not (query-function-spec? %)) (not= 1 (output-mode-count %)))
    :msg  (fn [t]
            (let [n-out (output-mode-count t)]
              (str "output-mutex: "
                   (if-let [nm (:transform-name t)] (str "transform " (pr-str (name nm)))
                           "a transform")
                   " must declare EXACTLY ONE of :output / :outputs / :output-columns — found "
                   n-out (when (pos? n-out)
                           (str " (" (str/join ", " (filter #(some? (not-empty (get t %)))
                                                            [:output :outputs :output-columns]))
                                ")"))
                   ".")))}
   {:when #(and (:language %) (seq (stale-backing %)))
    :gate compute-lang?
    :msg  #(str "nil-guard: the compute transform (:language " (name (:language %))
                ") feeds off :inputs + "
                (pr-str (langs/body-field (:language %)))
                ", so it must NOT carry the datasource-backing field(s) "
                (str/join ", " (map str (stale-backing %)))
                " — did you convert it from a datasource sync? Drop them.")}
   {:when #(and (:language %) (seq (bad-input-kinds %)))
    :gate compute-lang?
    :msg  #(str "input-kind: :inputs " (pr-str (vec (bad-input-kinds %)))
                " not a legal compute input — allowed: " (pr-str (sort kinds/compute-input-kinds))
                ".")}
   {:when #(xp/transform-problems %)
    :msg  #(str "checks (data expectations): " (str/join "; " (xp/transform-problems %)))}
   {:when #(unit-tests/transform-problems %)
    :msg  #(str "unit tests: " (str/join "; " (unit-tests/transform-problems %)))}
   {:when #(seq (param-errors %))
    :gate #(langs/query-language? (:language %))
    :msg  #(str/join "; " (param-errors %))}])

(defn transform-error
  [t]
  (or (some (fn [{w :when msg :msg}] (when (w t) (msg t))) transform-rules)
      (some->> (seq (shape-problems t)) (str/join "; ")
               (str "transform " (some-> (:transform-name t) name) ": "))
      "malformed transform"))

(defn- valid-transform?
  [t]
  (and (valid-shape? t)
       (not-any? (fn [{w :when g :gate}] (and ((or g any?) t) (w t))) transform-rules)))

(def Transform
  [:and TransformShape
   [:fn {:error/fn (fn [{:keys [value]} _] (transform-error value))}
    valid-transform?]])

(def SharedProperty
  [:and
   [:map {:closed false} [:type ValueType]]
   [:fn {:error/message ":identity is forbidden — use object-type :match-keys"}
    #(not (contains? % :identity))]])

(def Function
  [:and
   [:map {:closed false}
    [:function-name keyword?]
    [:target-type   {:optional true} RefId]
    [:language      [:enum :clojure :python :kernel]]
    [:source        string?]
    [:library       {:optional true} :boolean]
    [:uses          {:optional true} [:sequential keyword?]]
    [:param-types   {:optional true} [:maybe [:map-of :keyword :any]]]
    [:timeout-ms    {:optional true}
     [:int {:min (:min limits/timeout-ms) :max (:max limits/timeout-ms)}]]]
   [:fn {:error/message "an edit function names its :target-type; only a :library has none"}
    #(or (:library %) (contains? % :target-type))]
   [:fn
    {:error/message ":snapshot is gone: every run reads the state it started on, also after an external call — drop the option"}
    #(not (contains? % :snapshot))]])

(def Step
  [:map {:closed false}
   [:op keyword?]
   [:phase {:optional true} [:enum :writeback :side-effect]]])

(def Param
  [:map {:closed false}
   [:type     some?]
   [:multiple {:optional true} :boolean]])

(def Criterion
  [:map {:closed false}
   [:test map?]
   [:pred [:vector any?]]
   [:message {:optional true} string?]])

(def Edit
  [:or
   [:map {:closed false} [:object-type some?]]
   [:map {:closed false} [:link-type some?]]
   [:map {:closed false} [:interface some?]]])

(def OnBranch
  [:map {:closed true}
   [:webhooks       {:optional true} :boolean]
   [:notifications  {:optional true} :boolean]
   [:notification-recipients {:optional true} [:enum :branch-owner :default]]
   [:external-calls {:optional true} :boolean]])

(def Action
  [:map {:closed false}
   [:rules               {:optional true} [:maybe [:vector Step]]]
   [:params              {:optional true} [:maybe [:map-of keyword? Param]]]
   [:submission-criteria {:optional true} [:maybe [:vector Criterion]]]
   [:edits               {:optional true} [:maybe [:vector Edit]]]
   [:on-branch           {:optional true} [:maybe OnBranch]]])

(defn derive-edits
  [action]
  (let [edit-of (fn [step]
                  (when-let [k (case (:op step)
                                 (:create :update :delete-object :create-or-update) :object-type
                                 (:create-link :delete-link)      :link-type
                                 (:create-of-interface :update-of-interface :delete-of-interface
                                                       :create-link-of-interface
                                                       :delete-link-of-interface)
                                 :interface
                                 nil)]
                    (when-let [v (get step k)] {k v})))]
    (vec (distinct (keep edit-of (refs/steps (:rules action)))))))

(def function-rule-op
  :apply-edit-function)

(defn unknown-rule-ops
  [{known :rule-ops} action]
  (when known
    (not-empty (into (sorted-set)
                     (comp (keep :op)
                           (map #(if (string? %) (keyword %) %))
                           (remove #(contains? known %)))
                     (refs/steps (:rules action))))))

(defn unknown-rule-ops-problem
  [vocab action]
  (when-let [ops (unknown-rule-ops vocab action)]
    (str "rule op(s) " (str/join ", " (map pr-str ops))
         " name no step verb — the action would install and then refuse every"
         " submission with \"Unknown step op\"")))

(defn- param-order
  [action]
  (let [params (:params action)]
    (into []
          (comp (mapcat :params) (remove #(csub/hidden? (get params %))))
          (csub/form-sections (:form action) params))))

(defn- platform-binding?
  [k]
  (contains? #{\$ \_} (first (name k))))

(defn- when-reads
  [{:keys [field-refs]} when-expr]
  (when-let [refs (some-> field-refs (apply [when-expr]))]
    (into (sorted-set) (remove platform-binding?) refs)))

(defn- when-unknown-ops
  [{:keys [unknown-ops]} when-expr]
  (some-> unknown-ops (apply [when-expr]) not-empty))

(defn- base-value
  [spec k]
  (if (and (= k :required) (not (contains? spec k))) false (get spec k)))

(defn- redundant-keys
  [spec override]
  (for [[k v] (dissoc override :when)
        :when (and (or (contains? spec k) (= k :required))
                   (= v (base-value spec k)))]
    k))

(defn override-problems
  [vocab action]
  (let [action (csub/with-section-overrides action)
        params (:params action)
        at     (zipmap (param-order action) (range))
        err    (fn [m] {:severity :error :message m})]
    (vec
     (for [[p spec] (sort-by (comp str key) params)
           [i ovr]  (map-indexed vector (:overrides spec))
           problem
           (concat
            (when-let [ops (when-unknown-ops vocab (:when ovr))]
              [(err (str ":when names " (str/join ", " (map pr-str ops))
                         ", which the expression language does not know — every"
                         " submission would raise :unknown-op, and `override-holds?`"
                         " swallows it, so the override silently never fires"))])
            (for [r (when-reads vocab (:when ovr))
                  :let [problem
                        (cond
                          (not (contains? params r))
                          (str ":when reads " (pr-str r) ", which this action does not"
                               " declare — the kernel's `field` throws :unknown-field,"
                               " `override-holds?` swallows it, and the override"
                               " silently never fires")

                          (and (at r) (at p) (>= (at r) (at p)))
                          (str ":when reads " (pr-str r) ", which the form shows at or"
                               " after " (pr-str p) " — the condition would decide "
                               (pr-str p) " before " (pr-str r) " can be answered."
                               " Order the :form so what an override reads comes first."))]
                  :when problem]
              (err problem))
            (for [k (redundant-keys spec ovr)]
              {:severity :warn
               :message  (str "sets " (pr-str k) " " (pr-str (get ovr k))
                              ", which the parameter already is — it changes nothing")}))]
       (update problem :message
               #(str "parameter " (pr-str p) " override " (inc i) ": " %))))))

(defn function-rule
  [action]
  (first (filter #(= function-rule-op (:op %))
                 (refs/steps (:rules action)))))

(defn action-log-rule?
  [rule]
  (= [:get :log-id] (get-in rule [:values :log-id])))

(def ^:private beside-function-rule
  #{:invoke-webhook :invoke-function :notification})

(defn function-rule-problem
  [action]
  (when-let [step (function-rule action)]
    (let [rules (->> (:rules action)
                     (remove action-log-rule?)
                     (remove (comp beside-function-rule :op)))]
      (cond
        (nil? (:function step))
        "the function rule names no :function"

        (not= [step] (vec rules))
        (str "an action with a function rule may have NO other rule besides webhooks,"
             " notifications and the functions that feed them — the function computes every edit itself;"
             " found " (count rules) " top-level rule(s): " (pr-str (mapv :op rules)))

        (empty? (:edits action))
        (str "a function-backed action must declare its :edits provenance — "
             "the edits are computed at runtime, so the declaration is the "
             "only statement of what " (pr-str (:function step)) " may touch")

        (and (:batched step) (empty? (:inputs step)))
        (str "a :batched function rule must declare :inputs — they name the "
             "FIELDS of the struct each Action call becomes (field → action "
             "parameter). Without them every entry of the batch is empty.")))))

(def function-dep-ops
  #{function-rule-op :invoke-function})

(defn function-deps
  [action]
  (->> (refs/steps (:rules action))
       (filter #(contains? function-dep-ops (:op %)))
       (mapv #(select-keys % [:op :function :version :auto-upgrade :batched]))))

(defn function-dep-problem
  [{:keys [op function version auto-upgrade batched]}]
  (cond
    (and batched (not= op function-rule-op))
    (str op " on " (pr-str function) " sets :batched, which only the function"
         " rule (" function-rule-op ") takes — it is the whole batch of ACTION"
         " CALLS handed to the backing function in one execution")

    (and (some? version) (not (semver/valid? version)))
    (str op " on " (pr-str function) " names :version " (pr-str version)
         ", which is not a semantic version (MAJOR.MINOR.PATCH)")

    (and auto-upgrade (nil? version))
    (str op " on " (pr-str function) " enables :auto-upgrade without a :version"
         " — auto-upgrade is the backward-compatible range above a MINIMUM"
         " version, so it needs one to start from")

    (and auto-upgrade (semver/initial-development? version))
    (str op " on " (pr-str function) " enables :auto-upgrade at " version
         ", a 0.y.z version. Under initial development the function may change"
         " at any time, so a minor release is not a promise of compatibility"
         " and there is nothing safe to upgrade into. Pin it, or publish 1.0.0.")))

(defn function-deps-problem
  [action]
  (some function-dep-problem (function-deps action)))

(defn edits-consistent?
  [action]
  (or (some? (function-rule action))
      (let [derived (set (derive-edits action))]
        (every? derived (:edits action)))))

(def QueryType
  [:map {:closed false}
   [:root        RefId]
   [:param-slots {:optional true} [:maybe [:vector :any]]]
   [:order-by    {:optional true} [:maybe :keyword]]
   [:limit       {:optional true} [:maybe number?]]
   [:include     {:optional true} [:maybe [:vector :any]]]])

(defn- non-empty-strs?
  [x] (and (coll? x) (seq x) (every? string? x)))

(def ChangeLog
  [:map {:closed true}
   [:table string?]
   [:columns [:map {:closed true}
              [:table    string?]
              [:pk       string?]
              [:column   string?]
              [:previous string?]
              [:next     string?]
              [:actor    [:fn non-empty-strs?]]
              [:date     {:optional true} string?]
              [:time     {:optional true} string?]
              [:correlation {:optional true} string?]]]
   [:constants {:optional true} [:map-of string? :any]]
   [:insert-constants {:optional true} [:map-of string? :any]]
   [:delete-constants {:optional true} [:map-of string? :any]]])

(def ^:private changelog-valid? (m/validator ChangeLog))

(def AutoNumber
  [:map {:closed true}
   [:table  string?]
   [:where  [:and [:map-of string? :any] [:fn seq]]]
   [:prefix-column  {:optional true} string?]
   [:counter-column string?]
   [:width-column   {:optional true} string?]
   [:width          {:optional true} pos-int?]
   [:into   string?]])

(def ^:private autonumber-valid? (m/validator AutoNumber))

(defn- column-values-valid?
  [writable cv]
  (or (nil? cv)
      (and (map? cv) (seq cv)
           (every? (fn [[col vs]]
                     (and (string? col)
                          (contains? (set writable) col)
                          (coll? vs) (seq vs)))
                   cv))))

(def WritebackAllowlist
  [:and
   [:map-of string?
    [:and
     [:map {:closed false}
      [:pk string?]
      [:writable-columns [:fn non-empty-strs?]]]
     [:fn (fn [{:keys [object-type timestamp-attr source-of-record
                       insertable-columns identity match-key mode webhook-id
                       deletable? delete-guard on-duplicate]}]
            (and (or (nil? object-type) (keyword? object-type))
                 (or (nil? deletable?) (boolean? deletable?))
                 (or (nil? delete-guard) (non-empty-strs? delete-guard))
                 (or (nil? timestamp-attr) (keyword? timestamp-attr))
                 (contains? #{nil :apply-user-edits :apply-most-recent} source-of-record)
                 (or (nil? insertable-columns) (non-empty-strs? insertable-columns))
                 (or (nil? identity) (string? identity))
                 (or (nil? match-key) (string? match-key) (non-empty-strs? match-key))
                 (contains? #{nil :bind :abort} on-duplicate)
                 (contains? #{nil :jdbc :webhook} mode)
                 (or (nil? webhook-id) (keyword? webhook-id) (string? webhook-id))))]
     [:fn {:error/message ":log must be a change-log descriptor or an explicit :none"}
      (fn [{:keys [log]}] (or (= :none log) (changelog-valid? log)))]
     [:fn
      {:error/message ":actor-columns is {:insert [col …] :update [col …]} — the row's own who-changed-it columns, stamped with the action's actor"}
      (fn [{:keys [actor-columns]}]
        (or (nil? actor-columns)
            (and (map? actor-columns) (seq actor-columns)
                 (every? #{:insert :update} (keys actor-columns))
                 (every? non-empty-strs? (vals actor-columns)))))]
     [:fn
      {:error/message ":pk-columns is a vector of at least two column names; :identity, if set, must then equal the virtual :pk"}
      (fn [{:keys [pk-columns pk identity]}]
        (or (nil? pk-columns)
            (and (vector? pk-columns) (< 1 (count pk-columns))
                 (every? string? pk-columns)
                 (not (some #{pk} pk-columns))
                 (or (nil? identity) (= identity pk)))))]
     [:fn {:error/message ":deletable? needs a :delete-guard naming allowlisted columns"}
      (fn [{:keys [deletable? delete-guard writable-columns]}]
        (or (not deletable?)
            (and (seq delete-guard)
                 (every? (set writable-columns) delete-guard))))]
     [:fn
      {:error/message
       ":column-values must name allowlisted columns and offer a non-empty value set"}
      (fn [{:keys [column-values writable-columns]}]
        (column-values-valid? writable-columns column-values))]
     [:fn
      {:error/message ":column-map must map incoming column names onto allowlisted columns (or onto the :pk / :identity, which are resolvable but never written)"}
      (fn [{:keys [column-map writable-columns pk identity]}]
        (let [erlaubt (into #{} (remove nil?) (concat writable-columns [pk identity]))]
          (or (nil? column-map)
              (and (map? column-map) (seq column-map)
                   (every? (fn [[from to]]
                             (and (string? from) (string? to)
                                  (contains? erlaubt to)))
                           column-map)))))]
     [:fn
      {:error/message "an insertable, logging table needs :log :insert-constants (the foreign system's create marker)"}
      (fn [{:keys [insertable-columns log]}]
        (or (nil? insertable-columns)
            (= :none log)
            (and (map? log) (seq (:insert-constants log)))))]
     [:fn
      {:error/message ":mode :webhook needs a :webhook-id and an :identity-path into its response"}
      (fn [{:keys [mode webhook-id identity-path]}]
        (or (not= :webhook mode)
            (and (some? webhook-id)
                 (vector? identity-path)
                 (seq identity-path)
                 (every? #(or (keyword? %) (string? %)) identity-path))))]
     [:fn
      {:error/message ":sequence needs :column + :scope on insertable columns — and the caller may not supply them"}
      (fn [{:keys [sequence insertable-columns]}]
        (or (nil? sequence)
            (let [ic (set insertable-columns)]
              (and (map? sequence)
                   (string? (:column sequence)) (contains? ic (:column sequence))
                   (string? (:scope sequence))  (contains? ic (:scope sequence))
                   (or (nil? (:also sequence))
                       (and (non-empty-strs? (:also sequence))
                            (every? ic (:also sequence))))
                   (or (nil? (:pad sequence)) (nat-int? (:pad sequence)))))))]
     [:fn
      {:error/message ":sequence :pad zero-fills only the :also columns, and :also is empty — the pad would do nothing. A text sequence column is padded only when it is listed in :also too: {:column \"NR\" :also [\"NR\"] :pad 2}"}
      (fn [{:keys [sequence]}]
        (or (not (map? sequence))
            (nil? (:pad sequence))
            (seq (:also sequence))))]
     [:fn
      {:error/message
       ":autonumber must be well-formed, declare a width, and write into an insertable column"}
      (fn [{:keys [autonumber insertable-columns]}]
        (or (nil? autonumber)
            (every? (fn [a]
                      (and (autonumber-valid? a)
                           (or (:width a) (:width-column a))
                           (contains? (set insertable-columns) (:into a))))
                    (if (sequential? autonumber) autonumber [autonumber]))))]]]
   [:fn seq]])

(def writeback-valid? (m/validator WritebackAllowlist))

(def default-source-types
  {:rest {:label "REST API" :glyph "🌐"
          :desc "HTTP API (base URL + API key) — used in code via external transforms."
          :capability :external-transform
          :fields [{:key :base-url :label "Base URL" :kind :string :required true
                    :placeholder "https://api.example.com"}
                   {:key :api-key :label "API key" :kind :secret :required true
                    :placeholder "sk-…"}]
          :egress-from {:url-host-port :base-url}}
   :imap {:label "IMAP mailbox" :glyph "✉️"
          :desc "Mailbox pulled in code via an external transform (host/user + password)."
          :capability :external-transform
          :fields [{:key :host :label "Host" :kind :string :required true
                    :placeholder "imap.example.com"}
                   {:key :port :label "Port" :kind :number :placeholder "993"}
                   {:key :user :label "Username" :kind :string}
                   {:key :password :label "Password" :kind :secret :required true}]
          :egress-from {:host-port [:host :port]}}})

(defn source-types
  [connectors]
  (into default-source-types
        (for [[t c] connectors :when (and (:form c) (source/connector-in connectors t))]
          [t (:form c)])))

(defn vocabulary
  []
  {:rule-ops     (some-> (sys/provided (sys/process) :rules/known-ops) (apply []))
   :field-refs   (sys/provided (sys/process) :expr/field-refs)
   :unknown-ops  (sys/provided (sys/process) :expr/unknown-ops)
   :source-types (source-types (source/connectors))})

(defn descriptor-egress
  [{:keys [egress-from]} config]
  (cond
    (:url-host-port egress-from)
    (let [u (java.net.URI. (str (get config (:url-host-port egress-from))))
          h (.getHost u)
          p (let [p (.getPort u)]
              (if (neg? p) (if (= "https" (.getScheme u)) 443 80) p))]
      (when h [(str h ":" p)]))
    (:host-port egress-from)
    (let [[hk pk] (:host-port egress-from)
          h (some-> (get config hk) str not-empty)
          p (some-> (get config pk) str not-empty)]
      (when h [(str h (when p (str ":" p)))]))))

(def SourceConn
  source/SourceConn)

(def ^:private explain-writeback (m/explainer WritebackAllowlist))

(defn- source-error
  [{:keys [writeback]}]
  (let [why (when (some? writeback) (some-> (explain-writeback writeback) me/humanize))]
    (cond-> ":access must be nil/:read-only/:read-write, and a :read-write source needs an allowlist of what it may write: a well-formed :writeback (tables) or :webhooks (calls)"
      why (str " — :writeback by table: " (pr-str why)))))

(def Source
  [:and SourceConn
   [:fn {:error/fn (fn [{:keys [value]} _] (source-error value))}
    (fn [{:keys [access writeback webhooks]}]
      (and (contains? #{nil :read-only :read-write} access)
           (if (= :read-write access)
             (if (some? writeback)
               (writeback-valid? writeback)
               (and (map? webhooks) (seq webhooks)))
             (or (nil? writeback) (writeback-valid? writeback)))))]])

(def SourceEntity
  [:and Source
   [:map {:closed false} [:source-name keyword?]]
   [:fn
    {:error/message
     ":id must be a source RID; :markings/:export-markings/:description/:agent malformed"}
    (fn [{:keys [id description markings export-markings agent]}]
      (and (rid/well-formed-for? :sources id)
           (or (nil? agent) (and (keyword? agent) (re-matches #"[a-z0-9-]+" (name agent))))
           (or (nil? description) (string? description))
           (or (nil? markings) (and (sequential? markings) (every? keyword? markings)))
           (or (nil? export-markings)
               (and (sequential? export-markings) (every? keyword? export-markings)))))]])

(def transform-valid?  (m/validator Transform))
(def source-valid?     (m/validator Source))
(def object-set-valid? (m/validator QueryType))

(def ^:private authored-schemas
  {:object-types      ObjectType
   :link-types        ConnectionType
   :interfaces        InterfaceType
   :shared-properties SharedProperty
   :functions         Function
   :object-sets       QueryType
   :sources           SourceEntity
   :actions           Action
   :transforms        Transform})

(def schemas
  (into {} (map (fn [{:keys [section] :as row}]
                  [section (or (authored-schemas section) (canonical-schema row))]))
        ents/entities))

(def validators (update-vals schemas m/validator))

(def ^:private compiled-authored
  (delay (update-vals authored-schemas m/schema)))

(defn explain-entity [class v]
  (when-let [s (get @compiled-authored class)]
    (when-not (m/validate s v)
      (me/humanize (m/explain s v)))))
