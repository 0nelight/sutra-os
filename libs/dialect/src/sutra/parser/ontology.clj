(ns sutra.parser.ontology
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.parser.spec :as spec]
            [sutra.types.entities :as ents]
            [sutra.types.export :as export]
            [sutra.types.file-sync :as file-sync]
            [sutra.types.granular :as granular]
            [sutra.types.languages :as langs]
            [sutra.types.query :as q]
            [sutra.types.value-types :as vt]
            [sutra.types.virtual :as virtual]
            [sutra.capps.derived :as cd]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [malli.core :as m]
            [sutra.types.api-name :as api-name]
            [sutra.parser.ent-errors :refer [ent-errors-params]]))

(set! *warn-on-reflection* true)

(def pred-class
  {keyword? :keyword simple-keyword? :keyword qualified-keyword? :keyword string? :string
   int? :int integer? :int pos-int? :int nat-int? :int double? :double number? :number
   boolean? :boolean nil? :nil map? :map vector? :vector sequential? :vector})

(def type-class
  {:keyword :keyword :simple-keyword :keyword :qualified-keyword :keyword :string :string
   :re :string :int :int :double :double :boolean :boolean :nil :nil :map :map :map-of :map
   :vector :vector :tuple :vector :sequential :vector :cat :vector :catn :vector :set :set})

(def class-phrase
  {:keyword "a keyword" :string "a string" :int "an integer" :double "a decimal number"
   :number "a number" :boolean "a boolean" :nil "nil" :map "a map" :vector "a vector"
   :set "a set"})

(defn literal-class [x]
  (cond (keyword? x) :keyword (string? x) :string (int? x) :int (double? x) :double
        (number? x) :number (boolean? x) :boolean (nil? x) :nil))

(defn form-children [form]
  (let [[_ & r] form] (if (map? (first r)) (rest r) r)))

(defn form-class [form]
  (cond
    (keyword? form) (type-class form)
    (fn? form)      (pred-class form)
    (vector? form)
    (let [cs (form-children form)
          one (fn [ks] (when (and (= 1 (count ks)) (some? (first ks))) (first ks)))]
      (case (first form)
        (:enum :=) (one (set (map literal-class cs)))
        :and       (form-class (first cs))
        :or        (one (set (map form-class cs)))
        (type-class (first form))))))

(defn value-class [present v]
  (let [c (or (literal-class v)
              (cond (map? v) :map (sequential? v) :vector (set? v) :set :else :other))]
    (if (and (#{:int :double} c) (not (present c)) (present :number)) :number c)))

(defn refuse [msg] [:fn {:error/message msg} (constantly false)])

(defn closed-map-entries [form]
  (let [m (if (= :and (first form)) (first (form-children form)) form)]
    (when (and (vector? m) (= :map (first m)) (:closed (second m)))
      (for [e (form-children m)]
        [(first e) (not (and (map? (second e)) (:optional (second e))))]))))

(defn key-discriminated [origs dones]
  (let [entries (mapv closed-map-entries origs)
        owners  (frequencies (mapcat #(distinct (map first %)) entries))
        own     (mapv (fn [es] (some (fn [[k req?]] (when (and req? (= 1 (owners k))) k)) es))
                      entries)
        table   (map vector own (range))]
    (when (and (every? seq entries) (every? some? own))
      (into [:multi {:dispatch (fn [v] (some (fn [[k i]] (when (contains? v k) i)) table))}]
            (conj (mapv vector (range) dones)
                  [::m/default (refuse (str "a map naming one of "
                                            (str/join ", " (map pr-str own))))])))))

(defn head-literals [form]
  (when (and (vector? form) (= :tuple (first form)))
    (let [h (first (form-children form))]
      (when (and (vector? h) (#{:= :enum} (first h))) (form-children h)))))

(defn head-discriminated [origs dones]
  (let [heads (mapv head-literals origs)]
    (when (and (every? seq heads) (apply distinct? (apply concat heads)))
      (into [:multi {:dispatch #(when (sequential? %) (first %))}]
            (conj (vec (for [[hs d] (map vector heads dones), h hs] [h d]))
                  [::m/default
                   (refuse (str "a vector headed by one of "
                                (str/join ", " (map pr-str (apply concat heads)))))])))))

(declare discriminate)

(defn- or-branches
  [form]
  (into []
        (mapcat (fn [c] (if (and (vector? c) (= :or (first c))) (or-branches c) [c])))
        (form-children form)))

(defn or->multi
  [form]
  (let [origs   (or-branches form)
        dones   (mapv discriminate origs)
        classes (mapv form-class origs)
        within  (fn [idx]
                  (let [os (mapv origs idx) ds (mapv dones idx)]
                    (if (= 1 (count idx))
                      (first ds)
                      (or (key-discriminated os ds) (head-discriminated os ds) (into [:or] ds)))))]
    (if (some nil? classes)
      (into [:or] dones)
      (let [groups (group-by #(nth classes %) (range (count origs)))]
        (if (= 1 (count groups))
          (within (val (first groups)))
          (let [present (set (keys groups))]
            (into [:multi {:dispatch #(value-class present %)}]
                  (conj (mapv (fn [[c idx]] [c (within idx)]) groups)
                        [::m/default
                         (refuse (str "should be "
                                      (str/join " or " (map class-phrase (keys groups)))))]))))))))

(defn discriminate
  [form]
  (cond
    (and (vector? form) (= :or (first form))) (or->multi form)
    (vector? form) (mapv discriminate form)
    (map? form)    (update-vals form discriminate)
    :else          form))

(defn compile-grammar [form] (m/schema (discriminate form)))

(def ScalarType
  (into [:enum] (sort vt/authorable-types)))

(def ^:private DecimalType
  [:and
   [:tuple [:= :decimal] [:int {:min 1 :max 38}] [:int {:min 0}]]
   [:fn
    {:error/message
     "[:decimal precision scale]: the scale is at most the precision (1–38) — or write :decimal, stored as DECIMAL(38,10)"}
    (fn [[_ p s]] (<= s p))]])

(def ^:private struct-field-types
  #{:boolean :byte :short :integer :long :float :double :decimal
    :string :text :keyword :uuid :date :timestamp :instant :geohash})

(def ^:private StructField
  [:multi {:dispatch (fn [x] (if (sequential? x) (first x) :scalar))}
   [:scalar (into [:enum {:error/message (str "a struct field is a scalar, one of "
                                              (str/join " " (sort struct-field-types))
                                              " — structs do not nest and hold no arrays")}]
                  (sort struct-field-types))]
   [:decimal DecimalType]
   [::m/default
    [:fn
     {:error/message "a struct field is a scalar or [:decimal precision scale] — structs do not nest and hold no arrays"}
     (constantly false)]]])

(def Vtype
  [:schema
   {:registry
    {::t    [:multi {:dispatch (fn [x] (if (sequential? x) (first x) :scalar))}
             [:scalar ScalarType]
             [:decimal DecimalType]
             [:vector [:tuple [:= :vector] :int]]
             [:array  [:tuple [:= :array] [:ref ::elem]]]
             [:struct [:tuple [:= :struct]
                       [:and [:map-of :keyword StructField]
                        [:fn {:error/message "a struct has 1 to 10 fields"}
                         #(<= 1 (count %) 10)]]]]
             [::m/default
              [:fn
               {:error/message "not a base type: a scalar keyword, [:decimal precision scale], [:vector n], [:array T] or [:struct {…}]"}
               (constantly false)]]]
     ::elem [:multi
             {:dispatch (fn [x] (if (and (sequential? x) (= :array (first x))) :array :other))}
             [:array [:fn {:error/message "an array of arrays — nested arrays are not supported"}
                      (constantly false)]]
             [:other [:ref ::t]]]}}
   ::t])

(defn type-form-or-name [form]
  [:multi {:dispatch #(if (sequential? %) :form :name)}
   [:form form]
   [:name :keyword]])

(def VtypeOrRef (type-form-or-name Vtype))

(def Visibility
  [:enum :prominent :normal :hidden])

(def Status
  [:enum :active :experimental :deprecated :example :endorsed])

(def ResourceStatus
  [:enum
   {:error/message (str ":endorsed is an OBJECT-TYPE status — a property, "
                        "link type, interface or action type takes "
                        ":active / :experimental / :deprecated / :example")}
   :active :experimental :deprecated :example])

(def ReconcileMode
  [:enum :replace :merge])

(def Deprecation
  [:map {:closed true}
   [:reason      {:optional true} :string]
   [:deadline    {:optional true} :string]
   [:replacement {:optional true} :keyword]])

(def RenderHint
  [:enum :searchable :sortable :selectable :low-cardinality
   :keywords :long-text :identifier :disable-formatting])

(def Constraints
  [:map {:closed true}
   [:one-of         {:optional true} [:vector :any]]
   [:display-names  {:optional true} [:map-of :any :string]]
   [:min            {:optional true} :any]
   [:max            {:optional true} :any]
   [:exclusive-min  {:optional true} :any]
   [:exclusive-max  {:optional true} :any]
   [:regex          {:optional true} :string]
   [:format         {:optional true} :keyword]
   [:min-length     {:optional true} :int]
   [:max-length     {:optional true} :int]
   [:min-size       {:optional true} :int]
   [:max-size       {:optional true} :int]
   [:unique         {:optional true} :boolean]
   [:case-sensitive {:optional true} :boolean]])

(def value-format-kinds
  [:currency :unit :percentage
   :date :datetime :datetime-long :datetime-short :iso-instant :relative :time
   :duration :id :rid])

(def ValueFormat
  [:map {:closed true}
   [:kind     {:optional true}
    (into [:enum {:error/message
                  (str "a :format :kind is one of " (pr-str value-format-kinds)
                       " — the temporal ones ARE the render styles (:date "
                       "\"Mi., 22. Juli 2020\", :datetime-short \"22.07.2020, 13:00\", "
                       ":datetime-long, :time, :relative \"vor 8 Minuten\", "
                       ":iso-instant). There is no format PATTERN: the property "
                       "names a style, the viewer's locale decides the words, "
                       "separators and 12/24-hour clock.")}]
          value-format-kinds)]
   [:currency {:optional true} :string]
   [:unit     {:optional true} :string]
   [:prefix   {:optional true} :string]
   [:suffix   {:optional true} :string]
   [:decimals {:optional true} :int]
   [:grouping {:optional true} :boolean]
   [:notation {:optional true}
    [:enum {:error/message ":notation is :compact (1,2 Mio.), :scientific or :engineering"}
     :compact :scientific :engineering]]
   [:timezone {:optional true}
    [:string {:error/message (str ":timezone is an IANA zone id (\"Europe/Vienna\") that "
                                  "pins a timestamp to ONE zone; omit it and the "
                                  "timestamp renders in the viewer's own zone")}]]])

(def ConditionalFormatRule
  [:map {:closed true}
   [:when       {:optional true} :any]
   [:color      {:optional true} :string]
   [:background {:optional true} :string]
   [:align      {:optional true} :keyword]
   [:icon       {:optional true} :string]])

(def RollupArity
  [:fn {:error/message (str ":sum/:max/:min/:avg need :of; :arg-max needs :by + :pick; "
                            ":one needs :pick only and takes no :where; :count needs none")}
   (fn [m] (nil? (cd/rollup-arity-error m)))])

(def Rollup
  [:and
   [:map {:closed true}
    [:over :keyword]
    [:agg  (into [:enum] cd/aggs)]
    [:of   {:optional true} :keyword]
    [:by   {:optional true} :keyword]
    [:pick {:optional true} :keyword]
    [:where {:optional true}
     [:fn {:error/message (str ":where is a sutra.types.query predicate-IR node over the "
                               "LINKED object — [:= :code \"A03\"], [:> :betrag 0], "
                               "[:nil? :erledigt-am true] (unset; false = set), "
                               "[:and …], [:or …]")}
      (fn [p] (and (vector? p) (q/valid-predicate? p)))]]]
   RollupArity])

(def Compute
  [:and
   [:map {:closed true}
    [:language    {:optional true} :keyword]
    [:expression  {:optional true} [:or :string [:vector :any]]]
    [:rollup      {:optional true} Rollup]
    [:materialize {:optional true} :any]
    [:code        {:optional true} :string]
    [:code-file   {:optional true} :string]
    [:inputs      {:optional true} [:vector :keyword]]]
   [:fn {:error/message
         (str ":materialize is retired — a derived property is calculated at "
              "runtime, always. The reader that needed a column no longer does: "
              "a transform's :object-type input now carries the derived "
              "properties. If this value genuinely belongs in a column, compute "
              "it in a transform and drop :derived — it is then an ordinary "
              "backed property, not a derived one.")}
    (fn [m] (not (contains? m :materialize)))]])

(def SeriesSpec
  [:map {:closed true}
   [:sync   :keyword]
   [:rollup {:optional true} :keyword]
   [:unit   {:optional true} :string]])

(def AttrLink
  [:map {:closed true}
   [:to           spec/RefId]
   [:as           :keyword]
   [:reverse      {:optional true} :keyword]
   [:name         {:optional true} :keyword]
   [:strict       {:optional true} :boolean]
   [:forward-name {:optional true} :string]
   [:reverse-name {:optional true} :string]
   [:description  {:optional true} :string]
   [:satisfies    {:optional true} [:vector [:map {:closed true}
                                             [:interface spec/RefId]
                                             [:constraint :keyword]]]]])

(def PropOpts
  [:map {:closed true}
   [:link               {:optional true} [:or AttrLink [:vector AttrLink]]]
   [:name               {:optional true} :string]
   [:description        {:optional true} :string]
   [:visibility         {:optional true} Visibility]
   [:status             {:optional true} ResourceStatus]
   [:deprecation        {:optional true} Deprecation]
   [:required           {:optional true} :boolean]
   [:edit-only          {:optional true} :boolean]
   [:render-hints       {:optional true} [:vector RenderHint]]
   [:constraints        {:optional true} Constraints]
   [:value-type         {:optional true} :keyword]
   [:format             {:optional true} ValueFormat]
   [:conditional-format {:optional true} [:vector ConditionalFormatRule]]
   [:markings           {:optional true} [:vector :keyword]]
   [:media-set          {:optional true} :keyword]
   [:derived            {:optional true} Compute]
   [:fk                 {:optional true} spec/RefId]
   [:series             {:optional true} SeriesSpec]
   [:rationale          {:optional true} :string]
   [:project            {:optional true} :keyword]
   [:doc                {:optional true} [:map-of :keyword :any]]
   [:type-classes       {:optional true} [:map-of :keyword :any]]])

(def ApiName
  [:and :keyword
   [:fn {:error/fn (fn [{:keys [value]} _] (api-name/problem value))}
    api-name/valid?]])

(defn section-key
  [author]
  (if (:api-name (ents/by-section (ents/author->section author))) ApiName :keyword))

(def AttrItem
  [:multi {:dispatch (fn [x] (if (sequential? x) (count x) :ref))}
   [:ref :keyword]
   [2 [:tuple ApiName VtypeOrRef]]
   [3 [:tuple ApiName VtypeOrRef PropOpts]]
   [::m/default
    [:fn {:error/message "an attribute is `:shared-prop-ref`, `[name type]` or `[name type opts]`"}
     (constantly false)]]])

(def AttrList [:vector AttrItem])

(def InputSpec
  [:map {:closed true}
   [:kind        :keyword]
   [:datasource  {:optional true} :keyword]
   [:transform   {:optional true} spec/RefId]
   [:object-type {:optional true} spec/RefId]
   [:dataset     {:optional true} spec/RefId]
   [:media-set {:optional true} :keyword]
   [:table       {:optional true} :string]
   [:query       {:optional true} :string]
   [:source      {:optional true} :keyword]
   [:model       {:optional true} :string]
   [:base-url    {:optional true} :string]
   [:type        {:optional true} spec/ValueType]
   [:checks      {:optional true} [:vector :map]]
   [:stop-propagating {:optional true} [:vector :keyword]]
   [:stop-requiring   {:optional true} [:vector :keyword]]])

(def Inputs [:map-of :keyword InputSpec])

(def PolySource
  [:map {:closed true}
   [:from-type       spec/RefId]
   [:to-type         {:optional true} spec/RefId]
   [:language        {:optional true} :keyword]
   [:from-key-column {:optional true} :keyword]
   [:to-key-column   {:optional true} :keyword]
   [:match-column    {:optional true} :keyword]
   [:shared-property {:optional true} :keyword]
   [:inputs          {:optional true} Inputs]
   [:query           {:optional true} :string]
   [:code            {:optional true} :string]
   [:code-file       {:optional true} :string]
   [:datasource      {:optional true} :keyword]
   [:table           {:optional true} :string]
   [:transforms      {:optional true} [:vector :any]]])

(def IncrementalPull
  [:map {:closed true}
   [:column  :keyword]
   [:initial {:optional true} [:or :string :int :double]]])

(def ExtSource
  [:map {:closed true}
   [:incremental     {:optional true} IncrementalPull]
   [:strategy        {:optional true} :keyword]
   [:datasource      {:optional true} :keyword]
   [:dataset         {:optional true} :keyword]
   [:table           {:optional true} :string]
   [:filters         {:optional true} file-sync/Filters]
   [:query           {:optional true} :string]
   [:language        {:optional true} :keyword]
   [:code            {:optional true} :string]
   [:code-file       {:optional true} :string]
   [:inputs          {:optional true} Inputs]
   [:primary-key     {:optional true} :keyword]
   [:column-map      {:optional true} [:map-of :keyword :keyword]]
   [:from-key-column {:optional true} :keyword]
   [:to-key-column   {:optional true} :keyword]
   [:match-column    {:optional true} :keyword]
   [:shared-property {:optional true} :keyword]
   [:fk-side         {:optional true} :keyword]
   [:fk-attribute    {:optional true} :keyword]
   [:on-null         {:optional true} :keyword]
   [:on-unresolved   {:optional true} :keyword]
   [:on-duplicate    {:optional true} :keyword]
   [:transforms      {:optional true} [:vector :any]]
   [:sources         {:optional true} [:vector PolySource]]])

(def Backing
  [:map {:closed true}
   [:dataset     :keyword]
   [:primary-key {:optional true} :keyword]
   [:column-map  {:optional true} [:map-of :keyword :keyword]]
   [:replaces    {:optional true} :keyword]
   [:direct           {:optional true} :boolean]
   [:source-timestamp {:optional true} :keyword]
   [:consistency      {:optional true} [:enum :exactly-once :at-least-once]]])

(def SemanticSearch
  [:map {:closed true}
   [:property :keyword]
   [:model    {:optional true} :string]
   [:k        {:optional true} :int]
   [:metric   {:optional true} [:enum :cosine :dot :l2]]])

(def ObjectChecks
  [:vector [:map {:closed true}
            [:name     :string]
            [:expr     :any]
            [:inputs   {:optional true} [:vector :keyword]]
            [:on-error {:optional true} [:enum :warn :fail]]]])

(def EditConflict
  [:map {:closed true}
   [:strategy       [:enum :apply-user-edits :apply-most-recent]]
   [:timestamp-attr {:optional true} :keyword]])

(def LinkConstraint
  [:map {:closed true}
   [:link-name      :keyword]
   [:cardinality    {:optional true} spec/ConstraintCardinality]
   [:to-object-type {:optional true} spec/RefId]
   [:to-interface   {:optional true} spec/RefId]
   [:required       {:optional true} :boolean]])

(def InterfaceLinkMembership
  [:map {:closed true}
   [:interface  spec/RefId]
   [:constraint :keyword]])

(def JoinSpec
  [:and
   [:map {:closed true}
    [:on       {:optional true} [:tuple :keyword :keyword]]
    [:from-key {:optional true} :keyword]
    [:to-key   {:optional true} :keyword]
    [:from-pk  :keyword]
    [:to-pk    :keyword]
    [:where    {:optional true} [:enum :both-non-empty :from-non-empty :to-non-empty :none]]]
   [:fn {:error/message "a :join needs exactly one of :on OR (:from-key + :to-key)"}
    (fn [{:keys [on from-key to-key]}]
      (or (and on (not from-key) (not to-key))
          (and (not on) from-key to-key)))]])

(def FkSpec
  [:or :keyword
   [:map {:closed true}
    [:attribute     :keyword]
    [:side          {:optional true} [:enum :from :to]]
    [:on-null       {:optional true} [:enum :fail :skip]]
    [:on-unresolved {:optional true} [:enum :fail :skip]]
    [:on-duplicate  {:optional true} [:enum :fail :skip]]]])

(def FkResolveSpec
  [:and
   [:map {:closed true}
    [:key       :keyword]
    [:as        :keyword]
    [:language  {:optional true} (into [:enum] langs/compute-language-names)]
    [:query     {:optional true} :string]
    [:code      {:optional true} :string]
    [:code-file {:optional true} :string]
    [:function  {:optional true} :keyword]
    [:fk-type  {:optional true} VtypeOrRef]
    [:side     {:optional true} [:enum :from :to]]
    [:on-null       {:optional true} [:enum :fail :skip]]
    [:on-unresolved {:optional true} [:enum :fail :skip]]
    [:on-duplicate  {:optional true} [:enum :fail :skip]]]
   [:fn
    {:error/message "a :fk-resolve needs exactly ONE body: :query (:sql), :code/:code-file (:python/:clojure), or :function (a :python function ref)"}
    (fn [{:keys [query code code-file function]}]
      (= 1 (count (filter some? [query (or code code-file) function]))))]])

(def Step
  [:map [:op :keyword]])

(def OptionsFromMap
  [:map {:closed true}
   [:object-set  {:optional true} :keyword]
   [:object-type {:optional true} :keyword]
   [:property    {:optional true} :keyword]
   [:label       {:optional true} :keyword]
   [:args        {:optional true} [:map-of :keyword :keyword]]])

(def OptionsFrom
  [:and
   OptionsFromMap
   [:fn {:error/message (str ":options-from needs exactly one source — :object-set (a saved set)"
                             " or :object-type (every object of that type)")}
    (fn [{:keys [object-set object-type]}]
      (= 1 (count (remove nil? [object-set object-type]))))]])

(def ParamOverride
  [:map {:closed true}
   [:when        :any]
   [:exposure    {:optional true} :keyword]
   [:required    {:optional true} :boolean]
   [:default     {:optional true} :any]
   [:editable    {:optional true} :boolean]
   [:constraints {:optional true} Constraints]])

(def ParamRenderHint
  [:enum :long-text :rtf :hh-mm :prominent :compact :radio :segmented])

(def PropertyDefault
  [:map {:closed true}
   [:from     {:optional true} :keyword]
   [:property :keyword]])

(def ParamSpec
  [:map {:closed true}
   [:type         {:optional true} :some]
   [:name         {:optional true} :string]
   [:description  {:optional true} :string]
   [:required     {:optional true} :boolean]
   [:default      {:optional true}
    [:multi {:dispatch #(if (and (map? %) (contains? % :property)) :property :value)}
     [:property PropertyDefault]
     [:value :any]]]
   [:multiple     {:optional true} :boolean]
   [:exposure     {:optional true} :keyword]
   [:editable     {:optional true} :boolean]
   [:render-hints {:optional true} [:vector ParamRenderHint]]
   [:prefill      {:optional true} [:enum :current-object :current-user :uuid :now]]
   [:compute      {:optional true} [:vector :any]]
   [:constraints  {:optional true} Constraints]
   [:options-from {:optional true} OptionsFrom]
   [:overrides    {:optional true} [:vector ParamOverride]]])

(def ActionForm
  [:map {:closed true}
   [:sections [:vector [:map {:closed true}
                        [:title       {:optional true} :string]
                        [:description {:optional true} :string]
                        [:columns     {:optional true} [:enum 1 2]]
                        [:collapsible {:optional true} :boolean]
                        [:collapsed   {:optional true} :boolean]
                        [:layout      {:optional true} [:enum :totals]]
                        [:overrides   {:optional true}
                         [:vector [:map {:closed true}
                                   [:when     :any]
                                   [:exposure [:enum :hidden]]]]]
                        [:params      [:vector :keyword]]]]]])

(def LifecycleState [:or :keyword :string])

(def Lifecycle
  [:map {:closed true}
   [:property    :keyword]
   [:states      [:vector {:min 2} LifecycleState]]
   [:transitions {:optional true}
    [:vector [:map {:closed true}
              [:action :keyword]
              [:from   [:vector {:min 1} LifecycleState]]
              [:to     LifecycleState]]]]])

(def SubmissionCriterion
  [:map {:closed false}
   [:test    :map]
   [:pred    [:vector :any]]
   [:message {:optional true} :string]])

(def named-types
  {:ent/status          Status
   :ent/resource-status ResourceStatus
   :ent/deprecation     Deprecation
   :ent/visibility      Visibility
   :ent/constraints     Constraints
   :ent/value-format    ValueFormat
   :ent/render-hint     RenderHint
   :ent/vtype           Vtype
   :ent/ref-id          spec/RefId
   :ent/api-name        ApiName
   :ent/ext-source      ExtSource
   :ent/virtual         virtual/Virtual
   :ent/attr-list       AttrList
   :ent/backing         Backing
   :ent/semantic-search SemanticSearch
   :ent/object-checks   ObjectChecks
   :ent/edit-conflict   EditConflict
   :ent/security        granular/Security
   :ent/restricted-view granular/RestrictedView
   :ent/export-destination export/Destination
   :ent/export-mode     export/Mode
   :ent/media-set-sync  [:map {:closed true}
                         [:filters {:optional true} file-sync/SyncFilters]]
   :ent/link-constraint LinkConstraint
   :ent/interface-link-membership InterfaceLinkMembership
   :ent/step            Step
   :ent/param-spec      ParamSpec
   :ent/action-form     ActionForm
   :ent/lifecycle       Lifecycle
   :ent/submission-criterion SubmissionCriterion
   :ent/on-branch       spec/OnBranch})

(defn entry
  ([section] (entry section nil))
  ([section sugar]
   (into (into [:map {:closed true}]
               (for [[k {:keys [type required author-form]}] (get ents/fields section)
                     :when (and type (nil? author-form))]
                 [k {:optional (not required)} (walk/postwalk #(get named-types % %) type)]))
         sugar)))

(def ObjectEntry
  (entry :object-types
         [[:name             {:optional true} :string]
          [:groups           {:optional true} [:vector :keyword]]
          [:doc              {:optional true} [:map-of :keyword :any]]
          [:external-sources {:optional true} [:vector ExtSource]]
          [:reconcile-mode   {:optional true} ReconcileMode]]))

(def LinkOpts
  (entry :link-types
         [[:name            {:optional true} :string]
          [:visibility      {:optional true} Visibility]
          [:fk              {:optional true} FkSpec]
          [:join            {:optional true} JoinSpec]
          [:fk-resolve      {:optional true} FkResolveSpec]
          [:doc             {:optional true} [:map-of :keyword :any]]
          [:reconcile-mode  {:optional true} ReconcileMode]]))

(def LinkEntry
  [:multi {:dispatch count}
   [4 [:tuple (section-key :links) spec/RefId spec/RefId spec/Cardinality]]
   [5 [:tuple (section-key :links) spec/RefId spec/RefId spec/Cardinality LinkOpts]]
   [::m/default
    [:fn
     {:error/message
      "a link is `[name from to cardinality]` or `[name from to cardinality {opts}]`"}
     (constantly false)]]])

(def InterfaceEntry
  (entry :interfaces
         [[:reconcile-mode {:optional true} ReconcileMode]]))

(def ActionEntry
  (entry :actions
         [[:name          {:optional true} :keyword]
          [:action-log    {:optional true}
           [:or :boolean [:map {:closed true}
                          [:object-type {:optional true} :keyword]
                          [:project     {:optional true} :keyword]
                          [:status      {:optional true} :keyword]
                          [:edited      {:optional true} :boolean]
                          [:params      {:optional true} [:vector :keyword]]
                          [:values      {:optional true} [:map-of :keyword :any]]]]]
          [:notifications {:optional true} [:vector :any]]
          [:webhooks      {:optional true} [:vector :any]]]))

(def ObjectSetEntry
  (entry :object-sets [[:name {:optional true} :keyword]]))

(def SharedPropertyMap (entry :shared-properties))
(def SharedProperty
  [:and
   SharedPropertyMap
   [:fn {:error/message (str "a shared property needs a base: `:type :string`, or"
                             " `:value-type :iban-nr` (the type is then the value type's base)")}
    (fn [m] (or (some? (:type m)) (some? (:value-type m))))]])
(def ValueTypeEntry (entry :value-types))

(def OntologySchema
  [:map {:closed true}
   [:value-types       {:optional true} [:map-of (section-key :value-types) ValueTypeEntry]]
   [:shared-properties {:optional true} [:map-of (section-key :shared-properties) SharedProperty]]
   [:objects           {:optional true} [:map-of (section-key :objects) ObjectEntry]]
   [:links             {:optional true} [:vector LinkEntry]]
   [:interfaces        {:optional true} [:map-of (section-key :interfaces) InterfaceEntry]]
   [:actions           {:optional true} [:map-of (section-key :actions) ActionEntry]]
   [:object-sets       {:optional true} [:map-of :keyword ObjectSetEntry]]])

(def ^:private OntologySchema* (delay (compile-grammar OntologySchema)))

(defn validate!
  [schema source]
  (when-let [errs (some-> (m/explain @OntologySchema* schema)
                          ((requiring-resolve 'malli.error/humanize)))]
    (anomaly/coded! :refusal.parser/invalid-text :incorrect
                    "{what} ({source}): {n} problem(s) — each is `path — reason`:\n{lines}\n{hint}"
                    {:source source :errors {:schema errs}}
                    (ent-errors-params "Invalid ontology" {:schema errs})))
  schema)
