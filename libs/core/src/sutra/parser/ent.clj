(ns sutra.parser.ent
  (:require [sutra.kit.refusal :as refusal]
            [sutra.parser.ontology :as onto
             :refer [ActionEntry Deprecation InputSpec Inputs InterfaceEntry LinkEntry
                     ObjectEntry ObjectSetEntry OptionsFrom PolySource PropOpts ResourceStatus
                     RollupArity ScalarType SharedProperty ValueTypeEntry compile-grammar
                     section-key type-form-or-name]]
            [sutra.parser.ent-errors :refer [ent-errors-params]]
            [sutra.parser.ent-view
             :refer [LiveSpec ViewFile* body-vars-problem data-node entv->schema
                     reference-message rollup-entries view-factors]]
            [sutra.parser.sugar :as sugar]
            [sutra.parser.spec :as spec]
            [sutra.parser.operational :as op]
            [sutra.types.entities :as ents]
            [sutra.types.languages :as langs]
            [sutra.types.paths :as paths]
            [sutra.types.redirect-uri :as redirect-uri]
            [clojure.walk :as walk]
            [sutra.parser.resolve :as res]
            [sutra.capps.derived :as cd]
            [sutra.capps.catalog :as cat]
            [sutra.capps.layout :as lay]
            [sutra.capps.refs :as refs]
            [sutra.capps.variables :as vars]
            [sutra.types.stream :as strm]
            [sutra.types.table :as tbl]
            [sutra.types.template :as tmpl]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]
            [malli.util :as mu]))

(set! *warn-on-reflection* true)

(def ^:private FieldVtype
  [:schema {:registry
            {::t [:or
                  ScalarType
                  [:tuple [:= :decimal] :int :int]
                  [:tuple [:= :vector]  :int]
                  [:tuple [:= :array]   [:ref ::t]]
                  [:tuple [:= :struct]  [:map-of :keyword [:ref ::t]]]]}}
   ::t])

(def ^:private FieldVtypeOrRef (type-form-or-name FieldVtype))

(def ^:private ColumnItem
  [:multi {:dispatch (fn [x] (if (sequential? x) (count x) :ref))}
   [:ref :keyword]
   [2 [:tuple :keyword FieldVtypeOrRef]]
   [3 [:tuple :keyword FieldVtypeOrRef PropOpts]]
   [::m/default [:fn {:error/message "a column is `[name type]` or `[name type opts]`"}
                 (constantly false)]]])

(def ^:private ColumnList [:vector ColumnItem])

(def ^:private Stream
  [:map {:closed true}
   [:partitions       {:optional true} [:int {:min 1 :max 16}]]
   [:partition-keys   {:optional true} [:vector :keyword]]
   [:primary-key      {:optional true} [:map {:closed true}
                                        [:columns [:vector :keyword]]
                                        [:deletion-column {:optional true} :keyword]]]
   [:stream-type      {:optional true} (into [:enum] (sort (keys strm/stream-types)))]
   [:semantics        {:optional true} (into [:enum] (sort strm/semantics))]
   [:compressed?      {:optional true} :boolean]
   [:retention-ms     {:optional true} [:int {:min 0}]]
   [:archive-every-ms {:optional true} [:int {:min 1000}]]
   [:sync             {:optional true}
    [:and
     [:map {:closed true}
      [:source :keyword]
      [:query  {:optional true} :string]
      [:cursor {:optional true} :keyword]
      [:every-ms {:optional true} [:int {:min 1000}]]
      [:cdc    {:optional true} [:map {:closed true}
                                 [:table :string]
                                 [:schema {:optional true} :string]
                                 [:capture-instance {:optional true} :string]]]]
     [:fn {:error/message "a :sync reads either :cdc or a :query with its :cursor"}
      (fn [{:keys [cdc query cursor]}]
        (if cdc (not (or query cursor)) (and query cursor)))]]]
   [:shape            {:optional true} :keyword]
   [:derive           {:optional true}
    [:map {:closed true}
     [:from             :keyword]
     [:function         :keyword]
     [:stop-propagating {:optional true} [:vector :keyword]]
     [:stop-requiring   {:optional true} [:vector :keyword]]]]])

(def ^:private IncrementalOpts
  [:map {:closed true}
   [:read-mode       {:optional true} [:enum :added :current :previous]]
   [:write-mode      {:optional true} [:enum :modify :replace]]
   [:static-inputs {:optional true} [:vector :keyword]]
   [:strict-append   {:optional true} :boolean]
   [:require-incremental {:optional true} :boolean]
   [:allow-retention {:optional true} :boolean]])

(def ^:private TransformParam
  [:map {:closed true}
   [:type        [:enum :string :number :boolean]]
   [:default     {:optional true} :any]
   [:description {:optional true} :string]])

(def ^:private TransformOps
  [:vector [:map {:closed false} [:op :keyword]]])

(def ^:private TransformTest
  [:map {:closed true}
   [:name        :string]
   [:inputs      {:optional true} [:map-of :keyword [:vector :map]]]
   [:expect      [:vector :map]]
   [:output      {:optional true} :keyword]
   [:description {:optional true} :string]])

(def ^:private DoneKey
  [:map {:closed true} [:input :keyword] [:key :keyword]])

(def ^:private TransformResources
  [:map {:closed true}
   [:cpu-cores {:optional true} [:int {:min 1 :max 8}]]
   [:memory-gb {:optional true} [:int {:min 1 :max 64}]]])

(def ^:private PromptKey
  [:map {:closed true}
   [:input   :keyword]
   [:columns [:vector :keyword]]
   [:column  {:optional true} :keyword]])

(def ^:private GraphPayload
  [:map {:closed true}
   [:zoom {:optional true} number?]
   [:pan  {:optional true} [:vector number?]]
   [:arrangement {:optional true} :keyword]])

(def ^:private GraphScope
  [:map {:closed true}
   [:datasources  {:optional true} [:set :keyword]]
   [:datasets     {:optional true} [:set :keyword]]
   [:transforms   {:optional true} [:set :keyword]]
   [:object-types {:optional true} [:set :keyword]]
   [:link-types   {:optional true} [:set :keyword]]
   [:actions      {:optional true} [:set :keyword]]
   [:applications {:optional true} [:set :keyword]]
   [:writebacks   {:optional true} [:set :keyword]]])

(def ^:private OAuthClient
  [:map {:closed true}
   [:grant-types
    {:optional true}
    [:vector
     [:enum {:error/message
             (str "a grant type is :authorization-code (a person signs in and the "
                  "token carries THEIR permissions) or :client-credentials (a "
                  "generated service user, carrying the APPLICATION's)")}
      :authorization-code :client-credentials]]]
   [:redirect-uris
    {:optional true}
    [:vector
     [:and :string
      [:fn {:error/message redirect-uri/message} redirect-uri/valid?]]]]
   [:token-ttl {:optional true} [:int {:min 60 :max 86400}]]])

(def ^:private TemplateParam
  [:and
   [:map {:closed true}
    [:type [:enum :string :number :boolean :date :timestamp :object :object-set :graph]]
    [:object-type {:optional true} [:or spec/RefId [:vector {:min 1} spec/RefId]]]
    [:object-set  {:optional true} :keyword]
    [:edge        {:optional true} [:or :keyword [:vector {:min 1} :keyword]]]
    [:layout      {:optional true} :keyword]
    [:richtung    {:optional true} :keyword]
    [:search-around {:optional true} [:or :keyword [:vector {:min 1} :keyword]]]
    [:via         {:optional true} :any]
    [:of          {:optional true} :keyword]
    [:required    {:optional true} :boolean]
    [:default     {:optional true} :any]
    [:preview     {:optional true} :any]
    [:options-from {:optional true} OptionsFrom]
    [:description {:optional true} :string]]
   [:fn {:error/message (str "an :object / :object-set / :graph parameter must name its"
                             " :object-type — without it nothing can say which properties the"
                             " body may reach, and a typo becomes an empty render instead of"
                             " an error")}
    (fn [{:keys [type object-type]}]
      (or (not (contains? #{:object :object-set :graph} type)) (some? object-type)))]
   [:fn {:error/message ":object-type belongs to an :object, :object-set or :graph parameter"}
    (fn [{:keys [type object-type]}]
      (or (contains? #{:object :object-set :graph} type) (nil? object-type)))]
   [:fn {:error/message ":object-set belongs to an :object or :object-set parameter"}
    (fn [{:keys [type object-set]}]
      (or (contains? #{:object :object-set} type) (nil? object-set)))]
   [:fn {:error/message (str ":edge belongs to a :graph parameter and names the link SIDE the"
                             " edges run along, as seen from the :object-type")}
    (fn [{:keys [type edge]}] (or (= :graph type) (nil? edge)))]
   [:fn {:error/message (str "a :graph parameter names the link side its edges run along"
                             " (:edge) — objects without a link between them are a list, and a"
                             " list is an :object-set")}
    (fn [{:keys [type edge]}] (or (not= :graph type) (some? edge)))]
   [:fn {:error/message (str ":layout / :richtung belong to a :graph parameter — they say how"
                             " it is arranged, and nothing else is arranged")}
    (fn [{:keys [type layout richtung]}]
      (or (= :graph type) (and (nil? layout) (nil? richtung))))]
   [:fn {:error/message (str "only a :graph parameter may name SEVERAL :object-type /"
                             " :edge — an :object is one object and an :object-set one"
                             " type's objects; a CHAIN of types is a drawing")}
    (fn [{:keys [type object-type edge]}]
      (or (= :graph type) (and (not (sequential? object-type)) (not (sequential? edge)))))]
   [:fn {:error/message (str "a :graph parameter takes no :required, :default or :preview —"
                             " it is READ from the ontology, never passed in, so there is"
                             " nothing to demand of a caller and nothing to stand in for it")}
    (fn [{:keys [type required default preview]}]
      (or (not= :graph type)
          (and (nil? required) (nil? default) (nil? preview))))]
   [:fn {:error/message (str ":via is now :search-around and names link SIDES — their API"
                             " names as seen from the reached type")}
    (fn [p] (not (contains? p :via)))]
   [:fn {:error/message (str ":search-around and :of travel together — :search-around names the"
                             " link sides to walk and :of the :object parameter to walk them from."
                             " One without the other says where to go and not from where, or the"
                             " reverse.")}
    (fn [{:keys [search-around of]}] (= (some? search-around) (some? of)))]
   [:fn {:error/message (str ":search-around/:of belong to an :object-set parameter. A single"
                             " :object is named by its id; a SET that follows a link is the thing"
                             " that cannot be.")}
    (fn [{:keys [type search-around]}] (or (nil? search-around) (= :object-set type)))]])

(def ^:private TemplatePage
  [:map {:closed true}
   [:size        {:optional true} [:enum :a1 :a2 :a3 :a4 :a5 :letter :legal :tabloid]]
   [:orientation {:optional true} [:enum :portrait :landscape]]
   [:margin      {:optional true} :string]
   [:header      {:optional true} :string]
   [:footer      {:optional true} :string]
   [:css         {:optional true} :string]
   [:base-css    {:optional true} :boolean]])

(def ^:private TemplateLimits
  [:map {:closed true}
   [:max-rows    {:optional true} [:int {:min 1 :max 10000}]]
   [:max-objects {:optional true} [:int {:min 1 :max 10000}]]
   [:seconds     {:optional true} [:int {:min 1 :max 120}]]])

(def ^:private AppScope
  [:map {:closed true}
   [:object-types {:optional true} [:vector :keyword]]
   [:actions      {:optional true} [:vector :keyword]]
   [:functions    {:optional true} [:vector :keyword]]
   [:operations
    {:optional true}
    [:vector (into [:enum] paths/api-namespaces)]]])

(def ^:private AppSdk
  [:map {:closed true}
   [:namespace {:optional true} :string]])

(def ^:private AppPermissions
  [:map {:closed true}
   [:roles {:optional true} [:vector :keyword]]])

(def ^:private AppHosting
  [:map {:closed true}
   [:subdomain {:optional true} :string]
   [:root      {:optional true} :string]
   [:csp       {:optional true}
    [:map {:closed true}
     [:script-src  {:optional true} [:vector :string]]
     [:style-src   {:optional true} [:vector :string]]
     [:connect-src {:optional true} [:vector :string]]
     [:img-src     {:optional true} [:vector :string]]
     [:font-src    {:optional true} [:vector :string]]]]])

(def ^:private WorkspaceItem
  [:map {:closed true}
   [:kind        [:enum :app :object-type :object :object-set :link]]
   [:app         {:optional true} spec/RefId]
   [:object-type {:optional true} spec/RefId]
   [:object      {:optional true} :string]
   [:object-set  {:optional true} spec/RefId]
   [:url         {:optional true} :string]
   [:label       {:optional true} :string]
   [:icon        {:optional true} :string]])

(defn- item-names-its-kind?
  [{:keys [kind app object-type object object-set url]}]
  (= (if (= :object kind) #{:object-type :object} #{kind})
     (cond-> #{}
       (some? app)         (conj :app)
       (some? object-type) (conj :object-type)
       (some? object)      (conj :object)
       (some? object-set)  (conj :object-set)
       (some? url)         (conj :link))))

(def ^:private WorkspacePanel
  [:and
   [:map {:closed true}
    [:column      [:enum :a :b :c]]
    [:title       {:optional true} :string]
    [:description {:optional true} :string]
    [:display     {:optional true} [:enum :list :cards]]
    [:widget      {:optional true} [:enum :favorites :object-sets]]
    [:items       {:optional true} [:vector WorkspaceItem]]]
   [:fn {:error/message
         (str "a home widget is either curated (:items) or one of the default widgets "
              "(:widget :favorites | :object-sets), which fill themselves per viewer — "
              "not both, and not neither.")}
    (fn [{:keys [widget items]}] (not= (some? widget) (some? items)))]
   [:fn {:error/message
         (str "a home item names the target its :kind promises — :app needs "
              ":app, :object-type needs :object-type, :object needs :object-type and "
              ":object (its primary key), :object-set needs :object-set, :link needs "
              ":url — and nothing else. An item that carries two targets has no defined "
              "click.")}
    (fn [p] (every? item-names-its-kind? (:items p)))]])

(def ^:private AgentTool
  [:map {:closed true}
   [:kind         [:enum :action :function :object-query :ontology :calculator]]
   [:action       {:optional true} :keyword]
   [:function     {:optional true} :keyword]
   [:object-type  {:optional true} spec/RefId]
   [:object-types {:optional true} [:vector spec/RefId]]
   [:limit        {:optional true} pos-int?]
   [:description  {:optional true} :string]])

(def ^:private AgentRetrieval
  [:map {:closed true}
   [:kind        [:enum :semantic-search]]
   [:object-type spec/RefId]])

(def ^:private named-types
  (merge onto/named-types
         {:ent/agent-tool      AgentTool
          :ent/agent-retrieval AgentRetrieval
          :ent/stream          Stream
          :ent/cardinality     spec/Cardinality
          :ent/inputs          Inputs
          :ent/input-spec      InputSpec
          :ent/ops             TransformOps
          :ent/incremental-opts IncrementalOpts
          :ent/done-key        DoneKey
          :ent/prompt-key      PromptKey
          :ent/transform-resources TransformResources
          :ent/poly-source     PolySource
          :ent/column-list     ColumnList
          :ent/transform-param TransformParam
          :ent/transform-test  TransformTest
          :ent/graph-payload   GraphPayload
          :ent/graph-scope     GraphScope
          :ent/oauth-client    OAuthClient
          :ent/app-scope       AppScope
          :ent/app-sdk         AppSdk
          :ent/app-permissions AppPermissions
          :ent/app-hosting     AppHosting
          :ent/workspace-panel WorkspacePanel
          :ent/template-param  TemplateParam
          :ent/template-page   TemplatePage
          :ent/template-limits TemplateLimits}))

(defn- entry
  ([section] (entry section nil))
  ([section sugar]
   (into (into [:map {:closed true}]
               (for [[k {:keys [type required author-form]}] (get ents/fields section)
                     :when (and type (nil? author-form))]
                 [k {:optional (not required)} (walk/postwalk #(get named-types % %) type)]))
         sugar)))

(def ^:private TransformEntry
  (entry :transforms
         [[:code-file       {:optional true} :string]
          [:modules         {:optional true} [:map-of :string :string]]
          [:match-column    {:optional true} :keyword]
          [:shared-property {:optional true} :keyword]
          [:doc             {:optional true} [:map-of :keyword :any]]]))

(def ^:private BuildTransformEntry
  [:and TransformEntry
   [:fn {:error/fn (fn [{:keys [value]} _] (spec/transform-error value))}
    (fn [t]
      (or (not (#{:compute/sql :source} (spec/transform-kind t)))
          (some? (:board t))
          (= 1 (spec/output-mode-count t))))]])

(def SourceEntry
  (entry :sources
         [[:secrets   {:optional true} [:map-of :keyword :string]]
          [:doc       {:optional true} [:map-of :keyword :any]]]))

(def ^:private EditFunctionEntry
  (entry :functions
         [[:name  {:optional true} :keyword]
          [:types {:optional true} [:map-of :keyword :any]]]))

(def ^:private FunctionEntry
  [:multi {:dispatch #(if (and (map? %) (ents/edit-function? %)) :edit :query)}
   [:edit EditFunctionEntry]
   [:query TransformEntry]])

(def ^:private UploadSpec
  [:map {:closed true}
   [:dataset      spec/RefId]
   [:media-column :keyword]
   [:build        {:optional true} [:vector :keyword]]
   [:fields       {:optional true} [:vector :keyword]]
   [:key-field    {:optional true} :keyword]
   [:name-field   {:optional true} :keyword]
   [:stamp-field  {:optional true} :keyword]
   [:user-field   {:optional true} :keyword]
   [:size-field   {:optional true} :keyword]
   [:mime-field   {:optional true} :keyword]
   [:on-upload    {:optional true}
    [:map {:closed true}
     [:action :keyword]
     [:params {:optional true} [:map-of :keyword :any]]]]])

(def ^:private ExportSpec
  [:map {:closed true}
   [:object-type      spec/RefId]
   [:key-field        :keyword]
   [:value-field      :keyword]
   [:period-fields    {:optional true} [:vector :keyword]]
   [:cumulative-field {:optional true} :keyword]
   [:template-file    {:optional true} :string]
   [:template-b64     {:optional true} :string]
   [:code-file        {:optional true} :string]
   [:code             {:optional true} :string]
   [:python-deps      {:optional true} [:vector :string]]
   [:filename         {:optional true} :string]
   [:profile          {:optional true} :keyword]])

(def ^:private DocumentsSpec
  [:vector spec/RefId])

(def ^:private ActionsSpec
  [:vector [:map {:closed true}
            [:action     spec/RefId]
            [:parameters {:optional true} [:vector :keyword]]]])

(def ^:private MathExpr
  [:schema {:registry
            {::math [:or number? :keyword
                     [:cat (into [:enum :get] (sort cd/math-ops))
                      [:* [:schema [:ref ::math]]]]]}}
   [:ref ::math]])

(def ^:private ModuleAggregation
  [:and
   (into [:map {:closed true}
          [:kind  {:optional true} [:= :aggregation]]
          [:label {:optional true} :string]]
         rollup-entries)
   RollupArity])

(def ^:private ModuleColumnMath
  [:map {:closed true}
   [:kind       {:optional true} [:= :column-math]]
   [:label      {:optional true} :string]
   [:expression MathExpr]])

(def ^:private ModuleDerivedProperty
  [:multi {:dispatch (fn [m] (or (cd/kind-of m) :unknown))}
   [:aggregation ModuleAggregation]
   [:column-math ModuleColumnMath]
   [:unknown [:fn {:error/message (str "neither kind: an :aggregation names :over + :agg, "
                                       "a :column-math carries :expression")}
              (constantly false)]]])

(def ^:private ModuleDerived
  [:and
   [:map-of spec/RefId [:map-of spec/RefId ModuleDerivedProperty]]
   [:fn {:error/fn
         (fn [{:keys [value]} _]
           (str/join "; " (for [{:keys [type name error]} (cd/explain value)]
                            (str (clojure.core/name type) "." (clojure.core/name name)
                                 " — " error))))}
    (fn [m] (empty? (cd/explain m)))]])

(def ^:private DataAppFileBase
  [:map {:closed true}
   [:name            spec/RefId]
   [:display-name    {:optional true} :string]
   [:description     {:optional true} :string]
   [:icon            {:optional true} :string]
   [:auto-publish    {:optional true} :boolean]
   [:promoted        {:optional true} :boolean]
   [:collection      {:optional true} :keyword]
   [:tags            {:optional true} [:vector :keyword]]
   [:owner           {:optional true} :string]
   [:thumbnail       {:optional true} :string]
   [:object-types    {:optional true} [:vector spec/RefId]]
   [:live            {:optional true} LiveSpec]
   [:aggregate-types {:optional true} [:vector spec/RefId]]
   [:functions       {:optional true} [:vector spec/RefId]]
   [:derived-properties {:optional true} ModuleDerived]
   [:upload          {:optional true} UploadSpec]
   [:export          {:optional true} ExportSpec]
   [:documents       {:optional true} DocumentsSpec]
   [:actions         {:optional true} ActionsSpec]
   [:project         {:optional true} :keyword]
   [:repository      {:optional true} :keyword]])

(defn- reference-problem
  [{:keys [layout]}]
  (when layout
    (reference-message (cat/reference-problems layout))))

(defn- app-vars-problem
  [{:keys [layout form-factor] :as app}]
  (body-vars-problem layout (when form-factor app)))

(def ^:private AppVars
  [:fn {:error/fn (fn [{:keys [value]} _] (app-vars-problem value))}
   (fn [v] (nil? (app-vars-problem v)))])

(defn- data-app-file []
  [:and
   (conj DataAppFileBase [:layout {:optional true} (data-node)])
   [:fn {:error/fn (fn [{:keys [value]} _] (reference-problem value))}
    (fn [v] (nil? (reference-problem v)))]
   AppVars
   [:fn {:error/message (str ":promoted true braucht eine :collection — sie ist die Ueberschrift, "
                             "unter der die App im Portal steht. Ohne sie waere die App beworben "
                             "und trotzdem unsortiert.")}
    (fn [{:keys [promoted collection]}] (or (not promoted) (some? collection)))]])

(def ^:private CodeAppFile
  [:map
   [:name        spec/RefId]
   [:source-file {:optional true} :string]
   [:source      {:optional true} :string]
   [:layout      {:optional true} [:map-of :keyword :any]]])

(def ^:private AppFile*
  (delay (compile-grammar
          [:multi {:dispatch (fn [m] (if (or (:source-file m) (:source m)) :code :data))}
           [:data (data-app-file)]
           [:code CodeAppFile]])))

(def app-lifted-keys
  [[:object-types seq] [:aggregate-types seq] [:functions seq] [:derived-properties seq]
   [:upload some?] [:export some?] [:documents seq] [:actions seq]])

(defn fold-app-lifted
  [m src]
  (reduce (fn [acc [k present?]]
            (let [v (get src k)] (cond-> acc (present? v) (assoc k v))))
          m app-lifted-keys))

(defn enta->schema
  [{:keys [name derived-properties upload export actions documents layout] :as app}]
  {:applications
   {(keyword (clojure.core/name name))
    (cond-> (apply dissoc app :name (map first app-lifted-keys))
      (or layout upload export actions (seq documents) (seq derived-properties))
      (assoc :layout (fold-app-lifted (or layout {}) app)))}})

(def ^:private PipelineMember
  [:and
   TransformEntry
   [:fn {:error/message (str "a pipeline member is no-code: give it a `:board`, or a `:query` with"
                             " the `:language` of its SQL dialect ("
                             (str/join ", " (map #(str "`" % "`")
                                                 (filter langs/query-language?
                                                         langs/selectable-languages)))
                             "), never `:code`/`:code-file` — a python/clojure"
                             " transform is a Source Repository resource and belongs in an"
                             " `.ent`, not in a pipeline")}
    (fn [m] (let [lang (some-> (:language m) keyword)]
              (and (not (:code m))
                   (not (:code-file m))
                   (if (:board m)
                     (or (nil? lang) (langs/query-language? lang))
                     (langs/query-language? lang)))))]
   [:fn {:error/message (str "`:pipeline`/`:project`/`:repository` are stated ONCE, at the"
                             " file level — remove them from the member")}
    (fn [m] (not-any? #(contains? m %) [:pipeline :project :repository]))]])

(def ^:private PipelineFile
  [:map {:closed true}
   [:name        spec/RefId]
   [:kind        {:optional true} [:enum :batch :streaming]]
   [:description {:optional true} :string]
   [:status      {:optional true} ResourceStatus]
   [:deprecation {:optional true} Deprecation]
   [:project     {:optional true} :keyword]
   [:repository  {:optional true} :keyword]
   [:ignore-inherited {:optional true} :boolean]
   [:imports     {:optional true} [:vector spec/RefId]]
   [:outputs     {:optional true} [:vector spec/RefId]]
   [:transforms  {:optional true} [:map-of :keyword PipelineMember]]])

(def ^:private PipelineFile* (delay (compile-grammar PipelineFile)))

(defn entp->schema
  [{:keys [name kind description status deprecation project repository ignore-inherited
           imports outputs transforms]}]
  (let [p-kw (keyword (clojure.core/name name))]
    {:pipelines
     {p-kw (cond-> {:kind (or kind :batch)}
             description             (assoc :description description)
             status                  (assoc :status status)
             deprecation             (assoc :deprecation deprecation)
             (seq imports)           (assoc :imports (vec imports))
             (seq outputs)           (assoc :outputs (vec outputs))
             project                 (assoc :project project)
             repository              (assoc :repository repository)
             (some? ignore-inherited) (assoc :ignore-inherited ignore-inherited))}
     :transforms
     (into {}
           (map (fn [[t-kw body]]
                  [t-kw (cond-> (assoc body :language (or (:language body) :pg-sql)
                                       :pipeline p-kw)
                          project    (assoc :project project)
                          repository (assoc :repository repository))]))
           transforms)}))

(def ^:private TemplateTraversals
  [:fn {:error/fn (fn [{:keys [value]} _] (first (tmpl/parameter-problems (:parameters value))))}
   (fn [{:keys [parameters]}] (empty? (tmpl/parameter-problems parameters)))])

(def ^:private TemplateFile
  [:and
   (entry :templates)
   TemplateTraversals
   [:fn {:error/message "a .entt names itself: `:name :monatsbericht`"}
    (fn [m] (some? (:name m)))]
   [:fn {:error/message (str "a template needs exactly ONE body: `:source-file` (the usual"
                             " form — the body is html/markdown with `{{ }}` in it, and it"
                             " belongs in its own file where a reviewer reads it as the"
                             " document it is) or an inline `:source`")}
    (fn [{:keys [source source-file]}]
      (= 1 (count (filter some? [source source-file]))))]])

(def ^:private TemplateFile* (delay (compile-grammar TemplateFile)))

(defn entt->schema
  [{:keys [name] :as tpl}]
  {:templates {(keyword (clojure.core/name name)) (dissoc tpl :name)}})

(def ^:private AgentEntry (entry :agents))
(def ^:private OrganizationEntry (entry :organizations))
(def ^:private SpaceEntry (entry :spaces))
(def ^:private ProjectEntry (entry :projects))
(def ^:private EvalSuiteEntry (entry :eval-suites))

(def ^:private RepositoryEntry (entry :repositories))
(def ^:private ApplicationEntry [:and (entry :applications) AppVars])
(def ^:private CustomApplicationEntry (entry :custom-applications))
(def ^:private TemplateEntry
  [:and
   (entry :templates)
   TemplateTraversals
   [:fn {:error/message (str "a template needs exactly ONE body: `:source-file` (the usual"
                             " form — the body is a file in the tree, diffable on its own)"
                             " or an inline `:source`")}
    (fn [{:keys [source source-file]}]
      (= 1 (count (filter some? [source source-file]))))]])

(def ^:private CollectionEntry (entry :collections))
(def ^:private WorkspaceEntry
  [:and
   (entry :workspaces)
   [:fn {:error/message
         (str "every :anchored app must also be listed in :apps. :apps IS the "
              "workspace — the set of modules this group may open — and "
              ":anchored only picks which of them keep a permanent tab in the "
              "menu bar. A tab pointing at a module the workspace does not "
              "contain would open something the workspace never offered.")}
    (fn [{:keys [apps anchored]}]
      (every? (set apps) anchored))]
   [:fn {:error/message
         (str ":landing must be one of :apps. The landing module is the "
              "workspace's entry point, not a back door into a module the "
              "workspace does not otherwise carry.")}
    (fn [{:keys [apps landing]}]
      (or (nil? landing) (contains? (set apps) landing)))]
   [:fn {:error/message
         (str ":landing-params fill the inputs of the :landing module — without a "
              ":landing there is no module to hand them to.")}
    (fn [{:keys [landing landing-params]}] (or (some? landing) (empty? landing-params)))]])
(def ^:private PipelineEntry (entry :pipelines))
(def ^:private ObjectTypeGroupEntry (entry :object-type-groups))
(def ^:private LlmModelEntry (entry :llm-models))
(def ^:private ProjectionEntry (entry :projections))
(def ^:private MediaSetEntry (entry :media-sets))
(def ^:private ExportEntry (entry :exports))
(def ^:private RoleEntry (entry :roles))
(def ^:private MarkingEntry (entry :markings))

(def ^:private DatasetEntry
  [:and
   (entry :datasets)
   [:fn {:error/message
         (str ":kind :materialization needs the :object-type it materializes — "
              "a materialization is ontology → dataset, so it must name the type "
              "whose latest state it carries.")}
    (fn [m] (or (not= :materialization (:kind m)) (some? (:object-type m))))]
   [:fn {:error/message
         (str ":object-type/:properties belong to a `:kind :materialization` "
              "dataset. A dataset fed from OUTSIDE names its feed in "
              ":external-source; a dataset fed from the ONTOLOGY names its type "
              "here — but it has to say which one it is.")}
    (fn [m] (or (= :materialization (:kind m))
                (not (or (contains? m :object-type) (contains? m :properties)))))]
   [:fn {:error/message
         (str "a :kind :materialization dataset takes neither :columns nor"
              " :external-source. Its schema comes from the ontology definitions and its"
              " rows come from the object type — an external feed would make it a"
              " second, disagreeing producer of the same table. Use :properties to carry"
              " a subset.")}
    (fn [m] (or (not= :materialization (:kind m))
                (not (or (contains? m :columns) (contains? m :external-source)))))]
   [:fn {:error/message
         (str "a `:kind :materialization` dataset cannot carry `:stream`. A "
              "materialization's rows come FROM the ontology, so there is no "
              "buffer in front of them to publish into.")}
    (fn [m] (or (not (:stream m)) (not= :materialization (:kind m))))]
   [:fn {:error/message
         (str "`:kind :virtual` and `:virtual {:source … :table …}` go together. A virtual "
              "dataset holds no rows of its own — `:virtual` names the source table every "
              "read of it scans — so one without the other names nothing or is never read.")}
    (fn [m] (= (= :virtual (:kind m)) (contains? m :virtual)))]
   [:fn {:error/message
         (str "a `:kind :virtual` dataset takes neither `:external-source` nor `:stream`. "
              "Its rows are the source table's, read in place; a feed would make it a "
              "second producer of rows it does not store.")}
    (fn [m] (or (not= :virtual (:kind m))
                (not (or (contains? m :external-source) (contains? m :stream)))))]
   [:fn {:error/message
         (str "`:kind :restricted-view` and `:restricted-view {:dataset … :policy …}` go "
              "together, and such a dataset takes no `:columns`, `:external-source`, "
              "`:stream` or `:virtual`. A restricted view holds no rows: every read of it is "
              "its backing dataset's rows, filtered by the policy for whoever reads.")}
    (fn [m] (and (= (= :restricted-view (:kind m)) (contains? m :restricted-view))
                 (or (not= :restricted-view (:kind m))
                     (not-any? #(contains? m %)
                               [:columns :external-source :stream :virtual]))))]
   [:fn {:error/message
         (str "a `:stream` dataset needs `:columns`. A stream is inherently "
              "tabular and inherently structured "
              "— the schema is what a published record is validated against, "
              "and without it every batch would be admitted unread.")}
    (fn [m] (or (not (:stream m)) (seq (:columns m))))]
   [:fn {:error/message
         (str "`:stream :primary-key :deletion-column` has to be one of the "
              "declared `:columns` — it is read off each record to decide "
              "whether that record RETRACTS its key.")}
    (fn [m]
      (let [col (get-in m [:stream :primary-key :deletion-column])]
        (or (nil? col)
            (contains? (into #{} (map :attribute-name) (:columns m)) col))))]
   [:fn {:error/message
         (str "every `:stream` key column has to be one of the declared "
              "`:columns`. A key naming a column that does not exist is an "
              "ordering guarantee that silently is not one.")}
    (fn [m]
      (let [declared (into #{} (map :attribute-name) (:columns m))
            keys*    (concat (get-in m [:stream :partition-keys])
                             (get-in m [:stream :primary-key :columns]))]
        (or (empty? keys*) (every? declared keys*))))]])

(def ^:private LineageDrawing
  [:map {:closed true}
   [:name    {:optional true} :string]
   [:payload {:optional true} GraphPayload]
   [:scope   {:optional true} GraphScope]
   [:nodes   {:optional true} [:vector [:map {:closed true}
                                        [:kind :keyword] [:key :string]
                                        [:x number?] [:y number?]]]]
   [:color-schemes
    {:optional true}
    [:vector [:map {:closed true}
              [:name :string]
              [:is-default {:optional true} :boolean]
              [:rules [:vector [:map {:closed true}
                                [:type :keyword] [:color :string]]]]]]]])

(def EntSchema
  [:map {:closed true}
   [:value-types       {:optional true} [:map-of (section-key :value-types) ValueTypeEntry]]
   [:shared-properties {:optional true} [:map-of (section-key :shared-properties) SharedProperty]]
   [:objects           {:optional true} [:map-of (section-key :objects) ObjectEntry]]
   [:object-type-groups {:optional true} [:map-of :keyword ObjectTypeGroupEntry]]
   [:links             {:optional true} [:vector LinkEntry]]
   [:interfaces        {:optional true} [:map-of (section-key :interfaces) InterfaceEntry]]
   [:transforms        {:optional true} [:map-of :keyword BuildTransformEntry]]
   [:functions         {:optional true} [:map-of (section-key :functions) FunctionEntry]]
   [:datasets          {:optional true} [:map-of :keyword DatasetEntry]]
   [:media-sets        {:optional true} [:map-of :keyword MediaSetEntry]]
   [:exports           {:optional true} [:map-of :keyword ExportEntry]]
   [:llm-models        {:optional true} [:map-of :string LlmModelEntry]]
   [:sources           {:optional true} [:map-of :keyword SourceEntry]]
   [:actions           {:optional true} [:map-of (section-key :actions) ActionEntry]]
   [:object-sets       {:optional true} [:map-of :keyword ObjectSetEntry]]
   [:organizations     {:optional true} [:map-of :keyword OrganizationEntry]]
   [:spaces            {:optional true} [:map-of :keyword SpaceEntry]]
   [:projects          {:optional true} [:map-of :keyword ProjectEntry]]
   [:repositories      {:optional true} [:map-of :keyword RepositoryEntry]]
   [:applications      {:optional true} [:map-of :keyword ApplicationEntry]]
   [:custom-applications {:optional true} [:map-of :keyword CustomApplicationEntry]]
   [:collections       {:optional true} [:map-of :keyword CollectionEntry]]
   [:workspaces        {:optional true} [:map-of :keyword WorkspaceEntry]]
   [:pipelines         {:optional true} [:map-of :keyword PipelineEntry]]
   [:agents            {:optional true} [:map-of :keyword AgentEntry]]
   [:templates         {:optional true} [:map-of :keyword TemplateEntry]]
   [:projections       {:optional true} [:map-of :keyword ProjectionEntry]]
   [:roles             {:optional true} [:map-of :keyword RoleEntry]]
   [:markings          {:optional true} [:map-of :keyword MarkingEntry]]
   [:eval-suites       {:optional true} [:map-of :keyword EvalSuiteEntry]]])

(def EntFile
  [:map {:closed true}
   [:schema   EntSchema]
   [:automations {:optional true} [:vector op/EntAutomation]]
   [:schedules   {:optional true} [:vector op/EntSchedule]]
   [:health-checks {:optional true} [:vector op/EntHealthCheck]]
   [:lineage-drawing {:optional true} LineageDrawing]])

(def ^:private EntSchema* (delay (compile-grammar EntSchema)))

(def ^:private EntFile* (delay (compile-grammar EntFile)))

(defn explain
  [data]
  (some-> (m/explain @EntFile* data) me/humanize))

(defn- validate-text!
  [data source]
  (when (and (map? data) (contains? data :ontology))
    (refusal/refuse! :refusal.parser/ent-names-ontology
                     {:source source :offending-key :ontology
                      :ontology (:ontology data)}))
  (when (and (map? (:schema data)) (contains? (:schema data) :retire))
    (refusal/refuse! :refusal.parser/retire-section-gone
                     {:source source :offending-key :retire
                      :retire (get-in data [:schema :retire])}))
  (if-let [errs (explain data)]
    (refusal/refuse! :refusal.parser/invalid-text {:source source :errors errs}
                     (ent-errors-params "Invalid .ent" errs))
    data))

(def grammar-version
  "1.0.0")

(def grammar-version-key :sutra/grammar)

(def ^:private version-pattern #"^\d+(\.\d+){0,2}$")

(def GrammarVersion
  [:re {:error/message "a grammar version: \"1\", \"1.0\" or \"1.0.0\""} version-pattern])

(defn- major [v] (parse-long (first (str/split v #"\."))))

(defn without-grammar-version
  ([data] (without-grammar-version data nil))
  ([data source]
   (if-not (and (map? data) (contains? data grammar-version-key))
     data
     (let [v (get data grammar-version-key)]
       (when-not (and (string? v) (re-matches version-pattern v))
         (refusal/refuse! :refusal.parser/grammar-version-shape {:source source :grammar v}))
       (when (> (major v) (major grammar-version))
         (refusal/refuse! :refusal.parser/grammar-too-new
                          {:source source :grammar v :reads grammar-version}
                          {:major (major grammar-version)}))
       (dissoc data grammar-version-key)))))

(defn validate!
  [data source]
  (validate-text! (without-grammar-version data source) source))

(defn explain-schema
  [schema]
  (some-> (m/explain @EntSchema* schema) me/humanize))

(defn decode-schema
  [schema]
  (m/decode @EntSchema* schema (mt/json-transformer)))

(defn validate-schema!
  [schema source]
  (if-let [errs (explain-schema schema)]
    (refusal/refuse! :refusal.parser/invalid-text {:source source :errors errs}
                     (ent-errors-params "Invalid .ent schema" errs))
    (sugar/lower-schema schema)))

(defn- merge-section
  [section own req]
  (cond
    (nil? own) req
    (nil? req) own
    (and (map? own) (map? req))
    (reduce-kv (fn [m k v]
                 (cond (not (contains? m k)) (assoc m k v)
                       (= (get m k) v)       m
                       :else (refusal/refuse! :refusal.parser/require-conflict
                                              {:section section :key k})))
               own req)
    (and (vector? own) (vector? req)) (into own (remove (set own)) req)
    (= own req) own
    :else (refusal/refuse! :refusal.parser/require-section-conflict {:section section})))

(defn- deep-merge-schema [own req]
  (reduce-kv (fn [acc section req-val]
               (assoc acc section (merge-section section (get acc section) req-val)))
             own req))

(defn- merge-requires
  [fs raw path visited]
  (let [reqs (:require raw)]
    (if (empty? reqs)
      (dissoc raw :require)
      (let [self    ((:canonical fs) path)
            _       (when (contains? visited self)
                      (refusal/refuse! :refusal.parser/require-cycle {:path self}))
            visited (conj visited self)
            req-schemas (map (fn [rel]
                               (let [rp ((:resolve fs) path rel)]
                                 (:schema (merge-requires
                                           fs (without-grammar-version ((:read fs) rp) rp)
                                           rp visited))))
                             reqs)
            combined    (reduce deep-merge-schema {} req-schemas)]
        (-> (dissoc raw :require)
            (update :schema #(deep-merge-schema (or % {}) combined)))))))

(defn- lift-text-applications
  [data]
  (cond-> data (map? (:schema data)) (update :schema refs/lift-schema-applications)))

(defn load-ent
  [fs path]
  (let [raw  (merge-requires fs ((:read fs) path) path #{})
        data (-> raw lift-text-applications (validate! path)
                 (update :schema sugar/lower-schema))]
    (res/check! (:schema data) path)
    data))

(defn- parse-lean
  [ext grammar lower data source]
  (if-let [errs (some-> (m/explain grammar data) me/humanize)]
    (refusal/refuse! :refusal.parser/invalid-text {:source source :errors errs}
                     (ent-errors-params (str "Invalid " ext) errs))
    {:schema (lower data)}))

(defn lower-view-cards
  [data]
  (if (map? data)
    (reduce (fn [d f] (cond-> d (contains? d f) (update f lay/lower-view-body)))
            data view-factors)
    data))

(defn- lean-kind
  [ext grammar lower prepare]
  {:grammar grammar :lean? true
   :parse   (fn [data source]
              (parse-lean ext @grammar lower
                          (prepare (without-grammar-version data source)) source))})

(def kinds
  {:doc-template (lean-kind ".entt" TemplateFile* entt->schema identity)
   :view         (lean-kind ".entv" ViewFile*     entv->schema
                            (comp refs/lift-view-doc vars/lift-url-flags lower-view-cards))
   :app          (lean-kind ".enta" AppFile*      enta->schema
                            (comp refs/lift-app-doc vars/lift-url-flags))
   :pipeline     (lean-kind ".entp" PipelineFile* entp->schema identity)})

(def parse-entv (get-in kinds [:view :parse]))
(def parse-enta (get-in kinds [:app :parse]))
(def parse-entp (get-in kinds [:pipeline :parse]))
(def parse-entt (get-in kinds [:doc-template :parse]))

(defn read-text
  [kind path text]
  (let [data (edn/read-string text)]
    (if kind
      ((get-in kinds [kind :parse]) data path)
      (-> data vars/lift-url-flags lift-text-applications (validate! path)
          (update :schema sugar/lower-schema)))))

(defn merge-entity-schemas
  [schema-maps]
  {:schema (reduce deep-merge-schema {} (map :schema schema-maps))})

(defn- versioned
  [s]
  (case (m/type s)
    :map   (mu/merge s [:map [grammar-version-key {:optional true} GrammarVersion]])
    :and   (let [[c & more] (m/children s)]
             (m/into-schema :and (m/properties s) (into [(versioned c)] more) (m/options s)))
    :multi (m/into-schema :multi (m/properties s)
                          (mapv (fn [[k p c]] [k p (versioned c)]) (m/children s))
                          (m/options s))
    s))

(def ^:private versioned-grammar
  (memoize (fn [kind] (versioned (deref (if kind (get-in kinds [kind :grammar]) EntFile*))))))

(defn grammar-of
  [kind]
  (versioned-grammar kind))

(tbl/holds! "a reference rule's :required is what the grammar slot it names says"
            (for [{[sym k] :grammar :keys [required slot]} res/reference-rules
                  :when sym
                  :let [entry (some (fn [[ek props]] (when (= ek k) [props]))
                                    (m/children
                                     (m/schema
                                      @(or (ns-resolve 'sutra.parser.ent (symbol (name sym)))
                                           (ns-resolve 'sutra.parser.ontology
                                                       (symbol (name sym)))))))]
                  :when (or (nil? entry) (not= required (not (:optional (first entry)))))]
              [slot sym k]))
