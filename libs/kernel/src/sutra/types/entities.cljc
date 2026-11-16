(ns sutra.types.entities
  (:require [sutra.kit.coll :as coll]
            [sutra.types.table :as tbl]))

(def ^:private owned
  {:project          {:present :some :compare :lenient :type :keyword}
   :ignore-inherited {:present :some :compare :lenient :type :boolean}})

(def ^:private in-repo
  {:repository {:present :some :type :keyword}})

(defn- lifecycle [status-type]
  {:status      {:present :some :coerce :kw :compare :eq :type status-type}
   :deprecation {:present :some :compare :eq :type :ent/deprecation}})

(def placement-fields
  (into #{} (concat (keys owned) (keys in-repo) (keys (lifecycle nil)))))

(def ^:private published-history
  {:releases      {:present :seq :type [:map-of :string :any]}
   :published     {:present :some :type :string}
   :release-notes {:present :seq :type [:map-of :string :any]}})

(def ^:private project-ref    {:at [:project]    :targets #{:projects}})
(def ^:private repository-ref {:at [:repository] :targets #{:repositories}})

(def object-targets
  #{:object-types :interfaces})

(def action-param-refs
  [{:at [:params] :each-val [{:at [:type] :targets object-targets}
                             {:at [:options-from :object-type] :targets object-targets}]}
   {:at [:on] :targets object-targets}])

(def action-step-refs
  (into action-param-refs
        [{:at [:object-type] :targets object-targets}
         {:at [:link-type] :targets #{:link-types}}
         {:at [:interface] :targets #{:interfaces}}]))

(def action-rule-refs
  [{:steps action-step-refs}])

(def action-refs
  [{:steps action-step-refs}
   {:at [:edits] :each [{:steps action-step-refs}]}
   {:at [:parameters] :each-val [{:at [:object-type] :targets object-targets}]}])

(def ^:private action-step-name-refs
  [{:at [:function] :targets #{:functions}}
   {:at [:source] :targets #{:sources} :if {:eq {:op #{:invoke-webhook :use-llm}}}}
   {:at [:model] :targets #{:llm-models} :if {:eq {:op #{:use-llm}}}}
   {:at [:name] :targets #{:actions} :if {:eq {:op #{:action}}}}])

(def ^:private action-author-refs
  (let [step (into action-step-refs action-step-name-refs)]
    [{:steps step}
     {:at [:edits] :each [{:steps step}]}
     {:at [:action-log :object-type] :targets object-targets}
     {:at [:action-log :project] :targets #{:projects}}]))

(def action-edit-refs
  [{:at [:rules]
    :steps [{:at [:object-type] :targets #{:object-types}
             :if {:eq {:op #{:create :update :delete-object :create-or-update}}}}
            {:at [:link-type] :targets #{:link-types} :if {:eq {:op #{:create-link :delete-link}}}}
            {:at [:name] :targets #{:actions} :if {:eq {:op #{:action}}}}
            {:at [:function] :targets #{:functions} :if {:eq {:op #{:apply-edit-function}}}}]}
   {:at [:edits] :each [{:at [:object-type] :targets #{:object-types}}]}])

(def action-type-refs
  [{:at [:rules] :steps [{:at [:object-type] :targets #{:object-types}}]}
   {:at [:edits] :each [{:at [:object-type] :targets #{:object-types}}]}])

(def entities
  [{:tag :object-type :sweep-rank 7 :section :object-types :author :objects :key :rid
    :about "A kind of thing in the business world, e.g. customer or order: its attributes, key, backing dataset and presentation."
    :renamed-with-code? true
    :export {:scope :project}
    :structural? true
    :required? true
    :api-name true
    :tree :ontology :regime :declared :step-marks [:whole-declaration?]
    :axes {:identity     [:resolve-entity :primary-key :match-keys :survivorship]
           :presentation [:display-name :description :aliases :visibility :subtitle :row
                          :lifecycle]
           :binding      [:backing]
           :governance   [:rationale :markings :security]}
    :refs [{:at [:implements] :many true :targets #{:interfaces}}
           {:at [:dataset] :targets #{:datasets}}
           {:at [:backing :dataset] :targets #{:datasets}}
           project-ref]
    :name-refs [{:at [:external-source :datasource] :targets #{:sources}}
                {:at [:external-sources] :each [{:at [:datasource] :targets #{:sources}}]}
                {:at [:lifecycle :transitions] :each [{:at [:action] :targets #{:actions}}]}]
    :fields (merge {:attributes      {:custom true :type :ent/attr-list}
                    :implements      {:custom true :type [:vector :ent/ref-id]}
                    :external-source {:custom true :type :ent/ext-source}
                    :backing         {:present :map :compare :eq :type :ent/backing}
                    :symbol          {:present :seq :compare :lenient :type :string}
                    :plural          {:present :seq :compare :lenient :type :string}
                    :display-name    {:present :seq :compare :lenient :author-form :name-sugar
                                      :type :string}
                    :description     {:present :seq :compare :lenient :type :string}
                    :rationale       {:present :seq :compare :lenient :type :string}
                    :aliases         {:present :seq :coerce :vec :compare :lenient
                                      :type [:vector :string]}
                    :visibility      {:present :some :coerce :kw :compare :lenient
                                      :type :ent/visibility}
                    :title           {:as :title-key :present :some :coerce :kw :compare :lenient
                                      :type :keyword}
                    :subtitle        {:present :some :coerce :kw :compare :lenient :type :keyword}
                    :row             {:present :seq :coerce :vec :compare :lenient
                                      :type [:vector :keyword]}
                    :lifecycle       {:present :seq :compare :eq :type :ent/lifecycle}
                    :resolve-entity  {:present :truthy :compare :bool :type :boolean}
                    :requires-action-log {:present :truthy :compare :bool :type :boolean}
                    :survivorship    {:present :seq :compare :eq :type :any}
                    :primary-key     {:present :some :coerce :kw :compare :eq :type :keyword}
                    :match-keys      {:present :seq :coerce :vec :in :kws :compare :lenient
                                      :type [:vector :keyword]}
                    :checks         {:present :seq :compare :empty-folded :type :ent/object-checks}
                    :semantic-search {:present :seq :compare :empty-folded
                                      :type :ent/semantic-search}
                    :edit-conflict   {:present :some :compare :eq :type :ent/edit-conflict}
                    :markings        {:present :some :coerce :kws :compare :eq
                                      :type [:vector :keyword]}
                    :security        {:present :map :compare :eq :type :ent/security}}
                   (lifecycle :ent/status) owned)}

   {:tag :object-type-group :sweep-rank 3 :section :object-type-groups :key :rid :tree :ontology
    :about "A named group of object types."
    :export {:scope :project}
    :regime :declared
    :refs [{:at [:members] :many true :targets #{:object-types}} project-ref]
    :fields (merge {:description {:present :seq :type :string}
                    :members     {:present :seq :in :kws :type [:vector :keyword]}}
                   (lifecycle :ent/resource-status) owned)}

   {:tag :link-type :sweep-rank 5 :section :link-types :author :links :key :rid :required? true
    :about "A relation between two object types (or interfaces) with a cardinality, e.g. order -> customer, n:1."
    :renamed-with-code? true
    :export {:scope :project :shape :tuples}
    :structural? true
    :tree :ontology :regime :declared
    :api-name true
    :refs [{:at [:from-object-type] :targets #{:object-types :interfaces}}
           {:at [:to-object-type]   :targets #{:object-types :interfaces}}
           {:at [:satisfies] :each [{:at [:interface] :targets #{:interfaces}}]}
           {:at [:external-source :dataset] :targets #{:datasets}}
           project-ref]
    :author-tuple [:link-type-name :from-object-type :to-object-type :cardinality]
    :name-refs [{:at [:external-source :datasource] :targets #{:sources}}
                {:at [:external-source :shared-property] :targets #{:shared-properties}}]
    :fields (merge {:from            {:as :from-object-type :present :some :coerce :kw
                                      :author-form :positional :type :ent/ref-id}
                    :to              {:as :to-object-type   :present :some :coerce :kw
                                      :author-form :positional :type :ent/ref-id}
                    :cardinality     {:present :some :coerce :kw
                                      :author-form :positional :type :ent/cardinality}
                    :external-source {:custom true :type :ent/ext-source}
                    :forward-api-name {:present :some :coerce :kw :type :ent/api-name}
                    :reverse-api-name {:present :some :coerce :kw :type :ent/api-name}
                    :forward-name    {:present :seq :compare :lenient :type :string}
                    :reverse-name    {:present :seq :compare :lenient :type :string}
                    :forward-plural  {:present :seq :compare :lenient :type :string}
                    :reverse-plural  {:present :seq :compare :lenient :type :string}
                    :description     {:present :seq :compare :lenient :type :string}
                    :rationale       {:present :seq :compare :lenient :type :string}
                    :symbol          {:present :seq :compare :lenient :type :string}
                    :satisfies       {:present :seq :coerce :vec
                                      :type [:or :ent/interface-link-membership
                                             [:vector :ent/interface-link-membership]]}}
                   (lifecycle :ent/resource-status) owned)}

   {:tag :interface :sweep-rank 8 :section :interfaces :key :rid :tree :ontology :regime :declared
    :about "A shape several object types share: the properties and links an implementing type promises."
    :export {:scope :project}
    :structural? true
    :api-name true
    :refs [{:at [:extends] :many true :targets #{:interfaces}}
           {:at [:link-constraints] :each [{:at [:to-object-type] :targets #{:object-types}}
                                           {:at [:to-interface]   :targets #{:interfaces}}]}
           project-ref]
    :fields (merge {:property-list    {:custom true :type :ent/attr-list}
                    :properties       {:custom true :type :ent/attr-list}
                    :link-constraints {:custom true :type [:vector :ent/link-constraint]}
                    :links            {:custom true :type [:vector :ent/link-constraint]}
                    :extends          {:present :seq :coerce :kws :type [:vector :ent/ref-id]}
                    :display-name     {:present :seq :type :string}
                    :description      {:present :seq :type :string}
                    :searchable       {:present :some :coerce :bool :type :boolean}}
                   (lifecycle :ent/resource-status) owned)}

   {:tag :transform :sweep-rank 6 :section :transforms :key :rid :id-match? true :tree :repo
    :about "Python or SQL code in a repository that reads inputs and writes one output (dataset, object type, link type, media set or export)."
    :sweep-only :projected :rows-of :output
    :export {:scope :none :generated :transforms}
    :regime :declared
    :branching {:instance-when {:at [:input :kind] :is :datasource}}
    :refs [{:at [:output :object-type] :targets #{:object-types}}
           {:at [:output :link-type]   :targets #{:link-types}}
           {:at [:output :dataset]     :targets #{:datasets}}
           {:at [:output :export]      :targets #{:exports}}
           {:at [:output :media-set]   :targets #{:media-sets}}
           {:at [:media-outputs] :each-val [{:at [:media-set] :targets #{:media-sets}}]}
           {:at [:dataset]             :targets #{:datasets}}
           {:at [:input :transform]    :targets #{:transforms}}
           {:at [:input :object-type]  :targets #{:object-types}}
           {:at [:input :dataset]      :targets #{:datasets}}
           {:at [:inputs] :each-val [{:at [:transform]   :targets #{:transforms}}
                                     {:at [:object-type] :targets #{:object-types}}
                                     {:at [:dataset]     :targets #{:datasets}}
                                     {:at [:media-set]   :targets #{:media-sets}}]}
           {:at [:outputs] :each [{:at [:object-type] :targets #{:object-types}}
                                  {:at [:link-type]   :targets #{:link-types}}
                                  {:at [:dataset]     :targets #{:datasets}}
                                  {:at [:export]      :targets #{:exports}}]}
           {:at [:sources] :each [{:at [:from-type] :targets #{:object-types :interfaces}}
                                  {:at [:to-type]   :targets #{:object-types :interfaces}}
                                  {:at [:inputs]
                                   :each-val [{:at [:transform]   :targets #{:transforms}}
                                              {:at [:object-type] :targets #{:object-types}}]}]}
           project-ref
           repository-ref
           {:at [:pipeline] :targets #{:pipelines}}]
    :name-refs [{:at [:inputs] :each-val [{:at [:datasource] :targets #{:sources}}
                                          {:at [:source] :targets #{:sources}}]}]
    :fields (merge {:function-name    {:present :some :coerce :kw :author-form :minted
                                       :type :string}
                    :source           {:present :seq :author-form :minted :type :any}
                    :spec-source      {:present :some :coerce :kw :author-form :minted :type :any}
                    :input            {:present :seq :type :ent/input-spec}
                    :ops              {:present :seq :coerce :vec :type :ent/ops}
                    :column-map       {:present :seq :type [:map-of :keyword :keyword]}
                    :primary-key      {:present :some :type :keyword}
                    :output           {:present :seq :type :any}
                    :outputs          {:present :seq :coerce :vec :type [:vector :any]}
                    :from-key-column  {:present :some :type :keyword}
                    :to-key-column    {:present :some :type :keyword}
                    :fk-attribute     {:present :some :type :keyword}
                    :output-columns   {:custom true :type :ent/column-list}
                    :returns          {:present :seq :type [:map-of :keyword :any]}
                    :language         {:present :some :type :keyword}
                    :code             {:present :some :compute-only true :type :string}
                    :modules          {:present :seq :compute-only true :author-form :minted
                                       :type [:map-of :string :string]}
                    :inputs           {:present :seq :type :ent/inputs}
                    :query            {:custom true :compute-only true :type :string}
                    :board            {:custom true :compute-only true :type :any}
                    :external-systems {:present :seq :coerce :kws :compute-only true
                                       :type [:vector :keyword]}
                    :media-outputs    {:present :seq :compute-only true
                                       :type [:map-of :keyword [:map [:media-set :keyword]]]}
                    :incremental      {:present :truthy :type [:or :boolean :ent/incremental-opts]}
                    :semantic-version {:present :some :type :int}
                    :entry-point      {:present :some :type :string}
                    :code-path        {:present :some :type :string}
                    :version          {:present :some :type :string}
                    :releases         {:present :seq :type [:map-of :string :any]}
                    :profile          {:present :some :coerce :kw :compute-only true :type :keyword}
                    :resources        {:present :seq :compute-only true
                                       :type :ent/transform-resources}
                    :timeout-ms       {:present :some :compute-only true
                                       :type [:int {:min 1000 :max 600000}]}
                    :done-key         {:present :seq :compute-only true :type :ent/done-key}
                    :max-per-run      {:present :some :compute-only true :type :int}
                    :allowed-run-seconds {:present :some :compute-only true
                                          :type [:int {:min 1}]}
                    :container        {:present :seq :compute-only true
                                       :type [:map [:image :string] [:tag :string]]}
                    :prompt-key       {:present :seq :compute-only true :type :ent/prompt-key}
                    :strategy         {:present :some :coerce :kw :type :keyword}
                    :sources          {:present :seq :coerce :vec :type [:vector :ent/poly-source]}
                    :parameters       {:present :seq :compute-only true
                                       :type [:map-of :keyword :ent/transform-param]}
                    :tests            {:present :seq :coerce :vec :compute-only true
                                       :type [:vector :ent/transform-test]}
                    :pipeline         {:present :some :type :keyword}
                    :description      {:present :seq :type :string}
                    :rationale        {:present :seq :type :string}}
                   (lifecycle :ent/resource-status) owned in-repo)}

   {:tag :shared-property :section :shared-properties :key :rid :tree :ontology :regime :declared
    :about "A property declared once and used by several object types."
    :export {:scope :project}
    :structural? true
    :api-name true
    :refs [project-ref]
    :name-refs [{:at [:value-type] :targets #{:value-types}}]
    :fields (merge {:type         {:present :some :coerce :kw :default :string :type :ent/vtype}
                    :value-type   {:present :some :coerce :kw :type :keyword}
                    :name         {:author-only true :type :string}
                    :display-name {:present :seq :author-form :name-sugar :type :string}
                    :description  {:present :seq :type :string}
                    :visibility   {:present :some :coerce :kw :type :ent/visibility}
                    :status       {:present :some :coerce :kw :type :ent/resource-status}
                    :constraints  {:present :seq :type :ent/constraints}
                    :format       {:present :seq :type :ent/value-format}
                    :render-hints {:present :seq :coerce :vec :type [:vector :ent/render-hint]}}
                   owned)}

   {:tag :value-type :section :value-types :key :rid :tree :ontology :regime :declared
    :about "A constrained value type (a base type plus checks) that properties can use."
    :export {:scope :global}
    :structural? true
    :api-name true
    :refs []
    :fields {:type         {:present :some :default :string :required true :type :ent/vtype}
             :name         {:author-only true :type :string}
             :constraints  {:present :seq :type :ent/constraints}
             :format       {:present :seq :type :ent/value-format}
             :description  {:present :seq :type :string}
             :render-hints {:present :seq :coerce :vec :type [:vector :ent/render-hint]}}}

   {:tag :function :sweep-rank 6 :section :functions :key :rid :id-match? true :tree :repo
    :about "Code in a repository callable from actions, apps and queries: an edit, query or compute function."
    :export {:scope :project :generated :query-functions}
    :regime :declared
    :api-name true
    :versions {:mode :release :compat {:default :edit :query :query}}
    :refs [{:at [:target-type] :targets #{:object-types}} project-ref repository-ref]
    :name-refs [{:at [:inputs] :each-val [{:at [:object-type] :targets object-targets}]}]
    :fields (merge {:target-type {:present :some :coerce :kw :type :ent/ref-id}
                    :language    {:present :some :coerce :kw :default :clojure :type :keyword}
                    :source      {:present :some :type :string}
                    :param-types {:present :seq :coerce :kw-map :type [:map-of :keyword :any]}
                    :returns     {:present :seq :type [:map-of :keyword :any]}
                    :staged-writes {:present :truthy :coerce :bool :compare :bool :type :boolean}
                    :library     {:present :truthy :coerce :bool :compare :bool :type :boolean}
                    :uses        {:present :seq :coerce :kws :type [:vector :keyword]}
                    :timeout-ms  {:present :some :type :int}
                    :snapshot    {:present :some :coerce :kw
                                  :type [:enum :default :enabled :disabled]}
                    :version     {:present :some :type :string}
                    :releases    {:present :seq :type [:map-of :string :any]}
                    :description {:present :seq :type :string}}
                   (lifecycle :ent/resource-status) owned in-repo)}

   {:tag :source :sweep-rank 9 :section :sources :key :rid :id-match? true :tree :ontology
    :about "An external data source (database, file share, API) with its connection, egress and markings."
    :export {:scope :project}
    :regime :declared-on-main :branching :instance :audit :access
    :refs [project-ref]
    :fields (merge {:type               {:present :some :coerce :kw :required true :type :keyword}
                    :jdbc-url           {:present :seq :type :string}
                    :base-url           {:present :seq :type :string}
                    :path               {:present :seq :type :string}
                    :host               {:present :seq :type :string}
                    :remote-path        {:present :seq :type :string}
                    :command            {:present :seq :type :string}
                    :port               {:present :some :type :int}
                    :egress             {:present :seq :coerce :vec :type [:sequential :string]}
                    :allow-commands     {:present :seq :coerce :vec :type [:sequential :string]}
                    :access             {:present :some :coerce :kw :type :keyword}
                    :config             {:present :map :type [:map-of :keyword :any]}
                    :secret-cipher      {:present :seq :type :string}
                    :agent              {:present :some :coerce :kw :type :keyword}
                    :external-transform {:present :truthy :coerce :bool :type :boolean}
                    :markings           {:present :seq :coerce :kws :type [:vector :keyword]}
                    :export-markings    {:present :seq :coerce :kws :type [:vector :keyword]}
                    :writeback          {:present :map :type [:map-of :any :any]}
                    :webhooks           {:present :map :type [:map-of :keyword :any]}
                    :description        {:present :seq :type :string}
                    :rationale          {:present :seq :type :string}}
                   (lifecycle :ent/resource-status) owned)}

   {:tag :organization :section :organizations :key :rid :id-match? true :tree :ontology
    :about "The outermost owner: spaces and their members belong to an organization."
    :export {:scope :container}
    :regime :declared-on-main :branching :instance :audit :access
    :refs [{:at [:home-space] :targets #{:spaces}}
           {:at [:collaborates-with] :many true :targets #{:organizations}}]
    :fields {:description {:present :seq :type :string}
             :members     {:present :seq :coerce :sorted-strs :compare :eq :type [:vector :string]}
             :expand-access {:present :seq :coerce :sorted-strs :compare :eq
                             :type [:vector :string]}
             :guests      {:present :seq :coerce :sorted-strs :compare :eq :type [:vector :string]}
             :apply       {:present :seq :coerce :sorted-strs :compare :eq :type [:vector :string]}
             :home-space  {:present :some :coerce :kw :type :ent/ref-id}
             :collaborates-with {:present :seq :coerce :vec :in :kws :compare :lenient
                                 :type [:vector :ent/ref-id]}
             :discovery   {:present :map :compare :eq :type [:map-of :keyword :keyword]}
             :grants      {:custom true :type :any}
             :markings    {:present :some :coerce :kws :compare :eq :type [:vector :keyword]}}}

   {:tag :space :section :spaces :key :rid :id-match? true :tree :ontology :regime :declared-on-main
    :about "A space within an organization that holds projects and decides who may enter."
    :export {:scope :container}
    :branching :instance :audit :access
    :refs [{:at [:organization] :targets #{:organizations}}
           {:at [:organizations] :many true :targets #{:organizations}}
           {:at [:shared-with] :many true :targets #{:organizations}}]
    :fields {:description  {:present :seq :type :string}
             :organization {:present :some :coerce :kw :type :ent/ref-id}
             :organizations {:present :seq :coerce :vec :in :kws :compare :lenient
                             :type [:vector :ent/ref-id]}
             :shared-with  {:present :seq :coerce :vec :in :kws :compare :lenient
                            :type [:vector :ent/ref-id]}
             :grants       {:custom true :type :any}
             :markings     {:present :some :coerce :kws :compare :eq :type [:vector :keyword]}}}

   {:tag :project :section :projects :key :rid :id-match? true :tree :container
    :about "The unit that owns resources, imports other projects and grants roles on them."
    :export {:scope :imports}
    :regime :declared-on-main :branching :instance :audit :access
    :refs [{:at [:imports] :many true :targets #{:projects}}
           {:at [:space] :targets #{:spaces}}
           {:at [:shared-with] :many true :targets #{:organizations}}]
    :fields {:name        {:author-only true :type :string}
             :description {:present :seq :type :string}
             :space       {:present :some :coerce :kw :type :ent/ref-id}
             :shared-with {:present :seq :coerce :vec :in :kws :compare :lenient
                           :type [:vector :ent/ref-id]}
             :imports     {:present :seq :coerce :vec :in :kws :compare :lenient
                           :type [:vector :ent/ref-id]}
             :references  {:present :seq :coerce :vec :compare :lenient
                           :type [:vector :ent/ref-id]}
             :synonyms    {:present :map :compare :eq :type [:map-of :keyword :keyword]}
             :grants      {:custom true :type :any}
             :markings           {:present :some :coerce :kws :compare :eq :type [:vector :keyword]}
             :allowed-markings   {:present :some :coerce :kws :compare :eq :type [:vector :keyword]}
             :forbidden-markings {:present :some :coerce :kws :compare :eq
                                  :type [:vector :keyword]}}}

   {:tag :repository :section :repositories :key :rid :id-match? true :tree :container
    :about "A code repository of a project; it holds transforms, functions and apps."
    :export {:scope :repository}
    :regime :declared
    :refs [project-ref]
    :fields (merge {:description {:present :seq :type :string}
                    :type        {:present :some :coerce :kw :default :transforms
                                  :type [:enum :transforms :functions :applications]}
                    :python-deps {:present :seq :coerce :sorted-strs :type [:vector :string]}}
                   owned)}

   {:tag :application :sweep-rank 0 :section :applications :key :rid :id-match? true :tree :repo
    :about "An app built in the apphost: widgets laid out over the ontology."
    :sweep-only :not-object-view
    :regime :declared
    :versions {:mode :publish :first-publish true}
    :refs [{:at [:object-type] :targets #{:object-types :interfaces}}
           {:at [:collection] :targets #{:collections}}
           project-ref repository-ref]
    :name-refs [{:at [:layout :export :object-type] :targets object-targets}
                {:at [:layout :upload :dataset] :targets #{:datasets}}]
    :fields (merge {:name        {:author-only true :type :keyword}
                    :source-file {:author-only true :type :string}
                    :display-name {:present :seq :type :string}
                    :description {:present :seq :type :string}
                    :icon        {:present :seq :type :string}
                    :layout      {:present :seq :type [:map-of :keyword :any]}
                    :source      {:present :seq :type :string}
                    :promoted    {:present :some :coerce :bool :type :boolean}
                    :collection  {:present :some :in :kw :type :ent/ref-id}
                    :tags        {:present :seq :in :kws :type [:vector :keyword]}
                    :owner       {:present :seq :type :string}
                    :thumbnail   {:present :seq :type :string}
                    :form-factor {:present :some :coerce :kw
                                  :type [:enum :full :panel :panel-set :mobile]}
                    :sidebar     {:present :seq :type [:vector [:map-of :keyword :any]]}
                    :auto-publish {:present :some :coerce :bool :type :boolean}
                    :live        {:present :some
                                  :type [:or :boolean
                                         [:map {:closed true}
                                          [:types {:optional true} [:vector :ent/ref-id]]
                                          [:object-sets {:optional true} [:vector :any]]
                                          [:min-seconds {:optional true} number?]]]}
                    :object-type {:present :some :in :kw :type :ent/ref-id}}
                   published-history owned in-repo)}

   {:tag :template :sweep-rank 0 :section :templates :key :rid :id-match? true :tree :repo
    :about "A document template (source file, engine, format) rendered from ontology data."
    :export {:scope :project :into :units}
    :regime :declared
    :versions {:mode :release :compat :template :first-publish true}
    :refs [{:at [:parameters] :each-val [{:at [:object-type] :targets #{:object-types :interfaces}}
                                         {:at [:object-set]  :targets #{:object-sets}}]}
           project-ref repository-ref]
    :fields (merge {:name        {:author-only true :type :keyword}
                    :source-file {:author-only true :type :string}
                    :title       {:present :seq :type :string}
                    :description {:present :seq :type :string}
                    :icon        {:present :seq :type :string}
                    :engine      {:present :some :coerce :kw :default :jinja2
                                  :type [:enum :jinja2]}
                    :format      {:present :some :coerce :kw :default :html
                                  :type [:enum :html :markdown :text]}
                    :source      {:present :seq :type :string}
                    :parameters  {:present :seq :type [:map-of :keyword :ent/template-param]}
                    :page        {:present :map :compare :eq :type :ent/template-page}
                    :targets     {:present :seq :coerce :kws :type [:vector :keyword]}
                    :limits      {:present :map :compare :eq :type :ent/template-limits}
                    :status      {:present :some :coerce :kw :type [:enum :draft :published]}
                    :version     {:present :some :type :string}
                    :releases    {:present :seq :type [:map-of :string :any]}}
                   owned in-repo)}

   {:tag :custom-application :sweep-rank 0 :section :custom-applications
    :about "An app with its own client code, served by the app host under a scope."
    :export {:scope :project}
    :key :rid :id-match? true :tree :ontology :regime :declared
    :refs [{:at [:scope :object-types] :many true :targets #{:object-types :interfaces}}
           {:at [:scope :actions] :many true :targets #{:actions}}
           {:at [:scope :functions] :many true :targets #{:functions}}
           project-ref repository-ref]
    :fields (merge {:name         {:author-only true :type :keyword}
                    :display-name {:present :seq :type :string}
                    :description  {:present :seq :type :string}
                    :status       {:present :some :coerce :kw
                                   :type [:enum :draft :published]}
                    :client       {:present :seq :type :ent/oauth-client}
                    :scope        {:present :seq :type :ent/app-scope}
                    :sdk          {:present :seq :type :ent/app-sdk}
                    :permissions  {:present :seq :type :ent/app-permissions}
                    :hosting      {:present :seq :type :ent/app-hosting}
                    :metrics      {:present :some :coerce :bool :type :boolean}
                    :client-id    {:present :some :author-form :minted :type :string}}
                   owned in-repo)}

   {:tag :collection :section :collections :key :rid :id-match? true :tree :ontology
    :about "An ordered collection of resources for navigation."
    :export {:scope :project}
    :regime :declared
    :refs [project-ref]
    :fields (merge {:name        {:author-only true :type :string}
                    :description {:present :seq :type :string}
                    :icon        {:present :seq :type :string}
                    :order       {:present :some :type :int}}
                   owned)}

   {:tag :workspace :section :workspaces :key :rid :id-match? true :tree :ontology :regime :declared
    :about "A workspace: a named entry point that groups what a team works with."
    :export {:scope :project}
    :refs [{:at [:apps] :many true :targets #{:applications}}
           {:at [:anchored] :many true :targets #{:applications}}
           {:at [:discoverable] :many true :targets #{:applications}}
           {:at [:landing] :targets #{:applications}}
           {:at [:home] :each [{:at [:items]
                                :each [{:at [:app] :targets #{:applications}}
                                       {:at [:object-type] :targets #{:object-types}}
                                       {:at [:object-set] :targets #{:object-sets}}]}]}
           project-ref]
    :fields (merge {:name        {:author-only true :type :string}
                    :display-name {:present :seq :type :string}
                    :description {:present :seq :type :string}
                    :subtitle    {:present :seq :type :string}
                    :icon        {:present :seq :type :string}
                    :logo        {:present :seq :type :string}
                    :logo-size   {:present :seq
                                  :type [:map {:closed true}
                                         [:width {:optional true} pos-int?]
                                         [:height {:optional true} pos-int?]]}
                    :order       {:present :some :type :int}
                    :promoted    {:present :some :coerce :bool :type :boolean}
                    :nav-out     {:present :some :coerce :bool :type :boolean}
                    :landing     {:present :some :in :kw :type :ent/ref-id}
                    :landing-params {:present :seq :type [:map-of :keyword :any]}
                    :apps        {:present :seq :in :kws :type [:vector :ent/ref-id]}
                    :anchored    {:present :seq :in :kws :type [:vector :ent/ref-id]}
                    :discoverable {:present :seq :in :kws :type [:vector :ent/ref-id]}
                    :home        {:present :seq :type [:vector :ent/workspace-panel]}}
                   owned)}

   {:tag :pipeline :sweep-rank 4 :section :pipelines :key :rid :id-match? true :tree :repo
    :about "A group of transforms built as one unit with declared imports and outputs."
    :export {:scope :project :into :units}
    :regime :declared
    :refs [{:at [:imports] :many true :targets #{:datasets :object-types}}
           {:at [:outputs] :many true :targets #{:datasets :object-types}}
           project-ref
           repository-ref]
    :fields (merge {:description {:present :seq :type :string}
                    :kind        {:present :some :coerce :kw :type [:enum :batch :streaming]}
                    :imports     {:present :seq :in :kws :type [:vector :ent/ref-id]}
                    :outputs     {:present :seq :in :kws :type [:vector :ent/ref-id]}}
                   (lifecycle :ent/resource-status) owned in-repo)}

   {:tag :dataset :sweep-rank 8 :section :datasets :key :rid :id-match? true :tree :ontology
    :about "A table: raw data from a source or the result of a transform."
    :sweep-only :standalone-or-virtual :rows-of :self :renamed-with-code? true
    :export {:scope :project :kinds #{:standalone :virtual :materialization}}
    :structural? true
    :regime :declared
    :branching {:instance-via {:section :transforms :at [:output :dataset]}
                :instance-authored :external-source}
    :refs [{:at [:object-type] :targets #{:object-types}}
           {:at [:restricted-view :dataset] :targets #{:datasets}}
           project-ref]
    :name-refs [{:at [:virtual :source] :targets #{:sources}}
                {:at [:external-source :datasource] :targets #{:sources}}]
    :fields (merge {:kind            {:present :some :coerce :kw :type :keyword}
                    :markings        {:present :some :coerce :kws :compare :eq
                                      :type [:vector :keyword]}
                    :columns         {:present :seq :coerce :vec
                                      :type [:vector [:map {:closed false}
                                                      [:attribute-name :keyword]
                                                      [:attribute-type :any]]]}
                    :title           {:present :seq :type :string}
                    :description     {:present :seq :type :string}
                    :object-type     {:present :some :in :kw :type :ent/ref-id}
                    :properties      {:present :seq :coerce :vec :type [:vector :keyword]}
                    :former-names    {:present :seq :coerce :vec :compare :lenient
                                      :type [:vector :keyword]}
                    :stream          {:present :map :compare :eq :type :ent/stream}
                    :virtual         {:present :map :compare :eq :type :ent/virtual}
                    :restricted-view {:present :map :compare :eq :type :ent/restricted-view}
                    :external-source {:custom true :type :ent/ext-source}}
                   (lifecycle :ent/resource-status) owned)}

   {:tag :media-set :sweep-rank 9 :section :media-sets :key :rid :id-match? true
    :about "A set of files (documents, images) that transforms and functions read like rows."
    :export {:scope :project}
    :tree :ontology :regime :declared
    :branching {:instance-via {:section :transforms :at [:output :media-set]}
                :instance-authored :sync}
    :refs [{:at [:source] :targets #{:sources}} project-ref]
    :fields (merge {:title       {:present :seq :compare :lenient :type :string}
                    :description {:present :seq :compare :lenient :type :string}
                    :rationale   {:present :seq :compare :lenient :type :string}
                    :markings    {:present :some :coerce :kws :compare :eq
                                  :type [:vector :keyword]}
                    :formats     {:present :seq :coerce :vec :in :kws :compare :lenient
                                  :type [:vector :keyword]}
                    :retention   {:present :map :compare :eq
                                  :type [:map {:closed true} [:days :int]]}
                    :source      {:present :some :in :kw :type :ent/ref-id}
                    :subfolder   {:present :seq :compare :eq :type :string}
                    :virtual     {:present :some :coerce :bool :compare :eq :type :boolean}
                    :sync        {:present :map :compare :eq :type :ent/media-set-sync}}
                   (lifecycle :ent/resource-status) owned)}

   {:tag :export :sweep-rank 10 :section :exports :key :rid :id-match? true
    :about "A dataset written out to a destination (a source) in a given mode."
    :export {:scope :project}
    :tree :ontology :regime :declared
    :refs [{:at [:source] :targets #{:sources}}
           {:at [:dataset] :targets #{:datasets}}
           project-ref]
    :fields (merge {:source      {:present :some :in :kw :required true :type :ent/ref-id}
                    :dataset     {:present :some :in :kw :required true :type :ent/ref-id}
                    :destination {:present :map :compare :eq :type :ent/export-destination}
                    :mode        {:present :some :coerce :kw :compare :eq :type :ent/export-mode}
                    :title       {:present :seq :compare :lenient :type :string}
                    :description {:present :seq :compare :lenient :type :string}
                    :rationale   {:present :seq :compare :lenient :type :string}}
                   (lifecycle :ent/resource-status) owned)}

   {:tag :graph :section :graphs :key :rid :id-match? true :regime :view
    :about "A saved lineage or object graph view on a branch."
    :branching {:instance-when {:at [:mode] :is :lineage}}
    :refs [project-ref]
    :fields (merge {:title             {:present :seq :type :string}
                    :mode              {:present :some :coerce :kw :type :keyword}
                    :area              {:present :some :type :string}
                    :description       {:present :seq :type :string}
                    :branch            {:present :seq :type :string}
                    :fallback-branches {:present :seq :coerce :vec :type [:vector :string]}
                    :visible-types     {:present :some :type :ent/graph-scope}
                    :payload           {:present :some :type :ent/graph-payload}}
                   owned)}

   {:tag :role :sweep-rank 2 :section :roles :key :keyword :tree :ontology :regime :declared-on-main
    :about "A role: a named bundle of operations granted on projects."
    :export {:scope :all}
    :branching :instance :audit :access
    :refs []
    :fields {:operations  {:present :seq :coerce :kws :compare :eq :type [:vector :keyword]}
             :includes    {:present :some :coerce :kw :type :keyword}
             :description {:present :some :type :string}}}

   {:tag :marking-definition :sweep-rank 2 :section :markings :key :keyword :tree :ontology
    :about "A marking: a label on data that only cleared principals may read."
    :export {:scope :all}
    :regime :declared-on-main :audit :access
    :branching :instance
    :refs []
    :fields {:category    {:present :some :type :string}
             :rank        {:present :some :type :int}
             :disjunctive {:present :truthy :compare :bool :type :boolean}
             :required-on-projects {:present :truthy :compare :bool :type :boolean}
             :cleared     {:present :seq :coerce :sorted-strs :compare :eq :type [:vector :string]}
             :remove-access {:present :seq :coerce :sorted-strs :compare :eq
                             :type [:vector :string]}
             :description {:present :some :type :string}}}

   {:tag :projection :sweep-rank 2 :section :projections :key :keyword :tree :ontology
    :about "A projection: which texts make up the ontology, what each holds and how it is written."
    :export {:scope :all}
    :regime :declared
    :refs []
    :fields {:filename    {:present :seq :required true :type [:map-of :keyword :any]}
             :select      {:present :seq :type [:map-of :keyword :any]}
             :edits       {:present :seq :coerce :kws
                           :type [:vector [:enum :create :update :delete]]}
             :elements    {:present :seq :type [:map-of :keyword [:map-of :keyword :keyword]]}
             :description {:present :some :type :string}}}

   {:tag :eval-suite :sweep-rank 2 :section :eval-suites :key :keyword :tree :ontology
    :about "A suite of evaluation cases run against a function, agent or action."
    :export {:scope :owned}
    :regime :declared
    :refs [project-ref]
    :fields (merge {:description     {:present :some :type :string}
                    :function        {:present :some :type :any}
                    :agent           {:present :some :type :any}
                    :action          {:present :some :type :any}
                    :iterations      {:present :some :type pos-int?}
                    :parallel        {:present :some :type pos-int?}
                    :input-map       {:present :seq  :type [:map-of :keyword :any]}
                    :cases           {:present :seq  :coerce :vec :type [:vector :any]}
                    :cases-from      {:present :seq  :coerce :vec :type [:vector :any]}
                    :evaluators      {:present :seq  :coerce :vec :type [:vector :any]}
                    :results-dataset {:present :some :type :any}
                    :experiment      {:present :seq  :type [:map-of :keyword :any]}}
                   owned)}

   {:tag :action :sweep-rank 2 :section :actions :key :rid :id-match? true :tree :ontology
    :about "A change users or code may make, e.g. create order: parameters, submission rules and steps."
    :export {:scope :all}
    :regime :declared
    :api-name true
    :refs []
    :name-refs action-author-refs
    :fields (merge {:project             {:present :some :type :keyword}
                    :display-name        {:present :some :type :string}
                    :description         {:present :some :type :string}
                    :visibility          {:present :some :coerce :kw :type :ent/visibility}
                    :rules               {:present :seq :coerce :vec :type [:vector :ent/step]}
                    :params              {:present :seq
                                          :type [:map-of :ent/api-name :ent/param-spec]}
                    :form                {:present :seq :type :ent/action-form}
                    :submission-criteria {:present :seq :coerce :vec
                                          :type [:vector :ent/submission-criterion]}
                    :edits               {:present :seq :coerce :vec :type [:vector :any]}
                    :on-branch           {:present :seq :type :ent/on-branch}
                    :on                  {:present :some :type :keyword}
                    :revision            {:present :some :type pos-int?}}
                   (lifecycle :ent/resource-status))}

   {:tag :object-set :sweep-rank 3 :section :object-sets :key :rid :id-match? true
    :about "A saved query over an object type: filter, selection, grouping and order."
    :export {:scope :project}
    :tree :ontology :regime :declared
    :refs [{:at [:root]                  :targets #{:object-types :interfaces}}
           {:at [:include] :each [{:at [:link] :targets #{:link-types}}]}
           project-ref]
    :fields (merge {:description {:present :seq  :type :string}
                    :root        {:present :some :type :ent/ref-id}
                    :param-slots {:present :some :coerce :vec :type [:vector :any]}
                    :where       {:present :some :type [:vector :any]}
                    :select      {:present :seq  :coerce :vec :type [:vector :keyword]}
                    :aggregate   {:present :some :type :any}
                    :group-by    {:present :seq  :coerce :vec :type [:vector :any]}
                    :accuracy    {:present :some :coerce :kw :type :keyword}
                    :order-by    {:present :some :type :keyword}
                    :sort-dir    {:present :some :coerce :kw :type :keyword}
                    :limit       {:present :some :type number?}
                    :offset      {:present :some :type number?}
                    :include     {:present :seq  :coerce :vec :type [:vector :any]}}
                   owned)}

   {:tag :agent :sweep-rank 1 :section :agents :key :rid :tree :ontology :regime :declared
    :about "An LLM agent: model, prompt and the tools it may call, with versions."
    :export {:scope :project}
    :versions {:mode :publish}
    :refs [project-ref]
    :name-refs [{:at [:source] :targets #{:sources}}
                {:at [:model] :targets #{:llm-models}}
                {:at [:retrieval :object-type] :targets object-targets}
                {:at [:tools] :each [{:at [:action] :targets #{:actions}}
                                     {:at [:function] :targets #{:functions}}
                                     {:at [:object-type] :targets object-targets}]}]
    :fields (merge {:display-name  {:present :seq :type :string}
                    :description   {:present :seq :type :string}
                    :model         {:present :seq :type :string}
                    :temperature   {:present :some :type number?}
                    :system-prompt {:present :seq :type :string}
                    :source        {:present :some :coerce :kw :type :keyword}
                    :writes        {:present :some :coerce :kw :type [:enum :propose :apply :dry]}
                    :max-turns     {:present :some :type pos-int?}
                    :tools         {:present :seq :coerce :vec :type [:vector :ent/agent-tool]}
                    :retrieval     {:present :seq :type :ent/agent-retrieval}
                    :status        {:present :some :coerce :kw :type :ent/status}}
                   published-history owned)}

   {:tag :llm-model :section :llm-models :key :string :branching :instance :regime :declared-on-main
    :about "A language model the instance can call: provider, context window and prices."
    :refs []
    :name-refs [{:at [:source] :targets #{:sources}}]
    :fields {:in             {:present :some :required true :type number?}
             :out            {:present :some :required true :type number?}
             :currency       {:present :some :type :string}
             :deprecated     {:present :some :coerce :bool :type :boolean}
             :description    {:present :seq :coerce :str :type :string}
             :context-window {:present :number :coerce :long :type number?}
             :source         {:present :some :coerce :kw :type :keyword}
             :provider       {:present :some :coerce :kw :type :keyword}
             :provider-model {:present :seq :coerce :str :type :string}
             :capabilities   {:present :seq :coerce :vec
                              :type [:vector
                                     [:enum :tool-calling :vision :reasoning :structured-output]]}
             :default        {:present :some :coerce :bool :type :boolean}}}])

(def by-tag     (coll/index-by :tag entities))
(def by-section (coll/index-by :section entities))

(def sections (mapv :section entities))

(def access-sections
  (into #{} (comp (filter #(= :access (:audit %))) (map :section)) entities))
(def rid-sections (into #{} (comp (filter #(= :rid (:key %))) (map :section)) entities))

(def normalized-name-sections
  #{:organizations :spaces :projects :repositories :collections :workspaces :sources})

(defn edit-function? [f] (some? (:source f)))

(defn name-field [section]
  (keyword (str (name (:tag (by-section section))) "-name")))

(def section->tag (into {} (map (juxt :section :tag)) (filter #(= :rid (:key %)) entities)))
(def tag->section (into {} (map (juxt :tag :section)) entities))
(def author->section (into {} (map (juxt #(or (:author %) (:section %)) :section)) entities))
(def section->author (into {} (map (juxt :section #(or (:author %) (:section %)))) entities))

(def fields (into {} (map (juxt :section :fields)) entities))

(def decorator-fields
  [:inputs :output :outputs :media-outputs :primary-key :column-map :external-systems :parameters
   :incremental :semantic-version :done-key :max-per-run :prompt-key :allowed-run-seconds
   :container])

(def output-ontology-fields
  [:role])

(defn axis
  [section a]
  (get-in by-section [section :axes a] []))

(def whole-declaration
  (into (sorted-map)
        (for [{:keys [section axes]} entities :when axes]
          [section (vec (sort (mapcat val axes)))])))

(defn projected-fields [section]
  (into {} (remove (comp :author-only val)) (get fields section)))

(defn forwarded-fields [tag]
  (let [{:keys [fields step-marks]} (by-tag tag)]
    (into [:name]
          (concat (keep (fn [[k v]] (when-not (:author-only v) k)) fields)
                  step-marks))))

(def instance-sections
  (into #{} (comp (filter #(= :instance (:branching %))) (map :section)) entities))

(defn instance-section? [section] (contains? instance-sections section))

(def structural-sections
  (into #{} (comp (filter :structural?) (map :section)) entities))

(def ^:private entry-rules
  (into {} (keep (fn [{:keys [section branching]}] (when (map? branching) [section branching])))
        entities))

(defn instance-entry?
  [section entry]
  (when-let [{:keys [at is]} (get-in entry-rules [section :instance-when])]
    (= is (get-in entry at))))

(defn instance-via
  [section entry]
  (when (instance-entry? section entry)
    (for [[s {{from :section at :at} :instance-via}] entry-rules
          :when (= from section)
          :let [k (get-in entry at)]
          :when (some? k)]
      [s k])))

(defn instance-keys
  [W]
  (let [direct (into {} (for [[section {:keys [instance-when]}] entry-rules
                              :when instance-when]
                          [section
                           (into #{} (keep (fn [[k e]] (when (instance-entry? section e) k)))
                                 (get W section))]))
        via    (into {} (for [[section {{from :section at :at} :instance-via}] entry-rules
                              :when from]
                          [section
                           (into #{} (keep #(get-in W (into [from %] at))) (get direct from))]))]
    (merge (zipmap instance-sections (repeat :all)) direct via)))

(defn union-keys
  [a b]
  (merge-with (fn [x y] (if (or (= :all x) (= :all y)) :all (into x y))) a b))

(defn instance-view
  [ks W]
  (into {} (for [[s k] ks :let [v (get W s)] :when (some? v)]
             [s (if (= :all k) v (select-keys v k))])))

(defn authored-instance?
  [section entry]
  (boolean (or (instance-section? section)
               (when-let [f (get-in entry-rules [section :instance-authored])]
                 (some? (get entry f)))
               (instance-entry? section entry))))

(def mutable (vec (remove #(false? (:mutable? %)) entities)))

(def text-sections
  (into #{} (comp (filter #(and (= :ontology (:tree %)) (= :rid (:key %))
                                (not-any? :custom (vals (:fields %)))))
                  (map :section))
        entities))

(def sweep-order
  (mapv (juxt :tag #(or (:author %) (:section %)))
        (sort-by :sweep-rank (filterv :sweep-rank entities))))

(defn sweep-only [author] (get-in by-section [(author->section author) :sweep-only]))

(def refs (into {} (comp (filter #(= :rid (:key %))) (map (juxt :section :refs))) entities))

(defn- entry-author-refs
  [{:keys [refs name-refs fields]}]
  (let [spec (into (vec refs) name-refs)
        at?  (fn [p] (some #(= p (:at %)) spec))]
    (cond-> spec
      (and (contains? fields :project) (not (at? [:project])))       (conj project-ref)
      (and (contains? fields :repository) (not (at? [:repository]))) (conj repository-ref))))

(defn- in-tuple
  [cols spec]
  (let [pos (zipmap cols (range))]
    (mapv (fn [{:keys [at] :as slot}]
            (assoc slot :at (if-let [i (pos (first at))]
                              (into [i] (rest at))
                              (into [(count cols)] at))))
          spec)))

(def author-refs
  (into {} (for [e entities]
             [(or (:author e) (:section e))
              (cond->> (entry-author-refs e)
                (:author-tuple e) (in-tuple (:author-tuple e)))])))

(def author-body-refs
  (into [{:at [:objects] :each-key {:targets #{:object-types}
                                    :if      {:has :attributes
                                              :only-keys #{:attributes :project}}}}]
        (for [e     entities
              :let  [a (or (:author e) (:section e)) spec (author-refs a)]
              :when (seq spec)]
          (if (:author-tuple e) {:at [a] :each spec} {:at [a] :each-val spec}))))

(defn sections-of-tree [tree]
  (into #{} (comp (filter #(= tree (:tree %))) (map #(or (:author %) (:section %)))) entities))

(def regimes
  {:declared {:label "geprüfte Deklaration"
              :undo  "Vorschlag ablehnen, oder den Zweig nicht mergen"
              :why   (str "Andere lesen es als Versprechen, und ein Bruch zeigt sich weit weg von"
                          " der Änderung. Darum hält ein Text es, ein Zweig trägt es, ein Vorschlag"
                          " entscheidet, und zusammengeführt wird je Element — damit zwei Autoren"
                          " desselben Typs sich nicht überschreiben.")}
   :declared-on-main
   {:label "geprüfte Deklaration, nur am Stamm"
    :undo  "Vorschlag ablehnen; ein Zweig konnte es nie ändern"
    :why   (str "Wie :declared, mit einer Ausnahme, die zählt: ein Zweig TRÄGT es nicht."
                " Es gilt instanzweit — ein Zweig liest es"
                " von main und ändert es nie —, denn ein Zweig, der ein anderes Projekt"
                " oder eine andere Rolle behauptet als main, hätte dieselbe Frage zweimal"
                " verschieden beantwortet. Eine Tür, die es auf einem Zweig anbietet,"
                " meldet einen Commit für einen Schreibvorgang, der ins Leere läuft.")}
   :view     {:label "Ansicht"
              :undo  "neu zeichnen"
              :why   (str "Eine Zeichnung über dem, was deklariert ist. Sie verspricht niemandem"
                          " etwas, eine Freigabe wäre Zeremonie. Was sie braucht, ist die Regel,"
                          " nicht zu überleben, was sie zeigt.")}
   :setting  {:label "lebende Einstellung"
              :undo  "neu setzen; das Protokoll sagt, dass gewechselt wurde"
              :why   (str "Ein Wert dieser Instanz, nicht des Modells. Ein Zweig wäre eine Lüge:"
                          " dieselbe Frage hätte zwei Antworten, je nachdem wer fragt.")}
   :own-history {:label "eigene Versionskette"
                 :undo  "auf eine frühere Version zurückstellen"
                 :why   (str "Inhalt, den jemand schreibt. Niemand muss eine Notiz genehmigen, also"
                             " wäre eine Freigabe falsch; was zählt, ist der eigene Verlauf und das"
                             " Einfrieren — ein ausgedruckter Stand muss bleiben, was er war.")}
   :transactional {:label "Transaktionskette"
                   :undo  "auf einen Lauf zurückrollen, oder den nächsten abwarten"
                   :why   (str "Zeilen fallen in Mengen an. Eine Freigabe wäre Unsinn — niemand"
                               " genehmigt 50.000 Zeilen. Gebraucht wird „wie sah es gestern aus“"
                               " und „den letzten Lauf zurück“, dazu eine Aufbewahrungsfrist.")}
   :content-addressed {:label "inhaltsadressiert"
                       :undo  "den alten Fingerabdruck wieder referenzieren"
                       :why   (str "Der Fingerabdruck des Inhalts IST die Version: gleiche Bytes"
                                   " liegen einmal, und unter einem Verweis kann sich nichts"
                                   " heimlich ändern. Offen bleibt nur, ob noch jemand darauf"
                                   " zeigt — daher das Aufräumen statt eines Löschens.")}
   :log-only {:label "nur Protokoll"
              :undo  "die Quelle zurückrollen und neu aufbauen; Edits einzeln über das Protokoll"
              :why   (str "Was man sieht, ist eine schnelle Kopie; die Wahrheit ist die Quelle plus"
                          " die eingetragenen Änderungen. Eine Kopie zu versionieren würde"
                          " behaupten, die Wahrheit liege in ihr — also wird protokolliert, wer"
                          " wann was geändert hat, und genau das braucht eine Korrektur.")}})

(def reviewed
  #{:declared :declared-on-main})

(def section->regime (into {} (map (juxt :section :regime)) entities))

(defn reviewed-section?
  [section]
  (contains? reviewed (section->regime section)))

(def regime-verdicts
  {:no-regime                 "names no regime — nothing says who may say no to a change"
   :unknown-regime            "names a regime this table does not know"
   :no-text-for-a-contract
   ":declared, but no :tree holds it — a verb reaches it, no proposal sees it"
   :text-without-review
   "a text holds it, so branch and proposal cover it, while its regime says otherwise"
   :may-outlive-what-it-shows
   ":view pointing at reviewed resources — nothing retires it when they go"
   :branch-cannot-carry-it
   ":declared promises a branch carries it; :branching :instance says a branch never changes it"
   :branched-after-all
   ":declared-on-main is for what only the trunk carries, and this one is branched — review on a branch is the stronger door and it is available"})

(defn regime-matrix
  []
  (vec (for [e (sort-by (comp str :section) entities)]
         {:section   (:section e)
          :regime    (:regime e)
          :tree      (:tree e)
          :branching (:branching e)
          :points-at (vec (sort (for [r (:refs e) t (:targets r)
                                      :when (reviewed (section->regime t))]
                                  t)))})))

(defn regime-gaps
  [matrix]
  (vec (for [{:keys [regime tree points-at branching] :as row} matrix
             :let [v (cond
                       (nil? regime)                              :no-regime
                       (not (contains? regimes regime))                     :unknown-regime
                       (and (reviewed regime) (nil? tree))        :no-text-for-a-contract
                       (and (not (reviewed regime)) (some? tree)) :text-without-review
                       (and (= :declared regime)
                            (= :instance branching))              :branch-cannot-carry-it
                       (and (= :declared-on-main regime)
                            (not= :instance branching))           :branched-after-all
                       (and (= :view regime) (seq points-at))     :may-outlive-what-it-shows)]
             :when v]
         (assoc row :verdict v :why (regime-verdicts v)))))

(defn regime-report
  []
  (vec (for [{:keys [section regime]} (regime-matrix)
             :let [r (get regimes regime)]]
         {:resource section :regime regime
          :label (:label r) :why (:why r) :undo (:undo r)})))

(def ^:private regime-gaps-now (regime-gaps (regime-matrix)))

(tbl/holds! "every entity names a regime this table knows"
            (mapv :section (filter #(#{:no-regime :unknown-regime} (:verdict %)) regime-gaps-now)))

(tbl/holds! "a text holds only what review covers"
            (mapv :section (filter #(= :text-without-review (:verdict %)) regime-gaps-now)))

(tbl/holds! "a regime says WHERE a change is carried, and :branching says the same"
            (mapv (juxt :section :verdict)
                  (filter #(#{:branch-cannot-carry-it :branched-after-all} (:verdict %))
                          regime-gaps-now)))

(tbl/holds! "a tag and a section each name one entity"
            (for [[x n] (frequencies (concat (map :tag entities) sections)) :when (> n 1)] x))

(tbl/holds! "every field says what it holds; an untyped one is silently unauthorable"
            (for [[s fs] fields [k v] fs :when (not (:type v))] [s k]))

(tbl/holds! "a ref targets a section that exists"
            (let [targets (fn walk [rs]
                            (mapcat (fn [{:keys [targets each each-val]}]
                                      (concat targets (walk each) (walk each-val)))
                                    rs))]
              (remove (set sections) (targets (mapcat :refs entities)))))

(def section-roles
  {:buildable          #{:transforms :object-types :datasets}
   :job-spec           #{:datasets :object-types :link-types}
   :container          #{:organizations :spaces :projects}
   :widens-access      #{:organizations :spaces :projects :transforms}
   :lineage-referenced #{:datasets :object-types :link-types :actions :functions}
   :soft-referrer      #{:applications :custom-applications :object-type-groups :pipelines}
   :dangles-freely     #{:object-type-groups :pipelines}
   :ontology-phase     #{:object-types :interfaces :link-types :actions}
   :carries-datasets   #{:object-types :transforms}})

(defn sections-in
  [role]
  (get section-roles role #{}))

(defn in-role?
  [role section]
  (contains? (sections-in role) section))

(tbl/holds! "a section role names known sections"
            (for [[role ss] section-roles s ss :when (not (contains? (set sections) s))]
              [role s]))
