(ns sutra.foundation.contracts.b)

(def ^:private nm [:or :string :keyword])

(def ^:private body-of
  (fn [& fields]
    [:maybe (into [:map] (map (fn [[k s]] [k {:optional true} s])) (partition 2 fields))]))

(def registry
  {
   ::slice-pull
   [:map
    [:nodes :any]
    [:projections [:map-of nm [:map [:files [:map-of :string :string]]]]]
    [:code {:optional true} [:maybe [:map-of nm [:map-of :string :map]]]]]
   ::slice-push-in
   (body-of :projections :any :code :any :preview? :any :around :any :message :any
            :resolve :any)
   ::slice-push
   [:map
    [:projections :any]
    [:code [:sequential [:map [:repo :any] [:path :string]]]]
    [:proposal {:optional true} [:maybe [:map [:id :any]]]]
    [:commits {:optional true} :any]
    [:code-refused {:optional true} :any]
    [:inputs-changed {:optional true} :any]]

   ::container-image
   [:map
    [:name :string] [:tag :string] [:project :any] [:id :any]
    [:user {:optional true} :any] [:env {:optional true} :any]
    [:working-dir {:optional true} :any]
    [:layers [:sequential :string]] [:size :any]
    [:pushed-by :any] [:pushed-at :any]]
   ::container-images [:map [:images [:sequential ::container-image]]]
   ::container-image-push-in (body-of :image :any :config :any :layers :any :project :any)
   ::container-image-pushed [:map [:image ::container-image]]
   ::container-image-one [:map [:image ::container-image]]
   ::container-image-deleted [:map [:deleted :boolean]]

   ::feedback-session
   [:map
    [:id :string] [:benutzer :string] [:begonnen :string]
    [:aufgabe {:optional true} :any]
    [:beendet {:optional true} :any]
    [:erledigt {:optional true} :any]
    [:erledigt-von {:optional true} :any]
    [:fazit {:optional true} :any]
    [:verlauf {:optional true} [:sequential :map]]]
   ::feedback-read
   [:or
    [:map [:sitzungen [:sequential ::feedback-session]]]
    [:map [:sitzung ::feedback-session] [:verlauf [:sequential :map]]
     [:zusammenfassung :map]]]
   ::feedback-start-in (body-of :aufgabe :any)
   ::feedback-events-in (body-of :sitzung :any :ereignisse :any)
   ::feedback-events [:map [:angenommen :int] [:abgelehnt [:sequential :any]]]
   ::feedback-finish-in (body-of :sitzung :any :fazit :any)
   ::feedback-settle-in (body-of :sitzung :any :erledigt :any)

   ::lineage-status
   [:map [:last-build :any] [:last-logic-change :any] [:nodes [:map-of :any :map]]]
   ::lineage-facts
   [:map
    [:anchors [:sequential :any]] [:nodes [:map-of :any :any]]
    [:edges [:sequential :map]] [:transforms [:sequential :map]] [:beyond :any]]
   ::lineage-graph
   [:or
    [:map [:transforms [:sequential :map]] [:action-edits :any] [:app-uses :any]
     [:backings [:sequential :any]]]
    [:map [:transform :string] [:inputs :any] [:outputs :any]]
    :map]
   ::lineage-dependents
   [:or
    [:map [:object-type :any] [:unknown? :boolean]]
    [:map [:object-type :any] [:links :any] [:transforms :any] [:actions :any] [:apps :any]
     [:total :int]]]
   ::lineage-related
   [:map [:node :string] [:kind :string]
    [:items [:sequential [:map [:kind :string] [:name :any] [:via :string]]]]
    [:total :int]]
   ::lineage-rollback-in (body-of :node-kind :any :node-key :any :commit :any)
   ::lineage-rollback
   [:map [:summary [:map [:table :any] [:commit :any] [:rows-before :any] [:rows-after :any]]]]
   ::schedule-candidates
   [:map [:target :any] [:candidates [:sequential :map]]]

   ::linter-rules [:map [:rules [:sequential :map]]]
   ::linter-list
   [:map [:recommendations [:sequential :map]] [:by-rule [:map-of :any :int]]
    [:resolved :int]]
   ::linter-sweep [:map [:found :int] [:resolved :int] [:failed [:sequential :any]]]
   ::linter-target-in
   (body-of :rule :any :resource :any :days :any :reason :any :off :any :assignee :any)
   ::linter-target [:map [:rule :any] [:resource :any]]

   ::health-check
   [:map [:id :any] [:node-kind {:optional true} :any] [:node-key {:optional true} :any]
    [:kind {:optional true} :any] [:severity {:optional true} :any]]
   ::health-checks [:map [:checks [:sequential ::health-check]]]
   ::health-check-in
   (body-of :id :any :name :any :node-kind :any :node-key :any :kind :any :query :any
            :expects :any :enabled :any :target :any)
   ::health-result
   [:map [:check-id :any] [:status [:maybe :string]] [:value {:optional true} :any]
    [:detail {:optional true} :any]]
   ::health-results [:map [:results [:sequential :map]]]
   ::health-deleted [:map [:deleted :any]]
   ::health-watch-in (body-of :level :any)
   ::health-watch [:map [:watching [:maybe :string]]]
   ::health-snooze-in (body-of :hours :any :until :any :reason :any)
   ::health-snoozed
   [:or [:map [:check-id :any] [:snoozed-until :string]]
    [:map [:check-id :any] [:snoozed :nil]]]
   ::health-groups [:map [:groups [:sequential [:map [:id :any] [:subscribers :any]]]]]
   ::health-group-in (body-of :id :any :name :any :description :any :check-ids :any)
   ::health-group [:maybe [:map [:id :any] [:name :any] [:check-ids :any]]]
   ::health-subscribe-in (body-of :every-ms :any :every :any :only-failing :any)
   ::health-subscription
   [:or [:map [:group-id :any] [:every-ms :int]]
    [:map [:group-id :any] [:subscribed :boolean]]]
   ::health-overview
   [:map [:ok :boolean] [:verdict [:enum "green" "yellow" "red"]]
    [:checks :map] [:build [:map [:running [:sequential :map]] [:last :any]]]
    [:schedules [:map [:armed :int] [:total :int] [:rows [:sequential :map]]]]
    [:node :any]]

   ::agent-tests
   [:map [:suites [:sequential
                   [:map [:name :any] [:agent :string] [:cases [:sequential :any]]
                    [:runs [:sequential :any]] [:running [:sequential :any]]
                    [:failed :any]]]]]
   ::agent-test-case-in
   (body-of :agent :any :input :any :reference :any :must-contain :any :thread :any :run :any
            :history :any)
   ::agent-test-case
   [:map [:suite :string]
    [:case [:map [:id :string] [:input :any] [:expected :map] [:from {:optional true} :map]]]]
   ::agent-test-deleted [:map [:deleted :any]]
   ::agent-test-run-in (body-of :models :any :version :any)
   ::agent-test-run [:map [:suite :any] [:started [:sequential :string]]]
   ::doc-intel-sources [:map [:sources [:sequential :any]] [:models [:sequential :any]]]
   ::doc-intel-items [:map [:items [:sequential :any]]]
   ::doc-intel-extract-in
   (body-of :source :any :kind :any :name :any :address :any :page :any :strategy :any)
   ::doc-intel-evaluate-in
   (body-of :source :any :kind :any :name :any :address :any :page :any :model :any
            :markdown :any)
   ::doc-intel-deploy-in
   (body-of :source :any :kind :any :name :any :strategy :any :repository :any)
   ::doc-intel-deployed
   [:map [:path :any] [:files [:map-of :string :any]] [:repository :string] [:branch :string]
    [:commit :any]]

   ::llm-budget
   [:map [:budget-eur number?] [:spent-today number?] [:day :any] [:zone :any]
    [:interactive-reserve-pct number?] [:batch-cap-eur number?] [:project-quotas :any]
    [:pools :any] [:source-budgets :any] [:exhausted-sources :any] [:sources :any]
    [:llm-sources [:sequential :string]] [:projects [:sequential :any]]]
   ::llm-budget-in
   (body-of :eur :any :reserve-pct :any :project :any :source :any :quota :any :resume :any)
   ::llm-providers
   [:map [:wires [:sequential :string]]
    [:projects [:sequential [:map [:rid :string] [:name :string]]]]
    [:models [:sequential [:map [:id :string]]]]
    [:sources [:sequential [:map [:name :string] [:base-url :any] [:wire :string]
                            [:has-key? :boolean] [:local? :any] [:models :any]]]]]
   ::llm-provider-models [:map [:source :any] [:models [:sequential :any]]]
   ::llm-provider-test-in (body-of :source :any :model :any)
   ::llm-provider-test
   [:or [:map [:ok [:= true]] [:ms :int] [:answer :any]]
    [:map [:ok [:= false]] [:ms :int] [:error :any]]]
   ::llm-catalog [:map [:source :any] [:models [:sequential :any]]]
   ::llm-chat-in
   (body-of :source :any :model :any :request :any :caller :any :run_id :any)
   ::llm-rates-in [:maybe :map]

   ::document
   [:map [:id :any] [:name {:optional true} :any] [:format {:optional true} :any]
    [:body {:optional true} :any] [:template {:optional true} :any]
    [:template-version {:optional true} :any] [:parameters {:optional true} :any]
    [:page {:optional true} :any] [:links {:optional true} :any]
    [:author {:optional true} :any] [:created-at {:optional true} :any]
    [:updated-at {:optional true} :any] [:project {:optional true} :any]
    [:version {:optional true} :any]]
   ::document-saved [:map [:document ::document]]
   ::documents [:map [:documents [:sequential ::document]]]
   ::document-in
   (body-of :id :any :name :any :format :any :body :any :template :any :parameters :any
            :page :any :links :any :project :any :expect-version :any :live :any :label :any)
   ::document-deleted [:map [:deleted :string]]
   ::document-version
   [:map [:seq :any] [:document-id :any] [:label {:optional true} :any]
    [:endorsed {:optional true} :any] [:author {:optional true} :any]
    [:created-at {:optional true} :any] [:body {:optional true} :any]]
   ::document-versions [:map [:versions [:sequential ::document-version]]]
   ::document-restore-in (body-of :seq :any)
   ::document-regenerate-in (body-of :latest :any)
   ::document-sheet-in (body-of :locale :any :sheets :any :body :any)
   ::document-sheet [:map [:sheets :any] [:body :any]]
   ::document-export-in (body-of :targets :any :page :any :locale :any :sheet :any)
   ::rendered
   [:map {:description "What a render made per target: :html, :pdf_b64, :xlsx_b64, :csv …"}]
   ::document-widget-in (body-of :attrs :any)
   ::document-widget [:map [:html :string]]
   ::document-templatize-in (body-of :name :any :project :any)
   ::use-case-plan-in (body-of :preview? :any)
   ::use-case-plan
   [:map [:document :any] [:summary :any] [:project :any] [:install :any] [:retire :any]
    [:preview {:optional true} :any]]
   ::template-summary
   [:map [:template-name :any] [:parameters :any] [:versions [:sequential :any]]]
   ::templates [:map [:templates [:sequential ::template-summary]]]
   ::template
   [:map [:template-name :any] [:parameters :any] [:versions [:sequential :any]]
    [:source :any] [:options :any]]
   ::template-render-in
   (body-of :draft :any :version :any :values :any :targets :any :preview :any :layout :any)
   ::template-generate-in
   (body-of :version :any :values :any :project :any :name :any :links :any)
   ::app-document-in
   (body-of :template :any :version :any :values :any :link :any :media :any :save :any
            :name :any)})
