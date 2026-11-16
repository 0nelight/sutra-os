(ns sutra.parser.step-verbs
  (:require [sutra.types.endpoint-verbs :as ev]))

(set! *warn-on-reflection* true)

(def ^:private Named [:or :keyword :string])

(def ^:private InstallArgs
  [:map {:closed true}
   [:schema  {:optional true} :map]
   [:preset  {:optional true} :string]
   [:auto-ds {:optional true} :any]
   [:strict? {:optional true} :boolean]])

(def ^:private PrincipalsArgs
  [:map {:closed true}
   [:users [:vector [:map {:closed true}
                     [:ref      {:optional true} Named]
                     [:user-id  {:optional true} Named]
                     [:username :string]
                     [:password {:optional true} [:or :string :keyword]]
                     [:external     {:optional true} :string]
                     [:auth-source  {:optional true} Named]]]]])

(def ^:private GrantArgs
  [:map {:closed true}
   [:project Named]
   [:to      Named]
   [:role    {:optional true} :keyword]
   [:revoke? {:optional true} :boolean]])

(def ^:private OrgGuestArgs
  [:map {:closed true}
   [:op           [:enum "add" "remove"]]
   [:organization Named]
   [:principal    Named]
   [:stage?       {:optional true} :boolean]])

(def ^:private ActionArgs
  [:map {:closed true}
   [:name   Named]
   [:args   {:optional true} :map]
   [:stage? {:optional true} :boolean]
   [:plan?  {:optional true} :boolean]
   [:batch  {:optional true} :string]])

(def ^:private RunFunctionArgs
  [:map {:closed true}
   [:name   Named]
   [:inputs {:optional true} :map]])

(def ^:private BranchArgs
  [:map {:closed true}
   [:name {:pos 0} Named]
   [:from {:optional true} Named]])

(def ^:private LoginArgs
  [:map {:closed true} [:as {:optional true} Named]])

(def ^:private ReviewArgs
  [:map {:closed true}
   [:title    {:optional true} :string]
   [:verbose? {:optional true} :boolean]])

(def ^:private ConfirmArgs
  [:map {:closed true} [:message {:optional true} :string]])

(def ^:private ShowArgs
  [:map {:closed true}
   [:title   {:optional true} :string]
   [:objects {:optional true} Named]])

(def ^:private QueryArgs
  [:map [:title {:optional true} :string]])

(def ^:private SeriesArgs
  [:map [:property Named]])

(def ^:private ObjectSetsArgs
  [:map {:closed true} [:delete {:optional true} :string]])

(def ^:private AutomationControl
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:do   {:optional true} [:enum :status :run :preview :pause :unpause :delete :runs
                            :proposals :accept :reject :events
                            :executions :cancel :mute :unmute :subscribe :unsubscribe]]
   [:proposal {:optional true :pos 1} :string]
   [:status   {:optional true} [:enum :open :applied :rejected :failed :running :done]]
   [:object     {:optional true} :string]
   [:ids         {:optional true} [:or :string [:vector :string]]]
   [:where       {:optional true} :string]
   [:batch-size  {:optional true} :int]
   [:parallelism {:optional true} :int]
   [:with-model {:optional true} :boolean]
   [:limit    {:optional true} :int]
   [:offset   {:optional true} :int]
   [:args     {:optional true} [:map-of :keyword :any]]])

(def ^:private ScheduleControl
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:do   {:optional true} [:enum :status :run :pause :unpause :delete]]])

(def ^:private HealthCheckRunArgs
  [:map {:closed true} [:name {:pos 0} :string]])

(def ^:private Num [:or :string number?])

(def ^:private HealthCheckSetArgs
  [:map {:closed true}
   [:name         {:pos 0 :optional true} :string]
   [:from         {:optional true} :string]
   [:on           {:optional true} :keyword]
   [:target       {:optional true} :string]
   [:kind         {:optional true} :keyword]
   [:severity     {:optional true} :keyword]
   [:column       {:optional true} :keyword]
   [:within-hours {:optional true} Num]
   [:min          {:optional true} Num]
   [:max          {:optional true} Num]
   [:comparison   {:optional true} :keyword]
   [:query        {:optional true} :string]
   [:measure      {:optional true} :keyword]
   [:tolerance    {:optional true} Num]
   [:paused?      {:optional true} :boolean]])

(def ^:private HealthCheckDeleteArgs
  [:map {:closed true} [:name {:pos 0} :string]])

(def ^:private HealthCheckWatchArgs
  [:map {:closed true}
   [:name  {:pos 0} :string]
   [:level {:optional true} :keyword]])

(def ^:private HealthCheckSnoozeArgs
  [:map {:closed true}
   [:name   {:pos 0} :string]
   [:hours  {:optional true} Num]
   [:until  {:optional true} :string]
   [:reason {:optional true} :string]])

(def ^:private HealthCheckGroupSetArgs
  [:map {:closed true}
   [:name        {:pos 0} :string]
   [:checks      :string]
   [:description {:optional true} :string]])

(def ^:private HealthCheckGroupSubscribeArgs
  [:map {:closed true}
   [:name  {:pos 0} :string]
   [:every {:optional true} :string]
   [:all?  {:optional true} :boolean]])

(def ^:private NotificationSettingsArgs
  [:map {:closed true}
   [:set    {:optional true} :string]
   [:mute   {:optional true} :string]
   [:unmute {:optional true} :string]])

(def ^:private TransformCheckArgs
  [:map {:closed true} [:name {:pos 0} :string]])

(def ^:private LineageArgs
  [:map {:closed true} [:node {:optional true :pos 0} :string]])

(def ^:private LineageFactsArgs
  [:map {:closed true}
   [:node {:optional true :pos 0} :string]
   [:up {:optional true} :string]
   [:down {:optional true} :string]
   [:to {:optional true} :string]
   [:project {:optional true} :string]
   [:graph {:optional true} :string]
   [:dead? {:optional true} :boolean]
   [:stale {:optional true} :string]
   [:blocked? {:optional true} :boolean]
   [:diff-live? {:optional true} :boolean]
   [:column {:optional true} :string]
   [:out-of-date? {:optional true} :boolean]
   [:modified? {:optional true} :boolean]
   [:by {:optional true} :string]
   [:schedules? {:optional true} :boolean]
   [:detail {:optional true} :string]])

(def ^:private GraphHandle
  :string)

(def ^:private GraphListArgs
  [:map {:closed true}
   [:mode    {:optional true} :keyword]
   [:project {:optional true} :string]
   [:area    {:optional true} :string]])

(def ^:private GraphShowArgs
  [:map {:closed true} [:graph {:pos 0} GraphHandle]])

(def ^:private SdkGenerateArgs
  [:map {:closed true}
   [:ns {:pos 0} :string]
   [:out         {:optional true} :string]
   [:ontology    {:optional true} :string]
   [:package?    {:optional true} :boolean]
   [:runtime-git {:optional true} :string]
   [:runtime-sha {:optional true} :string]
   [:app {:optional true} :string]
   [:target {:optional true} [:enum "clj" "cljs"]]])

(def ^:private SdkVerifyArgs
  [:map {:closed true} [:file {:pos 0} :string]])

(def ^:private CustomAppArgs
  [:map {:closed true}
   [:name         {:optional true :pos 0} :string]
   [:declaration? {:optional true} :boolean]])

(def ^:private WidgetSetNameArgs
  [:map {:closed true} [:name {:pos 0} :string]])

(def ^:private WidgetSetCreateArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:project :string]
   [:description {:optional true} :string]])

(def ^:private WidgetSetInitArgs
  [:map {:closed true} [:dir {:pos 0} :string]])

(def ^:private WidgetSetPublishArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:dir {:pos 1} :string]
   [:version :string]
   [:notes {:optional true} :string]])

(def ^:private WidgetSetDevArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:dir {:pos 1 :optional true} :string]
   [:stop {:optional true} :boolean]])

(def ^:private WidgetSetPullArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:dir {:pos 1} :string]
   [:version {:optional true} :string]])

(def ^:private CustomAppOutArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:out  {:optional true} :string]])

(def ^:private CustomAppMintArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:grants       {:optional true} :string]
   [:redirect     {:optional true} :string]])

(def ^:private GraphDrawArgs
  [:map {:closed true}
   [:graph    {:pos 0} GraphHandle]
   [:out      {:optional true} :string]
   [:layout   {:optional true} :keyword]
   [:richtung {:optional true} :keyword]])

(def ^:private GraphCreateArgs
  [:map {:closed true}
   [:title {:pos 0} :string]
   [:mode        {:optional true} :keyword]
   [:into        {:optional true} :string]
   [:project     {:optional true} :string]
   [:branch      {:optional true} :string]
   [:fallback    {:optional true} :string]
   [:area        {:optional true} :string]
   [:description {:optional true} :string]
   [:node        {:optional true} :string]
   [:nodes       {:optional true} [:vector :string]]
   [:ancestors   {:optional true} [:or :string :boolean]]
   [:descendants {:optional true} [:or :string :boolean]]
   [:stage?      {:optional true} :boolean]])

(def ^:private GraphSaveAsArgs
  [:map {:closed true}
   [:graph {:pos 0} GraphHandle] [:title {:pos 1} :string]
   [:into   {:optional true} :string]
   [:stage? {:optional true} :boolean]])

(def ^:private GraphRenameArgs
  [:map {:closed true}
   [:graph {:pos 0} GraphHandle] [:title {:pos 1} :string]
   [:stage? {:optional true} :boolean]])

(def ^:private GraphMoveArgs
  [:map {:closed true}
   [:graph {:pos 0} GraphHandle]
   [:into  {:optional true} :string]])

(def ^:private GraphScopeArgs
  [:map {:closed true}
   [:graph {:pos 0} GraphHandle]
   [:add    {:optional true} :string]
   [:remove {:optional true} :string]
   [:ancestors   {:optional true} [:or :string :boolean]]
   [:descendants {:optional true} [:or :string :boolean]]
   [:stage? {:optional true} :boolean]])

(def ^:private GraphBranchArgs
  [:map {:closed true}
   [:graph {:pos 0} GraphHandle]
   [:branch   {:optional true} :string]
   [:fallback {:optional true} :string]
   [:stage?   {:optional true} :boolean]])

(def ^:private GraphArrangeArgs
  [:map {:closed true}
   [:graph       {:pos 0} GraphHandle]
   [:arrangement {:pos 1} :keyword]
   [:stage?      {:optional true} :boolean]])

(def ^:private GraphDeleteArgs
  [:map {:closed true}
   [:graph {:pos 0} GraphHandle]
   [:stage? {:optional true} :boolean]])

(def ^:private RestoreArgs
  [:map {:closed true} [:kind {:pos 0} :keyword] [:name {:pos 1} :string]])

(def ^:private EditArgs
  [:map {:closed true}
   [:kind :keyword]
   [:name {:pos 0} :string]
   [:print? {:optional true} :boolean]
   [:from   {:optional true} :string]])

(def ^:private DescribeArgs
  [:map {:closed true}
   [:section  {:optional true} :keyword]
   [:sections {:optional true} [:sequential :keyword]]
   [:project  {:optional true} :string]])

(def ^:private DatasetWriteArgs
  [:map {:closed true}
   [:name    Named]
   [:rows    [:vector :map]]
   [:mode    {:optional true} [:enum :append :snapshot]]
   [:key     {:optional true} Named]
   [:message {:optional true} :string]])

(def ^:private ExploreArgs
  [:map {:closed true}
   [:text {:pos 0} :string]
   [:type {:optional true} :keyword]])

(def ^:private AroundArgs
  [:map {:closed true} [:ids [:vector Named]] [:types {:optional true} [:vector Named]]])

(def ^:private PlaceArgs
  [:map {:closed true}
   [:resources [:vector Named]]
   [:into {:optional true} Named]
   [:as   {:optional true} Named]])

(def ^:private UnplaceArgs
  [:map {:closed true} [:resources [:vector Named]]])

(def ^:private RepoUnplaceArgs
  [:map {:closed true} [:resources [:vector Named]]])

(def ^:private RepoPlaceArgs
  [:map {:closed true}
   [:repository Named]
   [:resources  [:vector Named]]])

(def ^:private FolderArgs
  [:map {:closed true}
   [:op   :keyword]
   [:name {:pos 0} Named]
   [:to   {:optional true :pos 1} Named]
   [:into {:optional true} Named]])

(def ^:private AppArgs
  [:map {:closed true} [:name {:pos 0} Named]])

(def ^:private AppDataArgs
  [:map {:closed true}
   [:name {:pos 0} Named]
   [:type     {:optional true} :keyword]
   [:limit    {:optional true} :int]
   [:scenario {:optional true} :string]
   [:vars     {:optional true} :string]])

(def ^:private TemplateArgs
  [:map {:closed true}
   [:name {:pos 0} Named]
   [:version {:optional true} :string]])

(def ^:private RenderTemplateArgs
  [:map {:closed true}
   [:name    {:optional true :pos 0} Named]
   [:file    {:optional true} :string]
   [:version {:optional true} :string]
   [:values  {:optional true} [:vector :string]]
   [:pdf     {:optional true} :boolean]
   [:preview {:optional true} :boolean]
   [:layout  {:optional true} :keyword]
   [:out     {:optional true} :string]])

(def ^:private DocumentsArgs
  [:map {:closed true}
   [:template {:optional true} Named]
   [:project  {:optional true} :string]])

(def ^:private DocumentShowArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:out {:optional true} :string]])

(def ^:private DocumentCreateArgs
  [:map {:closed true}
   [:file    :string]
   [:name    {:optional true} :string]
   [:format  {:optional true} [:enum "markdown" "html" "text" "use-case" "diagram"]]
   [:project {:optional true} :string]])

(def ^:private DocIntelRunArgs
  [:map {:closed true}
   [:source        :string]
   [:kind          {:optional true} :string]
   [:address       :string]
   [:page          {:optional true} :string]
   [:strategy-file :string]
   [:judge         {:optional true} :string]])

(def ^:private DocIntelDeployArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:source        :string]
   [:kind          {:optional true} :string]
   [:strategy-file :string]
   [:repository    :string]])

(def ^:private DocumentUpdateArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:file    {:optional true} :string]
   [:label   {:optional true} :string]
   [:endorse {:optional true} :boolean]])

(def ^:private UseCasePlanArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:preview {:optional true} :boolean]])

(def ^:private UseCaseApplyArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:retire {:optional true} :boolean]])

(def ^:private UseCaseDiagramArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:out {:optional true} :string]])

(def ^:private DocumentGenerateArgs
  [:map {:closed true}
   [:template {:pos 0} Named]
   [:values   {:optional true} [:vector :string]]
   [:version  {:optional true} :string]
   [:name     {:optional true} :string]])

(def ^:private DocumentRefreshArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:latest {:optional true} :boolean]])

(def ^:private DocumentRestoreArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:seq {:optional true} :int]])

(def ^:private DocumentExportArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:html {:optional true} :boolean]
   [:out  {:optional true} :string]])

(def ^:private DocumentTemplatizeArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:template {:pos 1} Named]
   [:project    {:optional true} Named]
   [:repository {:optional true} Named]])

(def ^:private ScenariosArgs
  [:map {:closed true} [:name {:pos 0} Named]])

(def ^:private ScenarioNewArgs
  [:map {:closed true} [:name {:pos 0} Named] [:scenario-name {:pos 1} :string]])

(def ^:private ScenarioDropArgs
  [:map {:closed true} [:name {:pos 0} Named] [:scenario {:pos 1} :string]])

(def ^:private ScenarioActionsArgs
  [:map {:closed true} [:name {:pos 0} Named] [:scenario {:pos 1} :string]])

(def ^:private ScenarioSealArgs
  [:map {:closed true} [:name {:pos 0} Named] [:scenario {:pos 1} :string]])

(def ^:private ScenarioRebuildArgs
  [:map {:closed true} [:name {:pos 0} Named] [:scenario {:pos 1} :string]
   [:force {:optional true} [:maybe :boolean]]])

(def ^:private ScenarioApplyArgs
  [:map {:closed true} [:name {:pos 0} Named] [:scenario {:pos 1} :string]
   [:again {:optional true} [:maybe :boolean]]])

(def ^:private ScenarioDuplicateArgs
  [:map {:closed true} [:name {:pos 0} Named] [:scenario {:pos 1} :string]
   [:scenario-name {:pos 2} :string]])

(def ^:private ProjectMapArgs
  [:map {:closed true}
   [:project {:optional true} Named]])

(def ^:private CheckoutArgs
  [:map {:closed true} [:name {:pos 0} Named]])

(def ^:private BlobArgs
  [:map {:closed true}
   [:hash {:pos 0} :string]
   [:out {:optional true} :string]])

(def ^:private UploadArgs
  [:map {:closed true}
   [:file    :string]
   [:dataset :string]
   [:media-column {:optional true} :string]
   [:set   {:optional true} [:map-of :keyword :string]]
   [:on-upload {:optional true} Named]
   [:args  {:optional true} [:map-of :keyword :any]]
   [:build {:optional true} [:vector :keyword]]])

(def ^:private KontenImportArgs
  [:map {:closed true}
   [:file {:pos 0} :string]
   [:jahr    {:optional true} :int]
   [:art     {:optional true} :string]
   [:dataset {:optional true} :string]])

(def ^:private RunsArgs
  [:map {:closed true}
   [:kind  {:optional true} Named]
   [:hours {:optional true} :int]
   [:limit {:optional true} :int]])

(def ^:private SettingsArgs
  [:map {:closed true}
   [:key    {:optional true :pos 0} :string]
   [:value  {:optional true :pos 1} :string]
   [:reset? {:optional true} :boolean]])

(def ^:private PlatformArgs
  [:map {:closed true}
   [:title {:optional true} :string]
   [:logo  {:optional true} Named]
   [:file  {:optional true} :string]
   [:clear {:optional true} Named]])

(def ^:private LlmArgs
  [:map {:closed true}
   [:sub       {:optional true} :string]
   [:name      {:optional true :pos 0} Named]
   [:in        {:optional true} [:or :string number?]]
   [:out       {:optional true} [:or :string number?]]
   [:currency  {:optional true} :string]
   [:effective {:optional true} :string]
   [:reason    {:optional true} :string]
   [:per       {:optional true} Named]
   [:calls?    {:optional true} :boolean]
   [:history?  {:optional true} :boolean]
   [:since     {:optional true} :string]
   [:until     {:optional true} :string]
   [:today?    {:optional true} :boolean]
   [:caller    {:optional true} :string]
   [:source    {:optional true} :string]
   [:apply?    {:optional true} :boolean]
   [:set       {:optional true} [:or :string number?]]
   [:reserve   {:optional true} [:or :string number?]]
   [:project   {:optional true} Named]
   [:quota     {:optional true} [:or :string number?]]
   [:resume?   {:optional true} :boolean]
   [:provider  {:optional true} :string]
   [:provider-model {:optional true} :string]
   [:window    {:optional true} [:or :string number?]]
   [:tool-calling? {:optional true} :boolean]])

(def ^:private WatchArgs
  [:map {:closed true}
   [:llm?    {:optional true} :boolean]
   [:build?  {:optional true} :boolean]
   [:eval?   {:optional true} :boolean]
   [:jobs?   {:optional true} :boolean]
   [:errors? {:optional true} :boolean]
   [:once?   {:optional true} :boolean]
   [:full?   {:optional true} :boolean]
   [:since   {:optional true} :int]])

(def ^:private StopArgs
  [:map {:closed true}
   [:id    {:optional true :pos 0} Named]
   [:all   {:optional true} :boolean]
   [:force {:optional true} :boolean]])

(def ^:private SsoArgs
  [:map {:closed true}
   [:sub      {:optional true} :string]
   [:realm    {:optional true} :string]
   [:kdc      {:optional true} :string]
   [:spn      {:optional true} :string]
   [:keytab   {:optional true} :string]
   [:enable?  {:optional true} :boolean]
   [:disable? {:optional true} :boolean]])

(def ^:private LdapArgs
  [:map {:closed true}
   [:sub         {:optional true} :string]
   [:url         {:optional true} :string]
   [:bind-dn     {:optional true} :string]
   [:bind-pw     {:optional true} :string]
   [:base-dn     {:optional true} :string]
   [:user-filter {:optional true} :string]
   [:realm       {:optional true} :string]
   [:group-map   {:optional true} :string]
   [:enable?     {:optional true} :boolean]
   [:disable?    {:optional true} :boolean]
   [:dry-run?    {:optional true} :boolean]
   [:disable-gone? {:optional true} :boolean]])

(def ^:private IdpArgs
  [:map {:closed true}
   [:provider      {:optional true :pos 0} :string]
   [:sub           {:optional true :pos 1} :string]
   [:settings      {:optional true} [:map-of :keyword :any]]
   [:enable?       {:optional true} :boolean]
   [:disable?      {:optional true} :boolean]
   [:dry-run?      {:optional true} :boolean]
   [:disable-gone? {:optional true} :boolean]])

(def ^:private LinksArgs
  [:map {:closed true}
   [:type  {:optional true} Named]
   [:from  {:optional true} :string]
   [:limit {:optional true} :int]
   [:all   {:optional true} :boolean]])

(def ^:private DependentsArgs
  [:map {:closed true}
   [:type Named]])

(def ^:private DiffSchemaArgs
  [:map {:closed true}
   [:preset  {:optional true} :string]
   [:presets {:optional true} [:vector :string]]])

(def ^:private EvalArgs
  [:map {:closed true}
   [:op         {:optional true} [:enum :run :save :suites :history :cases :compare :job]]
   [:suite      {:optional true :pos 0} :string]
   [:iterations {:optional true} :int]
   [:parallel   {:optional true} :int]
   [:watch?     {:optional true} :boolean]
   [:detach?    {:optional true} :boolean]
   [:again?     {:optional true} :boolean]
   [:model      {:optional true} :string]
   [:version    {:optional true} :string]
   [:meta       {:optional true} :string]
   [:grid       {:optional true} :string]
   [:experiment {:optional true} :string]
   [:failing?   {:optional true} :boolean]
   [:run        {:optional true} :string]
   [:preview?   {:optional true} :boolean]
   [:by         {:optional true} :string]
   [:diff       {:optional true} [:vector :string]]])

(def ^:private AwaitArgs
  [:map {:closed true}
   [:handle    {:pos 0} :string]
   [:timeout-s {:optional true} :int]])

(def ^:private ExplainArgs
  [:map {:closed true}
   [:topic {:optional true} Named]
   [:item {:optional true} Named]])

(def ^:private GuideArgs
  [:map {:closed true}
   [:group {:optional true :pos 0} :keyword]])

(def ^:private RunArgs
  [:map {:closed true}
   [:id {:pos 0} Named]
   [:diff?  {:optional true} :boolean]
   [:trace? {:optional true} :boolean]])

(def ^:private JobsArgs
  [:map {:closed true} [:id {:pos 0} Named]])

(def ^:private TransactionsArgs
  [:map {:closed true}
   [:dataset Named]
   [:limit {:optional true} :int]])

(def ^:private DatasetPreviewArgs
  [:map {:closed true}
   [:dataset Named]
   [:limit {:optional true} :int]])

(def ^:private StreamNameArgs
  [:map {:closed true} [:name Named]])

(def ^:private StreamPublishArgs
  [:map {:closed true}
   [:name   Named]
   [:record {:optional true} :string]
   [:file   {:optional true} :string]])

(def ^:private StreamListenUdpArgs
  [:map {:closed true}
   [:name     Named]
   [:port     :int]
   [:bind     {:optional true} :string]
   [:batch    {:optional true} :int]
   [:flush-ms {:optional true} :int]])

(def ^:private StreamTailArgs
  [:map {:closed true}
   [:name      Named]
   [:partition {:optional true} :int]
   [:from      {:optional true} :int]
   [:limit     {:optional true} :int]])

(def ^:private StreamViewArgs
  [:map {:closed true}
   [:name  Named]
   [:limit {:optional true} :int]])

(def ^:private StreamResetArgs
  [:map {:closed true}
   [:name Named]
   [:yes? {:optional true} :boolean]])

(def ^:private ConsumptionArgs
  [:map {:closed true} [:transform Named]])

(def ^:private DeltaArgs
  [:map {:closed true} [:transform Named] [:input Named]])

(def ^:private ReindexArgs
  [:map {:closed true} [:object-type Named] [:force? {:optional true} :boolean]])

(def ^:private RestorePointArgs
  [:map {:closed true}
   [:label {:optional true} :string]
   [:to    {:optional true} :int]
   [:yes?  {:optional true} :boolean]])

(def ^:private PruneOrphansArgs
  [:map {:closed true} [:yes? {:optional true} :boolean]])

(def ^:private MediaGcArgs
  [:map {:closed true}
   [:apply?     {:optional true} :boolean]
   [:grace-days {:optional true} :int]
   [:yes?       {:optional true} :boolean]])

(def ^:private PruneResolutionArgs
  [:map {:closed true}
   [:apply? {:optional true} :boolean]
   [:yes?   {:optional true} :boolean]])

(def ^:private ReplayHistoryArgs
  [:map {:closed true}
   [:start?  {:optional true} :boolean]
   [:apply?  {:optional true} :boolean]
   [:verify? {:optional true} :boolean]
   [:repair? {:optional true} :boolean]
   [:drop-texts? {:optional true} :boolean]
   [:dedupe? {:optional true} :boolean]
   [:threads {:optional true} :int]
   [:yes?    {:optional true} :boolean]])

(def ^:private SchemaCodeBlobsArgs
  [:map {:closed true}
   [:apply? {:optional true} :boolean]
   [:limit  {:optional true} :int]
   [:yes?   {:optional true} :boolean]])

(def ^:private EditHistoryArgs
  [:map {:closed true}
   [:object-type Named]
   [:limit {:optional true} :int]
   [:pk {:optional true} :string]
   [:sort {:optional true} :string]
   [:token {:optional true} :string]
   [:kinds {:optional true} :string]
   [:editors {:optional true} :string]
   [:props {:optional true} :string]])

(def ^:private RenameArgs
  [:map {:closed true}
   [:from Named]
   [:to   Named]
   [:kind {:optional true} Named]])

(def ^:private BranchPipelineArgs
  [:map {:closed true}
   [:seed  Named]
   [:label {:optional true} Named]
   [:from  {:optional true} Named]])

(def ^:private BranchesArgs
  [:map {:closed true}
   [:closed? {:optional true} :boolean]
   [:machine? {:optional true} :boolean]])

(def ^:private RemoveResourcesArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:resources [:vector :string]]])

(def ^:private MergeProposalArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:build {:optional true} [:enum "all" "modified" "none"]]])

(def ^:private ProtectArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:on?            {:optional true} :boolean]
   [:off?           {:optional true} :boolean]
   [:review         {:optional true} :string]
   [:default-review? {:optional true} :boolean]
   [:retention-days {:optional true} :int]
   [:inactive-days  {:optional true} :int]
   [:require-checks?    {:optional true} :boolean]
   [:no-require-checks? {:optional true} :boolean]
   [:auto-land?         {:optional true} :boolean]
   [:no-auto-land?      {:optional true} :boolean]
   [:settings           {:optional true} [:map-of :keyword :any]]])

(def ^:private BranchRolesArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:own     {:optional true} :string]
   [:revoke  {:optional true} :string]])

(def ^:private SweepArgs
  [:map {:closed true}
   [:days   {:optional true} :int]
   [:apply? {:optional true} :boolean]])

(def ^:private RetentionRunArgs
  [:map {:closed true}
   [:dataset    {:optional true :pos 0} :string]
   [:apply?     {:optional true} :boolean]
   [:grace-days {:optional true} :int]])

(def ^:private RetentionPolicySetArgs
  [:map {:closed true}
   [:name {:pos 0} :string]
   [:datasets              {:optional true} :string]
   [:older-than-days       {:optional true} :int]
   [:view-count            {:optional true} :int]
   [:transaction-count     {:optional true} :int]
   [:views-older-than-days {:optional true} :int]
   [:latest-view?          {:optional true} :boolean]])

(def ^:private ProposeArgs
  [:map {:closed true}
   [:title  {:optional true :pos 0} :string]
   [:source {:optional true} :string]
   [:target {:optional true} :string]
   [:desc   {:optional true} :string]
   [:justification {:optional true} :string]
   [:merge-mode {:optional true} :keyword]])

(def ^:private ProposalsArgs
  [:map {:closed true}
   [:status {:optional true} :string]
   [:target {:optional true} :string]
   [:source {:optional true} :string]])

(def ^:private ProposalIdArgs
  [:map {:closed true}
   [:id {:pos 0} :string]])

(def ^:private AskReviewArgs
  [:map {:closed true}
   [:id {:pos 0} :string]
   [:reviewer {:optional true} :string]
   [:justification {:optional true} :string]])

(def ^:private ProtectionArgs
  [:map {:closed true}
   [:resource {:optional true :pos 0} :string]
   [:off?     {:optional true} :boolean]
   [:all?     {:optional true} :boolean]])

(def ^:private ProjectUseArgs
  [:map {:closed true}
   [:project  {:optional true :pos 0} :string]
   [:default? {:optional true} :boolean]])

(def ^:private SourceAdoptArgs
  [:map {:closed true}
   [:source   {:pos 0} :string]
   [:table    {:pos 1} :string]
   [:into     {:optional true} :string]
   [:dataset  {:optional true} :string]
   [:as       {:optional true} :string]
   [:key      {:optional true} :string]
   [:project  {:optional true} :string]
   [:preview? {:optional true} :boolean]
   [:land?    {:optional true} :boolean]
   [:stage?   {:optional true} :boolean]])

(def ^:private PackageArgs
  [:map {:closed true}
   [:package  {:pos 0} :string]
   [:preview? {:optional true} :boolean]
   [:land?    {:optional true} :boolean]
   [:stage?   {:optional true} :boolean]
   [:mine?    {:optional true} :boolean]
   [:theirs?  {:optional true} :boolean]])

(def ^:private Strings [:or :string [:vector :string]])

(def ^:private ProductPublishArgs
  [:map {:closed true}
   [:product     {:pos 0} :string]
   [:version     {:pos 1} :string]
   [:projects    Strings]
   [:channel     {:optional true} :string]
   [:description {:optional true} :string]
   [:requires    {:optional true} Strings]
   [:release?    {:optional true} :boolean]])

(def ^:private ProductInstallArgs
  [:map {:closed true}
   [:product       {:pos 0} :string]
   [:version       {:optional true :pos 1} :string]
   [:channel       {:optional true} :string]
   [:suffix        {:optional true} :string]
   [:prefix        {:optional true} :string]
   [:bind          {:optional true} Strings]
   [:lock?         {:optional true} :boolean]
   [:auto-upgrade? {:optional true} :boolean]
   [:window-days   {:optional true} :string]
   [:window-from   {:optional true} :string]
   [:window-to     {:optional true} :string]
   [:window-zone   {:optional true} :string]
   [:with-linked?  {:optional true} :boolean]
   [:preview?      {:optional true} :boolean]
   [:land?         {:optional true} :boolean]
   [:stage?        {:optional true} :boolean]])

(def ^:private ProductUpgradeArgs
  [:map {:closed true}
   [:installation {:pos 0} :string]
   [:version      {:optional true :pos 1} :string]
   [:preview?     {:optional true} :boolean]
   [:mine?        {:optional true} :boolean]
   [:theirs?      {:optional true} :boolean]
   [:land?        {:optional true} :boolean]
   [:stage?       {:optional true} :boolean]])

(def ^:private ProductUninstallArgs
  [:map {:closed true}
   [:installation {:pos 0} :string]
   [:preview?     {:optional true} :boolean]
   [:land?        {:optional true} :boolean]
   [:stage?       {:optional true} :boolean]])

(def ^:private ProductConfigureArgs
  [:map {:closed true}
   [:installation  {:pos 0} :string]
   [:channel       {:optional true} :string]
   [:auto-upgrade? {:optional true} :boolean]
   [:manual?       {:optional true} :boolean]
   [:window-days   {:optional true} :string]
   [:window-from   {:optional true} :string]
   [:window-to     {:optional true} :string]
   [:window-zone   {:optional true} :string]
   [:no-window?    {:optional true} :boolean]])

(def ^:private ProductExportArgs
  [:map {:closed true}
   [:product {:pos 0} :string]
   [:version {:pos 1} :string]
   [:out     {:optional true} :string]])

(def ^:private ProductImportArgs
  [:map {:closed true}
   [:file               {:pos 0} :string]
   [:channel            {:optional true} :string]
   [:require-signature? {:optional true} :boolean]])

(def ^:private ProjectPolicyArgs
  [:map {:closed true}
   [:project {:pos 0} :string]
   [:review  {:optional true} :string]
   [:default-review?  {:optional true} :boolean]
   [:protect-all?     {:optional true} :boolean]
   [:no-protect-all?  {:optional true} :boolean]])

(def ^:private DeleteConstructArgs
  [:map {:closed true}
   [:kind Named]
   [:name Named]
   [:cascade? {:optional true} :boolean]])

(def ^:private SetStatusArgs
  [:map {:closed true}
   [:kind        Named]
   [:name        Named]
   [:status      Named]
   [:deprecation {:optional true} [:map {:closed true}
                                   [:reason      {:optional true} :string]
                                   [:deadline    {:optional true} :string]
                                   [:replacement {:optional true} Named]]]])

(def ^:private AccessArgs
  [:map {:closed true}
   [:user    {:optional true :pos 0} :string]
   [:project {:optional true} :string]])

(def ^:private AccessRequestArgs
  [:map {:closed true}
   [:kind   {:pos 0} :string]
   [:target {:pos 1} :string]
   [:role   {:optional true} :string]
   [:for    {:optional true} :string]
   [:justification {:optional true} :string]])

(def ^:private AccessDecideArgs
  [:map {:closed true}
   [:id      {:pos 0} :string]
   [:reject? {:optional true} :boolean]
   [:note    {:optional true} :string]])

(def ^:private AccessExplainArgs
  [:map {:closed true}
   [:user    {:pos 0} :string]
   [:op      {:pos 1} :string]
   [:project {:pos 2} :string]])

(def ^:private SourceArgs
  [:map {:closed true} [:name {:pos 0} Named]])

(def ^:private SourceTablesArgs
  [:map {:closed true} [:name {:pos 0} Named] [:like {:optional true} :string]])

(def ^:private SourceColumnsArgs
  [:map {:closed true} [:name {:pos 0} Named] [:table {:pos 1} Named]])

(def ^:private PeekArgs
  [:map {:closed true} [:name {:pos 0} Named] [:table {:pos 1} Named]
   [:limit {:optional true} :int]])

(def ^:private SourceSqlArgs
  [:map {:closed true} [:name {:pos 0} Named] [:sql {:pos 1} :string]
   [:limit {:optional true} :int]])

(def ^:private SourceGraphArgs
  [:map {:closed true} [:name {:pos 0} Named] [:table {:optional true} Named]])

(def ^:private BuildEstimateArgs
  [:map {:closed true}
   [:only          {:optional true} [:vector Named]]
   [:strategy      {:optional true} Named]
   [:from          {:optional true} [:vector Named]]
   [:days          {:optional true} :int]
   [:calls-per-row {:optional true} number?]])

(def ^:private PreviewTransformArgs
  [:map {:closed true} [:name Named]
   [:limit  {:optional true} :int]
   [:sample {:optional true} :int]
   [:params {:optional true} [:vector :string]]
   [:output {:optional true} :string]])

(def ^:private TestTransformArgs
  [:map {:closed true} [:name Named]])

(def ^:private SourceAddArgs
  [:map {:closed true}
   [:name {:pos 0} Named]
   [:type     :keyword]
   [:url      :string]
   [:user     {:optional true} :string]
   [:password {:optional true} :string]
   [:egress   {:optional true} :string]
   [:access   {:optional true} :keyword]
   [:stage?   {:optional true} :boolean]])

(def ^:private SourceSecretArgs
  [:map {:closed true}
   [:name {:pos 0} Named]
   [:user     {:optional true} :string]
   [:password {:optional true} :string]
   [:field    {:optional true} :string]
   [:value    {:optional true} :string]
   [:cipher   {:optional true} :string]
   [:stage?   {:optional true} :boolean]])

(def ^:private ImagePushArgs
  [:map {:closed true}
   [:ref {:pos 0} :string]
   [:file {:pos 1} :string]
   [:project {:optional true} :string]])

(def ^:private ImageRefArgs
  [:map {:closed true} [:ref {:pos 0} :string]])

(def ^:private SyncArgs
  [:map {:closed true}
   [:name {:pos 0} Named]
   [:as    Named]
   [:table {:optional true :pos 1} :string]
   [:sql   {:optional true} :string]
   [:key   {:optional true} Named]])

(def ^:private RollbackArgs
  [:map {:closed true}
   [:name Named]
   [:to   :string]
   [:kind {:optional true} :keyword]])

(def ^:private VersionShowArgs
  [:map {:closed true}
   [:kind  Named]
   [:name  Named]
   [:as-of :string]])

(def ^:private VersionRowsArgs
  [:map {:closed true}
   [:type  Named]
   [:as-of :string]
   [:limit {:optional true} :int]])

(def ^:private RebaseStagedArgs
  [:map {:closed true}
   [:mine   {:optional true} :string]
   [:theirs {:optional true} :string]])

(def ^:private DiscardOneArgs
  [:map {:closed true} [:seq {:pos 0} :int]])

(def ^:private listed-steps
  [[:install             :req InstallArgs]
   [:commit              :opt :map]
   [:datasources         :req [:map-of :keyword :map]]
   [:principals          :req PrincipalsArgs]
   [:grant               :req GrantArgs]
   [:org-guest           :req OrgGuestArgs]
   [:sso                 :opt SsoArgs]
   [:ldap                :opt LdapArgs]
   [:idp                 :opt IdpArgs]
   [:run-import          :opt :map]
   [:build-estimate      :opt BuildEstimateArgs]
   [:dataset-write       :req DatasetWriteArgs]
   [:action              :req ActionArgs]
   [:run-function        :req RunFunctionArgs]
   [:branch              :req BranchArgs]
   [:automations         :req [:vector :op/automation]]
   [:automation          :req AutomationControl]
   [:automation-list     :opt :map]
   [:schedules           :req [:vector :op/schedule]]
   [:schedule            :req ScheduleControl]
   [:schedule-list       :opt :map]
   [:schedule-candidates :opt :map]
   [:schedule-create     :opt :map]
   [:health-checks       :req [:vector :op/health-check]]
   [:health-check-list   :opt :map]
   [:linter-snooze       :opt :map]
   [:health-check-run    :req HealthCheckRunArgs]
   [:health-check-set    :req HealthCheckSetArgs]
   [:health-check-delete :req HealthCheckDeleteArgs]
   [:health-check-watch  :req HealthCheckWatchArgs]
   [:health-check-snooze :req HealthCheckSnoozeArgs]
   [:health-check-group-list :opt :map]
   [:health-check-group-set :req HealthCheckGroupSetArgs]
   [:health-check-group-subscribe :req HealthCheckGroupSubscribeArgs]
   [:notification-settings :opt NotificationSettingsArgs]
   [:transform-check     :req TransformCheckArgs]
   [:media-set-items     :req TransformCheckArgs]
   [:function-list       :opt :map]
   [:function-show       :opt :map]
   [:agent-list          :opt :map]
   [:agent-show          :opt :map]
   [:agent-run           :opt :map]
   [:llm-usage           :opt :map]
   [:llm-next            :opt :map]
   [:new-project         :opt :map]
   [:new-repository      :opt :map]
   [:new-application     :opt :map]
   [:project-files       :opt :map]
   [:place               :req PlaceArgs]
   [:unplace             :req UnplaceArgs]
   [:repo-place          :req RepoPlaceArgs]
   [:repo-unplace        :req RepoUnplaceArgs]
   [:folder              :req FolderArgs]
   [:lineage             :opt LineageArgs]
   [:lineage-facts       :opt LineageFactsArgs]
   [:graph-list          :opt GraphListArgs]
   [:graph-show          :req GraphShowArgs]
   [:egress-policy-propose :req [:map {:closed true}
                                 [:address {:pos 0} :string]
                                 [:port {:pos 1} :string]
                                 [:why {:optional true} :string]
                                 [:agents {:optional true} :string]]]
   [:agent-proxy-allowlist :req [:map {:closed true}
                                 [:agent {:pos 0} :string]
                                 [:cidrs {:pos 1} :string]]]
   [:egress-policy-adopt :opt [:map {:closed true} [:except {:optional true} :string]]]
   [:sdk-generate        :req SdkGenerateArgs]
   [:sdk-verify          :req SdkVerifyArgs]
   [:sdk-fingerprint     :opt [:map {:closed true}]]
   [:widget-set-create   :req WidgetSetCreateArgs]
   [:widget-set-init     :req WidgetSetInitArgs]
   [:widget-set-publish  :req WidgetSetPublishArgs]
   [:widget-set-dev      :req WidgetSetDevArgs]
   [:widget-set-pull     :req WidgetSetPullArgs]
   [:widget-set-delete   :req WidgetSetNameArgs]
   [:custom-app          :opt CustomAppArgs]
   [:custom-app-sdk      :req CustomAppOutArgs]
   [:custom-app-docs     :req CustomAppOutArgs]
   [:custom-app-client-mint :req CustomAppMintArgs]
   [:graph-svg           :req GraphDrawArgs]
   [:graph-dot           :req GraphDrawArgs]
   [:graph-create        :req GraphCreateArgs]
   [:graph-save-as       :req GraphSaveAsArgs]
   [:graph-rename        :req GraphRenameArgs]
   [:graph-move          :req GraphMoveArgs]
   [:graph-scope         :req GraphScopeArgs]
   [:graph-branch        :req GraphBranchArgs]
   [:graph-arrange       :req GraphArrangeArgs]
   [:graph-delete        :req GraphDeleteArgs]
   [:edit                :req EditArgs]
   [:trash               :opt :map]
   [:restore             :req RestoreArgs]
   [:login               :req LoginArgs]
   [:review              :opt ReviewArgs]
   [:confirm             :opt ConfirmArgs]
   [:show                :opt ShowArgs]
   [:query               :req QueryArgs]
   [:series              :req SeriesArgs]
   [:object-sets         :opt ObjectSetsArgs]
   [:describe            :opt DescribeArgs]
   [:diff-schema         :req DiffSchemaArgs]
   [:upload              :req UploadArgs]
   [:bank-import       :req KontenImportArgs]
   [:explore             :req ExploreArgs]
   [:around              :req AroundArgs]
   [:checkout            :req CheckoutArgs]
   [:blob                :req BlobArgs]
   [:runs                :opt RunsArgs]
   [:watch               :opt WatchArgs]
   [:stop                :opt StopArgs]
   [:health              :opt :map]
   [:telemetry           :opt :map]
   [:client-log          :opt :map]
   [:settings            :opt SettingsArgs]
   [:eval                :req EvalArgs]
   [:audit               :opt :map]
   [:login-audit         :opt :map]
   [:feedback          :opt [:map {:closed true} [:id {:optional true} :string]
                             [:benutzer {:optional true} :string]
                             [:erledigt? {:optional true} :boolean]
                             [:offen? {:optional true} :boolean]
                             [:szenario? {:optional true} :boolean]
                             [:ereignis {:optional true} :string]
                             [:to {:optional true} :string]]]
   [:access-role         :req [:map {:closed true} [:user :string] [:set {:optional true} :string]]]
   [:access              :opt AccessArgs]
   [:access-explain      :req AccessExplainArgs]
   [:access-request      :req AccessRequestArgs]
   [:access-decide       :req AccessDecideArgs]
   [:egress              :opt :map]
   [:trace               :opt :map]
   [:profile             :opt :map]
   [:llm                 :opt LlmArgs]
   [:links               :opt LinksArgs]
   [:dependents          :req DependentsArgs]
   [:check               :opt :map]
   [:guide               :opt GuideArgs]
   [:explain             :opt ExplainArgs]
   [:context             :opt :map]
   [:lint                :opt :map]
   [:ontology-lint       :opt :map]
   [:platform            :opt PlatformArgs]
   [:apps                :opt :map]
   [:app                 :req AppArgs]
   [:app-data            :req AppDataArgs]
   [:templates           :opt :map]
   [:template            :req TemplateArgs]
   [:render-template     :req RenderTemplateArgs]
   [:documents           :opt DocumentsArgs]
   [:document            :req DocumentShowArgs]
   [:document-create     :req DocumentCreateArgs]
   [:document-update     :req DocumentUpdateArgs]
   [:use-case-plan       :req UseCasePlanArgs]
   [:use-case-apply      :req UseCaseApplyArgs]
   [:use-case-diagram    :req UseCaseDiagramArgs]
   [:document-generate   :req DocumentGenerateArgs]
   [:doc-intel-run       :req DocIntelRunArgs]
   [:doc-intel-deploy    :req DocIntelDeployArgs]
   [:document-refresh    :req DocumentRefreshArgs]
   [:document-restore    :req DocumentRestoreArgs]
   [:document-export     :req DocumentExportArgs]
   [:document-templatize :req DocumentTemplatizeArgs]
   [:workspaces          :opt :map]
   [:workspace           :req AppArgs]
   [:scenarios           :req ScenariosArgs]
   [:scenario-new        :req ScenarioNewArgs]
   [:scenario-drop       :req ScenarioDropArgs]
   [:scenario-actions    :req ScenarioActionsArgs]
   [:scenario-seal       :req ScenarioSealArgs]
   [:scenario-rebuild    :req ScenarioRebuildArgs]
   [:scenario-apply      :req ScenarioApplyArgs]
   [:scenario-duplicate  :req ScenarioDuplicateArgs]
   [:projects            :opt :map]
   [:repositories        :opt :map]
   [:project-map         :opt ProjectMapArgs]
   [:run                 :req RunArgs]
   [:await               :req AwaitArgs]
   [:jobs                :req JobsArgs]
   [:transactions        :req TransactionsArgs]
   [:dataset-preview     :req DatasetPreviewArgs]
   [:streams             :opt :map]
   [:stream-offsets      :req StreamNameArgs]
   [:stream-publish      :req StreamPublishArgs]
   [:stream-listen-udp   :req StreamListenUdpArgs]
   [:stream-tail         :req StreamTailArgs]
   [:stream-view         :req StreamViewArgs]
   [:stream-reset        :req StreamResetArgs]
   [:consumption         :req ConsumptionArgs]
   [:delta               :req DeltaArgs]
   [:reindex             :req ReindexArgs]
   [:prune-orphans       :opt PruneOrphansArgs]
   [:restore-point       :opt RestorePointArgs]
   [:media-gc            :opt MediaGcArgs]
   [:schema-code-blobs   :opt SchemaCodeBlobsArgs]
   [:replay-history      :opt ReplayHistoryArgs]
   [:prune-resolution    :opt PruneResolutionArgs]
   [:delete-construct    :req DeleteConstructArgs]
   [:set-status          :req SetStatusArgs]
   [:edit-history        :req EditHistoryArgs]
   [:rename              :req RenameArgs]
   [:branch-pipeline     :req BranchPipelineArgs]
   [:branches            :opt BranchesArgs]
   [:protect             :req ProtectArgs]
   [:branch-roles        :req BranchRolesArgs]
   [:sweep               :opt SweepArgs]
   [:retention-run       :opt RetentionRunArgs]
   [:retention-policy-set :req RetentionPolicySetArgs]
   [:propose             :opt ProposeArgs]
   [:proposals           :opt ProposalsArgs]
   [:proposal            :req ProposalIdArgs]
   [:ask-review          :req AskReviewArgs]
   [:protection          :opt ProtectionArgs]
   [:project-policy      :req ProjectPolicyArgs]
   [:project-use         :opt ProjectUseArgs]
   [:source-adopt        :req SourceAdoptArgs]
   [:package-install     :req PackageArgs]
   [:package-upgrade     :req PackageArgs]
   [:product-publish     :req ProductPublishArgs]
   [:product-install     :req ProductInstallArgs]
   [:product-upgrade     :req ProductUpgradeArgs]
   [:product-uninstall   :req ProductUninstallArgs]
   [:product-configure   :req ProductConfigureArgs]
   [:product-export      :req ProductExportArgs]
   [:product-import      :req ProductImportArgs]
   [:merge-proposal      :req MergeProposalArgs]
   [:remove-resources    :req RemoveResourcesArgs]
   [:preview-staged      :opt :map]
   [:rebase-staged       :opt RebaseStagedArgs]
   [:discard-one         :req DiscardOneArgs]
   [:rollback            :req RollbackArgs]
   [:version-show        :req VersionShowArgs]
   [:version-rows        :req VersionRowsArgs]
   [:sources             :opt :map]
   [:source              :req SourceArgs]
   [:source-tables       :req SourceTablesArgs]
   [:writeback-check     :req SourceColumnsArgs]
   [:peek                :req PeekArgs]
   [:source-sql          :req SourceSqlArgs]
   [:source-graph        :req SourceGraphArgs]
   [:preview-transform   :req PreviewTransformArgs]
   [:test-transform      :req TestTransformArgs]
   [:source-add          :req SourceAddArgs]
   [:source-secret       :req SourceSecretArgs]
   [:sync                :req SyncArgs]
   [:image-push          :req ImagePushArgs]
   [:image-delete        :req ImageRefArgs]])

(def steps (into listed-steps ev/cli-steps))

(def verbs (into #{} (map first) steps))
