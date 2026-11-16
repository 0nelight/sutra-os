(ns sutra.foundation.contracts.c)

(def ^:private n?
  [:or :string :keyword :symbol])

(defn- opt
  [& kvs]
  (into [:map] (map (fn [[k s]] [k {:optional true} s])) kvs))

(def schemas
  {
   ::blob-stored [:map [:hash :string] [:size nat-int?]]
   ::media-land-in
   [:map [:dataset n?] [:media-column n?] [:bytes-b64 :string]
    [:values {:optional true} [:maybe :map]] [:filename {:optional true} [:maybe :string]]
    [:mime {:optional true} [:maybe :string]] [:message {:optional true} [:maybe :string]]]
   ::media-landed
   [:map [:hash :string] [:size nat-int?] [:dataset :string] [:media-column :string]
    [:media-set :any] [:filename [:maybe :string]] [:written :any] [:branch :any]
    [:triggered-by [:sequential :any]]]
   ::media-gc-in [:maybe (opt [:apply? :any] [:grace-days [:maybe :int]])]
   ::count-bytes [:map [:count nat-int?] [:bytes {:optional true} nat-int?]]
   ::media-gc
   [:map [:branches [:sequential :any]]
    [:retired [:map [:count nat-int?] [:items [:sequential :any]]]]
    [:held ::count-bytes] [:referenced nat-int?] [:unreferenced ::count-bytes]
    [:within-grace [:map [:count nat-int?] [:grace-days :int]]]
    [:deleted ::count-bytes] [:applied? :boolean] [:blocked [:maybe :keyword]]
    [:errors [:sequential :map]]]

   ::notification
   [:map [:created-at [:maybe :string]] [:read? :boolean]
    [:id {:optional true} :any] [:heading {:optional true} [:maybe :string]]
    [:message {:optional true} [:maybe :string]] [:object-type {:optional true} :any]
    [:object-id {:optional true} :any] [:automation-id {:optional true} :any]
    [:branch {:optional true} :any] [:source {:optional true} :any]
    [:link-url {:optional true} [:maybe :string]]
    [:link-label {:optional true} [:maybe :string]]]
   ::notification-inbox
   [:map [:notifications [:sequential ::notification]] [:unread nat-int?]]
   ::notifications-read-in
   [:maybe (opt [:ids [:sequential :any]] [:all :any])]
   ::notifications-marked [:map [:marked :any] [:unread nat-int?]]
   ::notification-settings
   [:map [:settings :any] [:defaults :any] [:kinds [:sequential :any]]
    [:ways [:sequential :any]]]
   ::notification-settings-in [:maybe :map]

   ::object-row [:map [:id :any] [:values :any] [:staged? {:optional true} :boolean]]
   ::link-row :map
   ::links [:sequential ::link-row]
   ::knn-result
   [:map [:type :any] [:k :int] [:metric :any] [:results [:sequential :any]]]
   ::object-counts
   [:map [:branch :any] [:counts :map] [:by-type :any] [:branch-index {:optional true} :any]]
   ::instances
   [:or :sutra.foundation.contracts/preview-projection
    [:map [:unchanged :boolean] [:version :string]]]
   ::instances-search [:map [:objects :any]]
   ::instances-around-in
   [:map [:ids {:optional true} [:maybe [:sequential :any]]] [:types [:sequential :any]]
    [:links-only? {:optional true} :any] [:among? {:optional true} :any]]
   ::instances-around [:map [:objects :any] [:links :any]]
   ::objects-at-commit [:map [:commit :any] [:type :any] [:rows :any]]
   ::series-in [:maybe :map]
   ::series [:map [:series [:sequential :map]]]
   ::project-map [:map [:scope :any]]
   ::exploration
   [:map [:rid :string] [:private :boolean] [:mine :boolean]
    [:id {:optional true} :string] [:kind {:optional true} :any]
    [:object-type {:optional true} :any] [:name {:optional true} [:maybe :string]]
    [:description {:optional true} [:maybe :string]] [:state {:optional true} :any]
    [:project {:optional true} :any] [:created-by {:optional true} :any]
    [:created-at {:optional true} :any] [:updated-at {:optional true} :any]]
   ::explorations [:map [:explorations [:sequential ::exploration]]]
   ::exploration-in
   [:map [:kind n?] [:object-type n?] [:name :string] [:query :any]
    [:description {:optional true} [:maybe :string]] [:state {:optional true} :any]
    [:project {:optional true} :any]]
   ::exploration-change-in
   (opt [:name :string] [:description [:maybe :string]] [:state :any] [:query :any]
        [:private :any] [:project :any])
   ::exploration-deleted [:map [:deleted :string]]

   ::project
   [:map [:id :any] [:name :any] [:description [:maybe :string]] [:members :map]]
   ::projects [:map [:projects [:sequential ::project]]]
   ::project-deps
   [:map [:edges [:sequential :any]] [:declared :map] [:undeclared [:sequential :map]]
    [:without-project [:sequential :map]] [:stale [:sequential :map]]
    [:shared {:optional true} [:sequential :map]]]
   ::repository
   [:map [:id :any] [:name :any] [:description [:maybe :string]] [:type :any]
    [:project [:maybe :string]] [:members :map]]
   ::repositories [:map [:repositories [:sequential ::repository]]]
   ::kompass-node
   (opt [:rid :any] [:node-kind :any] [:parent-rid :any] [:name :any] [:ref :any]
        [:position :any] [:project :any] [:updated-at :any])
   ::kompass-nodes [:map [:nodes [:sequential ::kompass-node]]]
   ::kompass-annotations [:map [:annotations [:map-of :any :any]]]
   ::kompass-annotation-in
   [:map [:ref :any] [:tags {:optional true} :any] [:trashed {:optional true} :any]
    [:curated {:optional true} :any] [:doc {:optional true} :any]
    [:pinned {:optional true} :any] [:promoted {:optional true} :any]]
   ::kompass-annotated [:map [:ref :any] [:cleared? :boolean]]
   ::kompass-folder-in
   [:maybe (opt [:name :string] [:parent-rid [:maybe :string]] [:position [:maybe :int]]
                [:project :any])]
   ::rid-ref [:map [:rid :any]]
   ::deleted-ref [:map [:deleted :any]]
   ::kompass-rename-in [:maybe (opt [:to :string])]
   ::kompass-renamed [:map [:rid :any] [:name :any]]
   ::kompass-move-in [:maybe (opt [:parent-rid [:maybe :string]])]
   ::kompass-moved [:map [:rid :any] [:parent-rid :any]]
   ::kompass-place-in
   [:maybe (opt [:ref :string] [:parent-rid [:maybe :string]] [:name [:maybe :string]]
                [:position [:maybe :int]] [:project :any])]
   ::kompass-placed
   (opt [:rid :any] [:moved :boolean] [:ref :any] [:project :any])
   ::kompass-collection-in
   [:maybe (opt [:name :string] [:description [:maybe :string]]
                [:refs [:maybe [:sequential :any]]])]
   ::kompass-portfolio-in
   [:maybe (opt [:name :string] [:description [:maybe :string]]
                [:projects [:maybe [:sequential :any]]])]
   ::kompass-file
   (opt [:id :string] [:name :string] [:project :any] [:address :string] [:size :int]
        [:mime [:maybe :string]] [:author :any] [:created-at :any] [:updated-at :any])
   ::kompass-files [:map [:files [:sequential ::kompass-file]]]
   ::kompass-upload-in
   [:map [:name :string] [:bytes-b64 :string] [:project {:optional true} :any]
    [:mime {:optional true} [:maybe :string]]]
   ::kompass-tabular-in
   [:maybe (opt [:bytes-b64 :string] [:filename [:maybe :string]])]
   ::kompass-tabular
   [:map [:columns [:sequential [:map [:name :string] [:type :keyword]]]]
    [:rows [:sequential :map]]]
   ::kompass-grants
   [:map [:grants [:sequential (opt [:principal :any] [:role :any] [:via :any])]]
    [:link [:maybe (opt [:token :string] [:role :any])]]]
   ::kompass-grant-in
   [:map [:ref :string] [:principal {:optional true} :any] [:role {:optional true} :any]
    [:remove {:optional true} :any]]
   ::kompass-share-link-in
   [:maybe (opt [:ref :string] [:on :any] [:role :any])]
   ::kompass-redeemed [:map [:ref :any] [:role :any]]
   ::kompass-on-in [:maybe (opt [:on :any])]
   ::kompass-sharing [:map [:project :any] [:on :boolean]]
   ::kompass-imports-in [:maybe (opt [:imports [:maybe [:sequential :any]]])]
   ::kompass-imports [:map [:project :any] [:imports [:sequential :string]]]
   ::kompass-favorite-in [:maybe (opt [:ref :any] [:on :any])]
   ::kompass-favorite [:map [:ref :any] [:on :boolean]]
   ::kompass-index
   [:map [:modified [:map [:floor-at [:maybe :string]] [:computing :boolean] [:times :map]]]
    [:rights :map] [:shared :any] [:sharing :any] [:collections [:sequential :map]]
    [:portfolios [:sequential :map]] [:favorites [:sequential :any]] [:home-folders :any]
    [:me :any]]

   ::schedule (opt [:id :any] [:name :any] [:target :any] [:cron :any] [:every-ms :any]
                   [:trigger :any] [:paused :any] [:parameters :any])
   ::schedules [:map [:schedules [:sequential ::schedule]]]
   ::schedule-in
   (opt [:id :any] [:name n?] [:target n?] [:cron [:maybe :string]] [:every-ms [:maybe :int]]
        [:trigger :any] [:scope :any] [:from :any] [:paused :any] [:full :any]
        [:parameters :any] [:run-setup-key :any])
   ::schedule-run-in [:maybe (opt [:parameters :any])]
   ::schedule-started
   [:map [:run-id :string] [:status :string] [:target :any] [:started-at :any]
    [:run-setup {:optional true} :map]]
   ::schedule-deleted [:map [:deleted :any]]

   ::widget-set
   (opt [:name :any] [:description :any] [:project :any] [:author :any] [:created-at :any]
        [:updated-at :any] [:latest :any] [:versions [:sequential :any]]
        [:releases [:sequential :map]])
   ::widget-sets [:map [:widget-sets [:sequential ::widget-set]]]
   ::widget-set-one [:map [:widget-set ::widget-set]]
   ::widget-set-in
   (opt [:name n?] [:description [:maybe :string]] [:project :any])
   ::widget-release-in
   [:maybe (opt [:version :string] [:files [:map-of :any :any]] [:zip-b64 :string]
                [:notes [:maybe :string]] [:dev? :any])]
   ::widget-release-one [:map [:release :map]]
   ::widget-bundle [:map [:bundle-edn :string]]
   ::widget-bundle-negotiated [:or ::widget-bundle :string]
   ::ok [:map [:ok :boolean]]

   ::datasources [:map [:datasources :any]]
   ::datasource-test-in
   [:maybe (opt [:source n?] [:type :any] [:secret-cipher :any] [:password :any])]
   ::datasource-tables [:map [:tables :any]]
   ::datasource-table-in
   (opt [:table :string] [:columns [:sequential :any]] [:primary-key :any])
   ::datasource-columns [:map [:columns :any]]
   ::datasource-sample [:map [:datasource :any] [:table :any] [:rows :any]]
   ::datasource-relations [:map [:datasource :any] [:relations :any]]
   ::datasource-spec
   [:map [:datasource :any] [:as-of :any] [:spec :any]]
   ::preview-table
   (opt [:source :keyword] [:table :string] [:columns [:sequential :any]]
        [:rows [:sequential :any]] [:row-count nat-int?] [:truncated? :boolean]
        [:error :string])
   ::datasource-preview
   [:map [:datasource :any] [:tables [:sequential ::preview-table]] [:as-of :any]]
   ::writeback-check :map
   ::egress-pubkey [:or :map :string]
   ::egress-policy
   (opt [:id :string] [:address :any] [:port :any] [:agents :any] [:state :any]
        [:justification :any] [:paused :any] [:effective :any] [:used-by [:sequential :any]])
   ::egress-policies
   [:map [:policies [:sequential ::egress-policy]] [:uncovered [:sequential :any]]
    [:enforced :boolean]]
   ::egress-policy-in
   (opt [:address :string] [:port :any] [:agents :any] [:justification [:maybe :string]])
   ::egress-decide-in [:maybe (opt [:id :any] [:decision n?])]
   ::egress-adopt-in [:maybe (opt [:except [:maybe [:sequential :any]]])]
   ::egress-adopted [:map [:policies [:sequential ::egress-policy]]]
   ::egress-report-in [:maybe (opt [:dropped [:maybe [:sequential :any]]])]
   ::egress-reported [:map [:dropped nat-int?]]
   ::egress-status
   [:map [:reachable :boolean] [:agent-url :any] [:agents [:sequential :map]]
    [:agent-proxy :map]]
   ::agent-allowlist-in
   [:maybe (opt [:agent :string] [:allow-listed-cidrs [:maybe [:sequential :string]]])]
   ::agent-allowlist
   [:map [:agent :any] [:allow-listed-cidrs :any] [:applies :string]]
   ::source-types [:map [:source-types :any]]

   ::audit [:map [:audit :any]]
   ::client-logged
   [:map [:logged nat-int?] [:dropped {:optional true} nat-int?] [:mapped {:optional true} :any]]
   ::client-log-entries [:map [:entries :any]]
   ::code-reloaded [:map [:reloaded :any] [:count nat-int?] [:verified :any]]
   ::datasource-query-in [:maybe (opt [:sql :string] [:limit :any])]
   ::datasource-rows [:map [:datasource :any] [:rows :any]]
   ::decision-trace-in [:maybe (opt [:on :any] [:off :any] [:kinds :any] [:save :any])]
   ::egress-allowlist
   [:or [:map [:egress [:sequential :any]] [:agents :map] [:xf [:sequential :any]]] :string]
   ::eval-results [:map [:results [:sequential :map]]]
   ::eval-run-in
   [:maybe (opt [:suite :any] [:iterations :any] [:parallel :any] [:model :any]
                [:version :any] [:meta :any] [:experiment :any] [:preview :any])]
   ::eval-run-cases [:map [:run [:maybe :map]] [:cases [:sequential :map]]]
   ::eval-case-in [:maybe (opt [:case :any] [:preview :any])]
   ::eval-suites [:map [:suites [:sequential :map]]]
   ::fn-trace-in
   [:maybe (opt [:on :any] [:off :any] [:prefixes :any] [:limit :any] [:sort :any]
                [:shapes :any] [:slow-ms :any])]
   ::function-run-in
   [:maybe (opt [:function-name n?] [:language n?] [:source [:maybe :string]] [:types :any]
                [:inputs :any] [:target-type :any])]
   ::function-run
   (opt [:function :any] [:kind :keyword] [:rows [:sequential :map]] [:compiled :map]
        [:decisions [:sequential :any]] [:run-id :any] [:snapshot-id :any])
   ::settings [:map [:groups [:sequential :map]] [:settings [:sequential :map]]]
   ::settings-in [:maybe (opt [:set :any] [:reset :any])]
   ::login-audit [:map [:events :any]]
   ::platform-experience
   [:map [:title :any] [:logo :any] [:uploaded [:sequential :any]]]
   ::platform-experience-in [:maybe (opt [:title [:maybe :string]] [:logo :map])]
   ::profile-in
   [:maybe (opt [:on :any] [:off :any] [:interval :any] [:depth :any] [:gecko :any]
                [:label :any] [:limit :any] [:events :any])]
   ::apply-in [:maybe (opt [:apply? :any] [:limit :any])]
   ::prune-resolution
   [:map [:branch :any] [:resolved [:sequential :any]] [:by-type [:sequential :map]]
    [:elsewhere [:sequential :any]] [:swept :any] [:applied? :boolean]]
   ::rebooted [:map [:ok :boolean] [:branch :any] [:counts :any]]
   ::replay-in
   [:maybe (opt [:apply? :any] [:verify? :any] [:repair? :any] [:drop-texts? :any]
                [:dedupe? :any] [:threads :any])]
   ::schema-reloaded
   [:map [:reloaded :boolean] [:object-types nat-int?] [:link-types nat-int?]
    [:schema-error :any]]
   ::streams [:map [:streams [:sequential :map]]]
   ::stream-status [:map [:stream :string]]
   ::stream-offsets [:map [:stream :string] [:end-offsets :any]]
   ::stream-records [:map [:stream :string] [:partition :any] [:records [:sequential :map]]]
   ::stream-publish-in [:maybe (opt [:records [:sequential :any]])]
   ::stream-view [:map [:stream :string] [:rows [:sequential :any]]]
   ::system
   (opt [:routes :any] [:identity :any] [:product-offers [:sequential :map]]
        [:llm-catalogues [:sequential :map]] [:graph-layouts [:sequential :map]]
        [:source-types :any] [:connector-profiles :map] [:policy-fields [:sequential :map]])
   ::trace [:map [:backend :any] [:egress :any] [:nodes [:sequential :any]] [:scope :any]]
   ::trace-arm-in
   [:maybe (opt [:on :any] [:file :any] [:egress-file :any] [:capture-params :any]
                [:shapes :any] [:keep-ms :any])]
   ::trace-armed [:map [:backend :any] [:egress :any] [:egress-reachable :boolean] [:scope :any]]
   ::trace-kill-in [:maybe (opt [:seq :any] [:all :any])]
   ::trace-killed [:map [:backend :any] [:egress :any]]
   ::trace-explain-in [:maybe (opt [:seq :any] [:limit :any] [:raw :any])]
   ::requests-in-flight [:map [:deadline-ms :any]]
   ::report [:map]
   ::stream-published
   [:map [:stream :any] [:published {:optional true} nat-int?] [:offsets {:optional true} :any]
    [:rejected {:optional true} :any] [:records {:optional true} :any]]
   ::datasource-probe
   [:map [:ok :boolean] [:via {:optional true} :string] [:code {:optional true} :string]
    [:error {:optional true} :any] [:agent {:optional true} :any]]

   ::entity-in [:maybe :map]
   ::schema-step [:map [:schema {:optional true} :any]]
   ::rename-in [:maybe (opt [:to n?])]
   ::renamed :map})
