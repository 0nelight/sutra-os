(ns sutra.foundation.contracts
  (:require [sutra.kit.anomaly :as anomaly]
            [malli.core :as m]
            [malli.error :as me]
            [malli.transform :as mt]
            [sutra.foundation.contracts.a :as contracts-a]
            [sutra.foundation.contracts.b :as contracts-b]
            [sutra.foundation.contracts.c :as contracts-c]
            [sutra.types.languages :as langs]))

(def ^:private env-fields
  [[:authority ::authority]
   [:branch ::branch-ref]
   [:dry-run? {:optional true} :boolean]
   [:trace? {:optional true} :boolean]])

(defn- with-envelope
  [& fields]
  (into [:map] (concat env-fields fields)))

(def registry
  (merge
   (m/default-schemas)
   contracts-a/schemas
   contracts-b/registry
   contracts-c/schemas
   {::authority
    [:map
     [:user {:optional true} :any]
     [:clearances {:optional true} [:or [:= :all] [:set :any]]]
     [:denied-markings {:optional true} [:set :keyword]]
     [:hidden-object-types {:optional true} [:set :any]]
     [:operations {:optional true} [:or [:= :all] [:set :any]]]
     [:project-operations {:optional true} [:or [:= :all] [:map-of :any [:set :any]]]]
     [:system? {:optional true} :boolean]]
    ::branch-ref [:maybe :string]
    ::as-of      [:maybe :string]
    ::envelope   (into [:map] env-fields)

    ::predicate-ir
    [:map
     [:root :any]
     [:where {:optional true} :any]
     [:limit {:optional true} :any] [:aggregate {:optional true} :any]
     [:group-by {:optional true} :any] [:accuracy {:optional true} :any]
     [:select {:optional true} :any]]
    ::substrate-read-request
    (with-envelope [:as-of ::as-of] [:ir ::predicate-ir])

    ::build-request
    (with-envelope
      [:preset {:optional true} :any]
      [:consumers {:optional true} [:maybe [:vector :any]]]
      [:incremental? {:optional true} :boolean])
    ::consumed-cursor
    [:map [:consumer-rid :any] [:input-dataset-rid :any] [:last-snapshot-id :any]]
    ::job-result
    [:map
     [:rows-in [:map-of :any :int]]
     [:code-sha [:map-of :any :string]]
     [:consumed-cursors [:vector ::consumed-cursor]]
     [:bootstrapped [:set :any]]
     [:snapshot-run [:set :any]]
     [:incomplete [:set :any]]
     [:job-log [:map-of :any :string]]
     [:job-spans {:optional true} [:map-of :any [:vector :any]]]
     [:pending-observed {:optional true} [:map-of :any :int]]
     [:dropped-cols {:optional true} [:map-of :any [:set :any]]]
     [:input-sigs {:optional true} [:map-of :any :map]]
     [:trace {:optional true} [:maybe [:vector :any]]]]

    ::link-backing
    [:multi {:dispatch :strategy}
     [:endpoint-fk
      [:map
       [:strategy [:= :endpoint-fk]]
       [:fk-side [:enum :from :to]]
       [:fk-attribute :keyword]]]
     [:join-table
      [:map
       [:strategy [:= :join-table]]
       [:from-key-column :keyword]
       [:to-key-column :keyword]]]
     [:resolve-by-shared-property
      [:map
       [:strategy [:= :resolve-by-shared-property]]
       [:from-key-column [:maybe :keyword]]
       [:match-column [:maybe :keyword]]
       [:shared-property [:maybe :keyword]]]]
     [:polymorphic
      [:map
       [:strategy [:= :polymorphic]]
       [:sources [:vector [:map [:from-type :any]]]]]]]

    ::object-backing
    [:multi {:dispatch :role}
     [:primary    [:map [:role [:= :primary]]]]
     [:supplement [:map [:role [:= :supplement]]]]]

    ::transform-input
    [:map
     [:kind
      [:enum :datasource :transform :object-type :dataset :media-set :media-set-items :media
       :language-model :scalar]]]
    ::transform-output
    [:map [:kind [:enum :object-type :link-type :dataset :media-set]]]
    ::transform-contract
    [:map
     [:transform-name :keyword]
     [:language {:optional true} [:maybe (into [:enum] langs/compute-language-names)]]
     [:inputs   {:optional true} [:maybe [:map-of :keyword ::transform-input]]]
     [:output   {:optional true} ::transform-output]
     [:outputs  {:optional true} [:vector ::transform-output]]]

    ::tx-op     [:map [:op :keyword]]
    ::tx-vector [:sequential ::tx-op]
    ::substrate-write-request
    (with-envelope [:tx-vector ::tx-vector])

    ::action-request
    (with-envelope
      [:action :keyword]
      [:args [:map-of :keyword :any]])
    ::action-batch-request
    (with-envelope
      [:action :keyword]
      [:requests [:sequential [:map-of :keyword :any]]])
    ::action-wire
    [:map
     [:action :keyword]
     [:args {:optional true} [:map-of :keyword :any]]]
    ::action-batch-wire
    [:map
     [:action :keyword]
     [:requests [:sequential [:map-of :keyword :any]]]]
    ::action-edit
    [:map
     [:type [:enum :add-object :modify-object :delete-object]]
     [:object-type :any]
     [:object-id :any]
     [:primary-key {:optional true} :any]]
    ::action-edits
    [:map
     [:type [:= :edits]]
     [:edits [:sequential ::action-edit]]
     [:added-objects-count :int]
     [:modified-objects-count :int]
     [:deleted-objects-count :int]
     [:added-links-count :int]
     [:deleted-links-count :int]]
    ::action-result
    [:map
     [:tx-vector {:optional true} ::tx-vector]
     [:violations {:optional true} [:vector :any]]]

    ::schema-mutation-request
    (with-envelope
      [:edit :any]
      [:world {:optional true} :any])
    ::schema-delta
    [:map [:new-world :any] [:plan :any]]

    ::query-rows
    [:map
     [:object-type :any]
     [:rows [:sequential [:map [:id {:optional true} :any]
                          [:values {:optional true} :any]]]]
     [:count {:optional true} :int]]
    ::query-aggregate
    [:map
     [:object-type :any]
     [:keys {:optional true} [:map
                              [:group [:sequential :any]]
                              [:metrics [:sequential :any]]]]
     [:aggregations [:sequential [:map]]]]
    ::query-result
    [:multi {:dispatch (fn [x] (cond (not (map? x))             :other
                                     (contains? x :aggregations) :aggregate
                                     (contains? x :rows)         :rows
                                     :else                       :other))}
     [:rows ::query-rows]
     [:aggregate ::query-aggregate]
     [:other :any]]
    ::object-page
    [:map
     [:rows [:sequential :any]]
     [:offset :int]
     [:limit :int]
     [:total :int]
     [:next-offset {:optional true} [:maybe :int]]]
    ::action-response
    [:map
     [:action :keyword]
     [:dry-run {:optional true} :boolean]
     [:plan {:optional true} :any]
     [:result {:optional true} :any]
     [:action-id {:optional true} :any]
     [:staged {:optional true} :boolean]
     [:seq {:optional true} :any]
     [:id {:optional true} :any]
     [:edits {:optional true} ::action-edits]]
    ::action-batch-response
    [:map
     [:action :keyword]
     [:staged {:optional true} :boolean]
     [:seqs {:optional true} [:sequential :any]]
     [:ids {:optional true} [:sequential :any]]
     [:result {:optional true} :any]
     [:action-id {:optional true} :any]
     [:edits {:optional true} ::action-edits]]
    ::object-set-ref
    [:map
     [:rid :any]
     [:ir {:optional true} :any]
     [:kind {:optional true} :any]
     [:lifecycle {:optional true} :any]]
    ::object-set-listing
    [:map
     [:object-sets [:sequential [:map]]]
     [:temporary {:optional true} [:sequential [:map]]]]
    ::deleted [:map [:deleted :boolean]]

    ::stream-publish-request
    [:map [:records [:sequential [:map-of :keyword :any]]]]
    ::stream-publish-result
    [:map
     [:stream :any]
     [:published :int]
     [:offsets [:sequential [:map [:partition :int] [:offset :int]]]]]

    ::no-input  [:map]
    ::branch-in [:map [:branch :string]]
    ::locked-in [:map [:branch :string] [:schema {:optional true} [:maybe :map]]]
    ::node-ref  [:tuple :keyword :string]
    ::health-by-node
    [:map-of [:tuple :string :string]
     [:map [:status [:maybe :string]] [:checks :int] [:failed :int]]]
    ::issues-by-node [:map-of ::node-ref pos-int?]
    ::automations    [:vector [:map [:id :string]]]
    ::proposal-tasks [:maybe [:vector :map]]
    ::lineage-prints [:sequential [:tuple :string :string]]
    ::lineage-around
    [:map
     [:branch {:optional true} ::lineage-prints]
     [:main {:optional true} ::lineage-prints]
     [:logic {:optional true}
      [:map
       [:branch {:optional true} ::lineage-prints]
       [:main {:optional true} ::lineage-prints]]]]
    ::inputs-around-in
    [:map [:trunk [:maybe :map]] [:nodes [:sequential [:tuple some? some?]]]]
    ::push-report-in
    [:map
     [:trunk [:maybe :map]]
     [:around [:maybe ::lineage-around]]
     [:sent {:optional true} [:maybe :map]]
     [:next {:optional true} [:maybe :map]]
     [:before {:optional true} :any]
     [:after {:optional true} :any]]
    ::push-report
    [:map
     [:downstream {:optional true} [:map [:direct [:vector :string]] [:further :int]]]
     [:readers-unaffected {:optional true} :int]
     [:inputs-changed {:optional true}
      [:vector [:map [:node :string] [:where :string] [:what :string]]]]]
    ::feeds-line-in  [:map [:ids [:sequential :string]]]
    ::feeds-line     [:maybe :string]
    ::graph-handle   [:map [:handle :any]]
    ::saved-graph    [:maybe [:tuple :string :map]]
    ::branch-names   [:set :string]
    ::project-names  [:set :string]
    ::drawing-request [:map [:format [:enum :dot :svg]] [:drawing :map]]
    ::code           [:map [:code :string]]
    ::plan-source
    [:map {:closed true}
     [:datasource {:optional true} some?]
     [:from {:optional true} [:map {:closed true} [:datasource some?]]]
     [:join-table {:optional true} [:map {:closed true} [:datasource some?]]]]
    ::source-health
    [:map [:status :string] [:code {:optional true} :string]
     [:message {:optional true} :string]]
    ::schema-plan
    [:map {:closed true}
     [:import-plan [:map {:closed true}
                    [:objects [:map-of some? ::plan-source]]
                    [:supplements [:map-of some? ::plan-source]]
                    [:links [:map-of some? ::plan-source]]]]
     [:sources [:vector [:map [:source-name some?] [:health ::source-health]
                         [:consumers [:vector [:map [:transform some?]
                                               [:target {:optional true} any?]]]]]]]
     [:unresolved [:vector [:map {:closed true}
                            [:kind [:enum :object-type :supplement :link-type]]
                            [:name some?] [:error [:maybe :string]]]]]]
    ::projection-ref [:map [:projection [:or :keyword :string]]]
    ::placements
    [:map
     [:placement {:optional true} [:map-of ::node-ref :string]]
     [:fields {:optional true} [:map-of [:tuple ::node-ref :any] :string]]
     [:attributes {:optional true} [:map-of [:tuple ::node-ref :any] :string]]
     [:files {:optional true}
      [:map-of :string [:map [:format :keyword] [:owner [:maybe :string]] [:rule :keyword]]]]]
    ::record-fence
    [:tuple [:= :record] :qualified-keyword :string [:or [:= :sutra.store.api/absent] :map]]
    ::layout-move
    [:map [:branch :string] [:node-kind :keyword] [:from :string] [:to :string]]
    ::graphs-change
    [:map [:branch :string] [:before [:maybe :map]] [:after [:maybe :map]]]
    ::branch-policies [:map-of :string :map]
    ::staged-in [:map [:branch [:maybe :string]]]
    ::staged-rows [:sequential :map]
    ::expectation-result
    [:map [:branch :string]
     [:check [:map [:id :string] [:name :string] [:node-key :string] [:expects :map]]]
     [:result [:map [:status :string] [:value [:maybe :string]] [:detail :any]]]]
    ::staging-view
    [:map [:parent [:maybe :string]] [:staging [:maybe :string]] [:live? :boolean]
     [:base :any] [:read [:maybe :string]]]
    ::preview-projection
    [:map [:schema :map] [:schema-derived :any] [:staged-types :any] [:staged-ids set?]
     [:integrity :any] [:version :string]]

    ::source-spec [:map [:type :keyword] [:egress {:optional true} [:set :any]]]
    ::sync-request
    (with-envelope
      [:source ::source-spec]
      [:mode [:enum :batch :streaming :cdc :virtual]])}))

(def opts {:registry registry})

(def ^:private compiled
  (atom {}))

(defn- compiled-for
  [schema]
  (or (get @compiled schema)
      (let [v {:valid? (m/validator schema opts) :explain (m/explainer schema opts)}]
        (swap! compiled assoc schema v)
        v)))

(defn valid?
  [schema x] ((:valid? (compiled-for schema)) x))

(defn explain
  [schema x] (some-> ((:explain (compiled-for schema)) x) me/humanize))

(def ^:dynamic *check*
  false)

(defn checking? [] *check*)

(defn check!
  [schema x]
  (when (and *check* (not (valid? schema x)))
    (anomaly/raise! :fault "seam contract violation" {:schema schema :problem (explain schema x)}))
  x)

(defn http-check
  [schema x]
  (when-not (valid? schema x)
    (explain schema x)))

(def ^:private json-decoders (atom {}))

(defn decode-json
  [schema x]
  ((or (get @json-decoders schema)
       (let [d (m/decoder schema opts (mt/json-transformer))]
         (swap! json-decoders assoc schema d)
         d))
   x))
