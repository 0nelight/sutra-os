(ns sutra.authoring.http.oms
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.store.world :as world]
            [sutra.objects.api :as objects]
            [sutra.authoring.preview :as preview]
            [sutra.actions.api :as actions]
            [sutra.store.edge :as edge]
            [sutra.authoring.install.world :as install-world]
            [sutra.authoring.install.findings.sql :as sql-findings]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.retirement :as retirement]
            [sutra.catalog.values :as values]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.world-rules :as rules]
            [sutra.catalog.kernel :as kernel]
            [sutra.parser.project-export :as pe]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.types.paths :as paths]
            [sutra.types.finding :as finding]
            [sutra.types.schema :as sch]
            [sutra.types.ontologies :as ontologies]
            [sutra.types.schema-reach :as reach]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.types.forms :as forms]
            [sutra.parser.names :as names]
            [sutra.parser.lint :as lint]
            [sutra.store.access.scope :as ctx]
            [sutra.projects.api :as projects]
            [sutra.store.access.datasets :as dsets]
            [sutra.authoring.mutation :as mutation-cmd :refer [merge-and-install]]
            [sutra.authoring.http.mutation :as mutation]
            [sutra.authoring.ports :as ports]
            [sutra.authoring.stage :as stage]
            [sutra.authoring.instance-change :as instance]
            [sutra.store.seat :as seat]
            [sutra.http.endpoint :as ep]
            [sutra.http.refusals :as refusals]
            [sutra.http.responses :refer [ok fault]]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(defn set-status!
  [{:keys [path-params body-params] store :sutra/store current :sutra/schema branch :sutra/branch
    edge :sutra/edge}]
  (let [{:keys [kind name]} path-params
        kinds-key (rid/tag->kinds-key (keyword kind))
        {:keys [status deprecation]} body-params]
    (cond
      (nil? kinds-key) (refusal/refuse! :refusal.authoring/unknown-kind {:kind kind})
      (not status)     (refusal/refuse! :refusal.authoring/status-missing body-params)
      (and (contains? retirement/object-type-only-statuses (keyword status))
           (not= :object-types kinds-key))
      (refusal/refuse! :refusal.authoring/object-type-only-status {:kind kind :status status}
                       {:status-kw (keyword status)})
      :else
      (let [schema (or current {})
            r (rid/resolve-name->rid (sch/name-index schema) #{kinds-key} name)]
        (if-not (and r (get-in schema [kinds-key r]))
          (refusal/refuse! :refusal.authoring/no-entity-named {:kind kind :name name})
          (ok (merge-and-install (ctx/at store branch) current
                                 (fn [s]
                                   (retirement/set-status s kinds-key name status deprecation))
                                 {:edge edge})))))))

(def list-functions
  (ep/handler :function-schema {:call (fn [{:keys [schema]}] (or (:functions schema) {}))}))

(def get-function (ep/entry :function-by :functions :refusal.authoring/function-not-found))

(defn rename-transform
  [{:keys [path-params body-params] store :sutra/store current :sutra/schema branch :sutra/branch
    edge :sutra/edge}]
  (mutation/answer
   (mutation-cmd/rename-transform (ctx/at store branch) current (:name path-params)
                                  (:to body-params) {:edge edge})))

(def list-transforms
  (ep/handler :transform
              {:call (fn [{:keys [schema]}] {:transforms (or (:transforms schema) {})})}))

(def get-transform (ep/entry :transform-by :transforms :refusal.authoring/transform-not-found))

(defn- recompile-board
  [schema body]
  (outputs/recompile-board (or schema {}) body))

(def add-transform
  (mutation/step-handler :transform
                         #(kernel/step (ctx/code) :create :transform
                                       (recompile-board (:schema %) (:body %)))))

(def update-transform
  (mutation/step-handler :transform-by
                         #(kernel/step (ctx/code) :update :transform
                                       (assoc (recompile-board (:schema %) (:body %))
                                              :name (:name %)))))

(def delete-action-type (mutation/step :action-type-by :action :delete))

(defn- judged-dataset
  [auth schema {:keys [rid spec section]}]
  (case (get-in ents/by-section [section :rows-of])
    :self   rid
    :output (when-let [ds (some :dataset (outputs/outputs-of spec))]
              (grants/authorize-read! auth (get-in schema [:datasets ds]) ds)
              ds)
    nil))

(def transform-rows
  (ep/handler :transform-rows
              {:path {:name :kw}
               :respond (fn [{:keys [store branch name as-of auth schema req]}]
                          (if-let [ds (judged-dataset auth schema
                                                      (get-in req [:sutra/targets :name]))]
                            (try
                              (ok {:transform name :as-of as-of
                                   :rows (dsets/rows (ctx/at store branch) ds as-of)})
                              (catch Exception e
                                (fault e {:transform name :as-of as-of})))
                            (refusal/refuse! :refusal.authoring/no-transform-or-dataset
                                             {:transform name})))}))

(def transform-spec-at
  (ep/handler :transform-spec
              {:path {:name :kw}
               :respond (fn [{:keys [store branch name as-of]}]
                          (try
                            (let [schema (or (world/as-of (world/at store branch) as-of) {})
                                  spec
                                  (get-in schema
                                          [:transforms (lookup/rid-of schema :transforms name)])]
                              (if spec
                                (ok {:transform name :as-of as-of :spec spec})
                                (refusal/refuse! :refusal.authoring/transform-not-at-commit
                                                 {:transform name :as-of as-of})))
                            (catch Exception e
                              (fault e {:transform name :as-of as-of}))))}))

(defn- schema-fingerprint
  [schema]
  (sch/column schema ::fingerprint #(str (hash %))))

(defn- schema-parts
  [schema]
  (let [{:keys [import-plan sources unresolved]} (ports/schema-plan schema)]
    {:schema         (cond-> (reach/catalog schema)
                       (:sources schema) (update :sources update-vals values/public-source))
     :schema-derived (objects/derive-all schema)
     :import-plan    import-plan
     :datasources    (values/public-datasources schema)
     :sources        sources
     :unresolved     unresolved}))

(defn- in-chosen-ontology
  [schema req]
  (let [s (when (seq (:spaces schema))
            (some->> (get-in req [:headers "x-sutra-ontology"])
                     (ontologies/resolve-ontology schema)))]
    (if s (ontologies/in-ontology schema s) schema)))

(def get-snapshot
  (ep/handler
   :snapshot
   {:respond
    (fn [{:keys [store branch schema since req]}]
      (let [schema (in-chosen-ontology schema req)
            snap (objects/app-snapshot (ctx/at store branch) schema)
            v    (schema-fingerprint schema)]
        (ok (if (= since v)
              (assoc snap :schema-version v :schema-unchanged true)
              (let [{:keys [unresolved] :as parts} (schema-parts schema)]
                (-> snap
                    (merge (dissoc parts :unresolved))
                    (assoc :schema-version v)
                    (update :integrity
                            #(cond-> % (seq unresolved)
                                     (assoc :unresolved-backings unresolved)))))))))}))

(def get-schema
  (ep/handler :schema {:call :schema}))

(defn names-index
  [schema]
  (sch/column schema ::names #(names/index (forms/installed %))))

(def ^:private kind-alias {:object :object-type :link :link-type})

(defn name-suggestions
  [schema nm kind site]
  (names/suggest (names-index schema) nm (get kind-alias kind (or kind :object-type)) site))

(def name-suggest
  (ep/handler :schema-name-suggest
              {:call (fn [{:keys [schema name kind site]}]
                       (merge {:name name :verdict :create :exact [] :candidates []}
                              (when (seq name) (name-suggestions schema name kind site))))}))

(def get-by-rid
  (ep/handler :schema-by-rid
              {:respond
               (fn [{:keys [store branch schema rid]}]
                 (let [kk     (some (fn [kk] (when (contains? (get schema kk) rid) kk))
                                    rid/rid-kinds)]
                   (cond
                     kk
                     (ok {:rid rid :kind kk
                          :name (get-in schema [kk rid (rid/name-field-of kk)])
                          :spec (get-in schema [kk rid])})

                     (#{:folder :placement} (rid/rid-kind rid))
                     (if-let [node (projects/kompass-node (ctx/at store branch) rid)]
                       (ok {:rid rid :kind (:node-kind node) :name (:name node)
                            :parent-rid (:parent-rid node) :ref (:ref node)
                            :resource (rid/parse-ref (:ref node))})
                       (refusal/refuse! :refusal.authoring/no-resource-with-rid {:rid rid}))

                     :else
                     (refusal/refuse! :refusal.authoring/no-resource-with-rid {:rid rid}))))}))

(defn stage-schema
  [{store :sutra/store current :sutra/schema branch :sutra/branch project :sutra/project
    seen :sutra/edge :as req}
   {:keys [preview? justification] :as params} scope]
  (try (let [b          (or branch pe/trunk-branch)
             [inst own] (instance/split (instance/instance-pred current)
                                        {:schema (:schema params) :opts scope})
             arrived    (when (and seen (not= b (instance/home store b)))
                          (instance/arrive! store seen nil b inst preview? justification))
             current    (if (and arrived (not preview?))
                          (seat/branch-schema (world/at store b))
                          current)
             params     (if arrived (assoc params :schema (:schema own)) params)
             scope      (if arrived (:opts own) scope)]
         (ok (merge (stage/stage! {:store store :schema current :branch branch
                                   :project project}
                                  params scope)
                    arrived)))
       (catch clojure.lang.ExceptionInfo e
         (if-let [r (anomaly/of e)] (refusals/response req r) (throw e)))))

(defn install-preset
  [{:keys [body-params] :as req}]
  (stage-schema req body-params nil))

(defn blocking-drift
  [physical]
  (dissoc physical :orphan-tables :install-failures))

(defn- trunk-findings
  [store branch]
  (when-not (= pe/trunk-branch (str branch))
    (:findings (actions/of-staging-schema (ctx/at store pe/trunk-branch)
                                          ::assessment kernel/assessment))))

(defn- inherited-findings
  [findings trunk]
  (let [held (into #{} (map (juxt :rule :entity)) trunk)]
    (mapv #(cond-> % (held [(:rule %) (:entity %)]) (assoc :inherited? true)) findings)))

(defn schema-problems
  [{store :sutra/store branch :sutra/branch e :sutra/edge}]
  (let [cst      (ctx/at store branch)
        view     (edge/staging-view e)
        {logical :reasons findings :findings}
        (actions/of-staging-schema (ctx/at cst (:read view)) ::assessment kernel/assessment)
        physical (preview/preview-physical-drift cst view)
        blocking (blocking-drift physical)
        problems (cond-> (or logical {})
                   (seq physical) (assoc :physical-drift physical))]
    (ok {:valid?   (and (nil? logical) (empty? blocking))
         :findings (inherited-findings findings (trunk-findings store branch))
         :problems problems})))

(def prune-orphan-tables
  (ep/handler :schema-prune-orphans
              {:call
               #(preview/prune-orphan-tables! (ctx/at (:store %) (:branch %))
                                              (edge/staging-view (:edge %))
                                              {:dry-run? (:dry-run %)})}))

(defn schema-sql-check
  [{:keys [body-params] store :sutra/store current :sutra/schema}]
  (let [authored (:transforms body-params)
        specs    (if (map? authored)
                   (vec (for [[nm spec] authored
                              :when (sql-findings/sql-transform? spec)]
                          [(->kw nm) spec]))
                   (vec (sql-findings/live-sql-specs current)))
        verdicts (sql-findings/sql-verdicts store (ctx/code) current specs)
        findings (finding/worst-first
                  (concat (sql-findings/sql-trap-findings specs)
                          (sql-findings/sql-compile-findings verdicts)))
        refused  (into #{} (comp (filter finding/blocking?) (map :entity)) findings)
        unknown  (into #{} (comp (filter #(= :sql-unverified (:rule %))) (map :entity)
                                 (remove refused))
                       findings)]
    (ok {:checked   (mapv first specs)
         :compiled? (some? verdicts)
         :counts    {:checked (count specs)
                     :refused (count refused)
                     :unknown (count unknown)
                     :passed  (if verdicts (- (count specs) (count refused) (count unknown)) 0)}
         :findings  findings})))

(defn schema-lint
  [{store :sutra/store e :sutra/edge}]
  (let [read     (ctx/at store (:read (edge/staging-view e)))
        findings (stage/staged-lint read)]
    (ok {:findings findings :summary (lint/summary findings)})))

(def object-type-spec-at-commit
  (ep/handler :at-object-type
              {:respond
               (fn [{:keys [store branch commit type]}]
                 (try
                   (let [schema   (or (world/as-of (world/at store branch) commit) {})
                         type-rid
                         (lookup/rid-of schema :object-types
                                        (if (rid/rid? type) type (->kw type)))
                         spec     (get-in schema [:object-types type-rid])]
                     (if spec
                       (ok {:commit commit :type type-rid :spec spec})
                       (refusal/refuse! :refusal.authoring/type-not-at-commit
                                        {:commit commit :type type})))
                   (catch Exception e
                     (fault e {:commit commit :type type}))))}))

(defn schema-backing
  [{schema :sutra/schema}]
  (let [nm     (fn [x] (when x (if (keyword? x) (name x) (str x))))]
    (ok {:object-types
         (into {} (for [[rid spec] (:object-types schema)
                        :let [pk (rules/object-primary-key schema rid)]
                        :when pk]
                    [(nm (:object-type-name spec)) {:primary-key (nm pk)}]))
         :link-types
         (into {} (for [[rid spec] (:link-types schema)
                        :let [bk (backing/link-backing schema rid)]
                        :when bk]
                    [(nm (:link-type-name spec))
                     (-> bk
                         (update :kind nm)
                         (update :producer nm)
                         (cond-> (:language bk)        (update :language nm)
                                 (:shared-property bk) (update :shared-property nm)
                                 (:from-key-column bk) (update :from-key-column nm)
                                 (:to-key-column bk)   (update :to-key-column nm)
                                 (:fk-attribute bk)    (update :fk-attribute nm)))]))})))

(def ^:private crud-routes
  (into {} (mapcat (fn [{:keys [kind delete] :as row}]
                     (let [{:keys [base by rename]} (paths/crud-ids row)]
                       (cond-> {base   {:post (mutation/step base kind :add)}
                                by     {:put    (mutation/step by kind :update)
                                        :delete (mutation/step by kind (or delete :delete))}}
                         (not (get-in ents/by-tag [kind :renamed-with-code?]))
                         (assoc rename {:post (mutation/rename-handler
                                               rename (rid/tag->kinds-key kind))})))))
        paths/schema-crud))

(def ^:private project-renames
  {:project-rename    {:post (mutation/rename-handler :project-rename :projects)}
   :repository-rename {:post (mutation/rename-handler :repository-rename :repositories)}})

(def module
  {:name        :oms
   :provides    {:world/install install-world/install-world!
                 :schema/install mutation-cmd/install!}
   :contributes {:http/routes
                 (merge-with merge crud-routes project-renames
                             {:action-type-by           {:delete delete-action-type}
                              :at-object-type           {:get object-type-spec-at-commit}
                              :function-by              {:get get-function}
                              :function-schema          {:get list-functions}
                              :install                  {:post install-preset}
                              :schema                   {:get get-schema}
                              :schema-backing           {:get schema-backing}
                              :schema-by-rid            {:get get-by-rid}
                              :schema-lint              {:get schema-lint}
                              :schema-name-suggest      {:get name-suggest}
                              :schema-problems          {:get schema-problems}
                              :schema-prune-orphans     {:post prune-orphan-tables}
                              :schema-sql-check         {:post schema-sql-check}
                              :set-status               {:post set-status!}
                              :snapshot                 {:get get-snapshot}
                              :transform                {:get list-transforms :post add-transform}
                              :transform-by             {:get get-transform :put update-transform}
                              :transform-rename         {:post rename-transform}
                              :transform-rows           {:get transform-rows}
                              :transform-spec           {:get transform-spec-at}})}})
