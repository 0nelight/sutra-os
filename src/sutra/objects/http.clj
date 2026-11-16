(ns sutra.objects.http
  (:require [sutra.catalog.lookup :as lookup]
            [sutra.store.access.scope :as ctx]
            [sutra.objects.reads :as objects]
            [sutra.objects.ports :as ports]
            [sutra.store.edge :as edge]
            [sutra.store.seat :as seat]
            [sutra.objects.knn :as knn]
            [sutra.objects.views :as views]
            [sutra.objects.saved-sets :as temp-sets]
            [sutra.objects.explorer :as explorer]
            [sutra.catalog.project-scope :as scope]
            [clojure.string :as str]
            [sutra.catalog.grants :as grants]
            [sutra.types.object-set :as os]
            [sutra.store.access.substrate :as sub]
            [sutra.http.endpoint :as ep]
            [sutra.projects.api :as projects]
            [sutra.types.rid :as rid]
            [sutra.http.responses :refer [ok fault]]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(def get-objects
  (ep/handler :objects-of
              {:respond
               (fn [{:keys [store branch type limit offset] :as a}]
                 (if-let [rid (lookup/->type-rid (:schema a) :object-types type)]
                   (let [offset (max 0 offset)
                         schema (seat/rebuild-schema-from-storage (ctx/at store branch))
                         nm     (get-in schema [:object-types rid :object-type-name])
                         {:keys [rows count]} (views/run-object-query
                                               (ctx/at store branch) schema
                                               {:find nm :limit limit :offset offset :count? true})
                         seen (+ offset (clojure.core/count rows))]
                     (ok {:rows        (vec rows)
                          :offset      offset
                          :limit       limit
                          :total       count
                          :next-offset (when (< seen count) seen)}))
                   (refusal/refuse! :refusal.objects/no-object-type {:type type})))}))

(def knn-objects
  (ep/handler :objects-knn
              {:respond
               (fn [{:keys [store schema branch type vector q k metric]}]
                 (if-let [rid (lookup/->type-rid schema :object-types type)]
                   (if-let [ss (get-in schema [:object-types rid :semantic-search])]
                     (let [qv (or (knn/parse-query-vector vector)
                                  (when q (knn/embed (ctx/at store branch) schema (:model ss) q)))]
                       (if-not (seq qv)
                         (refusal/refuse! (if q
                                            :refusal.objects/no-embedding-model
                                            :refusal.objects/knn-query-missing)
                                          {:type type})
                         (let [k      (or k (:k ss))
                               metric (or metric (:metric ss))]
                           (ok {:type type :k (or k 10) :metric (or metric :cosine)
                                :results
                                  (knn/nearest qv
                                               (objects/objects-of-type
                                                (ctx/at store branch) schema rid)
                                               {:property (:property ss) :k k :metric metric})}))))
                     (refusal/refuse! :refusal.objects/no-semantic-search {:type type}))
                   (refusal/refuse! :refusal.objects/no-object-type {:type type})))}))

(def list-links
  (ep/handler :links
              {:respond
               (fn [{:keys [store schema branch type from to limit]}]
                 (let [filters  (cond-> {:from-id from :to-id to}
                                  (and limit (pos? limit)) (assoc :limit limit))
                       type-rid (when type (lookup/->type-rid schema :link-types type))]
                   (if (and type (nil? type-rid))
                     (refusal/refuse! :refusal.objects/no-link-type {:type type})
                     (ok (vec (if type-rid
                                (objects/links-of-type
                                 (ctx/at store branch) schema type-rid filters)
                                (objects/all-links (ctx/at store branch) schema filters)))))))}))

(def query-objects
  (ep/handler :ontology-query
              {:call (fn [{:keys [store branch body explain accuracy engine]}]
                       (let [schema (seat/rebuild-schema-from-storage (ctx/at store branch))]
                         (views/run-object-query (ctx/at store branch) schema
                                              (cond-> body
                                                explain  (assoc :explain? true)
                                                accuracy (assoc :accuracy accuracy)
                                                engine   (assoc :engine engine)))))}))

(def query-series
  (ep/handler :ontology-series
              {:respond (fn [{:keys [store branch body]}]
                          (let [schema (seat/rebuild-schema-from-storage (ctx/at store branch))]
                            (ok (views/run-series-query (ctx/at store branch) schema body))))}))

(def list-object-sets
  (ep/handler :object-sets
              {:call (fn [{:keys [store branch]}]
                       (let [schema (seat/rebuild-schema-from-storage (ctx/at store branch))
                             saved  (for [[r spec] (:object-sets schema)
                                          :when
                                            (grants/read-allowed?
                                             (grants/read-rights store) (:project spec)
                                             (:ignore-inherited spec))]
                                      {:rid r :name (some-> (:object-set-name spec) name)
                                       :project (:project spec)
                                       :description (:description spec)
                                       :lifecycle :permanent
                                       :kind (os/definition-kind (os/qt->ir (os/definition spec)))
                                       :param-slots (:param-slots spec)})]
                         {:object-sets (vec saved)
                          :temporary   (mapv #(dissoc % :ir)
                                             (temp-sets/list-for
                                              store (grants/acting-for store)))}))}))

(def create-temporary-object-set
  (ep/handler :object-sets-temp
              {:respond
               (fn [{:keys [store branch body]}]
                 (let [{:keys [ir materialize]} body
                       schema (seat/rebuild-schema-from-storage (ctx/at store branch))
                       me     (grants/acting-for store)
                       spec   (dissoc body :ir :materialize)]
                   (cond
                     (nil? me)
                     (refusal/refuse! :refusal.objects/temp-set-no-principal {})

                     (and (nil? ir) (empty? spec))
                     (refusal/refuse! :refusal.objects/temp-set-spec-missing {})

                     (some #(and (= :search-around (:op %)) (contains? % :dir)) (os/nodes ir))
                     (refusal/refuse! :refusal.objects/search-around-dir {})

                     :else
                     (let [ir (if ir
                                (os/resolve-sides schema ir)
                                (:ir (views/object-set-ir store schema spec)))
                           ir (if materialize
                                (views/materialize (ctx/at store branch) schema ir)
                                ir)]
                       (ok (temp-sets/create! store
                                              {:rid         (os/new-temporary-rid (random-uuid))
                                         :ir          ir
                                         :kind        (os/definition-kind ir)
                                         :created-by  me
                                         :branch      branch
                                         :ttl-seconds (os/temporary-ttl-seconds)}))))))}))

(def delete-temporary-object-set
  (ep/handler :object-set-temp-by
              {:call (fn [{:keys [store rid]}]
                       {:deleted (temp-sets/revoke! store rid (grants/acting-for store))})}))

(defn- set-definition
  [store branch schema {:keys [kind query]}]
  (let [at (ctx/at store branch)
        ir (:ir (views/object-set-ir at schema query))]
    (if (= :list kind) (views/materialize at schema ir) ir)))

(defn- ->kw [x] (some-> x name keyword))

(def list-explorations
  (ep/handler :explorations
              {:call (fn [{:keys [store object-type]}]
                       {:explorations (explorer/list-files store {:object-type object-type})})}))

(def create-exploration
  (ep/handler :explorations
              {:respond
               (fn [{:keys [store branch schema body]}]
                 (let [ot   (->kw (:object-type body))
                       kind (->kw (:kind body))
                       nm   (some-> (:name body) str/trim not-empty)]
                   (if-not (and ot kind nm (:query body))
                     (refusal/refuse! :refusal.objects/exploration-fields {})
                     (do (explorer/readable-type! store schema ot)
                         (ok (explorer/create!
                              store
                              (assoc (select-keys body [:description :state])
                                     :kind kind :object-type ot :name nm
                                     :project (scope/resolve-project schema (:project body))
                                     :definition (set-definition store branch schema
                                                                 {:kind kind
                                                                  :query (:query body)}))))))))}))

(def get-exploration
  (ep/handler :exploration
              {:respond (fn [{:keys [store id]}]
                          (if-let [f (explorer/read-file store id)]
                            (ok f)
                            (refusal/refuse! :refusal.objects/no-exploration {:id id})))}))

(defn- saved-changes
  [store branch schema {:keys [kind]} {:keys [query] :as body}]
  (cond-> (select-keys body [:name :description :state])
    query (assoc :definition (set-definition store branch schema {:kind kind :query query}))))

(def update-exploration
  (ep/handler :exploration
              {:respond
               (fn [{:keys [store branch schema auth id body]}]
                 (if-let [f (explorer/read-file store id)]
                   (ok (cond
                         (:private body) (explorer/make-private! store id)
                         (:project body) (explorer/move! store auth id
                                                         (scope/resolve-project schema
                                                                                (:project body)))
                         :else (explorer/update! store id
                                                 (saved-changes store branch schema f body))))
                   (refusal/refuse! :refusal.objects/no-exploration {:id id})))}))

(def delete-exploration
  (ep/handler :exploration
              {:respond (fn [{:keys [store id]}]
                          (if-let [out (explorer/delete! store id)]
                            (ok out)
                            (refusal/refuse! :refusal.objects/no-exploration {:id id})))}))

(def list-instances
  (ep/handler :instances
              {:respond (fn [{:keys [store branch graph since edge] :as a}]
                          (if (contains? a :graph)
                            {:status 410
                             :body
                               {:error "the bulk instance graph is gone — /api/instances carries the schema projection only"
                                :data  {:graph graph}
                                :hint  (str "read instances through a bounded query: "
                                            "POST /api/ontology/query {:find <type> :where … "
                                            ":order-by … "
                                            ":limit … :offset … :count? true}, or seed the canvas "
                                            "via "
                                            "GET /api/instances/search and expand with "
                                            "/api/instances/around.")}}
                            (let [p (ports/preview-projection (ctx/at store branch)
                                                              (edge/staging-view edge))]
                              (if (and since (= since (:version p)))
                                (ok {:unchanged true :version (:version p)})
                                (ok p)))))}))

(def search-instances
  (ep/handler :instances-search
              {:call #(objects/search-objects (ctx/at (:store %) (:branch %))
                                              (edge/staging-view (:edge %))
                                              (:q %) (:type %) (:limit %))}))

(def instances-around
  (ep/handler :instances-around
              {:call (fn [{:keys [store branch edge] {:keys [ids types] :as body} :body}]
                       (objects/neighbors (ctx/at store branch) (edge/staging-view edge)
                                          (or ids []) types
                                          (select-keys body [:links-only? :among?])))}))

(def object-counts
  (ep/handler :object-counts
              {:call (fn [{:keys [store schema branch]}]
                       (objects/object-counts (ctx/at store branch) schema))}))

(def objects-at-commit
  (ep/handler :at-objects
              {:respond (fn [{:keys [store schema branch commit type]}]
                          (if-let [type-rid (lookup/->type-rid schema :object-types type)]
                            (try
                              (ok {:commit commit :type type-rid
                                   :rows (sub/read-type (ctx/at store branch) type-rid commit)})
                              (catch Exception e
                                (fault e {:commit commit :type type-rid})))
                            (refusal/refuse! :refusal.objects/no-object-type-on-branch
                                             {:commit commit :type type})))}))

(def project-map
  (ep/handler :project-map
              {:respond (fn [{:keys [store branch schema project]}]
                          (let [answer (projects/project-map schema
                                                               (:applications
                                                                (rid/name-keyed schema))
                                                               (:by-type
                                                                (objects/object-counts
                                                                 (ctx/at store branch) schema))
                                                               project)]
                            (if (:error answer)
                              (refusal/refuse! :refusal.objects/project-map
                                               (dissoc answer :error) {:error (:error answer)})
                              (ok answer))))}))

(def module
  {:name        :oss
   :provides    {:objects/reads {:query     views/run-object-query
                                 :resolve   views/resolve-object-ref
                                 :connected views/connected}}
   :contributes {:kompass/movers {:exploration explorer/move!}
                 :kompass/file-rows {:exploration (fn [store _ id] (explorer/row store id))}
                 :http/routes
                 {:at-objects         {:get objects-at-commit}
                  :project-map        {:get project-map}
                  :instances          {:get list-instances}
                  :instances-around   {:post instances-around}
                  :instances-search   {:get search-instances}
                  :links              {:get list-links}
                  :object-counts      {:get object-counts}
                  :object-set-temp-by {:delete delete-temporary-object-set}
                  :object-sets        {:get list-object-sets}
                  :explorations       {:get list-explorations :post create-exploration}
                  :exploration        {:get get-exploration :put update-exploration
                                       :delete delete-exploration}
                  :object-sets-temp   {:post create-temporary-object-set}
                  :objects-knn        {:get knn-objects}
                  :objects-of         {:get get-objects}
                  :ontology-query     {:post query-objects}
                  :ontology-series    {:post query-series}}}})
