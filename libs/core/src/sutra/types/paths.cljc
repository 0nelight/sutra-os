(ns sutra.types.paths
  (:require [clojure.string :as str]
            [sutra.types.table :as tbl]
            [sutra.types.paths.rules :as rules]
            [sutra.types.paths.access :as access]
            [sutra.types.paths.actions :as actions]
            [sutra.types.paths.agents :as agents]
            [sutra.types.paths.apps :as apps]
            [sutra.types.paths.authoring :as authoring]
            [sutra.types.paths.automation :as automation]
            [sutra.types.paths.branching :as branching]
            [sutra.types.paths.builds :as builds]
            [sutra.types.paths.canvas :as canvas]
            [sutra.types.paths.checkouts :as checkouts]
            [sutra.types.paths.compute :as compute]
            [sutra.types.paths.documents :as documents]
            [sutra.types.paths.evals :as evals]
            [sutra.types.paths.feedback :as feedback]
            [sutra.types.paths.health :as health]
            [sutra.types.paths.lineage :as lineage]
            [sutra.types.paths.linter :as linter]
            [sutra.types.paths.llm :as llm]
            [sutra.types.paths.media :as media]
            [sutra.types.paths.notifications :as notifications]
            [sutra.types.paths.objects :as objects]
            [sutra.types.paths.peering :as peering]
            [sutra.types.paths.projects :as projects]
            [sutra.types.paths.schedules :as schedules]
            [sutra.types.paths.serve :as serve]
            [sutra.types.paths.sources :as sources]
            [sutra.types.paths.widget-sets :as widget-sets]))

(def schema-crud
  [{:kind :object-type :section :object-types :delete :cascade}
   {:kind :link-type :section :link-types :id :conn-type :delete :cascade}
   {:kind :interface :section :interfaces :delete :cascade}
   {:kind :object-type-group :section :object-type-groups}
   {:kind :function :section :functions :base :function-schema :read? true}
   {:kind :transform :section :transforms :read? true}
   {:kind :dataset :section :datasets :base :datasets :seg "datasets" :service :conductor
    :branch? true :delete :cascade :read? true}])

(defn crud-ids
  [{:keys [kind id base]}]
  (let [id (or id kind)]
    {:base (or base id) :by (keyword (str (name id) "-by"))
     :rename (keyword (str (name id) "-rename"))}))

(def reads rules/reads)
(def writes rules/writes)
(def ^:private effect-sets #{reads writes})

(def default-deadline-ms 20000)

(def ^:private listed-endpoints
  (let [parts [access/endpoints
               actions/endpoints
               agents/endpoints
               apps/endpoints
               authoring/endpoints
               automation/endpoints
               branching/endpoints
               builds/endpoints
               canvas/endpoints
               checkouts/endpoints
               compute/endpoints
               documents/endpoints
               evals/endpoints
               feedback/endpoints
               health/endpoints
               lineage/endpoints
               linter/endpoints
               llm/endpoints
               media/endpoints
               notifications/endpoints
               objects/endpoints
               peering/endpoints
               projects/endpoints
               schedules/endpoints
               serve/endpoints
               sources/endpoints
               widget-sets/endpoints]
        twice (for [[id n] (frequencies (mapcat keys parts)) :when (< 1 n)] id)]
    (assert (empty? twice) (str "endpoint ids declared by two modules: " (vec twice)))
    (apply merge parts)))

(defn- crud
  [k]
  (keyword "sutra.foundation.contracts.c" (name k)))

(def endpoints
  (into listed-endpoints
        (mapcat (fn [{:keys [kind seg service delete branch? section read?] :as row}]
                  (let [url (str "/api/" (or seg (str "schema/" (name kind))))
                        svc (or service :oms)
                        ids (crud-ids row)
                        on  [section :name]]
                    {(:base ids)   (cond-> {:url url :service svc
                                            :methods (cond-> {:post {:effects  writes
                                                                     :requires rules/c-schema
                                                                     :request  (crud :entity-in)
                                                                     :response (crud :schema-step)}}
                                                       read? (assoc :get {:effects  reads
                                                                          :requires rules/m-onto}))}
                                     branch? (assoc :branch? true))
                     (:by ids)     (cond-> {:url (str url "/:name") :service svc
                                            :methods
                                            (cond-> {:put    {:effects  writes
                                                              :requires {:op :update :on on}
                                                              :request  (crud :entity-in)
                                                              :response (crud :schema-step)}
                                                     :delete {:effects  writes
                                                              :requires {:op :delete :on on}
                                                              :response (crud :schema-step)}}
                                              read? (assoc :get {:effects  reads
                                                                 :requires {:op :read :on on}}))}
                                     (= :cascade delete) (assoc :query {:cascade :bool}))
                     (:rename ids) {:url (str url "/:name/rename") :service svc
                                    :request (crud :rename-in)
                                    :methods {:post {:effects  writes
                                                     :requires {:op :update :on on}}}}})))
        schema-crud))

(let [bad (into (sorted-set)
                (keep (fn [[id r]] (when-not (and (:url r) (:service r) (seq (:methods r))) id)))
                endpoints)]
  (assert (empty? bad)
          (str "endpoint rows missing :url, :service or :methods: " (vec bad))))

(def ^:private http-methods #{:get :post :put :delete})

(defn methods-of
  [id]
  (set (keys (get-in endpoints [id :methods]))))

(defn- method-row
  [id method]
  (get-in endpoints [id :methods (if (= :head method) :get method)]))

(defn effects
  [id method]
  (:effects (method-row id method)))

(defn writes?
  [id method]
  (some-> (effects id method) (contains? :write)))

(let [bad (tbl/where endpoints #(when-let [d (:deadline %)]
                                  (not (or (= :unbounded d) (pos-int? d)))))]
  (assert (empty? bad)
          (str ":deadline is ms or :unbounded: " (vec (sort bad)))))

(defn deadline-ms
  ([id] (deadline-ms id default-deadline-ms))
  ([id default]
   (let [d (get-in endpoints [id :deadline])]
     (cond (= :unbounded d) nil
           (some? d)        d
           :else            default))))

(let [bad (tbl/where endpoints #(and (= :admin (:auth %)) (nil? (:reason %))))]
  (assert (empty? bad)
          (str ":auth :admin rows missing :reason: " (vec (sort bad)))))

(def audits
  #{:access :schema})

(let [bad (tbl/where endpoints #(and (contains? % :audit) (not (audits (:audit %)))))]
  (assert (empty? bad) (str ":audit not one of " audits ": " (vec (sort bad)))))

(def declared-auth
  #{:admin :public :build :repo-token :consumer})

(defn- requirement-problem
  [{:keys [member custom op on also] :as spec}]
  (cond
    (nil? spec)                                                 "no :requires"
    (not (map? spec))                                           "not a map"
    (not= 1 (count (filter some? [member custom op])))
    "needs exactly one of :member, :custom, :op"
    (and (or member custom) (not (string? (or member custom)))) "a reason is a string"
    (and op (not (keyword? op)))                                ":op is an operation"
    (some #(and % (not= 2 (count %))) [on also])                ":on is [section param]"
    (and (or member custom) (or on also))                       "a reason has no target"))

(let [bad (for [[id r] endpoints
                [m {:keys [effects requires] :as mr}] (:methods r)
                :let [p (cond
                          (not (http-methods m))   "a method is :get :post :put or :delete"
                          (not (effect-sets effects)) ":effects is reads or writes"
                          (declared-auth (:auth r))
                          (when (contains? mr :requires) "the login decides: no :requires")
                          :else (requirement-problem requires))]
                :when p]
            [id m p])]
  (assert (empty? bad)
          (str "endpoint methods with an undeclared contract: " (vec (sort-by first bad)))))

(defn requirement
  [id method]
  (:requires (method-row id method)))

(def paths (tbl/column endpoints :url))

(def consumer-surface
  (into #{} (keep (fn [[id {:keys [url]}]] (when (str/starts-with? (str url) "/api/my-") id)))
        endpoints))

(def api-namespaces
  (into (sorted-set)
        (keep #(some->> (re-find #"^/api/([^/]+)" (str %)) second keyword))
        (vals paths)))

(def query (tbl/column endpoints :query))

(def encoding (tbl/column endpoints :encode))

(def plugin-of (tbl/column endpoints :plugin))

(def admin-only
  (tbl/column (tbl/rows endpoints #(= :admin (:auth %))) :reason any?))

(def branch-scoped (tbl/where endpoints :branch?))

(def in-conductor
  (tbl/where endpoints #(= :conductor (:process %))))

(def in-every-process
  (tbl/where endpoints #(= :every (:process %))))

(defn ids-with-auth [level]
  (keep (fn [[id r]] (when (= level (:auth r)) id)) endpoints))

(defn- seg
  [v]
  (let [s (if (keyword? v) (name v) (str v))]
    #?(:clj  (str/replace (java.net.URLEncoder/encode s "UTF-8") "+" "%20")
       :cljs (js/encodeURIComponent s))))

(defn params-of
  [tmpl]
  (into []
        (comp (filter #(str/starts-with? % ":"))
              (map #(keyword (subs % 1))))
        (str/split tmpl #"/")))

(defn path-for
  [tmpl params]
  (->> (str/split tmpl #"/")
       (map (fn [s]
              (if (str/starts-with? s ":")
                (let [k (keyword (subs s 1))]
                  (if (contains? params k)
                    (seg (get params k))
                    (throw (ex-info "missing path param"
                                    {:template tmpl :param k :params params}))))
                s)))
       (str/join "/")))

(defn url
  ([id] (or (paths id) (throw (ex-info "unknown endpoint" {:id id}))))
  ([id params] (path-for (url id) params)))

(defn query-string
  [m]
  (let [pairs (for [[k v] m :when (some? v)
                    v* (if (sequential? v) (remove nil? v) [v])]
                (str (seg (name k)) "=" (seg v*)))]
    (when (seq pairs) (str "?" (str/join "&" pairs)))))

(def ^:private shapes
  (into {} (map (fn [[id r]] [id {:params  (params-of (:url r))
                                  :allowed (set (keys (:query r)))}]))
        endpoints))

(defn address
  ([id] (address id nil))
  ([id m]
   (let [tmpl (url id)
         {:keys [params allowed]} (shapes id)
         extra (remove (into allowed params) (keys m))]
     (when (seq extra)
       (throw (ex-info "undeclared query param"
                       {:id id :params (vec extra) :declared (vec (keys (query id)))})))
     (str (path-for tmpl (select-keys m params))
          (query-string (select-keys m allowed))))))

(def ^:private templates
  (->> paths
       (map (fn [[id tmpl]] [id (str/split (str tmpl) #"/")]))
       (sort-by (fn [[id segs]] [(- (count (remove #(str/starts-with? % ":") segs))) (str id)]))
       vec))

(defn endpoint-of
  [path]
  (let [p    (-> (str path) (str/replace #"^[a-z]+://[^/]+" "") (str/replace #"[?#].*$" ""))
        segs (str/split p #"/")]
    (some (fn [[id t]]
            (when (and (= (count t) (count segs))
                       (every? (fn [[a b]] (or (= a b)
                                               (and (str/starts-with? a ":") (seq b))))
                               (map vector t segs)))
              id))
          templates)))
