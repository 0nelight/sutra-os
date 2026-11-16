(ns sutra.access.http.auth
  (:require [sutra.kit.refusal :as refusal]
            [sutra.http.request :as http-request]
            [sutra.catalog.project-scope :as scope]
            [sutra.kit.anomaly :as anomaly]
            [sutra.http.refusals :as refusals]
            [sutra.access.http.identity :as idp]
            [sutra.store.world :as world]
            [sutra.store.read-only :as read-only]
            [sutra.store.protection :as protection]
            [sutra.store.api :as api]
            [sutra.access.ports :as ports]
            [sutra.access.password-policy :as password-policy]
            [sutra.access.throttle :as throttle]
            [sutra.http.security-log :as seclog]
            [sutra.store.seat :as seat]
            [sutra.store.run :as run]
            [sutra.store.edge :as edge]
            [clojure.string :as str]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.ontologies :as ontologies]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.access.scope :as ctx]
            [sutra.access.sessions :as sessions]
            [sutra.access.mfa :as mfa]
            [sutra.store.control :as ctl]
            [sutra.access.principals :as pr]
            [sutra.types.ctx :as tctx]
            [sutra.access.authority :as authority]
            [sutra.types.authority :as policy]
            [sutra.kit.datalog :as dl]
            [sutra.types.paths :as paths]
            [sutra.foundation.media-grant :as grant]
            [sutra.access.oauth :as oauth]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok]]
            [sutra.foundation.env :as env]))

(set! *warn-on-reflection* true)

(def ^:private public-paths
  (into #{} (map paths/url) (paths/ids-with-auth :public)))

(defn- template-match?
  [tpl uri]
  (let [ts (str/split tpl #"/" -1) us (str/split uri #"/" -1)]
    (and (= (count ts) (count us))
         (every? true? (map #(or (str/starts-with? %1 ":") (= %1 %2)) ts us)))))

(def ^:private build-templates
  (mapv paths/url (paths/ids-with-auth :build)))

(defn- build-path? [uri] (boolean (some #(template-match? % uri) build-templates)))

(def ^:private consumer-allowed-paths
  (into #{} (map paths/url) (paths/ids-with-auth :consumer)))

(defn admin-gated
  [id handler]
  (let [reason (or (paths/admin-only id)
                   (anomaly/raise! :fault (str "admin-gated on undeclared endpoint " id)
                                          {:id id :declared (set (keys paths/admin-only))}))
        denial (refusal/refused :refusal.access/admin-only-requires-the-admin {} {:reason reason})]
    (with-meta
      (fn [{:keys [principal] :as req}]
        (if (and (= (:username principal) grants/admin-username) (not (:sutra/agent? req)))
          (handler req)
          denial))
      {::admin-gated id})))

(defn- target-sections
  [req sections]
  (cond
    (keyword? sections) [sections]
    (map? sections)     (let [s (keyword (get-in req [:path-params (:param sections)]))]
                          (when-let [sec (if (contains? (:sutra/schema req) s)
                                           s
                                           (rid/tag->kinds-key s))]
                            [sec]))
    :else               sections))

(defn- named-value
  [req param]
  (or (get-in req [:path-params param]) (get-in req [:query-params (name param)])))

(defn- target
  [{:sutra/keys [schema] :as req} [sections param]]
  (when-let [v (named-value req param)]
    (some (fn [sec]
            (when-let [r (if (contains? (get schema sec) v)
                           v
                           (lookup/rid-of schema sec (keyword (str v))))]
              (when-let [spec (get-in schema [sec r])]
                {:rid r :spec spec :section sec})))
          (target-sections req sections))))

(defn- requiring
  [handler {:keys [op on also] :as rq}]
  (fn [{auth :sutra/auth :as req}]
    (let [judged (into {}
                       (for [t (remove nil? [on also])]
                         [(second t) (target req t)]))]
      (doseq [t (or (seq (remove nil? [on also])) [nil])
              :let [{:keys [rid spec]} (when t (get judged (second t)))]]
        (grants/authorize! auth {:op op :project (:project spec)
                                 :ignore-inherited (:ignore-inherited spec)
                                 :resource (or rid (when t (named-value req (second t))))}
                           (or rid "no resource")))
      (handler (assoc req :sutra/requirement rq :sutra/targets judged)))))

(defn gated
  [id method h]
  (let [{:keys [auth]} (paths/endpoints id)
        rq (paths/requirement id method)]
    (cond
      (= :admin auth)         (if (::admin-gated (meta h)) h (admin-gated id h))
      (paths/declared-auth auth) h
      (:op rq)                (with-meta (requiring h rq) (meta h))
      (or (:member rq) (:custom rq)) h
      :else (anomaly/raise! :fault (str "endpoint " id " " method " declares no requirement")
                                   {:id id :method method}))))

(defn methods-mismatch
  [bindings]
  (concat
   (for [[id binds] bindings
         method     (keys binds)
         :when      (not ((paths/methods-of id) method))]
     [id method :undeclared])
   (for [[id binds] bindings
         method     (paths/methods-of id)
         :when      (not (contains? binds method))]
     [id method :unbound])))

(defn gate-bindings
  [bindings]
  (when-let [bad (seq (methods-mismatch bindings))]
    (anomaly/raise! :fault (str "bound methods differ from their contract (paths :methods): "
                                (vec (sort bad)))
                           {:mismatch (vec bad)}))
  (into {}
        (map (fn [[id binds]]
               [id (into {} (map (fn [[method h]] [method (gated id method h)])) binds)]))
        bindings))

(defn list-principals
  [{store :sutra/store schema :sutra/schema auth :sutra/auth}]
  (let [viewer  (get-in auth [:subject :organizations])
        hidden? (if (or (nil? schema) (empty? (:organizations schema))
                        (= grants/admin-username (:username auth)))
                  (constantly false)
                  #(ontologies/principal-hidden? schema viewer
                                                 [(:user-object-id %) (:username %)]))]
    (ok {:principals (vec (for [p (pr/all-rows store)
                                :when (not (hidden? p))]
                            {:user-id  (:user-object-id p)
                             :username (:username p)
                             :admin?   (= (:username p) grants/admin-username)}))})))

(defn- resolve-principal
  [store {:keys [provider source subject]}]
  (if (= provider :password)
    (pr/fetch store subject)
    (pr/fetch-by-external store (or source (name provider)) subject)))

(defn client-of
  [{:keys [headers] :as req}]
  {:user-agent (get headers "user-agent")
   :ip (http-request/client-ip req)})

(defn establish-session!
  ([store assertion] (establish-session! store assertion {}))
  ([store {:keys [principal provider subject client] :as assertion} opts]
   (when assertion
     (if-let [p (or principal (resolve-principal store assertion))]
       (if (pr/service-user? p)
         (do (sessions/record-login!
              store {:outcome :failure :provider provider
                     :username (:username p) :subject subject
                     :detail "a service user signs in only through its OAuth client"})
             nil)
         (sessions/open! store p {:provider provider :subject subject} client opts))
       (do (sessions/record-login! store {:outcome :failure :provider provider
                                          :username subject :subject subject
                                          :detail "principal not provisioned"})
           nil)))))

(defn- password-assertion
  [store username password]
  (when-let [p (pr/verify store (pr/fetch store username) password)]
    {:provider :password :subject username :principal p}))

(def initial-password-hours
  (max 1 (or (some-> (env/getenv "SUTRA_INITIAL_PASSWORD_HOURS") str/trim parse-long) 72)))

(defn- initial-expired?
  [p now-ms]
  (boolean (and (:pw-must-change p)
                (some-> ^java.util.Date (:pw-set-at p) .getTime
                        (+ (* initial-password-hours 3600 1000)) (< now-ms)))))

(defn- first-steps
  [p]
  (cond-> #{}
    (:pw-must-change p)                                           (conj :password)
    (and (mfa/required-for? (:username p)) (not (mfa/enrolled? p))) (conj :mfa)))

(defn- refused-sign-in
  [store username detail outcome]
  (sessions/record-login! store {:outcome :failure :provider :password :username username
                                 :subject username :detail detail})
  outcome)

(defn sign-in!
  [store username password code client]
  (if-let [a (or (password-assertion store username password)
                 (idp/password-assertion store username password))]
    (let [p   (or (:principal a) (resolve-principal store a))
          now (ctl/millis store)]
      (cond
        (nil? p)
        (do (establish-session! store a) nil)

        (initial-expired? p now)
        (refused-sign-in store username "initial password expired"
                         {:refused :initial-password-expired})

        (and (mfa/enrolled? p) (str/blank? (str code)))
        {:mfa-required true}

        (and (mfa/enrolled? p) (not (mfa/check! store (:username p) code now)))
        (refused-sign-in store username "wrong second-factor code" nil)

        :else
        (let [steps (first-steps p)
              token (establish-session! store (assoc a :principal p :client client)
                                        (when (seq steps)
                                          {:scope (sessions/limited-scope steps)}))]
          (when token (cond-> {:token token} (seq steps) (assoc :limited steps))))))
    (refused-sign-in store username "invalid username or password" nil)))

(defn login!
  ([store username password] (login! store username password nil))
  ([store username password client]
   (:token (sign-in! store username password nil client))))

(defn logout! [store token] (sessions/end! store token))

(defn session-for [store token] (sessions/fetch store token))

(def ^:private cookie-max-age (sessions/ttl-of :login))

(def ^:private plain-cookie "sutra-session")

(def ^:private host-cookie "__Host-sutra-session")

(defn- cookie
  [nm value max-age secure?]
  (str nm "=" value "; Path=/; HttpOnly; SameSite=Lax; Max-Age=" max-age
       (when secure? "; Secure")))

(defn session-cookies
  [req token]
  (if (http-request/secure? req)
    [(cookie host-cookie token cookie-max-age true) (cookie plain-cookie "" 0 false)]
    [(cookie plain-cookie token cookie-max-age false)]))

(defn- clear-cookies
  [req]
  (cond-> [(cookie plain-cookie "" 0 false)]
    (http-request/secure? req) (conj (cookie host-cookie "" 0 true))))

(defn access-overview
  [{:keys [principal] store :sutra/store req-branch :sutra/branch}]
  (if (not req-branch)
    (refusal/refused :refusal.http/no-branch {})
    (let [branch (or (:branch principal) "main")]
      (world/with-snapshot (world/at store branch)
        (fn [s]
          (let [schema     (api/read s nil [:schema/world {:derived? true}])
                id->name   (authority/marking-names s schema)
                registry   (vec (sort (map name (vals id->name))))
                cleared-t  (lookup/rid-of schema :link-types :cleared-for)
                user-t     (lookup/rid-of schema :object-types :user)
                users      (when user-t (api/read s grants/system-authority [:objects user-t]))
                id->role   (into {}
                                 (map (fn [u] [(:id u) (some-> (get-in u [:values :rolle]) str)]))
                                 users)
                clears-of  (fn [uid]
                             (if (and id->name cleared-t uid)
                               (into #{} (keep #(some-> (id->name (:to-object-id %)) name))
                                     (api/read s grants/system-authority
                                               [:links cleared-t {:from [(str uid)]}]))
                               #{}))
                prin-rows  (pr/all-rows store)
                clears-by  (into {} (map (fn [p] [(:username p) (clears-of (:user-object-id p))]))
                                 prin-rows)
                principals (vec (for [p prin-rows
                                      :let [uid    (:user-object-id p)
                                            admin? (= (:username p) grants/admin-username)
                                            role   (if admin? "admin" (id->role uid))]]
                                  {:username     (:username p)
                                   :user-id      uid
                                   :role         (or role "—")
                                   :auth-source  (:auth-source p)
                                   :external     (:external-subject p)
                                   :has-password (:has-password p)
                                   :admin        admin?
                                   :consumer     (= role "consumer")
                                   :clearances   (vec (sort (clears-by (:username p))))}))
                kinds      rid/project-owned-kinds
                rcount     (fn [rid] (reduce + (for [k kinds]
                                                 (count (filter (fn [[_ s]] (= (:project s) rid))
                                                                (get schema k))))))
                projects   (conj
                            (vec (for [[rid spec] (:projects schema)]
                                   {:id             rid
                                    :name           (some-> (:project-name spec) str)
                                    :description    (:description spec)
                                    :resource-count (rcount rid)
                                    :grants         (vec (for [g (:grants spec)]
                                                           {:principal (:principal g)
                                                            :role      (some-> (:role g) name)}))}))
                            {:id             nil
                             :name           "unassigned"
                             :description    "resources owned by no project — outside project-based access control"
                             :resource-count (rcount nil)
                             :grants         []})
                roles      (vec (for [[rname _] (sort-by key grants/standard-roles)]
                                  {:role       (name rname)
                                   :operations
                                   (vec (sort (map name (grants/role-operations rname))))}))
                markings   (vec (for [m registry]
                                  {:name       m
                                   :cleared-by
                                   (vec (sort (for [[u cs] clears-by :when (contains? cs m)]
                                                u)))}))
                matrix     (vec (for [p prin-rows
                                      :let [uid    (:user-object-id p)
                                            held
                                            (grants/held-operations
                                             schema
                                             (authority/policy-subject
                                              uid (authority/read-subject s schema uid)))
                                            grants (vec (for [[rid spec] (:projects schema)
                                                              :let [ops (get held rid)]
                                                              :when (seq ops)]
                                                          {:project
                                                           (some-> (:project-name spec) str)
                                                           :operations
                                                           (vec (sort (map name ops)))}))]
                                      :when (seq grants)]
                                  {:username (:username p) :grants grants}))]
            (ok {:principals principals
                 :roles      roles
                 :projects   projects
                 :markings   markings
                 :matrix     matrix})))))))

(defn- cookie-token
  [req]
  (let [header (str (get-in req [:headers "cookie"]))]
    (some #(second (re-find (re-pattern (str "(?:^|;\\s*)" % "=([^;]+)")) header))
          [host-cookie plain-cookie])))
(defn- request-token [req]
  (or (http-request/bearer-token req) (cookie-token req)))

(def ^:dynamic *allow-anon*
  (ep/truthy? (env/getenv "SUTRA_ALLOW_ANON")))

(def ^:private consumer-allowed-prefixes
  ["/api/my-application/" "/api/my-workspace/"])

(defn- consumer-blocked?
  [role uri]
  (and (= role "consumer")
       (not (contains? public-paths uri))
       (not (contains? consumer-allowed-paths uri))
       (not (some #(str/starts-with? uri %) consumer-allowed-prefixes))))

(defn- request-store
  [branch]
  (world/viewing (ctx/store (or branch tctx/default-branch))))

(defn principal-context
  ([sess] (principal-context (request-store (:branch sess)) sess))
  ([store sess] (authority/context-for (world/at store (or (:branch sess) tctx/default-branch))
                                       sess)))

(defn- no-branch! [b]
  (api/refuse! :no-branch (str "no branch " (pr-str b)) {:branch b}))

(defn with-principal-context*
  ([ctx f] (with-principal-context* (request-store nil) ctx f))
  ([store ctx f]
   (with-principal-context* store ctx false (fn [store schema branch _] (f store schema branch))))
  ([store ctx writes? f]
   (let [branch (tctx/branch ctx)
         auth   (tctx/authority ctx)
         e      (edge/read-edge store branch (grants/current-user-id auth) {:writes? writes?})
         schema (or (:schema ctx) (:schema e) (no-branch! branch))]
     (f (ctx/act-for (world/at store branch) auth) schema branch e))))

(def ^:private local-user
  (not-empty (env/getenv "SUTRA_LOCAL_USER")))

(defn- local-browser?
  [{:keys [remote-addr headers]}]
  (let [host   (get headers "host")
        origin (get headers "origin")]
    (and (contains? #{"127.0.0.1" "0:0:0:0:0:0:0:1" "::1"} remote-addr)
         (some? host)
         (re-matches #"(127\.0\.0\.1|localhost|\[::1\])(:\d+)?" host)
         (or (nil? origin) (= origin (str "http://" host)))
         (contains? #{nil "same-origin" "none"} (get headers "sec-fetch-site")))))

(defonce ^:private local-token (atom nil))

(defn- local-session-token
  [store req]
  (when (and local-user (nil? (http-request/bearer-token req)) (local-browser? req))
    (let [t @local-token]
      (if (and t (= local-user (:username (session-for store t))))
        t
        (when-let [p (pr/fetch store local-user)]
          (when-let [t (establish-session! store
                                           {:provider :local :subject local-user :principal p
                                            :client (client-of req)})]
            (reset! local-token t)))))))

(defn- request-branch
  [req]
  (or (not-empty (get-in req [:query-params "branch"])) tctx/default-branch))

(defn- writes?
  [req]
  (boolean (some-> (:sutra/endpoint req)
                   (paths/writes? (:request-method req)))))

(defn- safe-request
  [req]
  (cond-> req
    (and (:sutra/store req) (not (writes? req)))
    (update :sutra/store read-only/of)))

(defn- serving
  [handler schema req]
  (let [req (safe-request req)
        res (anomaly/catching #(handler req))
        res (if (anomaly/refusal? res) (refusals/response req (scope/named schema res)) res)]
    (cond-> res
      (and (map? res) (:sutra/branch req))
      (assoc-in [:headers "X-Sutra-Branch"]
                (.replace (java.net.URLEncoder/encode (str (:sutra/branch req)) "UTF-8") "+"
                          "%20")))))

(def ^:private limited-paths
  #{"/api/me" "/api/logout" "/api/password" "/api/sessions" "/api/sessions/end"})

(defn- limited-path? [uri]
  (boolean (or (limited-paths uri) (str/starts-with? (str uri) "/api/mfa/"))))

(defn- session-kind
  [sess role uri]
  (cond
    (consumer-blocked? role uri)  :blocked
    (oauth/app-scoped? sess)      :app
    (sessions/agent-scoped? sess) :agent
    (sessions/limited-scoped? sess) (if (limited-path? uri) :limited :limited-route)
    (some? (:scope sess))         :clone-token
    :else                         :principal))

(defn- sessionless-kind
  [uri]
  (cond
    *allow-anon*                 :anonymous
    (contains? public-paths uri) :public
    :else                        :unauthenticated))

(def ^:private refusals
  {:blocked          (refusal/refused :refusal.access/consumer-access-is-restricted-to {})
   :agent-route      (refusal/refused :refusal.access/an-agent-token-reaches-only {})
   :limited-route    (refusal/refused :refusal.access/this-session-must-first-change {:limited true})
   :clone-token      (refusal/refused :refusal.access/a-repository-clone-token-cannot {})
   :no-apps          (refusal/refused :refusal.access/custom-applications-are-not-installed {})
   :unauthenticated  {:status 401 :body {:error "authentication required"}}})

(defn- principal-request
  [req sess ctx store schema branch e]
  (assoc req :principal (assoc sess :role (:role ctx))
         :session-token (request-token req)
         :sutra/store store :sutra/auth (tctx/authority ctx)
         :sutra/schema schema :sutra/branch branch :sutra/edge e))

(defn- app-served
  [handler req sess store ctx]
  (let [app-rid (oauth/scoped-app-rid sess)]
    (with-principal-context*
      store ctx (writes? req)
      (fn [store schema branch e]
        (let [{:keys [guard record!]} (ports/auth-app-guard store)]
          (if-not guard
            (:no-apps refusals)
            (or (guard schema app-rid req
                       {:username (:username sess) :role (:role ctx)
                        :granted-operations (oauth/granted-operations sess)})
                (let [t0  (System/nanoTime)
                      res (serving handler schema
                                   (assoc (principal-request req sess ctx store schema branch e)
                                          :custom-application app-rid))]
                  (record! store schema app-rid req (or (:status res) (anomaly/status res))
                           (quot (- (System/nanoTime) t0) 1000000))
                  res))))))))

(defn- principal-served
  [handler req sess store ctx]
  (with-principal-context*
    store ctx (writes? req)
    (fn [store schema branch e]
      (let [asked   (get-in req [:headers "x-sutra-project"])
            onto    (get-in req [:headers "x-sutra-ontology"])
            project (delay (protection/context-project store schema asked))
            space   (cond (and asked (seq (:spaces schema))) (sch/project-space schema asked)
                          (and onto (seq (:spaces schema)))
                          (ontologies/resolve-ontology schema onto))
            schema  (if space (sch/in-scope schema space) schema)]
        (serving handler schema
                 (assoc (principal-request req sess ctx store schema branch e)
                        :sutra/project project))))))

(defn- anonymous-served
  [handler req]
  (let [b     (request-branch req)
        store (world/viewing (ctx/system-store b))
        e     (edge/read-edge store b (grants/current-user-id grants/system-authority)
                              {:writes? (writes? req)})]
    (serving handler (:schema e)
             (assoc req :sutra/store store :sutra/auth grants/system-authority
                    :sutra/schema (:schema e) :sutra/branch (when (:exists? e) b)
                    :sutra/edge e))))

(defn- agent-served
  [handler req sess store ctx]
  (if (some #(template-match? (paths/url %) (:uri req)) (sessions/agent-routes sess))
    (principal-served handler (assoc req :sutra/agent? true) sess store ctx)
    (:agent-route refusals)))

(defn- session-served
  [handler req sess]
  (let [store (request-store (:branch sess))
        ctx   (principal-context store sess)
        kind  (session-kind sess (:role ctx) (:uri req))]
    (case kind
      :app       (app-served handler req sess store ctx)
      :agent     (agent-served handler req sess store ctx)
      :principal (principal-served handler req sess store ctx)
      :limited   (principal-served handler req sess store ctx)
      (refusals kind))))

(defn- sessionless-served
  [handler req]
  (let [kind (sessionless-kind (:uri req))]
    (case kind
      :anonymous (anonymous-served handler req)
      :public    (handler (safe-request (assoc req :sutra/store (ctx/store))))
      (refusals kind))))

(defn- build-served
  [handler req]
  (if (grant/admits? (http-request/bearer-token req) (:uri req) (System/currentTimeMillis))
    (handler (safe-request (assoc req :sutra/store (ctx/store))))
    (:unauthenticated refusals)))

(defn wrap-auth
  [handler]
  (fn auth [req]
    (if (build-path? (:uri req))
      (build-served handler req)
      (if-let [sess (some-> (session-for (ctx/store) (request-token req))
                            (assoc :branch (request-branch req)))]
        (do (sessions/seen! (ctx/store) (request-token req))
            (session-served handler req sess))
        (if-let [token (local-session-token (ctx/store) req)]
          (some-> (auth (assoc-in req [:headers "authorization"] (str "Bearer " token)))
                  (assoc-in [:headers "Set-Cookie"] (session-cookies req token)))
          (sessionless-served handler req))))))

(defn- text-field
  [m k max-len]
  (let [v (get m k)]
    (when (and (string? v) (not (str/blank? v)) (<= (count v) max-len)) v)))

(defn login-body
  [body]
  (when (map? body)
    (let [u (text-field body :username 255)
          p (text-field body :password 4096)
          c (text-field body :code 16)]
      (when (and u p) (cond-> {:username (str/trim u) :password p} c (assoc :code c))))))

(def ^:private bad-login-body
  (refusal/refused :refusal.access/a-login-takes-username-password {}))

(def ^:private bad-credentials {:status 401 :body {:error "invalid credentials"}})

(def ^:private code-missing
  {:status 401 :body {:error "a second-factor code is required" :mfa-required true}})

(def ^:private initial-expired
  {:status 401 :body {:error (str "the initial password has expired — ask an administrator "
                                  "for a new one")}})

(defn login-handler
  [{:keys [body-params] store :sutra/store :as req}]
  (if-let [{:keys [username password code]} (login-body body-params)]
    (let [client  (client-of req)
          attempt {:account username :ip (:ip client)}
          fields  {:username username :provider :password}]
      (throttle/throttled
       req attempt fields
       #(let [{:keys [token limited mfa-required refused]}
              (sign-in! store username password code client)]
          (cond
            mfa-required code-missing
            token
            (do (throttle/attempted! req attempt true fields)
                (some->> (cookie-token req) (logout! store))
                {:status 200
                 :headers {"Set-Cookie" (session-cookies req token)}
                 :body (cond-> {:token token :username username}
                         (seq limited) (assoc :limited (vec (sort (map name limited)))))})
            :else
            (do (throttle/attempted! req attempt false fields)
                (if (= :initial-password-expired refused) initial-expired bad-credentials))))))
    (do (seclog/security-event! :login/failure req :failure {:reason "malformed body"})
        bad-login-body)))

(defn logout-handler [{store :sutra/store :as req}]
  (logout! store (request-token req))
  {:status 200
   :headers {"Set-Cookie" (clear-cookies req)}
   :body {:ok true}})

(defn- password-change
  [store principal current new]
  (let [p (pr/fetch store (:username principal))]
    (cond
      (and (some? (:scope principal)) (not (sessions/limited-scoped? principal)))
      (refusal/refused :refusal.access/only-a-sign-in-session {})

      (or (nil? p) (not= "password" (:auth-source p)) (empty? (:pw-hash p)))
      (refusal/refused :refusal.access/this-principal-signs-in-through {} {:value (or (:auth-source p) "another provider")})

      (not (pr/verify store p current))
      bad-credentials

      :else
      (when-let [ps (seq (password-policy/problems {:username (:username p) :password new}))]
        (refusal/refused :refusal.access/passed-password-policy-explain-ps {:problems (vec ps)} {:message (password-policy/explain ps)})))))

(defn password-handler
  [{:keys [principal body-params] store :sutra/store :as req}]
  (let [current (text-field body-params :current 4096)
        new     (text-field body-params :new 4096)
        attempt {:account (:username principal) :ip (http-request/client-ip req)}]
    (cond
      (nil? (:username principal)) {:status 401 :body {:error "not authenticated"}}
      (not (and current new))
      (refusal/refused :refusal.access/a-password-change-takes-current {})
      :else
      (throttle/throttled
       req attempt {:username (:username principal) :provider :password-change}
       #(if-let [refusal (password-change store principal current new)]
          (let [fields {:username (:username principal)
                        :status   (or (:status refusal) (anomaly/status refusal))}]
            (if (= bad-credentials refusal)
              (throttle/attempted! req attempt false fields :password/changed)
              (seclog/security-event! :password/changed req :failure fields))
            refusal)
          (let [_     (pr/set-password! store (:username principal) new)
                p     (pr/fetch store (:username principal))
                steps (first-steps p)
                token (sessions/open! store p
                                      {:provider :password-change
                                       :subject (:username principal)}
                                      (client-of req)
                                      (if (seq steps)
                                        {:scope (sessions/limited-scope steps)}
                                        {}))]
            (throttle/attempted! req attempt true {:username (:username principal)}
                                 :password/changed)
            {:status 200
             :headers {"Set-Cookie" (session-cookies req token)}
             :body (cond-> {:ok true :token token :username (:username principal)
                            :note "every other session of this principal has ended"}
                     (seq steps) (assoc :limited (vec (sort (map name steps)))))}))))))

(defn sessions-handler
  [{:keys [principal] store :sutra/store :as req}]
  (if-let [u (:username principal)]
    (ok {:sessions (sessions/of-user store u (request-token req))})
    {:status 401 :body {:error "not authenticated"}}))

(defn- revoke-pick
  [{:keys [id others]} mine]
  (if-let [id (some-> id str not-empty)]
    {:id id}
    (when others {:except mine})))

(defn- admin-caller? [principal req]
  (and (= grants/admin-username (:username principal)) (not (:sutra/agent? req))))

(defn sessions-end-handler
  [{:keys [principal body-params] store :sutra/store :as req}]
  (let [u      (:username principal)
        target (some-> (:username body-params) str str/trim not-empty)
        pick   (revoke-pick body-params (some-> (request-token req) sessions/session-id))]
    (cond
      (nil? u)    {:status 401 :body {:error "not authenticated"}}
      (and target (not= target u) (not (admin-caller? principal req)))
      (refusal/refused :refusal.access/only-the-admin-ends-another {})
      (and target (not= target u))
      (let [n (sessions/revoke! store target {})]
        (seclog/security-event! :session/revoked req :success
                                {:username target :ended n :by u})
        (ok {:ended n}))
      (nil? pick) (refusal/refused :refusal.access/name-a-session-id-or {})
      :else       (let [n (sessions/revoke! store u pick)]
                    (seclog/security-event! :session/revoked req :success
                                            {:username u :ended n})
                    (ok {:ended n})))))

(defn- own-password-principal
  [store principal]
  (let [p (some->> (:username principal) (pr/fetch store))]
    (cond
      (nil? (:username principal)) {:answer {:status 401 :body {:error "not authenticated"}}}
      (or (nil? p) (pr/service-user? p))
      {:answer (refusal/refused :refusal.access/this-principal-has-no-second {})}
      :else {:principal p})))

(defn mfa-status-handler
  [{:keys [principal] store :sutra/store}]
  (let [{:keys [answer] p :principal} (own-password-principal store principal)]
    (or answer
        (ok {:enrolled (mfa/enrolled? p)
             :required (mfa/required-for? (:username p))
             :limited  (vec (sort (map name (sessions/limited-reasons principal))))}))))

(defn mfa-enroll-handler
  [{:keys [principal body-params] store :sutra/store :as req}]
  (let [{:keys [answer] p :principal} (own-password-principal store principal)
        password (text-field body-params :password 4096)
        attempt  {:account (:username principal) :ip (http-request/client-ip req)}]
    (or answer
        (throttle/throttled
         req attempt {:username (:username p) :provider :mfa-enroll}
         #(if (and (seq (:pw-hash p)) (not (pr/verify store p password)))
            (do (throttle/attempted! req attempt false {:username (:username p)}
                                     :mfa/enrolled)
                bad-credentials)
            (ok (mfa/begin! store (:username p))))))))

(defn mfa-confirm-handler
  [{:keys [principal body-params] store :sutra/store :as req}]
  (let [{:keys [answer] p :principal} (own-password-principal store principal)
        code    (text-field body-params :code 16)
        attempt {:account (:username principal) :ip (http-request/client-ip req)}]
    (or answer
        (throttle/throttled
         req attempt {:username (:username p) :provider :mfa-confirm}
         #(if-not (mfa/confirm! store (:username p) code (ctl/millis store))
            (do (throttle/attempted! req attempt false {:username (:username p)}
                                     :mfa/enrolled)
                (refusal/refused :refusal.access/that-code-does-not-match {}))
            (let [p     (pr/fetch store (:username p))
                  steps (first-steps p)]
              (throttle/attempted! req attempt true {:username (:username p)} :mfa/enrolled)
              (if (sessions/limited-scoped? principal)
                (let [token (sessions/open! store p {:provider :mfa-enrol
                                                     :subject (:username p)}
                                            (client-of req)
                                            (if (seq steps)
                                              {:scope (sessions/limited-scope steps)}
                                              {}))]
                  (logout! store (request-token req))
                  {:status 200
                   :headers {"Set-Cookie" (session-cookies req token)}
                   :body (cond-> {:ok true :token token :username (:username p)}
                           (seq steps) (assoc :limited (vec (sort (map name steps)))))})
                (ok {:ok true}))))))))

(defn mfa-disable-handler
  [{:keys [principal body-params] store :sutra/store :as req}]
  (let [{:keys [answer] p :principal} (own-password-principal store principal)
        password (text-field body-params :password 4096)
        code     (text-field body-params :code 16)
        attempt  {:account (:username principal) :ip (http-request/client-ip req)}]
    (cond
      answer answer
      (mfa/required-for? (:username p))
      (refusal/refused :refusal.access/a-second-factor-is-required {})
      :else
      (throttle/throttled
       req attempt {:username (:username p) :provider :mfa-disable}
       #(if (and (or (empty? (:pw-hash p)) (pr/verify store p password))
                 (mfa/check! store (:username p) code (ctl/millis store)))
          (do (mfa/disable! store (:username p))
              (throttle/attempted! req attempt true {:username (:username p)} :mfa/disabled)
              (ok {:ok true}))
          (do (throttle/attempted! req attempt false {:username (:username p)} :mfa/disabled)
              bad-credentials))))))

(defn mfa-reset-handler
  [{:keys [path-params principal] store :sutra/store :as req}]
  (let [u (str/trim (str (:user path-params)))]
    (if-not (pr/fetch store u)
      (refusal/refused :refusal.access/no-principal {} {:u u})
      (do (mfa/disable! store u)
          (let [n (sessions/revoke! store u {})]
            (seclog/security-event! :mfa/reset req :success
                                    {:username u :by (:username principal) :ended n})
            (ok {:ok true :ended n}))))))

(def assignable-roles
  (into #{} (map name) (keys grants/standard-roles)))

(defn- role-target
  [store user]
  (let [s (str/trim (str user))
        p (or (pr/fetch store s)
              (first (filter #(= s (str (:user-object-id %))) (pr/all-rows store))))]
    (when p {:username (:username p) :user-id (str (:user-object-id p))})))

(defn- user-object
  [store t uid]
  (world/with-snapshot (world/at store "main")
    #(first (api/read % grants/system-authority [:objects t [(str uid)]]))))

(defn- user-rolle
  [store schema uid]
  (when-let [t (lookup/rid-of schema :object-types :user)]
    (some-> (user-object store t uid) (get-in [:values :rolle]) str)))

(defn access-explain-handler
  [{:keys [path-params] :as req store :sutra/store}]
  (let [q       #(not-empty (str (get-in req [:query-params %])))
        op      (some-> (q "op") (str/replace #"^:" "") keyword)
        project (some-> (q "project") (str/replace #"^:" ""))]
    (if-let [{:keys [username user-id]} (role-target store (:user path-params))]
      (let [schema (authority/usable-schema store "main")
            rid    (some (fn [[rid spec]]
                           (when (or (= project rid) (= project (some-> (:project-name spec) name)))
                             rid))
                         (:projects schema))
            admin? (= username grants/admin-username)
            subject (authority/read-authority store "main" user-id schema)
            db     (grants/policy-database schema
                                           (authority/policy-subject
                                            user-id subject
                                            {:global-role
                                             (some-> (user-rolle store schema user-id) keyword)
                                             :unrestricted? admin?}))]
        (cond
          (nil? op)  (refusal/refused :refusal.access/name-the-operation-op-read {})
          (nil? rid) (refusal/refused :refusal.access/no-such-project {:projects (vec
                                          (sort (keep #(some-> (:project-name (val %)) name)
                                                      (:projects schema))))} {:project project})
          :else
          (let [{:keys [allow? because]} (policy/decide db {:who user-id :do op :on rid})]
            {:status 200 :body {:username username :user-id user-id :op op :project project
                                :allow? allow? :because because}})))
      (refusal/refused :refusal.access/no-such-principal {} {:user (:user path-params)}))))

(defn- marked-by
  [db rel]
  (reduce (fn [m [x mk]] (update m x (fnil conj #{}) (keyword (name mk)))) {} (get db rel)))

(defn- resource-names
  [schema]
  (into {}
        (for [section (concat [:organizations :spaces :projects] policy/marked-sections)
              [r spec] (get schema section)
              :let [nm (get spec (rid/name-field-of section))]]
          [(str r) (str (name section) "/" (if nm (name nm) r))])))

(defn markings-handler
  [{store :sutra/store}]
  (let [schema (authority/usable-schema store "main")
        db     (grants/schema-policy schema)
        count* (fn [rel] (frequencies (mapcat val (marked-by db rel))))
        own    (count* :mark)
        in     (count* :marked)]
    {:status 200
     :body {:markings
            (vec (for [[m spec] (sort-by (comp str key) (:markings schema))
                       :let [k (keyword (name m))]]
                   (merge {:marking (name m) :own (get own k 0) :in-force (get in k 0)}
                          (select-keys spec [:category :rank :disjunctive :required-on-projects
                                             :cleared :remove-access :description]))))}}))

(defn marking-handler
  [{:keys [path-params] store :sutra/store}]
  (let [schema (authority/usable-schema store "main")
        k      (keyword (str/replace (str (:marking path-params)) #"^:" ""))
        spec   (some (fn [[m s]] (when (= (name m) (name k)) s)) (:markings schema))
        db     (grants/schema-policy schema)
        names  (resource-names schema)
        nm     #(get names (str %) (str %))
        with   (fn [rel] (sort (for [[x ms] (marked-by db rel) :when (ms k)] (nm x))))]
    (if (nil? spec)
      (refusal/refused :refusal.access/no-such-marking {:markings (vec (sort (map name (keys (:markings schema)))))} {:k (name k)})
      (let [own (with :mark)]
        {:status 200
         :body {:marking (name k) :declaration spec :own (vec own)
                :inherited (vec (remove (set own) (with :marked)))
                :stops (vec (for [[t y m] (sort-by str (get db :stop-propagating))
                                  :when (= (name m) (name k))]
                              {:transform (nm t) :input (nm y)}))}}))))

(defn access-blocked-handler
  [{:keys [path-params] :as req store :sutra/store}]
  (let [target (some-> (get-in req [:query-params "resource"]) str str/trim not-empty)]
    (if-let [{:keys [username user-id]} (role-target store (:user path-params))]
      (let [schema  (authority/usable-schema store "main")
            names   (resource-names schema)
            x       (or (some (fn [[r n]] (when (or (= target r) (= target n)
                                                    (str/ends-with? n (str "/" target)))
                                            r))
                              names)
                        target)
            subject (authority/read-authority store "main" user-id schema)
            db      (grants/policy-database
                     schema
                     (authority/policy-subject
                      user-id subject
                      {:global-role (some-> (user-rolle store schema user-id) keyword)
                       :unrestricted? (= username grants/admin-username)}))
            walls   (reduce (fn [m [g o]] (update m g (fnil conj #{}) o)) {} (get db :walled))
            covered (into #{} (keep (fn [[u m]] (when (= u (str user-id)) m))) (get db :covers))
            inside  (into #{} (keep (fn [[u o]] (when (= u (str user-id)) o))) (get db :inside))
            b       [(str user-id) x]]
        (cond
          (nil? target) (refusal/refused :refusal.access/name-the-resource-resource-rid {})
          :else
          {:status 200
           :body {:username username :user-id user-id :resource (get names x x)
                  :blocked? (contains? (get db :blocked) b)
                  :markings-missing
                  (vec (sort (for [[y m] (get db :marked) :when (and (= y x) (not (covered m)))]
                               (name m))))
                  :walls-outside
                  (vec (for [[y g] (sort-by str (get db :requires))
                             :when (= y x)
                             :let [os (get walls g)]
                             :when (not-any? inside os)]
                         {:wall (get names g g) :any-of (vec (sort (map #(get names % %) os)))}))
                  :because (when (contains? (get db :blocked) b)
                             (dl/explain db [:blocked b]))}}))
      (refusal/refused :refusal.access/no-such-principal {} {:user (:user path-params)}))))

(defn access-role-handler
  [{:keys [path-params body-params request-method principal] store :sutra/store}]
  (if-let [{:keys [username user-id]} (role-target store (:user path-params))]
    (let [schema (world/authored (world/at store "main"))
          user-t (lookup/rid-of schema :object-types :user)
          before (user-rolle store schema user-id)
          role   (some-> (:role body-params) name str/trim not-empty)]
      (cond
        (not= :post request-method)
        {:status 200 :body {:username username :user-id user-id :role before}}

        (nil? user-t)
        (refusal/refused :refusal.access/this-instance-has-no-user {})

        (not (contains? assignable-roles role))
        (refusal/refused :refusal.access/role-must-be-one-of {:role role} {:list (str/join ", " (sort assignable-roles)) :value (when (= "admin" role)
                              " — admin is the built-in principal, not a role")})

        (nil? (user-object store user-t user-id))
        (refusal/refused :refusal.access/has-no-user-object {} {:username username :user-id user-id})

        :else
        (do
          (when (not= before role)
            (let [ops [{:op :update-object :type user-t
                        :payload {:id user-id :values {:rolle role}}}]]
              (run/run! (world/at store "main")
                        {:fences [[:type user-t]]
                         :decide (fn [_] {:tx {:ops ops :actor (grants/principal store)}})}))
            (sessions/record-login! store {:outcome :success :provider :access-role
                                           :username username :subject (:username principal)
                                           :detail (str "rolle " (or before "—") " → " role)}))
          {:status 200 :body {:username username :user-id user-id
                              :before before :role role :changed (not= before role)
                              :effective
                              "next request; the browser reloads the page to switch shells"}})))
    (refusal/refused :refusal.access/no-principal-a-username-or {} {:user (:user path-params)})))

(def ^:dynamic *password-policy?*
  true)

(def ^:dynamic *initial-passwords?*
  nil)

(defn register-handler
  [{:keys [body-params] store :sutra/store}]
  (if-let [ps (when (and *password-policy?* (some? (:password body-params)))
                (seq (password-policy/problems (select-keys body-params
                                                            [:username :password]))))]
    (refusal/refused :refusal.access/passed-password-policy-explain-ps {:problems (vec ps)} {:message (password-policy/explain ps)})
    (do (pr/register-principal! store
                                (assoc body-params :initial?
                                       (if (some? *initial-passwords?*)
                                         *initial-passwords?*
                                         (env/auth-hardening?))))
        {:status 200 :body {:ok true :username (:username body-params)}})))

(defn me-handler [{:keys [principal] store :sutra/store branch :sutra/branch}]
  (let [schema-error (seat/schema-load-error)
        role (:role principal)]
    (if principal
      {:status 200 :body (cond-> {:user (:user-id principal)
                                  :clearances
                                  (let [c (grants/clearances (grants/read-rights store))]
                                    (if (set? c) (vec c) []))
                                  :branch branch
                                  :allow-anon *allow-anon*}
                           (:username principal) (assoc :username (:username principal))
                           role         (assoc :role role)
                           schema-error (assoc :schema-error schema-error))}
      {:status 401 :body (cond-> {:error "not authenticated"
                                  :allow-anon *allow-anon*}
                           schema-error (assoc :schema-error schema-error))})))

(defn- negotiate-token [req]
  (some->> (get-in req [:headers "authorization"])
           (re-find #"(?i)^Negotiate\s+(.+)$") second))

(defn- negotiated
  [store req attempt]
  (if (empty? (idp/ready store :negotiate))
    (refusal/refused :refusal.access/single-sign-on-negotiate-is {})
    (if-let [tok (negotiate-token req)]
      (let [{:keys [provider complete? subject out-token] :as r} (idp/negotiation store tok)]
        (cond
          complete?
          (let [token (establish-session! store (assoc r :client (client-of req)))]
            (throttle/attempted! req attempt (some? token)
                                 (cond-> {:username subject :provider provider}
                                   (nil? token) (assoc :reason "principal not provisioned")))
            (if token
              {:status 200
               :headers (cond-> {"Set-Cookie" (session-cookies req token)}
                          out-token (assoc "WWW-Authenticate" (str "Negotiate " out-token)))
               :body {:token token :subject subject}}
              (refusal/refused :refusal.access/principal-not-provisioned {:subject subject} {:subject subject})))
          out-token
          {:status 401 :headers {"WWW-Authenticate" (str "Negotiate " out-token)}
           :body {:error "continue"}}
          :else
          (let [reason (if (:failed? r) "negotiate failed" "negotiate rejected")]
            (sessions/record-login! store {:outcome :failure :provider provider
                                           :username subject :subject subject :detail reason})
            (throttle/attempted! req attempt false
                                 {:username subject :provider provider :reason reason})
            {:status 401 :headers {"WWW-Authenticate" "Negotiate"} :body {:error reason}})))
      {:status 401 :headers {"WWW-Authenticate" "Negotiate"} :body {:error "negotiate"}})))

(defn negotiate-handler
  [{store :sutra/store :as req}]
  (let [attempt {:ip (http-request/client-ip req)}]
    (throttle/throttled req attempt {:provider :negotiate}
                        #(negotiated store req attempt))))

(def module
  {:name        :auth
   :contributes {:http/routes
                 {:negotiate     {:get negotiate-handler :post negotiate-handler}
                  :access        {:get access-overview}
                  :access-role   {:get access-role-handler :post access-role-handler}
                  :access-explain {:get access-explain-handler}
                  :access-blocked {:get access-blocked-handler}
                  :markings      {:get markings-handler}
                  :marking       {:get marking-handler}
                  :login         {:post login-handler}
                  :logout        {:post logout-handler}
                  :me            {:get me-handler}
                  :principals    {:get list-principals}
                  :sessions      {:get sessions-handler}
                  :sessions-end  {:post sessions-end-handler}
                  :register      {:post register-handler}
                  :password      {:post password-handler}
                  :mfa-status    {:get mfa-status-handler}
                  :mfa-enroll    {:post mfa-enroll-handler}
                  :mfa-confirm   {:post mfa-confirm-handler}
                  :mfa-disable   {:post mfa-disable-handler}
                  :mfa-reset     {:post mfa-reset-handler}}}})
