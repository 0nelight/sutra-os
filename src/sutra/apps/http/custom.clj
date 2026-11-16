(ns sutra.apps.http.custom
  (:require [sutra.kit.coll :as coll]
            [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]
            [sutra.repos.api :as repos]
            [sutra.store.access.scope :as store-scope]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.seat :as seat]
            [platform-sdk.scope :as scope]
            [sutra.http.endpoint :as ep]
            [sutra.http.request :as http-request]
            [sutra.http.responses :refer [ok]]
            [sutra.types.redirect-uri :as redirect-uri]
            [sutra.types.paths :as paths]
            [sutra.access.api :as access]))

(set! *warn-on-reflection* true)

(defn- apps-of [schema]
  (or (:custom-applications schema) {}))

(defn find-app
  [schema nm]
  (when-let [r (some->> nm name (lookup/rid-of schema :custom-applications))]
    (find (apps-of schema) r)))

(defn- published? [spec]
  (= :published (some-> (:status spec) name keyword)))

(defn- ontology-name [branch]
  (or branch "main"))

(defn- summary
  [{{:keys [clients-of metrics-summary]} :oauth} store schema rid spec]
  (let [d (scope/declared schema spec)]
    {:name        (:custom-application-name spec)
     :rid         rid
     :display-name (:display-name spec)
     :description (:description spec)
     :status      (or (:status spec) :draft)
     :project     (:project spec)
     :repository  (:repository spec)
     :hosting     (:hosting spec)
     :permissions (:permissions spec)
     :sdk         (:sdk spec)
     :scope       {:object-types (vec (sort (map name (:object-types d))))
                   :actions      (vec (sort (map name (:actions d))))
                   :functions    (vec (sort (map name (:functions d))))
                   :operations   (vec (sort (map name (:operations d))))}
     :clients     (mapv #(select-keys % [:client-id :grant-types :redirect-uris
                                         :created-at :revoked-at])
                        (clients-of store rid))
     :metrics     (when (:metrics spec) (metrics-summary store rid))}))

(def list-custom-apps
  (ep/handler
   :custom-apps
   {:call (fn [{:keys [host store schema]}]
            {:custom-applications
             (vec (for [[rid spec] (sort-by (comp str :custom-application-name val)
                                            (apps-of schema))]
                    (summary host store schema rid spec)))})}))

(def get-custom-app
  (ep/handler
   :custom-app
   {:call (fn [{:keys [host store name schema]}]
            (when-let [[rid spec] (find-app schema name)]
              (summary host store schema rid spec)))
    :or-404 :refusal.apps/no-such-custom-application}))

(def custom-app-sdk
  (ep/handler
   :custom-app-sdk
   {:call (fn [{:keys [name schema branch]}]
            (when-let [[_ spec] (find-app schema name)]
              (scope/generate-sdk schema spec {:ontology (ontology-name branch)})))
    :or-404 :refusal.apps/no-such-custom-application}))

(def custom-app-docs
  (ep/handler
   :custom-app-docs
   {:call (fn [{:keys [name schema branch]}]
            (when-let [[_ spec] (find-app schema name)]
              {:name name
               :markdown (scope/docs schema spec {:ontology (ontology-name branch)})}))
    :or-404 :refusal.apps/no-such-custom-application}))

(def custom-app-metrics
  (ep/handler
   :custom-app-metrics
   {:call (fn [{:keys [host store name schema]}]
            (when-let [[rid spec] (find-app schema name)]
              (if-not (:metrics spec)
                {:enabled false
                 :hint "declare :metrics true on the custom application to record calls"}
                {:enabled true
                 :summary ((-> host :oauth :metrics-summary) store rid)
                 :rows ((-> host :oauth :metrics-of) store rid 200)})))
    :or-404 :refusal.apps/no-such-custom-application}))

(defn mint-client
  [{:keys [path-params body-params principal] store :sutra/store schema :sutra/schema
    {{:keys [register-client!]} :oauth} :sutra/host}]
  (let [nm (:name path-params)]
    (if-let [[rid spec] (find-app schema nm)]
      (let [declared (:client spec)
            grants   (or (:grant-types body-params) (:grant-types declared)
                         [:authorization-code])
            uris     (or (:redirect-uris body-params) (:redirect-uris declared) [])]
        (cond
          (seq (redirect-uri/invalid uris))
          (refusal/refused :refusal.apps/passed-redirect-uri-message {:redirect-uris (redirect-uri/invalid uris)} {:message redirect-uri/message})

          (contains? body-params :service-user)
          (refusal/refused :refusal.apps/a-client-does-not-run {})

          :else
          (let [r (register-client!
                   store
                   {:app rid :app-name (:custom-application-name spec)
                    :grant-types grants :redirect-uris uris
                    :created-by (:username principal)})]
            (ok (cond-> r
                  (:service-user r)
                  (assoc :next (str "the service user " (:service-user r) " has no access yet —"
                                    " grant it the roles the application needs")))))))
      (refusal/refused :refusal.apps/no-such-custom-application {:name nm}))))

(defn client-action
  [{:keys [request-method path-params] store :sutra/store schema :sutra/schema
    {{:keys [clients-of revoke-client! rotate-secret!]} :oauth} :sutra/host}]
  (let [{:keys [name client-id]} path-params]
    (if-let [[rid _] (find-app schema name)]
      (let [client (first (filter #(= client-id (:client-id %)) (clients-of store rid)))]
        (cond
          (nil? client) (refusal/refused :refusal.apps/no-such-client-for-this {:name name :client-id client-id})
          (= :delete request-method) (ok {:revoked (revoke-client! store client-id)})
          :else (if-let [r (rotate-secret! store client-id)]
                  (ok r)
                  (refusal/refused :refusal.apps/client-is-revoked-rotation-needs {}))))
      (refusal/refused :refusal.apps/no-such-custom-application {:name name}))))

(defn- app-for-client
  [schema client]
  (find (apps-of schema) (:app client)))

(defn- issuing
  [{{:keys [minting new-token]} :sessions {:keys [app-scope]} :oauth} store at schema branch client
   principal requested]
  (let [[rid spec] (app-for-client schema client)]
    (cond
      (nil? spec)
      {:result (refusal/refused :refusal.apps/client-app-undeclared
                                {:client-id (:client-id client) :app (:app client)})}

      (not (published? spec))
      {:result (refusal/refused :refusal.apps/app-not-published {}
                                {:app-name (clojure.core/name (:custom-application-name spec))})}

      :else
      (let [d       (scope/declared schema spec)
            allowed (:operations d)
            outside (seq (remove allowed requested))]
        (if outside
          {:result {:status 400
                    :body {:error "invalid_scope"
                           :detail (str "this application does not declare the API namespace(s) "
                                        (str/join ", " (sort (map name outside)))
                                        " — a token is never wider than the declaration")
                           :declared (vec (sort (map name allowed)))}}}
          (let [granted (if (seq requested) (set requested) allowed)
                ttl     (or (get-in spec [:client :token-ttl]) 3600)
                minted  ((minting (new-token) (assoc principal :branch branch)
                                  {:scope      (app-scope rid granted)
                                   :ttl        ttl
                                   :client-id  (:client-id client)
                                   :secret-gen (long (or (:secret-gen client) 0))})
                         store at)]
            (assoc minted :result (ok {:access_token (:result minted)
                                       :token_type "Bearer"
                                       :expires_in ttl
                                       :application (:custom-application-name spec)
                                       :scope (str/join " " (sort (map name granted)))}))))))))

(defn- parse-scope
  [scope]
  (when-let [t (not-empty (str/trim (str scope)))]
    (into #{} (comp (remove access/oidc-scopes) (map keyword)) (str/split t #"[\s,]+"))))

(defn- stored-scope
  [scope]
  (some-> (not-empty (cond-> (vec (sort (map name (parse-scope scope))))
                       (access/openid? scope) (conj "openid")))
          (->> (str/join " "))))

(defn- bounded [s n] (some-> s str not-empty (#(subs % 0 (min n (count %))))))

(defn- with-id-token
  [answer req store row client-id p]
  (if (and (= 200 (:status answer)) (access/openid? (:scope row)))
    (assoc-in answer [:body :id_token]
              (access/id-token store {:iss (access/oidc-issuer req) :client-id client-id :p p
                                      :nonce (:nonce row) :auth-time (:auth-time row)
                                      :now-s (quot (System/currentTimeMillis) 1000)}))
    answer))

(defn- wants-json?
  [{:keys [headers]}]
  (let [ct (str (get headers "content-type")) acc (str (get headers "accept"))]
    (or (str/includes? ct "application/x-www-form-urlencoded")
        (and (str/includes? acc "application/json")
             (not (str/includes? acc "application/transit+json"))))))

(defn- as-wire
  [req {:keys [status body] :as answer}]
  (if (and (wants-json? req) (map? body))
    {:status status
     :headers (merge (:headers answer) {"Content-Type" "application/json;charset=UTF-8"
                                        "Cache-Control" "no-store" "Pragma" "no-cache"})
     :body (json/write-str body)}
    answer))

(defn- redeeming-into-token
  [host req schema branch code client-id redirect-uri verifier]
  (fn [store at c]
    (let [{row :result :as redeemed} (((-> host :oauth :redeeming) code client-id) store at)]
      (cond
        (nil? row)
        {:result {:status 400 :body {:error "invalid_grant"
                                     :detail "code unknown, already redeemed, or expired"}}}

        (not= (:redirect-uri row) redirect-uri)
        {:result {:status 400 :body {:error "invalid_grant" :detail "redirect_uri mismatch"}}}

        (not ((-> host :oauth :verifier-ok?) (:code-challenge row) verifier))
        {:result {:status 400 :body {:error "invalid_grant"
                                     :detail "code_verifier does not match (PKCE S256)"}}}

        :else
        (if-let [p ((-> host :principals :fetch) store (:username row))]
          (-> (issuing host store at schema branch c p (parse-scope (:scope row)))
              (update :records #(into (vec (:records redeemed)) %))
              (update :result with-id-token req store row client-id p))
          {:result (refusal/refused :refusal.apps/approver-gone {})})))))

(defn- token-answer
  [{store :sutra/store host :sutra/host :as req} params client_id client_secret attempt fields]
  (let [{{:keys [authenticate-client grants? issuing!]} :oauth} host
        fetch-principal (-> host :principals :fetch)
        {:strs [grant_type code redirect_uri scope code_verifier]} params
        schema (ep/request-schema req)
        branch (ep/request-branch req)
        client (authenticate-client store client_id client_secret)]
    (cond
      (nil? client)
      (do (access/attempted! req attempt false fields)
          {:status 401 :body {:error "invalid_client"}})

      (not (grants? client grant_type))
      {:status 400 :body {:error "unauthorized_client"
                          :detail (str "this client does not carry the "
                                       (pr-str grant_type) " grant")}}

      (= :client-credentials (access/grant-key (or grant_type "")))
      (let [p (some->> (:service-user client) (fetch-principal store))]
        (if (and (access/own-service-user? client) (access/service-user? p))
          (or (issuing! store client_id client_secret
                        (fn [store at c]
                          (issuing host store at schema branch c p (parse-scope scope))))
              {:status 401 :body {:error "invalid_client"}})
          {:status 400
           :body {:error "unauthorized_client"
                  :detail "this client has no service user of its own — mint a new one"}}))

      (= :authorization-code (access/grant-key (or grant_type "")))
      (or (do (access/ensure-signing-key! store)
              (issuing! store client_id client_secret
                        (redeeming-into-token host req schema branch code client_id
                                              redirect_uri code_verifier)))
          {:status 401 :body {:error "invalid_client"}})

      :else
      {:status 400 :body {:error "unsupported_grant_type"
                          :supported ["client_credentials" "authorization_code"]}})))

(defn oauth-token
  [req]
  (let [params (access/wire-params req)
        [client_id client_secret] (access/client-credentials req params)
        attempt {:client (str client_id) :ip (http-request/client-ip req)}
        fields  {:client-id client_id :provider :oauth-client}]
    (as-wire req (access/throttled
                  req attempt fields
                  #(token-answer req params client_id client_secret attempt fields)))))

(declare permits-principal?)

(defn- approved-for?
  [spec {:keys [username role]}]
  (let [{:keys [users roles]} (:permissions spec)]
    (and (or (seq users) (seq roles))
         (permits-principal? spec {:username username :role role}))))

(defn oauth-authorize
  [{:keys [body-params principal session-token] store :sutra/store schema :sutra/schema
    {{:keys [fetch-client grants? redirect-allowed? mint-code! challenge-problem
             code-ttl-seconds]} :oauth} :sutra/host}]
  (let [{:keys [client-id redirect-uri code_challenge code_challenge_method]} body-params
        client (fetch-client store client-id)
        [_ spec] (when client (app-for-client schema client))]
    (cond
      (some? (:scope principal))
      (refusal/refused :refusal.apps/only-a-sign-in-session {})

      (nil? client) (refusal/refused :refusal.apps/no-such-client {:client-id client-id})

      (not (grants? client :authorization-code))
      (refusal/refused :refusal.apps/this-client-does-not-carry {})

      (not (redirect-allowed? client redirect-uri))
      (refusal/refused :refusal.apps/redirect-uri-is-not-registered {} {:redirect-uri redirect-uri})

      (nil? spec)
      (refusal/refused :refusal.apps/the-client-points-at-a {})

      (not (published? spec))
      (refusal/refused :refusal.apps/a-draft-application-authorizes-no {})

      (not (approved-for? spec principal))
      (refusal/refused :refusal.apps/the-application-s-permissions-do {} {:username (:username principal) :role (:role principal)})

      (challenge-problem code_challenge code_challenge_method)
      (refusal/refused :refusal.apps/passed-challenge-problem-code-challenge-code {} {:message (challenge-problem code_challenge code_challenge_method)})

      :else
      (ok {:code (mint-code! store {:client-id client-id
                                    :code-challenge code_challenge
                                    :user-id (:user-id principal)
                                    :username (:username principal)
                                    :redirect-uri redirect-uri
                                    :scope (stored-scope (:scope body-params))
                                    :nonce (bounded (:nonce body-params) 512)
                                    :auth-time (access/authenticated-at store session-token)})
           :expires_in code-ttl-seconds}))))

(defn api-namespace
  [uri]
  (some-> (re-find #"^/api/([^/]+)" (str uri)) second keyword))

(def ^:private always-allowed
  #{:custom-apps :oauth})

(defn permits-principal?
  [spec {:keys [username role]}]
  (let [as-name #(some-> % name)
        {:keys [users roles]} (:permissions spec)
        users (into #{} (keep as-name) (or users []))
        roles (into #{} (keep as-name) (or roles []))]
    (or (and (empty? users) (empty? roles))
        (contains? users (as-name username))
        (contains? roles (as-name role)))))

(defn guard
  [schema app-rid req {:keys [granted-operations] :as principal}]
  (let [spec     (get (apps-of schema) app-rid)
        uri      (:uri req)
        ns-      (api-namespace uri)
        deny     (fn [msg] (refusal/refused :refusal.apps/guard-denied {:application (some-> spec :custom-application-name)} {:message msg}))]
    (cond
      (nil? spec)
      (deny (str "this token belongs to custom application " app-rid
                 ", which is no longer declared"))

      (not (published? spec))
      (deny "this custom application is not :published")

      (not (permits-principal? spec principal))
      (deny (str "this custom application's :permissions do not admit "
                 (or (:username principal) "this principal")))

      (contains? always-allowed ns-) nil

      (not (scope/permits? schema spec :operation ns-))
      (deny (str "the API namespace " (pr-str ns-) " is not in this application's"
                 " declared :operations — add it to the declaration, not to the call"))

      (not (contains? (set granted-operations) ns-))
      (deny (str "the API namespace " (pr-str ns-) " is not in this TOKEN's granted scope"
                 " — the application may do it, this token carries only:"
                 " " (if (seq granted-operations)
                       (str/join ", " (sort (map name granted-operations)))
                       "nothing")))

      :else
      (let [act (get-in req [:body-params :action])
            ot  (or (get-in req [:path-params :type])
                    (get-in req [:path-params :object-type]))]
        (cond
          (and act (not (scope/permits? schema spec :action act)))
          (deny (str "action " (pr-str act) " is not in this application's declared"
                     " :scope — the SDK does not carry it either"))

          (and ot (not (scope/permits? schema spec :object-type ot)))
          (deny (str "object type " (pr-str ot) " is not in this application's"
                     " declared :scope"))

          (when (= uri (paths/address :ontology-query))
            (some #(not (scope/permits? schema spec :object-type %))
                  (scope/query-types schema (:body-params req))))
          (deny (str "this query reads an object type outside this application's"
                     " declared :scope"))

          (and (= :function ns-)
               (not (scope/permits? schema spec :function
                                    (get-in req [:body-params :function-name]))))
          (deny (str "function " (pr-str (get-in req [:body-params :function-name]))
                     " is not in this application's declared :scope"))

          :else nil)))))

(defn record!
  [store schema app-rid req status ms]
  (when-let [spec (get (apps-of schema) app-rid)]
    (when (:metrics spec)
      ((-> req :sutra/host :oauth :record-call!)
       store app-rid (api-namespace (:uri req))
       {:ms ms :error? (>= (long (or status 200)) 400)}))))

(def ^:private content-types
  {"html" "text/html; charset=utf-8"   "js"   "text/javascript; charset=utf-8"
   "mjs"  "text/javascript; charset=utf-8" "css" "text/css; charset=utf-8"
   "json" "application/json"           "svg"  "image/svg+xml"
   "png"  "image/png"  "jpg" "image/jpeg" "jpeg" "image/jpeg" "gif" "image/gif"
   "webp" "image/webp" "ico" "image/x-icon" "woff" "font/woff" "woff2" "font/woff2"
   "map"  "application/json"           "txt"  "text/plain; charset=utf-8"
   "wasm" "application/wasm"})

(defn- content-type-of [path]
  (or (content-types (str/lower-case (or (second (re-find #"\.([^./]+)$" (str path))) "")))
      "application/octet-stream"))

(defn- csp-header
  [csp]
  (cond
    (string? csp) csp
    (map? csp) (str/join "; "
                         (for [[k v] csp]
                           (str (str/replace (name k) #"([a-z])([A-Z])" "$1 $2")
                                " "
                                (if (sequential? v) (str/join " " (map str v)) (str v)))))
    :else nil))

(defn- hosted-apps
  [schema]
  (into {} (for [[rid spec] (apps-of schema)
                 :let [sub (get-in spec [:hosting :subdomain])]
                 :when (and sub (published? spec))]
             [(str sub) [rid spec]])))

(def ^:private hosted-ttl-ms 5000)

(defonce ^:private hosted-cache (atom nil))

(defn- load-hosted [store]
  {:db (:db store) :branch (:branch store) :at (ctl/millis store)
   :table (hosted-apps (seat/branch-schema store))})

(defn- hosted-table
  [store]
  (let [c @hosted-cache]
    (if (or (nil? c) (not (identical? (:db c) (:db store))) (not= (:branch c) (:branch store)))
      (:table (reset! hosted-cache (load-hosted store)))
      (do (when (and (> (- (ctl/millis store) (:at c)) hosted-ttl-ms)
                     (not (:loading? c))
                     (compare-and-set! hosted-cache c (assoc c :loading? true)))
            (future
              (try (reset! hosted-cache (load-hosted store))
                   (catch Throwable t
                     (println "[apps] hosted apps not reloaded:" (ex-message t))
                     (swap! hosted-cache assoc :loading? false :at (ctl/millis store))))))
          (:table c)))))

(defn- repo-tip
  [store repo-rid branch]
  (or (:tip-sha (repos/repo-anchor store repo-rid (or branch "main")))
      (some #(when (= repo-rid (:repo-rid %)) (:tip-sha %)) (repos/repo-anchors store))))

(defn- serve-file
  [store repo-rid tip root path]
  (let [pre   (fn [p] (str/replace (str root "/" p) #"^/+" ""))
        files (filter #(= "file" (:plane %)) (repos/files-of store repo-rid tip))
        by    (coll/index-by :path files)
        hit   (or (get by (pre path))
                  (get by (pre (str path (when (seq path) "/") "index.html")))
                  (get by (pre "index.html")))]
    (when hit
      (when-let [bytes (repos/blob-bytes (store-scope/repos) repo-rid (:blob-sha hit))]
        {:path (:path hit) :bytes bytes}))))

(defn- plain [status msg]
  {:status status :headers {"Content-Type" "text/plain; charset=utf-8"} :body msg})

(defn hosting-refusal
  [spec]
  (let [h (:hosting spec) nm (some-> (:custom-application-name spec) name)]
    (cond
      (not (:repository spec))
      (plain 503 (str "custom application " nm " declares :hosting but no"
                      " :repository — there is nothing to serve"))

      (str/blank? (str (:root h)))
      (plain 503 (str "custom application " nm " declares :hosting without a"
                      " :root — name the built directory (e.g. :root \"dist\");"
                      " serving a repository root would publish its sources"))

      :else nil)))

(defn hosted-handler
  [{:keys [store-for host]} req]
  (let [uri    (str (:uri req))
        hostname (some-> (get-in req [:headers "host"]) (str/split #":") first)
        label  (some-> hostname (str/split #"\.") first)
        store     (store-for)
        hosted (hosted-table store)
        [sub path] (if-let [m (re-find #"^/hosted/([^/]+)/?(.*)$" uri)]
                     [(second m) (nth m 2)]
                     [label (str/replace uri #"^/+" "")])]
    (when-let [[rid spec] (get hosted (str sub))]
      (or (hosting-refusal spec)
          (let [h  (:hosting spec)]
            (if-let [tip (repo-tip store (:repository spec) (:branch h))]
              (if-let [{:keys [path
                               bytes]} (serve-file store (:repository spec) tip (:root h) path)]
                (do (when (:metrics spec)
                      ((-> host :oauth :record-call!) store rid :hosted {:ms 0 :error? false}))
                    {:status 200
                     :headers (cond-> {"Content-Type" (content-type-of path)
                                       "Cache-Control" (or (:cache-control h) "no-cache")}
                                (csp-header (:csp h))
                                (assoc "Content-Security-Policy" (csp-header (:csp h))))
                     :body (java.io.ByteArrayInputStream. bytes)})
                (plain 404 "not found in the hosted bundle"))
              (plain 503 "the linked repository has no commit yet")))))))

(def module
  {:name        :custom-apps
   :contributes {:http/routes
                 {:custom-app         {:get get-custom-app}
                  :custom-app-client  {:post client-action :delete client-action}
                  :custom-app-clients {:post mint-client}
                  :custom-app-docs    {:get custom-app-docs}
                  :custom-app-metrics {:get custom-app-metrics}
                  :custom-app-sdk     {:get custom-app-sdk}
                  :custom-apps        {:get list-custom-apps}
                  :oauth-authorize    {:post oauth-authorize}
                  :oauth-token        {:post oauth-token}}
                 :http/raw-handlers {:hosted-apps (fn [opts] (partial hosted-handler opts))}}
   :provides    {:auth/app-guard {:guard guard :record! record!}}})
