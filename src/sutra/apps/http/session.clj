(ns sutra.apps.http.session
  (:require [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [sutra.foundation.encoding :as enc]
            [clojure.string :as str]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.parser.ent :as ent]
            [sutra.http.responses :refer [ok json-response negotiated]]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.versions :as vers]
            [sutra.governance.api :as governance]
            [sutra.store.seat :as seat]
            [sutra.apps.http.files :refer [named-entry]]
            [sutra.apps.http.scope :refer [app-scope granted-app readable-app
                                                    workspace-rights]]))

(set! *warn-on-reflection* true)

(def ^:private scenario-prefix "scenario/")

(defn scenario-app-prefix [rid] (str scenario-prefix rid "/"))

(defn scenario-branch    [rid seg] (str (scenario-app-prefix rid) seg))

(def ^:private safe-seg-re #"^[A-Za-z0-9._-]+$")

(defn- safe-seg?
  [seg]
  (boolean (and (string? seg) (re-matches safe-seg-re seg) (not (str/includes? seg "..")))))

(defn- scenario-branch-exists? [store nm]
  (boolean (some #(= nm (:name %)) (governance/branch-parents store))))

(defn scenario-at
  [store branch sb]
  (ctx/at store (or sb branch)))

(defn scenario-schema
  [store schema sb]
  (if sb (seat/branch-schema (ctx/at store sb)) schema))

(defn app-body [req]
  (or (:body-params req)
      (some-> (:body req) slurp (enc/json-or-nil :key-fn keyword))))

(def ^:private preview-ttl-ms (* 60 60 1000))

(defn- age-ms [store row]
  (- (inst-ms (ctl/clock store)) (inst-ms (:created-at row))))

(defn- kept-preview [store id]
  (when-let [row (ctl/get store :app-previews {:id (str id)})]
    (when (< (age-ms store row) preview-ttl-ms) row)))

(defn- keep-preview! [store row]
  (let [expired (into #{} (comp (filter #(>= (age-ms store %) preview-ttl-ms)) (map :id))
                      (ctl/list store :app-previews {}))]
    (ctl/commit! store
                 (cond-> [#(ctl/swapping %1 :app-previews {:id (:id row)} (constantly row) %2)]
                   (seq expired) (conj (fn [st _] (ctl/deleting st :app-previews
                                                               {:id expired})))))))

(defn- preview-rights-ok?
  [auth schema live file-app]
  (and (or (nil? live) (:edit (workspace-rights auth schema live)))
       (or (and live (= (->kw (:project live)) (->kw (:project file-app))))
           (:edit (workspace-rights auth schema (select-keys file-app [:project]))))))

(defn- preview-app
  [auth req schema rid]
  (when-let [{:keys [username app live-name]} (kept-preview (:sutra/store req) rid)]
    (let [live (some->> live-name (named-entry schema :applications) second)]
      (when (and (= username (ep/signed-in-user req))
                 (preview-rights-ok? auth schema live app))
        app))))

(defn- asked-version
  [req]
  (let [v (some-> (get-in req [:query-params "v"]) str/trim not-empty)]
    (cond (nil? v)              :published
          (= "latest" v)        :published
          (= "dev" v)           :latest
          (re-matches #"\d+" v) (parse-long v)
          :else                 ::unreadable)))

(defn- at-version
  [req a which may-edit?]
  (let [nm   (:application-name a)
        head (or (second (named-entry (:sutra/head-schema req) :applications nm)) a)]
    (cond
      (= :published which)
      {:app a :version (when-let [v (:published head)]
                         {:seq v :published? true})}

      (not may-edit?)
      {:error (str "only the published version of " (clojure.core/name nm)
                   " is served here — reading an unpublished one is editing the app")}

      :else
      (let [ask (if (= :latest which) "dev" (str which))]
        (try
          (let [spec (vers/as-version head (when-not (= :latest which) (str which)))
                spec (if (= :latest which) head spec)]
            {:app     spec
             :version {:seq        (if (= :latest which) nil which)
                       :published? (and (not= :latest which)
                                        (= (str which) (str (:published head))))
                       :ask        ask}})
          (catch clojure.lang.ExceptionInfo e
            {:error (ex-message e)}))))))

(defn- scenario-seg
  [req seg]
  (case seg
    :query (some-> (get-in req [:query-params "scenario"]) str/trim not-empty)
    :path  (get-in req [:path-params :uuid])
    nil))

(defn- app-refusal
  [{:keys [branch? branch a draft which vres seg]}]
  (cond
    (and branch? (not branch))      [409 {:error "no branch"}]
    (and (nil? a) draft)
    [404 {:error (str "application " (clojure.core/name (:application-name draft))
                      " is not published yet — its saved draft opens under /dev")}]
    (nil? a)                        [404 {:error "application not found"}]
    (= ::unreadable which)          [400 {:error "?v= is latest, dev or a version number"}]
    (:error vres)                   [403 {:error (:error vres)}]
    (and seg (not (safe-seg? seg))) [400 {:error "invalid scenario id"}]))

(defn- app-rid [schema r]
  (if (or (nil? r) (contains? (:applications schema) r))
    r
    (or (lookup/rid-of schema :applications (keyword r)) r)))

(defn with-app
  [req {:keys [seg json? branch? preview?]} f]
  (let [store  (:sutra/store req)
        auth   (:sutra/auth req)
        answer (if json? json-response #(negotiated req %1 %2))
        schema (:sutra/schema req)
        rid    (app-rid schema (get-in req [:path-params :rid]))
        branch (:sutra/branch req)
        pv     (when preview? (preview-app auth req schema rid))
        which  (asked-version req)
        pub    (or pv (granted-app auth schema rid))
        draft  (when-not pub (readable-app auth schema rid))
        live   (or pub
                   (when (and draft (= :latest which)
                              (:edit (workspace-rights auth schema draft)))
                     draft))
        vres   (when (and live (nil? pv) (not= ::unreadable which))
                 (at-version req live which
                             (:edit (workspace-rights auth schema live))))
        a      (if vres (:app vres) live)
        seg    (scenario-seg req seg)
        sb     (when seg (scenario-branch rid seg))]
    (if-let [[status body] (or (app-refusal {:branch? branch? :branch branch :a a :draft draft
                                             :which which :vres vres :seg seg})
                               (when (and seg (not (scenario-branch-exists? store sb)))
                                 [404 {:error "scenario not found"}]))]
      (answer status body)
      (f {:rid rid :schema schema :branch branch :a a :seg seg :sb sb :body (app-body req)
          :version (:version vres)
          :scope (app-scope schema a) :store store :auth auth :edge (:sutra/edge req)}))))

(defn application-preview
  [{{:keys [name]} :path-params :keys [body-params] auth :sutra/auth schema :sutra/schema
    :as req}]
  (let [{:keys [file]} body-params]
    (if-not (map? file)
      (refusal/refused :refusal.apps/preview-needs-a-file-map body-params)
      (let [nm     (keyword (or (some-> (:name file) clojure.core/name) name))
            parsed (try {:ok (ent/parse-enta (assoc file :name nm)
                                             (str (clojure.core/name nm) ".enta"))}
                        (catch clojure.lang.ExceptionInfo e {:msg (ex-message e)}))
            app    (some-> (:ok parsed) :schema :applications (get nm)
                           (assoc :application-name nm))
            [_ live] (named-entry schema :applications nm)]
        (cond
          (:msg parsed)
          (refusal/refused :refusal.apps/passed-msg-parsed {:app (clojure.core/name nm)} {:message (:msg parsed)})

          (not (preview-rights-ok? auth schema live app))
          (refusal/refused :refusal.apps/previewing-an-app-is-editing {} {:nm (clojure.core/name nm) :value (when-let [p (or (:project live) (:project app))]
                                (str " (project " (clojure.core/name (->kw p)) ")"))})

          :else
          (let [id (str "preview-" (java.util.UUID/randomUUID))]
            (keep-preview! (:sutra/store req)
                           {:id        id
                            :username  (ep/signed-in-user req)
                            :app       app
                            :live-name (when live (clojure.core/name nm))})
            (ok {:id id :expires-in-ms preview-ttl-ms})))))))
