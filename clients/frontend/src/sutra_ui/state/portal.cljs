(ns sutra-ui.state.portal
  (:require [goog.object :as gobj]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.apphost.client :as sdk-client]
            [sutra-ui.debug :as dbg]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.object-view :as object-view]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]
            [sutra.i18n.core :as i18n]))

(defonce
  consumer-apps (db/cursor [:portal :consumer-apps] rd/loading))
(defonce unlisted-apps (db/cursor [:portal :unlisted-apps] []))

(defn consumer-app-list
  []
  (rd/value-or @consumer-apps []))

(declare refresh-workspaces!)

(db/reg :sutra.portal/unlisted
        (fn [db [_ resp]] {:db (assoc-in db [:portal :unlisted-apps] (vec (:unlisted resp)))}))

(defn refresh-consumer-apps!
  []
  (refresh-workspaces!)
  (db/load! [:portal :consumer-apps] [api/get! :my-applications]
            {:pick (comp vec :applications) :report "applications"
             :then [[:sutra.portal/unlisted]]}))

(defonce current-consumer-app (db/cursor [:portal :current-consumer-app] nil))

(defonce ^:private consumer-app-watch (atom nil))

(defn- stop-consumer-app-watch! []
  (when-let [stop @consumer-app-watch] (stop))
  (reset! consumer-app-watch nil))

(defn- url-params []
  (if-not (exists? js/window)
    {}
    (into {} (js->clj (js/Array.from (.entries (js/URLSearchParams.
                                                (.. js/window -location -search))))))))

(defonce
  consumer-app-version (db/cursor [:portal :consumer-app-version] nil))

(defn- with-version
  [params]
  (cond-> params (some? @consumer-app-version) (assoc :v @consumer-app-version)))

(defn- app-params
  [card data]
  (with-version
    (cond-> {:rid (:id card) :url (js/JSON.stringify (clj->js (url-params)))}
      data (assoc :data data))))

(db/reg :sutra.portal/reread
        (fn [db [_ card]]
          (rq/request db {:ask     [api/get! :my-application (app-params card "rows")]
                          :path    [:portal :current-consumer-app :data] :pick :data
                          :current [[:portal :current-consumer-app :card :id] (:id card)]})))

(defn- gebundene-typen
  [descriptor]
  (into (sorted-set) (map name) (vals (:object-names descriptor))))

(defn- watch-consumer-app!
  [card descriptor]
  (stop-consumer-app-watch!)
  (let [live (:live descriptor)
        live (if (map? live) live (when (not= false live) {}))]
    (when (and live (not (and (seq (:object-sets live)) (:layout descriptor))))
      (let [typen (or (seq (map #(if (keyword? %) (name %) (str %)) (:types live)))
                      (seq (gebundene-typen descriptor)))]
        (when (seq typen)
          (reset! consumer-app-watch
                  (sdk-client/watch! descriptor
                                     {:types       typen
                                      :min-seconds (:min-seconds live)
                                      :on-change   (fn [_]
                                                     (when (sdk-client/refresh-page!)
                                                       (db/send! [:sutra.portal/reread
                                                                  card])))})))))))

(declare workspace-object-type)

(defn close-consumer-app! []
  (stop-consumer-app-watch!)
  (db/put! workspace-object-type nil)
  (db/put! current-consumer-app nil))

(defn app-label
  [app]
  (or (not-empty (str (:display-name app)))
      (let [nm (:name app)] (if (keyword? nm) (name nm) (str nm)))))

(defn- early-app [card]
  (when-let [e (and (exists? js/window) (gobj/get js/window "__sutraEarlyApp"))]
    (js-delete js/window "__sutraEarlyApp")
    (when (and (nil? @consumer-app-version)
               (= (gobj/get e "slug") (some-> (:name card) name)))
      e)))

(defn- request-app! [card]
  (let [ask #(api/get! :my-application (app-params card nil))]
    (if-let [e (early-app card)]
      (-> (gobj/get e "response")
          (.then (fn [^js r]
                   (if (.-ok r) (api/adopt! (gobj/get e "path") (js/Promise.resolve r)) (ask))))
          (.catch (fn [_] (ask))))
      (ask))))

(defn- opened-app
  [card branch resp]
  {:card       card
   :branch     branch
   :descriptor (cond-> (:application resp)
                 (contains? resp :data-vars) (assoc :data-vars (:data-vars resp)))
   :data       (:data resp)
   :loading?   false})

(db/reg :sutra.portal/open
        (fn [db [_ card branch t0]]
          (let [asked (inc (get-in db [:portal :asked] 0))]
            (rq/request (update db :portal assoc :asked asked
                                :current-consumer-app {:card card :loading? true :asked asked})
                        {:ask     [request-app! card]
                         :current [[:portal :current-consumer-app :asked] asked]
                         :then    [[:sutra.portal/opened card branch t0]]
                         :else    [[:sutra.portal/open-failed]]}))))

(db/reg :sutra.portal/opened
        (fn [db [_ card branch t0 resp]]
          {:db (assoc-in db [:portal :current-consumer-app] (opened-app card branch resp))
           :fx [(when (contains? resp :data) [:invoke [dbg/measure! :app/refresh t0]])
                [:invoke [sdk-client/preload-around! (get-in resp [:application :id])
                          (:around resp)]]
                [:invoke [watch-consumer-app! card (:application resp)]]]}))

(db/reg :sutra.portal/open-failed
        (fn [db [_ sentence]]
          {:db (update-in db [:portal :current-consumer-app] assoc :loading? false
                          :error (i18n/t :ui/app-load-failed {:reason sentence}))}))

(defn open-consumer-app!
  [card]
  (db/send! [:sutra.portal/open card (api/effective-page-branch) (dbg/now)]))

(db/reg :sutra.portal/reopen
        (fn [db _]
          (when-let [card (get-in db [:portal :current-consumer-app :card])]
            {:fx [[:invoke [open-consumer-app! card]]]})))

(defn reload-consumer-apps!
  []
  (sdk-client/forget-preloaded!)
  (refresh-workspaces!)
  (db/load! [:portal :consumer-apps] [api/get! :my-applications]
            {:pick (comp vec :applications) :report "applications"
             :then [[:sutra.portal/unlisted] [:sutra.portal/reopen]]}))

(defonce
  workspaces (db/cursor [:portal :workspaces] rd/loading))

(defn workspace-list
  []
  (rd/value-or @workspaces []))

(defn reads-branch?
  []
  (and (object-view/builder-session?) (feature/branching? (shell/current-app))))

(api/set-branch-scope! reads-branch?)

(defn consumer-app-of-name
  [nm]
  (let [s (cond (keyword? nm) (name nm) (some? nm) (str nm))]
    (when (seq (str s))
      (some (fn [c] (when (= s (some-> (:name c) name)) c))
            (concat (:apps @auth/current-workspace) (consumer-app-list) @unlisted-apps)))))

(defn app-reachable?
  [nm]
  (or (not (rd/value? @consumer-apps)) (some? (consumer-app-of-name nm))))

(defn refresh-workspaces!
  []
  (db/load! [:portal :workspaces] [api/get! :my-workspaces]
            {:pick (comp vec :workspaces) :dedup ::workspaces}))

(defn ensure-workspaces!
  []
  (if (rd/loading? @workspaces) (refresh-workspaces!) (js/Promise.resolve nil)))

(defonce workspace-editor
  (db/cursor [:portal :workspace-editor]
             {:open? false :tab :general
              :name nil :draft nil :saved nil
              :rights nil :projects [] :list nil :can-create false
              :saving? false :error nil :staged nil}))

(defn workspace-editor-dirty? []
  (let [{:keys [draft saved]} @workspace-editor]
    (and (some? draft) (not= draft saved))))

(def ^:private editor [:portal :workspace-editor])

(defn- edit [db f & args] (apply update-in db editor f args))

(db/reg :sutra.portal/editable
        (fn [_ _]
          {:fx [[:call {:call [api/get! :workspace-editable {}]
                        :ok [:sutra.portal/editable-read]
                        :err [:sutra.portal/editable-read nil]}]]}))

(db/reg :sutra.portal/editable-read
        (fn [db [_ r]]
          {:db (edit db assoc
                     :list (vec (:workspaces r))
                     :projects (vec (:projects r))
                     :can-create (boolean (:can-create r)))}))

(defn refresh-editable-workspaces! [] (db/send! [:sutra.portal/editable]))

(db/reg :sutra.portal/draft
        (fn [db [_ nm]]
          (rq/request (edit db assoc :error nil :staged nil :name nm)
                      {:ask   [api/get! :workspace-file {:name (name nm)}]
                       :then  [[:sutra.portal/draft-read]]
                       :error (conj editor :error)})))

(db/reg :sutra.portal/draft-read
        (fn [db [_ r]]
          (let [f (or (:file r) {:apps [] :home []})]
            {:db (edit db assoc :draft f :saved f :rights (:rights r)
                       :projects (vec (:projects r)))})))

(defn load-workspace-draft! [nm] (db/send! [:sutra.portal/draft nm]))

(defn open-workspace-editor!
  ([] (open-workspace-editor! nil))
  ([nm]
   (db/edit! workspace-editor assoc :open? true :tab :general)
   (refresh-editable-workspaces!)
   (when nm (load-workspace-draft! nm))))

(defn close-workspace-editor! []
  (db/edit! workspace-editor assoc :open? false))

(defn edit-workspace-field! [k v]
  (db/edit! workspace-editor update :draft assoc k v))

(defn discard-workspace-draft!
  []
  (db/edit! workspace-editor (fn [s] (assoc s :draft (:saved s) :error nil :staged nil))))

(db/reg :sutra.portal/stage
        (fn [db _]
          (let [{:keys [name draft]} (get-in db editor)]
            (rq/request (edit db assoc :error nil)
                        {:ask   [api/post! :workspace-file {:name (clojure.core/name name)}
                                 {:file draft}]
                         :busy  (conj editor :saving?) :error (conj editor :error)
                         :path  (conj editor :staged)
                         :then  [[:rf/set (conj editor :saved) draft]]
                         :fx    [staging/refresh [:invoke [refresh-workspaces!]]]}))))

(defn stage-workspace! [] (db/send! [:sutra.portal/stage]))

(defonce
  workspace-object-type (db/cursor [:portal :workspace-object-type] nil))

(defonce
  workspace-runtime (db/cursor [:portal :workspace-runtime] {}))

(db/reg :sutra.portal/runtime
        (fn [db [_ rid]]
          (let [held (get-in db [:portal :workspace-runtime])]
            (when (and rid (or (not (contains? held rid)) (rd/failed? (get held rid))))
              {:db (assoc-in db [:portal :workspace-runtime rid] rd/loading)
               :fx [(db/load [:portal :workspace-runtime rid] [api/get! :my-workspace {:rid rid}]
                             {:pick (comp rd/of-answer :workspace)})]}))))

(defn load-workspace-runtime!
  [rid]
  (db/send! [:sutra.portal/runtime rid]))

(defn workspace-runtime-of
  [rid]
  (get @workspace-runtime rid rd/loading))

(add-watch auth/current-workspace ::workspace-runtime
           (fn [_ _ _ ws] (load-workspace-runtime! (:id ws))))

(defonce
  workspace-home-shown? (db/cursor [:portal :workspace-home-shown?] false))

(defn enter-workspace! [ws]
  (db/put! workspace-home-shown? false)
  (close-consumer-app!)
  (auth/set-current-workspace! ws))

(defn leave-workspace! []
  (close-consumer-app!)
  (auth/set-current-workspace! nil))

(defn close-app-back!
  []
  (let [ws @auth/current-workspace]
    (when (and ws (= (get-in @current-consumer-app [:card :id]) (:landing ws)))
      (db/put! workspace-home-shown? true))
    (close-consumer-app!)))

(defn open-workspace-object-type!
  [rid]
  (close-consumer-app!)
  (db/put! workspace-object-type rid))

(defn open-workspace-app!
  [card]
  (db/put! workspace-home-shown? false)
  (db/put! workspace-object-type nil)
  (open-consumer-app! card))

(defn set-consumer-app-version!
  [v]
  (db/put! consumer-app-version v))

(defn consumer-app-failed!
  [card error]
  (db/put! current-consumer-app {:card card :error error}))

(defn set-workspace-editor-tab! [tab] (db/edit! workspace-editor assoc :tab tab))
