(ns sutra-ui.apps.source-manager.events
  (:require [sutra-ui.state.forms :as fs]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.credentials :as creds]
            [sutra-ui.forms.base :as forms]
            [sutra-ui.forms.external :as ext-forms]
            [sutra-ui.forms.link :as link-forms]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync-state]))

(def blank-source
  {:name "" :type "sqlite" :file-path "" :host "localhost" :port "" :db-name ""
   :user "" :password ""})

(def poll-ms 10000)

(def tail-cap 200)

(defn- invoke [f & args] [:invoke (into [f] args)])

(defn- sealed-upsert!
  [cred seal-opts base]
  (if (seq cred)
    (-> (creds/seal cred seal-opts)
        (.then #(data/upsert-datasource! (assoc base :secret-cipher %))))
    (data/upsert-datasource! base)))

(defn- test-connection!
  [cred seal-opts body]
  (-> (if (seq cred)
        (.then (creds/seal cred seal-opts) #(assoc body :secret-cipher %))
        (js/Promise.resolve body))
      (.then api/test-datasource)))

(defn- present [form ks] (into {} (for [k ks :when (seq (str (get form k)))] [k (get form k)])))

(defn- jdbc-cred [{:keys [user password]}]
  (cond-> {} (seq user) (assoc :user user) (seq password) (assoc :password password)))

(defn create-request
  [form fields jdbc-url]
  (let [{:keys [type description]} form
        base {:name (keyword (str/trim (str (:name form)))) :type (keyword type)}
        base (cond-> base (seq description) (assoc :description description))]
    (if fields
      (let [secret? #(= :secret (:kind %))]
        [(present form (map :key (filter secret? fields)))
         (assoc base :config (present form (map :key (remove secret? fields))))])
      [(when-not (= "sqlite" (or type "sqlite")) (jdbc-cred form))
       (assoc base :jdbc-url jdbc-url)])))

(defn test-result
  [resp]
  (cond
    (:ok resp)                    {:status :ok}
    (= "agent-down" (:code resp)) {:status :agent-down :msg (:agent resp)}
    :else                         {:status :error :msg (:error resp)}))

(defn- load-sync-form!
  [{:keys [kind key entry]}]
  (if (= kind :object)
    (let [obj-kw (catalog/name-of key)]
      (object-forms/load-object-form! obj-kw)
      (fs/edit! forms/edit-object-form assoc :external
                (ext-forms/ext-source->form-external
                 (merge (select-keys entry [:primary-key :column-map]) (:from entry))))
      {:kind :object :type obj-kw})
    (when (link-forms/load-link-form! key)
      (fs/edit! forms/edit-link-form assoc
                :backing-mode :synced
                :external (ext-forms/ext-source->form-external
                           (merge (select-keys entry [:from-key-column :to-key-column :strategy
                                                      :fk-side :fk-attribute :on-null
                                                      :on-unresolved :on-duplicate])
                                  (:from entry))))
      {:kind :link :label (some-> (catalog/name-of key) name)})))

(machine/reg-fx :sources.sync/load-form
                (fn [{f :frame} sync]
                  (when-let [edit (load-sync-form! sync)]
                    (machine/dispatch [:sources.sync/editing edit] {:frame f}))))

(defonce ^:private event-source (atom nil))

(defn- close-source! []
  (when-let [^js s @event-source] (.close s))
  (reset! event-source nil))

(machine/reg-fx :sources.stream/close (fn [_ _] (close-source!)))

(machine/reg-fx :sources.stream/open
                (fn [{f :frame} nm]
                  (close-source!)
                  (let [es    (js/EventSource. (api/on-page-branch (api/stream-subscribe-url nm)))
                        reply #(machine/dispatch % {:frame f})]
                    (reset! event-source es)
                    (.addEventListener es "open" #(reply [:sources.stream/live true]))
                    (.addEventListener
                     es "records"
                     (fn [ev]
                       (let [payload (js->clj (js/JSON.parse (.-data ev)) :keywordize-keys true)]
                         (reply [:sources.stream/records nm (vec (:records payload))]))))
                    (.addEventListener es "error" #(reply [:sources.stream/live false])))))

(machine/reg-event :sources/tab (fn [{:keys [db]} [_ tab]] {:db (assoc db :top-tab tab)}))

(machine/reg-event :sources/show
                   (fn [{:keys [db]} [_ k top-tab]]
                     {:db (cond-> (assoc db :selected-source k :source-tab :overview
                                         :selected-sync nil)
                            top-tab (assoc :top-tab top-tab))}))

(machine/reg-event :sources/back
                   (fn [{:keys [db]} _] {:db (assoc db :selected-source nil :selected-sync nil)}))

(machine/reg-event :sources.source/tab
                   (fn [{:keys [db]} [_ tab]]
                     {:db (assoc db :source-tab tab :selected-sync nil)}))

(machine/reg-event :sources/activity
                   (fn [{:keys [db]} _] {:db (update db :activity-open? not)}))

(machine/reg-event :sources.runs/kind
                   (fn [{:keys [db]} [_ kind]] {:db (assoc db :run-kind kind)}))

(machine/reg-event :sources/reindex
                   (fn [{:keys [db]} _]
                     (rq/request (assoc db :reindex {:status :pending})
                                 {:ask  [api/post! :import-poll] :path [:reindex]
                                  :pick #(assoc % :status :ok)
                                  :fx   [(invoke data/refresh-import-status!)
                                         (invoke data/refresh-runs!)
                                         (invoke instances/refresh-instance-state!)]
                                  :else [[:sources/reindex-failed]]})))

(machine/reg-event :sources/reindex-failed
                   (fn [{:keys [db]} [_ reason]]
                     {:db (assoc db :reindex {:status :error :msg reason})}))

(machine/reg-event :sources/run-all
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask [sync-state/run-import!]
                                     :fx  [(invoke data/refresh-import-status!)]})))

(machine/reg-event :sources.wizard/open
                   (fn [{:keys [db]} _]
                     {:db (assoc db :manage-datasources-modal? false :wizard? true
                                 :wizard-step 0)}))

(machine/reg-event :sources.wizard/enter
                   (fn [{:keys [db]} _]
                     (when (nil? (:source-types db))
                       (rq/request db {:ask  [api/get! :source-types] :path [:source-types]
                                       :pick #(or (:source-types %) {})
                                       :else [[:rf/set [:source-types] {}]]}))))

(machine/reg-event :sources.wizard/step
                   (fn [{:keys [db]} [_ step]] {:db (assoc db :wizard-step step)}))

(machine/reg-event :sources.wizard/field
                   (fn [{:keys [db]} [_ k v]]
                     {:db (assoc db :new-source (assoc (:new-source db blank-source) k v))}))

(machine/reg-event :sources.wizard/close
                   (fn [{:keys [db]} _]
                     {:db (assoc db :wizard? false :wizard-step 0 :new-source blank-source
                                 :ds-test nil)}))

(machine/reg-event :sources.wizard/create
                   (fn [{:keys [db]} [_ fields jdbc-url]]
                     (let [[cred base] (create-request (:new-source db blank-source) fields
                                                       jdbc-url)]
                       (rq/request db {:ask  [sealed-upsert! cred {} base]
                                       :fx   [(invoke data/refresh-sources!)]
                                       :then [[:sources.wizard/close]]}))))

(machine/reg-event :sources.manage/open
                   (fn [{:keys [db]} _] {:db (assoc db :manage-datasources-modal? true)}))

(machine/reg-event :sources.manage/close
                   (fn [{:keys [db]} _] {:db (assoc db :manage-datasources-modal? false)}))

(defn- testing-connection [db cred seal-opts body]
  (rq/request (assoc db :ds-test {:status :pending})
              {:ask  [test-connection! cred seal-opts body] :path [:ds-test] :pick test-result
               :else [[:sources.test/failed]]}))

(machine/reg-event :sources.test/jdbc
                   (fn [{:keys [db]} [_ jdbc-url]]
                     (let [form  (:new-source db blank-source)
                           type  (keyword (or (:type form) "sqlite"))
                           agent (some-> (:agent form) name not-empty)
                           body  (cond-> {:type type :jdbc-url jdbc-url}
                                   agent (assoc :agent agent))]
                       (testing-connection db (when-not (= :sqlite type) (jdbc-cred form))
                                           (if agent {:agent agent} {}) body))))

(machine/reg-event :sources.test/source
                   (fn [{:keys [db]} [_ k]] (testing-connection db nil {} {:source (name k)})))

(machine/reg-event :sources.test/failed
                   (fn [{:keys [db]} [_ reason]]
                     {:db (assoc db :ds-test {:status :error :msg reason})}))

(machine/reg-event :sources.source/delete
                   (fn [_ [_ k back?]]
                     {:fx [[:confirm {:ask {:title (i18n/t :ui/source-delete-confirm
                                                           {:name (name k)})
                                            :body (i18n/t :ui/dc-source-delete-body)
                                            :danger? true}
                                      :ok [:sources.source/delete-confirmed k back?]}]]}))

(machine/reg-event :sources.source/delete-confirmed
                   (fn [{:keys [db]} [_ k back?]]
                     (rq/request db {:ask  [data/delete-datasource! (name k)]
                                     :fx   [(invoke data/refresh-sources!)]
                                     :then (when back? [[:rf/set [:selected-source] nil]])})))

(machine/reg-event :sources.reseal/field
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:reseal k] v)}))

(machine/reg-event :sources.reseal/save
                   (fn [{:keys [db]} [_ k spec]]
                     (let [cred (jdbc-cred (:reseal db))]
                       (when (seq cred)
                         (rq/request db {:ask  [sealed-upsert! cred {:source (name k)}
                                                (cond-> {:name k :type (:type spec)
                                                         :jdbc-url (:jdbc-url spec)}
                                                  (:agent spec) (assoc :agent (:agent spec)))]
                                         :fx   [(invoke data/refresh-sources!)]
                                         :then [[:rf/set [:reseal] {}]]})))))

(machine/reg-event :sources.explore/enter
                   (fn [{:keys [db]} [_ k]]
                     (rq/request (assoc db :explore {:source k :table nil})
                                 {:ask [data/fetch-ds-tables! k]})))

(machine/reg-event :sources.explore/table
                   (fn [{:keys [db]} [_ k table]]
                     (rq/request (assoc db :explore {:source k :table table})
                                 {:ask  [api/get! :datasource-preview {:name k}]
                                  :path [:source-preview]
                                  :else [[:rf/set [:source-preview] {:tables []}]]})))

(machine/reg-event :sources.sync/select
                   (fn [{:keys [db]} [_ k]] {:db (assoc db :selected-sync k)}))

(machine/reg-event :sources.sync/run
                   (fn [{:keys [db]} [_ {:keys [kind key target]}]]
                     (rq/request (assoc-in db [:sync-run key] nil)
                                 {:ask  (if (= kind :object)
                                          [api/post! :import-run {:only [(name target)]}]
                                          [sync-state/run-import!])
                                  :busy [:sync-run key :busy?] :error [:sync-run key :error]
                                  :fx   [(invoke data/refresh-import-status!)
                                         (invoke data/refresh-runs!)]})))

(machine/reg-event :sources.sync/edit
                   (fn [_ [_ sync]] {:fx [[:sources.sync/load-form sync]]}))

(machine/reg-event :sources.sync/editing
                   (fn [{:keys [db]} [_ edit]] {:db (assoc db :sync-edit edit)}))

(machine/reg-event :sources.sync/close
                   (fn [{:keys [db]} _] {:db (assoc db :sync-edit nil)}))

(defn- save-sync [ask]
  (rq/ask {:ask ask :fx [(invoke data/refresh-import-plan!)] :then [[:rf/set [:sync-edit] nil]]}))

(machine/reg-event :sources.sync/save-object
                   (fn [_ [_ type form]]
                     (let [form (assoc form :backing-mode :synced)]
                       (if (nil? (object-forms/object-form-external-source form))
                         {:fx [(invoke shell/fail!
                                       (i18n/t :ui/external-source-required-object))]}
                         {:fx [(save-sync [ontology/update-object-type! type
                                           (object-forms/object-form->spec form)])]}))))

(machine/reg-event :sources.sync/save-link
                   (fn [_ [_ form]]
                     (let [{nm :name :keys [from to cardinality external]} form
                           es (ext-forms/ext->link-external-source external cardinality)]
                       (if (nil? es)
                         {:fx [(invoke shell/fail! (i18n/t :ui/external-source-required-link))]}
                         {:fx [(save-sync [ontology/update-link-type!
                                           (merge {:name nm :from from :to to
                                                   :cardinality cardinality :external-source es}
                                                  (link-forms/link-side-fields form))])]}))))

(machine/reg-event :sources.agents/enter
                   (fn [{:keys [db]} _]
                     {:db (assoc db :agents-polling? true)
                      :fx [[:dispatch [:sources.agents/check]]
                           [:after {:key :agents-poll :ms poll-ms
                                    :event [:sources.agents/tick]}]]}))

(machine/reg-event :sources.agents/leave
                   (fn [{:keys [db]} _]
                     {:db (assoc db :agents-polling? false) :fx [[:cancel :agents-poll]]}))

(machine/reg-event :sources.agents/tick
                   (fn [{:keys [db]} _]
                     (when (:agents-polling? db)
                       {:fx [[:dispatch [:sources.agents/check]]
                             [:after {:key :agents-poll :ms poll-ms
                                      :event [:sources.agents/tick]}]]})))

(machine/reg-event :sources.agents/check
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask  [api/get! :egress-status] :path [:agents]
                                     :pick #(vec (:agents %))
                                     :else [[:rf/set [:agents] :unreachable]]})))

(defn- stream-reads
  [nm]
  [(rq/ask {:ask   [api/get! :stream-by {:name (name nm)}] :path [:streams :detail]
            :error [:streams :error]})
   (rq/ask {:ask  [api/get! :stream-view {:name (name nm) :limit 100}]
            :path [:streams :view] :else [[:rf/set [:streams :view] nil]]})])

(def ^:private list-streams
  (rq/ask {:ask  [api/get! :streams] :path [:streams :list] :pick #(vec (:streams %))
           :else [[:sources.streams/failed]]}))

(machine/reg-event :sources.streams/enter (fn [_ _] {:fx [list-streams]}))

(machine/reg-event :sources.streams/leave
                   (fn [{:keys [db]} _]
                     {:db (assoc-in db [:streams :live?] false)
                      :fx [[:sources.stream/close nil]]}))

(machine/reg-event :sources.streams/failed
                   (fn [{:keys [db]} [_ reason]]
                     {:db (assoc-in db [:streams :error]
                                    (i18n/t :ui/streams-list-failed {:message reason}))}))

(machine/reg-event :sources.stream/select
                   (fn [{:keys [db]} [_ nm]]
                     {:db (update db :streams assoc :selected nm :detail nil :view nil
                                  :error nil :tail [] :live? false)
                      :fx (conj (stream-reads nm) [:sources.stream/open nm])}))

(machine/reg-event :sources.stream/deselect
                   (fn [{:keys [db]} _]
                     {:db (update db :streams assoc :selected nil :detail nil :view nil
                                  :tail [] :live? false)
                      :fx [[:sources.stream/close nil]]}))

(machine/reg-event :sources.stream/live
                   (fn [{:keys [db]} [_ on?]] {:db (assoc-in db [:streams :live?] on?)}))

(machine/reg-event :sources.stream/records
                   (fn [{:keys [db]} [_ nm records]]
                     {:db (update-in db [:streams :tail]
                                     #(vec (take tail-cap (concat (reverse records) %))))
                      :fx (conj (stream-reads nm) list-streams)}))

(machine/reg-event :sources.stream/flush
                   (fn [{:keys [db]} [_ nm]]
                     (rq/request db {:ask  [api/post! :stream-archive {:name (name nm)} {}]
                                     :busy [:streams :flushing?] :error [:streams :error]
                                     :fx   (conj (stream-reads nm) list-streams)})))
