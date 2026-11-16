(ns sutra-ui.apps.source-manager.core
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui :refer [link tag]]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.display :as display]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra-ui.apphost.platform :as platform]
            [clojure.string :as str]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.shell :as shell]
            [sutra.kit.remote :as rd]
            [sutra-ui.ui.style :as style]
            [sutra-ui.apps.source-manager.events :as ev]
            [sutra-ui.apps.source-manager.model :as model]
            [sutra-ui.apps.source-manager.streams :as streams]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.forms.base :as forms]
            [sutra-ui.forms.external :as ext-forms]
            [sutra-ui.forms.link :as link-forms]
            [sutra-ui.resource.link :as rl]
            [sutra.i18n.core :as i18n]))

(def ^:private ui model/ui)

(def ^:private send! model/send!)

(defn- new-source [] (:new-source @ui ev/blank-source))

(defn- ds-test-banner []
  (when-let [{:keys [status msg]} (:ds-test @ui)]
    [ui/callout {:class  "dataconn-testbanner"
                 :intent (case status :pending nil :ok :success :danger)}
     (case status
       :pending (i18n/t :ui/link-testing)
       :ok      (i18n/t :ui/link-ok)
       :agent-down (i18n/t :ui/link-agent-down {:agent msg})
       :error   (i18n/t :ui/link-failed {:message msg}))]))

(defn- plan-syncs
  []
  (let [plan @data/import-plan
        row  (fn [kind [k e]] {:kind kind :key k :target (catalog/name-of k)
                               :source (get-in e [:from :datasource])})]
    (concat (map (partial row :object) (:objects plan))
            (map (partial row :link) (:links plan)))))

(defn- kw-name [x] (some-> x name keyword))

(defn- run-types [run]
  (some->> (get-in run [:summary :scope :object-types]) seq (map kw-name) set))

(defn- run-subject
  [run]
  (when (= :import (kw-name (:kind run)))
    (if-let [ts (run-types run)]
      {:types ts
       :sources (->> (plan-syncs)
                     (filter #(contains? ts (kw-name (:target %))))
                     (keep :source) distinct)}
      (when (:summary run) {:all? true}))))

(defn- run-reads?
  [run target]
  (let [{:keys [all? types]} (run-subject run)]
    (or all? (contains? types (kw-name target)))))

(defn- status-badge [status]
  (let [[intent label] (case status
                         "succeeded" [:success :ui/status-succeeded]
                         "running"   [:warning :ui/dc-run-running]
                         "failed"    [:danger :ui/dc-run-failed]
                         [nil nil])]
    [tag {:minimal true :intent intent} (if label (i18n/t label) (str status))]))

(defn- format-instant [inst]
  (when inst
    (try (-> inst .toString (subs 0 19) (str/replace "T" " "))
         (catch :default _ (str inst)))))

(defn- run-subject-line [run]
  (let [{:keys [all? types sources]} (run-subject run)]
    (cond
      all? [:span (i18n/t :ui/dc-run-all-syncs)]
      (seq types)
      [:span {:class "u-flex-wrap-gap-6"}
       (for [src sources] ^{:key (str "s" src)} [rl/resource-link :source src])
       [:span "→"]
       (for [t (sort types)] ^{:key (str "t" t)} [rl/resource-link :object-type t])]
      (= :datasource (kw-name (:kind run))) [:span (i18n/t :ui/dc-run-source-change)]
      :else nil)))

(defn- run-row [{:keys [id status started-at finished-at objects-count links-count
                        snapshot-id error-message] :as run}]
  [ui/tooltip {:content (str (i18n/t :ui/dc-run-id) " " id
                             (when snapshot-id
                               (str " · " (i18n/t :ui/commit) " " snapshot-id)))}
   [:div {:class "dataconn-runs__row"}
    [status-badge status]
    [:span {:class "dataconn-runs__kind-chip"}
     (i18n/t (if (= :datasource (kw-name (:kind run))) :ui/dc-kind-source :ui/dc-kind-sync))]
    [:div
     [:div {:class "u-bold"}
      [run-subject-line run]
      (when error-message
        [:span {:class "dataconn-runs__error"} (str "— " error-message)])]
     [:div {:class "dataconn-runs__meta"}
      (i18n/t :ui/started-suffix {:at (format-instant started-at)})
      (when finished-at (i18n/t :ui/finished-suffix {:at (format-instant finished-at)}))]]
    [:span
     (i18n/t :ui/objects-count {:n (or objects-count 0)})]
    [:span (i18n/t :ui/links-count {:n (or links-count 0)})]]])

(def ^:private dc-run-kinds #{:import :datasource})

(defn- dc-runs
  []
  (filter #(contains? dc-run-kinds (kw-name (:kind %))) (data/run-list)))

(defn- run-all-buttons []
  (let [ds       (data/datasource-map)
        plan     @data/import-plan
        ready?   (and (seq ds) (or (seq (:objects plan)) (seq (:links plan))))
        loading? (:loading? @shell/ui-state)]
    [:div {:class "u-flex-items-gap-8"}
     [ui/tooltip {:content (i18n/t :ui/reindex-hint)}
      [ui/button {:on-click #(send! [:sources/reindex]) :disabled (or (empty? ds) loading?)}
       (i18n/t :ui/dc-reindex-changed)]]
     [ui/tooltip {:content (i18n/t :ui/run-import-hint)}
      [ui/button
       {:intent :primary
        :on-click #(send! [:sources/run-all])
        :disabled (or (not ready?) loading?)}
       (i18n/t (if loading? :ui/running :ui/dc-run-all-syncs-button))]]]))

(defn- runs-panel []
  (when @data/runs-kind-filter (data/set-runs-kind-filter! nil))
  (fn []
    (let [kind (:run-kind @ui)
          runs (cond->> (dc-runs) kind (filter #(= kind (kw-name (:kind %)))))]
      [:div {:class "dataconn-runs"}
       [:div {:class "dataconn-runs__header"}
        [:div {:class "u-section-label"}
         (str (i18n/t :ui/dc-runs) " (" (count runs) ")")]
        [run-all-buttons]]
       [ui/segmented-control
        {:small true :class "u-mb-10" :value (or kind :all)
         :on-value-change #(send! [:sources.runs/kind (when (not= :all %) %)])
         :options [{:value :all :label (i18n/t :ui/filter-all)}
                   {:value :import :label (i18n/t :ui/dc-filter-syncs)}
                   {:value :datasource :label (i18n/t :ui/dc-filter-source-changes)}]}]
       (if (empty? runs)
         [:div {:class "dataconn-runs__empty"} (i18n/t :ui/activity-none)]
         (for [r runs] ^{:key (:id r)} [run-row r]))])))

(def ^:private default-port
  {"postgres" "5432"
   "mysql"    "3306"
   "mssql"    "1433"})

(defn- build-jdbc-url
  [{:keys [type file-path host port db-name]}]
  (let [t  (or type "sqlite")
        h  (-> host str str/trim)
        p  (let [s (-> port str str/trim)]
             (if (seq s) s (default-port t)))
        db (-> db-name str str/trim)
        fp (-> file-path str str/trim)]
    (case t
      "sqlite"   (when (seq fp) (str "jdbc:sqlite:" fp))
      "postgres" (when (and (seq h) (seq db)) (str "jdbc:postgresql://" h ":" p "/" db))
      "mysql"    (when (and (seq h) (seq db)) (str "jdbc:mysql://" h ":" p "/" db))
      "mssql"    (when (and (seq h) (seq db)) (str "jdbc:sqlserver://" h ":" p ";databaseName=" db))
      nil)))

(defn- conn-param-fields
  [form set-f]
  (let [t (or (:type form) "sqlite")]
    (if (= "sqlite" t)
      [:div
       [:label {:class style/label} (i18n/t :ui/file-path)]
       [ui/input-group
        {:value (:file-path form)
         :placeholder (i18n/t :ui/dc-sqlite-file-placeholder
                              {:file (str (or (not-empty (some-> (:name form) str/trim)) "ops")
                                          ".sqlite")})
         :on-change #(set-f :file-path (.. % -target -value))
         :class "u-mb-8"}]]
      [:<>
       [:div {:class "dataconn-form__hostgrid"}
        [:div
         [:label {:class style/label} (i18n/t :ui/host)]
         [ui/input-group {:value (:host form)
                          :placeholder "localhost"
                          :on-change #(set-f :host (.. % -target -value))
                          :class "u-m-0"}]]
        [:div
         [:label {:class style/label} (i18n/t :ui/port)]
         [ui/input-group {:value (:port form)
                          :placeholder (default-port t)
                          :on-change #(set-f :port (.. % -target -value))
                          :class "u-m-0"}]]
        [:div
         [:label {:class style/label} (i18n/t :ui/database)]
         [ui/input-group {:value (:db-name form)
                          :placeholder (i18n/t :ui/database-name)
                          :on-change #(set-f :db-name (.. % -target -value))
                          :class "u-m-0"}]]]])))

(def ^:private source-type-icon
  {:csv :document :sqlite :database :postgres :database :mysql :database :mssql :database
   :sftp :exchange :command :console :rest :globe-network})

(defn- source-icon [t] (get source-type-icon (some-> t keyword) :data-connection))

(def ^:private coded-connector-cards
  [{:type "sqlite"   :label "SQLite"     :desc :ui/connector-sqlite-desc}
   {:type "postgres" :label "PostgreSQL" :desc :ui/connector-network-desc}
   {:type "mysql"    :label "MySQL"      :desc :ui/connector-network-desc}
   {:type "mssql"    :label "SQL Server" :desc :ui/connector-network-desc}])

(defn- registry-cards
  []
  (for [[t d] (sort-by key (:source-types @ui))]
    {:type (name t) :label (:label d)
     :desc (:desc d) :dynamic? true}))

(def ^:private wizard-steps
  [:ui/step-connector :ui/step-configure :ui/credentials :ui/step-finish])

(defn- wizard-stepper [step]
  [display/stepper (map-indexed (fn [i label] {:id i :label (i18n/t label)}) wizard-steps) step])

(defn- wizard-field-input
  [form set-f {:keys [key label kind placeholder]}]
  [:div
   [:label {:class style/label} (or label (clojure.core/name key))]
   [ui/input-group {:type (if (= :secret kind) "password" "text")
                    :value (get form key)
                    :placeholder (or placeholder "")
                    :on-change #(set-f key (.. % -target -value))
                    :class "u-mb-8"}]])

(defn- with-default-sqlite-file
  [form]
  (let [nm (-> form :name str str/trim)]
    (cond-> form
      (and (= "sqlite" (or (:type form) "sqlite"))
           (str/blank? (:file-path form)) (seq nm))
      (assoc :file-path (str nm ".sqlite")))))

(defn- wizard-body []
  (let [form     (new-source)
        set-f    (fn [k v] (send! [:sources.wizard/field k v]))
        step     (:wizard-step @ui 0)
        set-step (fn [s] (send! [:sources.wizard/step s]))
        jdbc-url (build-jdbc-url (with-default-sqlite-file form))
        descr    (get (:source-types @ui) (some-> (:type form) keyword))
        dyn?     (some? descr)
        dfields  (:fields descr)
        dconfig  (filter #(not= :secret (:kind %)) dfields)
        dsecrets (filter #(= :secret (:kind %)) dfields)
        dyn-ready (every? (fn [{:keys [key required]}]
                            (or (not required) (seq (str (get form key)))))
                          dconfig)
        local?   (= "sqlite" (or (:type form) "sqlite"))
        endpoint (if dyn? (when dyn-ready :ok) jdbc-url)
        nm       (-> form :name str str/trim)
        close    #(send! [:sources.wizard/close])
        create   #(send! [:sources.wizard/create (when dyn? (or dfields [])) jdbc-url])]
    [overlay/modal {:on-close #(close)
                    :size 520
                    :title (i18n/t :ui/source-new)
                    :foot [:<>
                           (when (pos? step)
                             [ui/button
                              {:on-click #(set-step (dec step))}
                              (i18n/t :ui/back-step)])
                           (if (< step 3)
                             [ui/button
                              {:intent :primary :on-click #(set-step (inc step))
                               :disabled (or (and (= step 0) (nil? (:type form)))
                                             (and (= step 1) (or (empty? nm) (not endpoint))))}
                              (i18n/t :ui/next-step)]
                             [ui/button
                              {:intent   :primary :on-click create
                               :disabled (or (empty? nm) (not endpoint))}
                              (i18n/t :ui/source-create)])]
                    :foot-end? true}
     [:<>
      [wizard-stepper step]
      (case step
        0 [:div
           [:div {:class "dataconn-wizard__hint"}
            (i18n/t :ui/connector-pick)]
           [:div {:class "dataconn-wizard__connector-grid"}
            (for [{:keys [type label desc]} (concat coded-connector-cards (registry-cards))]
              ^{:key type}
              [:div
               {:on-click #(do (set-f :type type) (set-step 1))
                :class ["dataconn-wizard__connector"
                        (when (= type (:type form)) "dataconn-wizard__connector--on")]}
               [ui/icon {:icon (source-icon type) :size 20}]
               [:div {:class "dataconn-wizard__connector-label"} label]
               [:div {:class "dataconn-wizard__connector-desc"}
                (if (keyword? desc) (i18n/t desc) desc)]])]]
        1 [:div
           [:label {:class style/label} (i18n/t :ui/name)]
           [ui/input-group
            {:value (:name form) :placeholder (i18n/t :ui/source-name-example)
             :on-change #(set-f :name (.. % -target -value))
             :class "u-mb-8"}]
           (if dyn?
             (into [:div] (for [f dconfig] [wizard-field-input form set-f f]))
             [conn-param-fields form set-f])
           [:label {:class style/label} (i18n/t :ui/description-optional-label)]
           [ui/input-group {:value (:description form)
                            :on-change #(set-f :description (.. % -target -value))
                            :class "u-mb-8"}]
           [:div {:class [style/small-muted "u-mt-8"]}
            (i18n/t (if dyn? :ui/egress-joins-on-deploy :ui/egress-managed-elsewhere))]]
        2 (cond
            (and (not dyn?) local?)
            [:div {:class "dataconn-wizard__note"}
             (i18n/t :ui/sqlite-no-credentials)]
            dyn?
            (if (seq dsecrets)
              (into [:div
                     [:div {:class [style/small-muted "u-mb-8"]}
                      (i18n/t :ui/credentials-sealed-note)]]
                    (for [f dsecrets] [wizard-field-input form set-f f]))
              [:div {:class "dataconn-wizard__note"}
               (i18n/t :ui/connector-no-credential)])
            :else
            [:div
             [:div {:class [style/small-muted "u-mb-8"]}
              (i18n/t :ui/credentials-sealed-note)]
             [:label {:class style/label} (i18n/t :ui/username)]
             [ui/input-group {:value (:user form)
                              :on-change #(set-f :user (.. % -target -value))
                              :class "u-mb-8"}]
             [:label {:class style/label} (i18n/t :ui/password)]
             [ui/input-group {:type "password" :value (:password form)
                              :on-change #(set-f :password (.. % -target -value))}]])
        3 [:div
           [:div {:class "dataconn-wizard__summary"}
            [:b (or (not-empty nm) (i18n/t :ui/unnamed))] " · " (:type form)]
           (when-not dyn?
             [ui/button
              {:class "u-mb-8" :on-click #(send! [:sources.test/jdbc jdbc-url])
               :disabled (nil? jdbc-url)}
              (i18n/t :ui/link-test)])
           [ds-test-banner]]
        nil)]]))

(def ^:private wizard-dialog
  (machine/entering model/sources [:sources.wizard/enter] wizard-body))

(defn new-source-wizard [] (when (:wizard? @ui) [wizard-dialog]))

(defn- ds-list-row [k spec]
  [:div {:class "dataconn-dslist__row"}
   [:div {:class "dataconn-dslist__name"} (name k)]
   [:div {:class "dataconn-dslist__type u-muted"} (some-> (:type spec) name)]
   [:div {:class "dataconn-dslist__url"} (or (:jdbc-url spec) (:base-url spec))]
   [ui/tooltip {:content (i18n/t :ui/source-delete)}
    [ui/button {:icon :cross :small true :outlined true :intent :danger
                :on-click #(send! [:sources.source/delete k false])}]]])

(defn manage-datasources-modal []
  [:<>
   (when (:manage-datasources-modal? @ui)
     (let [close #(send! [:sources.manage/close])
           dsv   (data/datasources-view)]
       [overlay/modal {:on-close #(close)
                       :size 600
                       :title (i18n/t :ui/datasources-manage-title)
                       :foot [ui/button {:on-click close} (i18n/t :ui/done)]
                       :foot-end? true}
        [:<>
         [:div {:class "dataconn-managemodal__caption u-muted"}
          (i18n/t :ui/datasources-manage-caption)]
         [:div {:class "dataconn-managemodal__list"}
          (case (:show dsv)
            :loading [busy/block nil (i18n/t :ui/datasources-loading)]
            :failed  [ui/problem (i18n/t :ui/datasources-failed {:reason (:reason dsv)}) nil]
            :empty   [ui/empty-state {:title (i18n/t :ui/datasources-none-yet)}]
            (into [:<>] (for [[k spec] (:rows dsv)]
                          ^{:key k} [ds-list-row k spec])))]]]))
   [new-source-wizard]])

(defn- sync-editor-modal []
  (when-let [{:keys [kind type label]} (:sync-edit @ui)]
    (let [close #(send! [:sources.sync/close])
          conn? (= kind :link)
          title (i18n/t :ui/sync-edit-title {:name (or label (some-> type clojure.core/name))})
          save  (if conn?
                  #(send! [:sources.sync/save-link @forms/edit-link-form])
                  #(send! [:sources.sync/save-object type @forms/edit-object-form]))]
      [overlay/form-dialog {:on-close close :size 600 :title title
                            :ok-label (i18n/t :ui/save-mapping) :on-ok save}
       [:div {:class [style/small-muted "u-mb-10"]}
        (i18n/t (if conn? :ui/sync-edit-link-hint :ui/sync-edit-object-hint))]
       (if conn?
         (let [f @forms/edit-link-form]
           [link-forms/link-external-section
            {:form-atom forms/edit-link-form
             :from-kw (keyword (:from f)) :to-kw (keyword (:to f))}])
         [ext-forms/object-external-section {:form-atom forms/edit-object-form}])])))

(defn- source-endpoint
  [spec]
  (or (:jdbc-url spec) (:path spec)
      (when (seq (:host spec))
        (str (:host spec) (when (:port spec) (str ":" (:port spec)))))
      (some-> (:rest spec) :base-url)
      "—"))

(defn- egress-routed-source?
  [spec]
  (boolean (and (not (#{:csv :sqlite} (:type spec)))
                (or (:secret-ref spec) (seq (:egress spec))))))

(defn- agent-of [spec] (or (some-> (:agent spec) name) "default"))

(defn- pill [label intent] [tag {:minimal true :intent intent} label])

(defn- health-pill
  [health]
  (let [{:keys [status message]} health]
    (when (and status (not= "healthy" status))
      (let [[label intent] (case status
                             "misconfigured" [(i18n/t :ui/health-misconfigured) :danger]
                             "unreachable"   [(i18n/t :ui/health-unreachable) :warning]
                             [(i18n/t :ui/health-unhealthy) :danger])]
        [ui/tooltip {:content (or message (name status))}
         [pill label intent]]))))

(defn- health-banner
  [health]
  (let [{:keys [status code message]} health]
    (when (and status (not= "healthy" status))
      [ui/callout {:intent :danger :class "dataconn-testbanner"}
       [:span {:class "u-bold"}
        (case status
          "misconfigured" (i18n/t :ui/source-misconfigured)
          "unreachable"   (i18n/t :ui/source-unreachable)
          (i18n/t :ui/source-unhealthy))]
       [:span message]
       (when code [:span {:class "u-mono-12"} (str " (" code ")")])])))

(defn- access-pill [access]
  (case access
    :read-write [pill (i18n/t :ui/access-read-write) :warning]
    [pill (i18n/t :ui/access-read-only) :primary]))

(defn- source-card [src-key spec]
  (let [consumers (:consumers spec)]
    [:div
     {:on-click #(send! [:sources/show src-key nil])
      :class ["dataconn-sourcecard"
              (when (= src-key (:selected-source @ui)) "dataconn-sourcecard--on")]}
     [:div {:class "dataconn-sourcecard__head"}
      [ui/icon {:icon (source-icon (:type spec)) :size 20 :class "u-muted"}]
      [:div {:class "u-flex-1-min-0"}
       [:div {:class "dataconn-sourcecard__name"}
        (name src-key)]
       [:div {:class "dataconn-sourcecard__type"}
        (some-> (:type spec) name)]]
      [health-pill (:health spec)]
      [access-pill (:access spec)]]
     (when (seq (:description spec))
       [:div {:class "dataconn-sourcecard__desc"}
        (:description spec)])
     [:div {:class "dataconn-sourcecard__endpoint"}
      (source-endpoint spec)]
     [:div {:class "dataconn-sourcecard__footer"}
      [:span (i18n/t (if (= 1 (count consumers)) :ui/feeds-one :ui/feeds-many)
                     {:n (count consumers)})]
      (when (egress-routed-source? spec)
        [ui/tooltip {:content (i18n/t :ui/egress-credential-hint)}
         [:span
          (i18n/t :ui/dc-via-agent {:agent (agent-of spec)})]])]]))

(defn- activity-footer []
  (let [open? (:activity-open? @ui)]
    [ui/section
     {:title (str (i18n/t :ui/dc-runs-recent) " (" (count (dc-runs)) ")") :collapsible true
      :compact true
      :collapse-props {:is-open (boolean open?) :on-toggle #(send! [:sources/activity])}
      :class "dataconn-activity"}
     [ui/section-card
      [runs-panel]]]))

(defn sources-pane []
  (let [sources @data/sources]
    [:div {:class "dataconn-pane"}
     [page/toolbar
      [:b (i18n/t (if (= 1 (count sources)) :ui/sources-count-one :ui/sources-count-many)
                  {:n (count sources)})]
      [ui/button {:intent :primary :icon :plus
                  :on-click #(send! [:sources.wizard/open])}
       (i18n/t :ui/source-new-button)]]
     (if (empty? sources)
       [ui/empty-state {:icon :database :title (i18n/t :ui/sources-none)}]
       [:div {:class "dataconn-sourcecard__grid"}
        (for [[k spec] (sort-by (comp name first) (seq sources))]
          ^{:key k} [source-card k spec])])
     [:div {:class "dataconn-activity__wrap"}
      [activity-footer]]]))

(defn- detail-card [title body]
  [:div {:class "dataconn-detail__card"}
   (when title [:div {:class "u-section-label"} title])
   body])

(defn- kv [label v] (when label {:prop label :label label :render v}))

(defn- props [& entries]
  [display/property-list (filter some? (flatten entries)) nil])

(defn- consumer-target [target]
  (let [k (catalog/name-of target)]
    (cond
      (contains? (:object-types @catalog/schema) k) [rl/resource-link :object-type k]
      (contains? (:link-types @catalog/schema) k)   [rl/resource-link :link-type k]
      :else [:span (some-> k name)])))

(defn- source-overview [_src-key spec]
  [:div {:class "dataconn-pane"}
   [health-banner (:health spec)]
   [detail-card (i18n/t :ui/source)
    [props
     (kv (i18n/t :ui/type) (some-> (:type spec) name))
     (kv (i18n/t :ui/endpoint) [:span {:class "u-mono-12"} (source-endpoint spec)])
     (kv (i18n/t :ui/access) [access-pill (:access spec)])
     (when (egress-routed-source? spec)
       (kv (i18n/t :ui/dc-agent)
        [link {:href "#" :on-click (fn [e] (.preventDefault e) (send! [:sources/tab :agents]))}
         (agent-of spec)]))
     (when (seq (:description spec)) (kv (i18n/t :ui/description) (:description spec)))]]
   [detail-card (i18n/t :ui/consumed-by)
    (if (seq (:consumers spec))
      [:div
       (for [{:keys [transform target]} (:consumers spec)]
         ^{:key (str transform "/" target)}
         [:div {:class "dataconn-detail__consumer u-flex-wrap-gap-6"}
          (if target [consumer-target target] [rl/resource-link :transform transform])
          (when target
            [:span
             (i18n/t :ui/dc-via-transform) " " [rl/resource-link :transform transform]])])]
      [:div {:class "dataconn-detail__empty"}
       (i18n/t :ui/consumed-by-none)])]
   [detail-card (i18n/t :ui/governance)
    [:<>
     [props
      (kv (i18n/t :ui/markings)
       (if (seq (:markings spec))
         [:div {:class "u-flex-wrap-gap-6"}
          (for [m (:markings spec)] ^{:key m} [pill (name m) nil])]
         [:span {:class "u-text-empty"} (i18n/t :ui/none)]))
      (kv (i18n/t :ui/credential)
       (if (egress-routed-source? spec)
         (i18n/t :ui/credential-held-by-agent)
         [:span {:class "u-text-empty"} (i18n/t :ui/credential-none-local)]))]
     [ui/section
      {:title (i18n/t :ui/dc-details) :collapsible true :compact true
       :collapse-props {:default-is-open false}}
      [ui/section-card
       [props
        (kv (i18n/t :ui/egress-allowlist)
         (if (seq (:egress spec))
           [:div {:class "u-mono-12"}
            (for [e (:egress spec)] ^{:key e} [:div e])]
           [:span {:class "u-text-empty"} "—"]))
        (when (:secret-ref spec)
          (kv (i18n/t :ui/dc-secret-ref)
           [:span {:class "u-mono-12"} (name (:secret-ref spec))]))]]]]]])

(defn- source-link [src-key spec]
  (let [jdbc-url (:jdbc-url spec)
        egress?  (egress-routed-source? spec)
        reseal   (:reseal @ui)]
    [:div {:class "dataconn-pane"}
     [detail-card (i18n/t :ui/dc-connection)
      [:<>
       [props
        (kv (i18n/t :ui/type) (some-> (:type spec) name))
        (kv (i18n/t :ui/endpoint) [:span {:class "u-mono-12"} (source-endpoint spec)])]
       (when (seq (:egress spec))
         [ui/section
          {:title (i18n/t :ui/dc-details) :collapsible true :compact true
           :collapse-props {:default-is-open false}}
          [ui/section-card
           [props
            (kv (i18n/t :ui/egress-allowlist)
             [:span {:class "u-muted"} (i18n/t :ui/egress-agent-managed)])]]])]]
     (when egress?
       [detail-card (i18n/t :ui/credentials)
        [:div
         [:div {:class [style/small-muted "u-mb-8"]}
          (i18n/t :ui/credentials-reseal-hint)]
         [:label {:class style/label} (i18n/t :ui/username)]
         [ui/input-group {:value (:user reseal)
                          :on-change #(send! [:sources.reseal/field :user
                                              (.. % -target -value)])
                          :class "u-mb-8"}]
         [:label {:class style/label} (i18n/t :ui/password)]
         [ui/input-group
          {:type "password" :value (:password reseal)
           :on-change #(send! [:sources.reseal/field :password (.. % -target -value)])
           :class "u-mb-10"}]
         [ui/button {:intent :primary :class "u-mr-8"
                     :on-click #(send! [:sources.reseal/save src-key spec])
                     :disabled (not (or (seq (:user reseal)) (seq (:password reseal))))}
          (i18n/t :ui/reseal-and-save)]]])
     [detail-card (i18n/t :ui/actions)
      [:div {:class "u-flex-items-gap-8"}
       (when jdbc-url
         [ui/button {:on-click #(send! [:sources.test/source src-key])}
          (i18n/t :ui/link-test)])
       [ui/button {:outlined true :intent :danger
                   :on-click #(send! [:sources.source/delete src-key true])}
        (i18n/t :ui/source-delete)]]
      [ds-test-banner]]]))

(def ^:private source-explore
  (machine/entering
   model/sources (fn [src-key _] [:sources.explore/enter src-key])
   (fn [src-key _spec]
     (let [tables  (get-in @data/ds-introspection [:tables src-key])
           preview (:source-preview @ui)
           health  (data/source-health src-key)
           down?   (data/source-unhealthy? src-key)
           explore (:explore @ui)
           sel*    (when (= src-key (:source explore)) (:table explore))]
       [:div {:class "dataconn-pane"}
        [health-banner health]
        [page/toolbar
         (if (empty? tables)
           [:span {:class "u-muted"}
            (i18n/t (if down? :ui/tables-unavailable :ui/tables-none-reachable))]
           [ui/html-select {:value (or sel* "")
                            :on-change #(send! [:sources.explore/table src-key
                                                (.. % -target -value)])}
            [:option {:value "" :disabled true}
             (str (i18n/t :ui/tables) " (" (count tables) ")")]
            (for [t tables] ^{:key t} [:option {:value t} t])])]
        (let [rows (mapcat :rows (:tables preview))
              cols (some-> (first rows) keys vec)]
          (cond
            (nil? preview)
            [ui/empty-state {:icon :th :title (i18n/t :ui/pick-table-for-preview)}]
            (empty? rows)
            [ui/empty-state
             {:icon :th
              :title (i18n/t (if down? :ui/preview-unavailable :ui/rows-not-materialized))}]
            :else
            [table/data-table
             {:scroll? true
              :rows    (vec (take 50 rows))
              :cols    (for [c cols]
                         {:key (str c) :label (str c) :mono? true
                          :render #(str (get % c))})}]))]))))

(defn- sync-status
  [target]
  (let [per (:per-type (rd/value-or @data/import-status {}))]
    (or (get per target) (get per (kw-name target)) (get per (keyword (str target))))))

(defn- sync-reads [{:keys [entry]}]
  (or (get-in entry [:from :table]) (i18n/t :ui/query)))

(defn- sync-target-link [{:keys [kind target]}]
  [rl/resource-link (if (= kind :link) :link-type :object-type) target])

(defn- last-run-of [{:keys [target]}]
  (first (filter #(run-reads? % target) (dc-runs))))

(defn- run-sync-button [{:keys [kind key] :as sync}]
  (let [{:keys [busy?]} (get-in @ui [:sync-run key])
        link? (= kind :link)]
    [ui/tooltip
     {:content (i18n/t (if link? :ui/dc-sync-run-link-hint :ui/dc-sync-run-hint))}
     [ui/button
      {:on-click (fn [e] (.stopPropagation e) (send! [:sources.sync/run sync]))
       :disabled (or busy? (:loading? @shell/ui-state))}
      (i18n/t (cond busy? :ui/running link? :ui/dc-sync-run-plan :else :ui/run))]]))

(defn- sync-last-run-cell [sync]
  (if-let [r (last-run-of sync)]
    [:span [status-badge (:status r)] " " (format-instant (:started-at r))]
    [:span {:class "u-text-empty"} "—"]))

(defn- sync-state [{:keys [kind target entry]}]
  (let [n (:count (sync-status target))]
    (if (= kind :object)
      (if n (i18n/t :ui/imported-count {:n n}) (i18n/t :ui/not-yet-imported))
      [:span {:class "u-flex-wrap-gap-6"}
       [rl/resource-link :object-type (:from-type entry)]
       [ui/icon {:icon :arrows-horizontal :size 12}]
       [rl/resource-link :object-type (:to-type entry)]])))

(defn- sync-table [syncs]
  [table/data-table
   {:rows         (sort-by #(str (rl/label :object-type (:target %))) syncs)
    :row-key      :key
    :on-row-click #(send! [:sources.sync/select (:key %)])
    :cols [{:label (i18n/t :ui/dc-sync-target) :render (fn [s] [sync-target-link s])}
           {:label (i18n/t :ui/dc-sync-reads) :mono? true :render sync-reads}
           {:label (i18n/t :ui/status) :render sync-state}
           {:label (i18n/t :ui/dc-last-run) :render (fn [s] [sync-last-run-cell s])}
           {:label "" :render (fn [s] [run-sync-button s])}]}])

(defn- sync-detail [src-key {:keys [kind key target entry] :as sync}]
  (let [{:keys [primary-key column-map from-key-column to-key-column from-type to-type]} entry
        {n :count last-at :last-at} (sync-status target)
        runs  (filter #(run-reads? % target) (dc-runs))
        err   (get-in @ui [:sync-run key :error])
        link? (= kind :link)]
    [:div {:class "dataconn-pane"}
     [page/toolbar
      [ui/button {:minimal true :icon :arrow-left :on-click #(send! [:sources.sync/select nil])}
       (i18n/t :ui/dc-back-to-syncs)]
      [run-sync-button sync]
      [ui/button
       {:on-click #(send! [:sources.sync/edit sync])}
       (i18n/t :ui/sync-edit)]
      (when link? [:span {:class style/small-muted} (i18n/t :ui/dc-sync-run-link-hint)])]
     (when err [ui/problem err nil])
     [detail-card
      (i18n/t :ui/dc-sync-title {:name (rl/label (if link? :link-type :object-type) target)})
      [props
       (kv (i18n/t :ui/source) [rl/resource-link :source src-key])
       (kv (i18n/t :ui/dc-sync-reads) [:span {:class "u-mono-12"} (sync-reads sync)])
       (kv (i18n/t :ui/dc-sync-target) [sync-target-link sync])
       (if link?
         (kv (i18n/t :ui/dc-sync-keys)
          [:span {:class "u-flex-wrap-gap-6"}
           [rl/resource-link :object-type from-type] (str "." (some-> from-key-column name))
           [ui/icon {:icon :arrow-right :size 12}]
           [rl/resource-link :object-type to-type] (str "." (some-> to-key-column name))])
         [(kv (i18n/t :ui/dc-primary-key)
              [:span {:class "u-mono-12"} (some-> primary-key name)])
          (kv (i18n/t :ui/dc-columns) (i18n/t :ui/cols-count {:n (count column-map)}))
          (kv (i18n/t :ui/status)
              (if n
                (str (i18n/t :ui/imported-count {:n n})
                     (when last-at (str " · " (format-instant last-at))))
                (i18n/t :ui/not-yet-imported)))])
       (kv (i18n/t :ui/dc-last-run) [sync-last-run-cell sync])]]
     [detail-card (str (i18n/t :ui/dc-sync-history) " (" (count runs) ")")
      (if (empty? runs)
        [ui/empty-state {:title (i18n/t :ui/dc-sync-history-none)}]
        [:div (for [r (take 20 runs)] ^{:key (:id r)} [run-row r])])]]))

(defn- source-syncs [src-key]
  (let [syncs (filter #(= src-key (:source %)) (plan-syncs))
        objs  (filter #(= :object (:kind %)) syncs)
        links (filter #(= :link (:kind %)) syncs)
        sel   (:selected-sync @ui)]
    (if-let [s (some #(when (= sel (:key %)) %) syncs)]
      [sync-detail src-key s]
      [:div {:class "dataconn-pane"}
       [detail-card (str (i18n/t :ui/object-syncs) " (" (count objs) ")")
        (if (empty? objs)
          [ui/empty-state {:title (i18n/t :ui/object-syncs-none)}]
          [sync-table objs])]
       [detail-card (str (i18n/t :ui/link-syncs) " (" (count links) ")")
        (if (empty? links)
          [ui/empty-state {:title (i18n/t :ui/link-syncs-none)}]
          [sync-table links])]])))

(defn- source-webhooks [spec]
  [:div {:class "dataconn-pane"}
   (when (:rest spec)
     [detail-card (i18n/t :ui/rest-endpoint)
      [props
       (kv (i18n/t :ui/base-url) [:span {:class "u-mono-12"} (get-in spec [:rest :base-url])])
       (when-let [its (get-in spec [:rest :read-itemtypes])]
         (kv (i18n/t :ui/item-types) (str/join ", " (map name its))))]])
   [detail-card (i18n/t :ui/webhooks)
    (if (seq (:webhooks spec))
      [props
       (for [[wid w] (:webhooks spec)]
         (kv (name wid)
             [:span {:class "u-mono-12"}
              (str (some-> (:method w) name str/upper-case) " " (:path-template w))]))]
      [ui/empty-state {:title (i18n/t :ui/webhooks-none)}])]])

(defn- source-detail
  []
  (let [src-key (:selected-source @ui)
        spec    (get @data/sources src-key)]
    (when spec
      (let [tab (:source-tab @ui :overview)
            webhooks? (or (seq (:webhooks spec)) (:rest spec))]
        [:div {:class "dataconn-detail"}
         [page/header {:title    (name src-key)
                       :subtitle (some-> (:type spec) name)
                       :icon     [ui/icon {:icon (source-icon (:type spec))}]}
          [access-pill (:access spec)]
          [ui/button {:minimal true :icon :cross :aria-label (i18n/t :ui/back-to-sources)
                      :on-click #(send! [:sources/back])}]]
         [ui/tabs {:id :dataconn-source :selected-tab-id tab
                   :on-change #(send! [:sources.source/tab %])}
          [ui/tab {:id :overview :title (i18n/t :ui/overview)}]
          [ui/tab {:id :link :title (i18n/t :ui/dc-connection)}]
          [ui/tab {:id :explore :title (i18n/t :ui/explore)}]
          [ui/tab {:id :syncs :title (i18n/t :ui/syncs)}]
          (when webhooks?
            [ui/tab {:id :webhooks :title (i18n/t :ui/webhooks)}])]
         (case tab
           :link     [source-link src-key spec]
           :explore  [source-explore src-key spec]
           :syncs    [source-syncs src-key]
           :webhooks [source-webhooks spec]
           [source-overview src-key spec])]))))

(defn- fmt-duration [ms]
  (let [s (quot (or ms 0) 1000) m (quot s 60) h (quot m 60) d (quot h 24)]
    (cond (pos? d) (str d " d " (mod h 24) " h")
          (pos? h) (str h " h " (mod m 60) " min")
          (pos? m) (str m " min")
          :else    (str s " s"))))

(defn- agent-card [{:keys [agent address reachable status]} routed]
  (let [mine  (filter (fn [[_ s]] (= agent (agent-of s))) routed)
        allow (sort (distinct (mapcat (comp :egress second) mine)))]
    [detail-card nil
     [:div
      [:div {:class "dataconn-agent__head"}
       [ui/icon {:icon :shield :size 20 :class "u-muted"}]
       [:div {:class "u-flex-1"}
        [:div {:class "dataconn-agent__name"} agent]
        [:div {:class "dataconn-agent__addr"} address]]
       (if reachable
         [pill (i18n/t :ui/agent-reachable) :success]
         [pill (i18n/t :ui/agent-unreachable) :danger])]
      (when status
        [props
         (kv (i18n/t :ui/agent-fingerprint)
          [:span {:class "dataconn-agent__addr"} (:fingerprint status)])
         (kv (i18n/t :ui/agent-uptime) (fmt-duration (:uptime-ms status)))
         (kv (i18n/t :ui/agent-heap) (str (get-in status [:heap :used-mb]) " / "
                                              (get-in status [:heap :max-mb]) " MB"))
         (kv (i18n/t :ui/agent-trace)
          (i18n/t (if (get-in status [:trace :armed]) :ui/agent-trace-armed :ui/agent-trace-off)))
         (kv (i18n/t :ui/agent-sessions)
          (if (empty? (:sessions status))
            (i18n/t :ui/agent-no-sessions)
            [:div (for [{:keys [tx source age-ms]} (:sessions status)]
                    ^{:key tx} [:div (str source " · " (fmt-duration age-ms))])]))])
      (when (and reachable (nil? status))
        [:div {:class [style/small-muted "u-mb-8"]} (i18n/t :ui/agent-status-unknown)])
      (when-not reachable
        [:div {:class "dataconn-agent__start-hint"}
         (i18n/t :ui/egress-agent-start
                 {:command "clojure -M:dev -m sutra.sources.egress"})])
      [props
       (kv (i18n/t :ui/egress-allowlist)
        (if (seq allow)
          [:div (for [e allow] ^{:key e} [:div {:class "dataconn-agent__addr"} e])]
          [:span {:class "dataconn-agent__empty"} (i18n/t :ui/agent-allow-none)]))]
      [:div {:class "u-section-label"}
       (str (i18n/t :ui/routed-sources) " (" (count mine) ")")]
      (if (empty? mine)
        [:div {:class "dataconn-agent__empty"} (i18n/t :ui/routed-sources-none)]
        [:div
         (for [[k s] (sort-by (comp name first) mine)]
           ^{:key k}
           [:div {:on-click #(send! [:sources/show k :sources])
                  :class "dataconn-agent__routed-row"}
            [:span {:class "u-bold"} (name k)]
            [:span {:class "dataconn-agent__routed-meta"}
             (or (some-> (:secret-ref s) name) (some-> (:type s) name))]])])]]))

(def agents-pane
  (machine/entering
   model/sources [:sources.agents/enter] [:sources.agents/leave]
   (fn []
     (let [routed (filter (fn [[_ s]] (egress-routed-source? s)) (seq @data/sources))
           agents (:agents @ui)]
       [:div {:class "dataconn-pane"}
        [:div {:class [style/small-muted "u-mb-8"]} (i18n/t :ui/egress-agent-note)]
        (cond
          (nil? agents)           [pill (i18n/t :ui/agent-checking) nil]
          (= :unreachable agents) [pill (i18n/t :ui/agent-unreachable) :danger]
          :else (for [a agents] ^{:key (:agent a)} [agent-card a routed]))
        [ui/button {:small true :class "u-mt-8" :on-click #(send! [:sources.agents/check])}
         (i18n/t :ui/recheck)]]))))

(defn- list-view []
  (let [tab (:top-tab @ui :sources)]
    [:<>
     [page/header {:title (i18n/t :ui/app-source-manager)}
      [ui/tabs {:id :dataconn-top :selected-tab-id tab
                :on-change #(send! [:sources/tab %])}
       (for [[id label] [[:sources :ui/sources] [:streams :ui/streams] [:agents :ui/agents]]]
         [ui/tab {:id id :title (i18n/t label) :class (str "dataconn-tab--" (name id))}])]]
     (case tab
       :agents  [agents-pane]
       :streams [streams/streams-pane]
       [sources-pane])
     [sync-editor-modal]]))

(defn- detail-view []
  (case (:top-tab @ui :sources)
    :streams [streams/detail-pane]
    :sources [source-detail]
    nil))

(def ^:private detail
  (reify
    IDeref
    (-deref [_]
      (case (:top-tab @ui :sources)
        :streams (some-> (streams/selected) vector)
        :sources (let [k (:selected-source @ui)] (when (get @data/sources k) [k]))
        nil))
    IReset
    (-reset! [_ _]
      (send! (if (= :streams (:top-tab @ui)) [:sources.stream/deselect] [:sources/back]))
      nil)))

(page/view! :sources/list list-view)
(page/view! :sources/detail detail-view)

(def ^:private tree
  (page/liste-detail {:list :sources/list :detail :sources/detail :open detail
                      :detail-w 620 :resize :sources/detail}))

(defn source-manager-panel [] [platform/module :source-manager tree])

(defn open-manage-datasources! [] (send! [:sources.manage/open]))
