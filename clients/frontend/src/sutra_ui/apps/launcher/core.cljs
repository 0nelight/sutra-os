(ns sutra-ui.apps.launcher.core
  (:require [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra-ui.apphost.platform :as platform]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui :refer [link]]
            [sutra-ui.shell.unavailable :refer [unavailable]]
            [sutra.i18n.core :as i18n]
            [sutra.capps.portal]
            [sutra.kit.decisions :as decisions]
            [sutra-ui.apps.launcher.events]
            [sutra-ui.feature :as feature]
            [sutra-ui.shell.consumer :as consumer]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.shell :as shell]))

(defonce ^:private ui (shell/slice :applications))

(defonce ^:private launcher (machine/frame ui {:name :launcher}))

(defn send! [event] (machine/dispatch event {:frame launcher}))

(defn app-access
  [app-rid]
  (let [sc   @catalog/canonical-schema
        app  (some (fn [[_ s]] (when (= (:id s) app-rid) s)) (:applications sc))
        proj (:project app)]
    {:project proj
     :grants  (vec (get-in sc [:projects proj :grants]))}))

(defn apps-of-project
  [proj-name]
  (vec (for [[_ s] (:applications @catalog/canonical-schema)
             :when (= (:project s) proj-name)]
         {:id     (:id s)
          :name   (:application-name s)
          :icon   (:icon s)})))

(defn shelves-of
  [app shelves]
  (let [by-id (into {} (map (juxt :id :name)) shelves)]
    (mapv #(or (by-id %) "—") (:workspaces app))))

(defn- app-card
  [a]
  (let [nm (some-> (:name a) name)]
    (some #(when (or (and (:id a) (= (str (:id a)) (str (:id %))))
                     (= nm (some-> (:name %) name)))
             %)
          (pstate/consumer-app-list))))

(defn- at [k] @(machine/subscribe [:launcher/at k] {:frame launcher}))

(defn- grant-pill [principal role]
  [ui/tag {:minimal true :round true}
   (principals/principal-label principal)
   (when role [:span.u-muted " " (name (keyword role))])])

(defn- grant-pills [grants]
  [:div.u-flex-wrap-gap-6.u-mt-8
   (for [g grants]
     ^{:key (str (:principal g) (:role g))}
     [grant-pill (:principal g) (:role g)])])

(defn- shelf-line
  [app]
  (when-let [all (seq (pstate/workspace-list))]
    (let [shelves (shelves-of app all)]
      [:div
       [:span.u-muted (i18n/t :ui/workspaces) ": "]
       (if (seq shelves)
         [:strong (str/join ", " shelves)]
         [:span {:class "launcher__unplaced"} (i18n/t :ui/apps-unassigned)])])))

(defn- project-link [pk]
  (if pk
    [link {:href "#" :class "u-bold"
           :on-click (fn [e] (.preventDefault e) (feature/command! :open-project pk))}
     (name pk)]
    [:strong "—"]))

(defn- app-access-footer
  [app]
  (let [{:keys [project grants]} (app-access (:id app))]
    [:div.u-mt-border-top.u-fs-12 {:on-click #(.stopPropagation %)}
     [:div [:span.u-muted (i18n/t :ui/project-label)] [project-link (some-> project keyword)]]
     [shelf-line app]
     (if (seq grants)
       [grant-pills grants]
       [:div.u-muted.u-mt-8 (i18n/t :ui/grants-none)])]))

(defn- project-card [proj]
  (let [pk   (some-> (:name proj) keyword)
        apps (apps-of-project pk)]
    [ui/card {:class "u-mb-8"}
     [:div.u-flex-between
      [project-link pk]
      [:span.sutra-muted-sm
       (i18n/t :ui/apps-grants-count {:apps (count apps) :grants (count (:grants proj))})]]
     (when (seq apps)
       [:div.sutra-muted-sm.u-mt-8
        (i18n/t :ui/apps-label)
        (interpose ", "
                   (for [a apps
                         :let [card (app-card a)]]
                     ^{:key (str (:name a))}
                     (if card
                       [link {:href "#"
                              :on-click (fn [e] (.preventDefault e)
                                          (pstate/open-consumer-app! card))}
                        (pstate/app-label card)]
                       [:span (pstate/app-label a)])))])
     (if (seq (:grants proj))
       [grant-pills (:grants proj)]
       [:div.sutra-muted-sm.u-mt-8 (i18n/t :ui/grants-none-default)])]))

(def ^:private people-cols
  [{:label (i18n/t :ui/user)
    :render (fn [p] [:<> (:username p)
                     (when (:admin p) [:span.u-muted " " (i18n/t :ui/admin-suffix)])])}
   {:label (i18n/t :ui/role) :render :role}
   {:label (i18n/t :ui/auth) :render :auth-source :cell-class "u-muted"}
   {:label (i18n/t :ui/clearances) :render #(str/join ", " (:clearances %))
    :cell-class "u-muted"}])

(defn- access-matrix
  []
  (if-let [snap (at :access)]
    [:<>
     [ui/h4 (i18n/t :ui/projects-apps-access)]
     (for [proj (sort-by :name (:projects snap))]
       ^{:key (:id proj)} [project-card proj])
     [ui/h4 {:class "u-mt-12"} (i18n/t :ui/people)]
     [table/data-table {:cols    people-cols
                        :rows    (sort-by :username (:principals snap))
                        :row-key :user-id}]]
    [busy/block (i18n/t :ui/loading)]))

(defn- app-grid []
  (let [placed (or (at :placement) :all)
        {:keys [show reason apps shelves loose]}
        (decisions/decide :view/portal @pstate/consumer-apps @pstate/workspaces)
        shown (if (= :unplaced placed) loose (vec apps))]
    (cond
      (= :loading show) [busy/block (i18n/t :ui/loading)]
      (= :failed show)  [unavailable (i18n/t :ui/apps-failed {:reason reason})]
      (= :no-apps show) [ui/empty-state {:icon :application :title (i18n/t :ui/no-apps-published)}]
      :else
      [:div
       [consumer/workspace-strip]
       (when (seq shelves)
         [:div {:class "launcher__filter"}
          (for [[k lbl n] [[:all (i18n/t :ui/apps-all) (count apps)]
                           [:unplaced (i18n/t :ui/apps-unassigned) (count loose)]]]
            ^{:key k}
            [ui/button {:small true :active (= k placed)
                        :on-click #(send! [:launcher/placement k])}
             (str lbl " (" n ")")])])
       (if (seq shown)
         [:div {:class "consumer__grid"}
          (for [app shown]
            ^{:key (:id app)}
            [ui/card {:class       "consumer__card"
                      :interactive true
                      :on-click    #(pstate/open-consumer-app! app)}
             [:div {:class "consumer__card-icon"} (or (:icon app) [ui/icon {:icon :application}])]
             [:div {:class "consumer__card-name"} (pstate/app-label app)]
             (when-let [d (:description app)]
               [:div {:class "consumer__card-desc"} d])
             [app-access-footer app]])]
         [ui/empty-state {:icon :application
                          :title (i18n/t (if (seq shelves)
                                           :ui/apps-all-assigned
                                           :ui/no-apps-published))}])])))

(defn- admin? [] (= "admin" (:role @auth/auth)))

(defn- head []
  (let [tab (or (at :tab) :apps)]
    [page/header {:title (i18n/t :ui/applications)}
     (when (admin?)
       [ui/button-group
        (for [[k lbl] [[:apps (i18n/t :ui/apps)] [:access (i18n/t :ui/access)]]]
          ^{:key k}
          [ui/button {:small true :active (= tab k) :on-click #(send! [:launcher/tab k])}
           lbl])])]))

(defn- body []
  (if (and (admin?) (= :access (at :tab)))
    [access-matrix]
    [app-grid]))

(page/view! :launcher/head head)
(page/view! :launcher/body body)

(def ^:private tree
  (page/dokument {:header :launcher/head
                  :body   {:kind :rows :padding :compact :items [(page/view :launcher/body)]}}))

(defn applications-launcher
  []
  (if-let [open @pstate/current-consumer-app]
    [consumer/consumer-app-view open (str "← " (i18n/t :ui/applications))]
    [platform/module :applications tree]))
