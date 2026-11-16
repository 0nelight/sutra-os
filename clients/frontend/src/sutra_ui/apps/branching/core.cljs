(ns sutra-ui.apps.branching.core
  (:require [sutra-ui.ui.core :as ui :refer [tag]]
            [sutra-ui.ui.filter :as ui-filter]
            [sutra-ui.ui.page :as page]
            [sutra-ui.ui.table :as table]
            [sutra-ui.apphost.platform :as platform]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.branching.events]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.style :as style]))

(defonce ^:private ui (shell/slice :branching))

(defonce ^:private branching (machine/frame ui {:name :branching}))

(defn- send! [event] (machine/dispatch event {:frame branching}))

(defn reload! [] (send! [:branching/reload]))

(defn- at [k] @(machine/subscribe [:branching/at k] {:frame branching}))

(defn- me [] (str (:user @auth/auth)))

(defn- matches? [text]
  (let [q (str/lower-case (str/trim (str (at :q))))]
    (or (empty? q) (str/includes? (str/lower-case (str text)) q))))

(defn- status-intent [status]
  (case (str status)
    ("passed" "merged") :success
    ("failed" "rejected") :danger
    "running" :warning
    "open" :primary
    nil))

(defn- status-pill
  ([status] (status-pill status (i18n/t (keyword "ui" (str "branch-status-" status)))))
  ([status label]
   [tag {:minimal true :intent (status-intent status)
         :class (str "prop-pill--" status)}
    label]))

(def ^:private branch-cols
  [{:label (i18n/t :ui/branch-col-name) :render :name}
   {:label (i18n/t :ui/branch-col-status) :render #(vector status-pill (:status %))}
   {:label (i18n/t :ui/branch-col-who)
    :render #(some-> (:last-committer %) principals/principal-label)}
   {:label (i18n/t :ui/branch-col-when) :render #(str (:last-commit-at %))}])

(defn- branches-tab []
  (let [mine? (at :mine?)
        bs    (filter #(and (matches? (:name %))
                            (or (not mine?) (= (me) (str (:last-committer %)))))
                      (at :branches))]
    [:<>
     [ui/checkbox {:checked   (boolean mine?)
                   :on-change #(send! [:branching/set :mine? (.. % -target -checked)])
                   :label     (i18n/t :ui/branch-only-mine)}]
     [table/data-table {:scroll?      true
                        :cols         branch-cols
                        :rows         bs
                        :row-key      :name
                        :on-row-click #(send! [:branching/open (:name %)])}]]))

(def ^:private proposal-cols
  [{:label (i18n/t :ui/branch-col-title) :render :title}
   {:label (i18n/t :ui/branch-col-source) :render :source-branch}
   {:label (i18n/t :ui/branch-col-status)
    :render #(vector status-pill (:status %) (:status %))}
   {:label (i18n/t :ui/branch-col-who) :render #(principals/principal-label (:author %))}])

(defn- proposals-tab []
  [table/data-table {:scroll?      true
                     :cols         proposal-cols
                     :rows         (filter #(matches? (:title %)) (gov/proposal-list))
                     :row-key      :id
                     :on-row-click #(feature/command! :open-proposal (:id %))}])

(defn- roles-part [nm]
  (let [draft (or (at :owner-draft) "")]
    [:div.u-mt-12
     [:div.u-section-label (i18n/t :ui/branch-owners)]
     (for [{:keys [principal]} (:roles (at :roles))]
       ^{:key principal}
       [:div.u-flex-items-gap-8
        [:span (principals/principal-label principal)]
        [ui/button {:icon :cross :small true :minimal true
                    :aria-label (i18n/t :ui/remove)
                    :on-click #(send! [:branching.role/set nm
                                       {:principal principal :revoke? true}])}]])
     [:div.u-flex-items-gap-8.u-mt-2
      [ui/input-group {:value draft :class "u-flex-1"
                       :placeholder (i18n/t :ui/branch-owner-add)
                       :on-change #(send! [:branching/set :owner-draft
                                           (.. % -target -value)])
                       :small true}]
      [ui/button
       {:small true :disabled (str/blank? draft) :on-click #(send! [:branching.role/add nm])}
       (i18n/t :ui/add)]]]))

(defn- open-row [nm] (some #(when (= nm (:name %)) %) (at :branches)))

(defn- branch-head []
  (when-let [nm (at :open)]
    [page/header {:title nm}
     (when-let [row (open-row nm)] [status-pill (:status row)])
     [ui/button {:icon :cross :minimal true :aria-label (i18n/t :ui/close)
                 :on-click #(send! [:branching/back])}]]))

(defn- branch-page []
  (when-let [nm (at :open)]
    (let [row (open-row nm)
          {:keys [tasks target proposal]} (at :changes)]
      [:<>
       [page/toolbar
        [ui/button {:small true :on-click #(sync/checkout-branch! nm)}
         (i18n/t :ui/branch-switch-to)]
        (when (= "inactive" (:status row))
          [ui/button {:small true :on-click #(send! [:branching/reactivate nm])}
           (i18n/t :ui/branch-reactivate)])
        (when-let [id (:id proposal)]
          [ui/button {:small true :on-click #(feature/command! :open-proposal id)}
           (i18n/t :ui/branch-proposal-view)])
        [ui/button {:small true :on-click #(send! [:branching/archive nm])}
         (i18n/t :ui/branch-close)]]
       (when row
         [:div {:class style/small-muted}
          (i18n/t :ui/branch-retention-note {:inactive (:inactive-days row)
                                             :closed   (:retention-days row)})])
       [:div.u-section-label.u-mt-12
        (i18n/t :ui/branch-changes-against {:target (or target "main") :n (count tasks)})]
       (for [{:keys [task name status protected]} tasks]
         ^{:key task}
         [:div.u-flex-items-gap-8
          [:span (str status " · " name)]
          (when protected [ui/icon {:icon :lock :size 12}])
          [ui/tooltip {:content (i18n/t :ui/branch-remove-resource)}
           [ui/button {:icon :cross :small true :minimal true
                       :on-click #(send! [:branching/remove-task nm task])}]]])
       [roles-part nm]])))

(defn- policy-field [k label]
  [:label.u-flex-items-gap-8
   (i18n/t label)
   [ui/numeric-input
    {:min 1 :value (or (get-in (at :policy) [k]) "") :small true
     :on-value-change (fn [_ s _]
                        (let [v (js/parseInt s 10)]
                          (when-not (js/isNaN v) (send! [:branching.policy/field k v]))))}]])

(def ^:private retention-tab
  (machine/entering
   branching [:branching.policy/enter]
   (fn []
     [:div
      [:div {:class style/small-muted} (i18n/t :ui/branch-retention-hint)]
      [ui/html-select {:value (or (at :space) "*")
                       :on-change #(send! [:branching.policy/space
                                           (.. % -target -value)])}
       [:option {:value "*"} (i18n/t :ui/branch-retention-all)]
       (for [s (keys (:spaces @sync/snapshot))]
         ^{:key (str s)} [:option {:value (str s)} (str s)])]
      [policy-field :inactive-days :ui/branch-retention-inactive]
      [policy-field :retention-days :ui/branch-retention-closed]
      [ui/button {:small true :on-click #(send! [:branching.policy/save])}
       (i18n/t :ui/save)]])))

(def ^:private tabs
  [[:branches :ui/branch-tab-branches]
   [:proposals :ui/branch-tab-proposals]
   [:retention :ui/branch-tab-retention]])

(defn- list-head []
  [page/header {:title (i18n/t :ui/app-branching)}
   [ui/tabs {:id :branching :selected-tab-id (or (at :tab) :branches)
             :on-change #(send! [:branching/set :tab %])}
    (for [[id k] tabs]
      [ui/tab {:id id :title (i18n/t k)}])]])

(defn- list-view []
  (let [tab (or (at :tab) :branches)]
    [:<>
     (when-not (= :retention tab)
       [ui-filter/bar {:value       (or (at :q) "")
                       :on-search   #(send! [:branching/set :q %])
                       :placeholder (i18n/t :ui/branch-search)}
        [ui/html-select {:value     (or (at :status) "open")
                         :on-change #(send! [:branching/status (.. % -target -value)])}
         [:option {:value "open"} (i18n/t :ui/branch-filter-open)]
         [:option {:value "all"} (i18n/t :ui/branch-filter-all)]]])
     (case tab
       :branches  [branches-tab]
       :proposals [proposals-tab]
       :retention [retention-tab])]))

(def ^:private open-branch
  (reify
    IDeref
    (-deref [_] (at :open))
    IReset
    (-reset! [_ _] (send! [:branching/back]) nil)))

(page/view! :branching/list-head list-head)
(page/view! :branching/list list-view)
(page/view! :branching/head branch-head)
(page/view! :branching/detail branch-page)

(defn- pane [head body]
  {:kind  :rows
   :items [(page/view head)
           {:kind :flow :padding :compact :box {:grow? true} :items [(page/view body)]}]})

(def ^:private tree
  (page/liste-detail {:list     (pane :branching/list-head :branching/list)
                      :detail   (pane :branching/head :branching/detail)
                      :open     open-branch
                      :detail-w 480
                      :resize   :branching/detail}))

(defn root [] [platform/module :branching tree])
