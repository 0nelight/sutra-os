(ns sutra-ui.apps.admin-settings.core
  (:require [clojure.string :as str]
            [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.admin-settings.egress-policies :as egress-policies]
            [sutra-ui.apps.admin-settings.feedback-inbox :as feedback-inbox]
            [sutra-ui.apps.admin-settings.home-page :as home-page]
            [sutra-ui.apps.admin-settings.llm-budget :as llm-budget]
            [sutra-ui.apps.admin-settings.model :as model]
            [sutra-ui.apps.admin-settings.markings :as markings]
            [sutra-ui.apps.admin-settings.organizations :as organizations]
            [sutra-ui.apps.admin-settings.peering :as peering]
            [sutra-ui.platform :as platform]
            [sutra-ui.ui.page :as page]
            [sutra-ui.apphost.platform :as module]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.ui.form :as form]
            [sutra-ui.ui.inputs :as inputs]
            [sutra-ui.ui.style :as style]))

(defn- admin?
  []
  (let [{:keys [username user]} @auth/auth]
    (or (= "admin" username) (contains? @principals/principal-admins user))))

(defn- pick-image!
  [e on-data]
  (when-let [file (aget (.. e -target -files) 0)]
    (let [rdr (js/FileReader.)]
      (set! (.-onload rdr) (fn [_] (on-data (.-result rdr))))
      (.readAsDataURL rdr file))))

(def ^:private size-label
  {:favicon :ui/platform-logo-favicon
   :small   :ui/platform-logo-small
   :medium  :ui/platform-logo-medium
   :large   :ui/platform-logo-large})

(defn- shown-logo
  [draft size]
  (let [d (get-in draft [:logo size])]
    (cond (nil? d)         (platform/logo size)
          (str/blank? d)   nil
          :else            d)))

(defn- logo-slot [{:keys [size src own? on-pick on-clear disabled?]}]
  (let [id (str "platform-logo-" (name size))]
    [:div {:class "admin-logo"}
     [:div {:class "u-bold u-mb-4"} (i18n/t (size-label size))]
     [:div {:class "admin-logo__stage"}
      (if src
        [:img {:class "admin-logo__img" :src src :alt ""}]
        [:span {:class style/small-muted} (i18n/t :ui/platform-logo-unset)])]
     [:div {:class [style/small-muted "u-mb-4"]}
      (when (and src (not own?)) (i18n/t :ui/platform-logo-inherited))]
     [:input {:id id :type "file" :accept "image/*" :hidden true
              :on-change (fn [e] (pick-image! e on-pick))}]
     [:div {:class "u-flex-gap-6 admin-logo__actions"}
      [ui/button {:disabled disabled? :on-click #(.click (.getElementById js/document id))}
       (i18n/t :ui/platform-logo-choose)]
      (when own?
        [ui/button {:outlined true :intent :danger :disabled disabled? :on-click on-clear}
         (i18n/t :ui/remove)])]]))

(defn- preview
  [title draft]
  (let [small (shown-logo draft :small)
        fav   (shown-logo draft :favicon)]
    [ui/callout {:class "u-mb-12"}
     [:div {:class style/section-h3} (i18n/t :ui/cp-preview)]
     [:div {:class "u-flex-row-center-6 u-mb-8"}
      (when fav [:img {:class "admin-favicon" :src fav :alt ""}])
      [:span title]]
     [:div {:class "shell-bar admin-preview-bar"}
      (if small
        [:img {:class "shell-bar__logo-image" :src small :alt title}]
        [:div {:class "shell-bar__logo-mark admin-preview-mark"}
         (str/upper-case (subs (str title " ") 0 1))])
      [:span {:class "shell-bar__logo-text"} title]]]))

(defn- platform-experience-pane* []
  (let [{d :draft :keys [busy error saved]} (:platform @model/db)
        {:keys [title uploaded]} @platform/state
        uploaded? (set uploaded)
        admin     (admin?)
        off?      (or busy (not admin))
        text      (or (:title d) title "")
        dirty?    (seq d)]
    [:div
     [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/platform-experience-hint)]
     (when-not admin
       [ui/problem (i18n/t :ui/platform-admin-only) nil "u-mb-12"])

     [ui/callout {:class "u-mb-12"}
      [form/field {:label (i18n/t :ui/platform-title)
                   :help  (i18n/t :ui/platform-title-hint {:default i18n/default-platform})}
       [inputs/text-input (model/draft-at [:admin.platform/title] text)
        {:placeholder i18n/default-platform :disabled off? :class "admin-field"}]]]

     [ui/callout {:class "u-mb-12"}
      [:div {:class style/section-h3} (i18n/t :ui/platform-logo)]
      [:div {:class "u-flex-wrap-gap-6"}
       (for [size platform/sizes]
         (let [drafted (get-in d [:logo size])]
           ^{:key size}
           [logo-slot {:size size
                       :src (shown-logo d size)
                       :own? (if (some? drafted) (not (str/blank? drafted))
                                 (contains? uploaded? size))
                       :disabled? off?
                       :on-pick #(model/send! [:admin.platform/logo size %])
                       :on-clear #(model/send! [:admin.platform/logo size ""])}]))]
      [:div {:class [style/small-muted "u-mt-8"]} (i18n/t :ui/platform-logo-hint)]]

     [preview (let [t (str/trim (str text))] (if (str/blank? t) i18n/default-platform t)) d]

     [:div {:class "u-flex-items-gap-8"}
      [ui/button {:intent :primary :disabled (or off? (not dirty?))
                  :on-click #(model/send! [:admin.platform/save])}
       (i18n/t (if busy :ui/cp-saving :ui/save))]
      [ui/button {:disabled (or busy (not dirty?))
                  :on-click #(model/send! [:admin.platform/discard])}
       (i18n/t :ui/cp-discard)]
      (when dirty? [:span {:class style/small-muted} (i18n/t :ui/cp-unsaved)])
      (when (and saved (not dirty?) (not error))
        [:span {:class "u-text-added"} (i18n/t :ui/saved)])]
     (when error [ui/problem error nil "u-mt-8"])
     (when admin [:div {:class "u-mt-12"} [home-page/section {:disabled? (not admin)}]])]))

(def ^:private platform-experience-pane
  (machine/entering model/admin [:admin.platform/enter] platform-experience-pane*))

(defn- tabs
  []
  (concat [[:platform :ui/platform-experience]]
          (for [{:keys [id label-key]} (feature/contributions :admin-section)] [id label-key])
          [[:budget :ui/admin-tab-llm-budget]
           [:egress :ui/admin-tab-egress]
           [:organizations :ui/admin-tab-organizations]
           [:markings :ui/admin-tab-markings]
           [:peering :ui/admin-tab-peering]
           [:feedback :ui/admin-tab-feedback]]))

(defn- alle-sitzungen! [] (model/send! [:admin/all-sessions]))

(defn- current-tab []
  (let [{eine :sitzung tab :tab} @model/db]
    (if eine :feedback (or tab :platform))))

(defn- nav []
  (let [t (current-tab)]
    [ui/menu
     (for [[k label] (tabs)]
       ^{:key k}
       [ui/menu-item {:text     (i18n/t label)
                      :active   (= t k)
                      :on-click #(model/send! [:admin/tab k])}])]))

(defn- section []
  (let [t (current-tab)]
    (if-let [c (some #(when (= t (:id %)) %) (feature/contributions :admin-section))]
      [(:view c)]
      (case t
        :budget        [llm-budget/view]
        :egress        [egress-policies/view]
        :organizations [organizations/view]
        :markings      [markings/view]
        :peering       [peering/view]
        :feedback      [feedback-inbox/view (:sitzung @model/db) alle-sitzungen!]
        [platform-experience-pane]))))

(page/view! :admin/nav nav)
(page/view! :admin/section section)

(def ^:private tree (page/einstellungen {:nav :admin/nav :sections [:admin/section]}))

(defn admin-settings [] [module/module :admin-settings tree])
