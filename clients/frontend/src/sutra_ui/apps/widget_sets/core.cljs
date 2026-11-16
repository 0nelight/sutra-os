(ns sutra-ui.apps.widget-sets.core
  (:require [cljs.reader :as reader]
            [sutra-ui.ui.core :as ui :refer [tag file-input]]
            [sutra-ui.ui.busy :as busy]
            [sutra-ui.ui.page :as page]
            [sutra-ui.apphost.platform :as platform]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apps.widget-sets.events]
            [sutra-ui.apphost.widgets.custom :as custom-widget]
            [sutra-ui.resource.core :as res]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.ui.style :as style]
            [sutra.kit.remote :as rd]))

(defonce ^:private ui (shell/slice :widget-sets))

(defonce ^:private widget-sets-frame (machine/frame ui {:name :widget-sets}))

(defn- send! [event] (machine/dispatch event {:frame widget-sets-frame}))

(defn select! [nm] (send! [:widget-sets/select nm]))

(defn- at [k] @(machine/subscribe [:widget-sets/at k] {:frame widget-sets-frame}))

(defn- new-form []
  (let [draft (at :draft)]
    [:div.u-flex-col-gap-6
     [ui/input-group {:placeholder (i18n/t :ui/widget-sets-name) :value (:name draft)
                      :on-change #(send! [:widget-sets/draft :name
                                          (.. % -target -value)])}]
     [res/project-select (:project draft) #(send! [:widget-sets/draft :project %]) false]
     [ui/input-group
      {:placeholder (i18n/t :ui/widget-sets-description) :value (:description draft)
       :on-change #(send! [:widget-sets/draft :description (.. % -target -value)])}]
     [ui/button {:small true :on-click #(send! [:widget-sets/create])}
      (i18n/t :ui/widget-sets-create)]]))

(defn- rail []
  [:<>
   [ui/button {:icon :plus :minimal true :intent :primary
               :on-click #(send! [:widget-sets/toggle-new])}
    (i18n/t :ui/widget-sets-create)]
   (when (at :adding?) [new-form])
   (let [{:keys [show rows reason]} (rd/list-view @(ks/lists :widget-sets))]
     (case show
       :loading [busy/block (i18n/t :ui/widget-sets-loading)]
       :failed  [ui/problem (i18n/t :ui/widget-sets-failed {:reason reason}) nil]
       :empty   nil
       [ui/menu
        (for [{:keys [name latest]} (sort-by :name rows)]
          ^{:key name}
          [ui/menu-item {:text          name
                         :active        (= name (at :selected))
                         :label-element [:span {:class "devcon-mono"} (or latest "—")]
                         :on-click      #(select! name)}])]))])

(defn- type-label
  [t]
  (if (sequential? t) (str/join " " (map clojure.core/name t)) (some-> t clojure.core/name)))

(defn- widget-card
  [id {:keys [name description parameters events]}]
  [ui/callout {:class "u-mb-8"}
   [:strong (or name (clojure.core/name id))]
   " " [:span {:class "devcon-mono"} (clojure.core/name id)]
   (when description [:div {:class style/small-muted} description])
   (when (seq parameters)
     [:div
      [:div {:class style/small-muted} (i18n/t :ui/widget-sets-parameters)]
      (for [[k {:keys [type]}] parameters]
        ^{:key k} [:div {:class "devcon-mono"}
                   (str (clojure.core/name k) " : " (type-label type))])])
   (when (seq events)
     [:div
      [:div {:class style/small-muted} (i18n/t :ui/widget-sets-events)]
      (for [[k {:keys [updates]}] events]
        ^{:key k} [:div {:class "devcon-mono"}
                   (str (clojure.core/name k)
                        (when (seq updates)
                          (str " → " (str/join ", " (map clojure.core/name updates)))))])])])

(defn- upload-form [nm]
  [:div.u-flex-items-gap-8
   [ui/input-group {:placeholder "1.0.0" :value (or (at :upload-version) "")
                    :class "devcon-input--s"
                    :on-change #(send! [:widget-sets/set :upload-version
                                        (.. % -target -value)])}]
   [file-input
    {:text        ".zip"
     :input-props {:accept ".zip"}
     :on-input-change
     (fn [e]
       (when-let [file (aget (.. e -target -files) 0)]
         (send! [:widget-sets.release/upload nm file])))}]])

(defn- versions-pane
  [{:keys [name releases]}]
  [:div.u-mt-12
   [:div {:class style/small-muted} (i18n/t :ui/widget-sets-upload)]
   [upload-form name]
   [:div {:class [style/small-muted "u-mt-12"]} (i18n/t :ui/widget-sets-versions)]
   (for [{:keys [version author created-at notes]} releases]
     ^{:key version}
     [:div.u-flex-wrap-gap-6.u-mt-8
      [:strong {:class "devcon-mono"} version]
      [:span {:class style/small-muted} (str author " · " created-at)]
      (when notes [:span notes])])])

(defn- value-input
  [{:keys [type]} v on-change]
  (case type
    :boolean [ui/checkbox {:checked (boolean v)
                           :on-change #(on-change (.. % -target -checked))}]
    :number  [ui/numeric-input
              {:value (str v) :on-value-change (fn [_ s _] (on-change (js/parseFloat s)))}]
    (:string :date :timestamp)
    [ui/input-group {:value (str v) :on-change #(on-change (.. % -target -value))}]
    [ui/input-group {:input-class-name "devcon-mono" :value (if (some? v) (pr-str v) "")
                     :placeholder "EDN"
                     :on-change #(on-change (try (reader/read-string (.. % -target -value))
                                                 (catch :default _ v)))}]))

(defn- playground-rpc
  [_op _spec]
  (js/Promise.reject (i18n/t :ui/widget-sets-no-app)))

(defn- playground-view [_ _]
  (let [{:keys [bundle widget values log]} (at :playground)]
    (when bundle
      (let [id     widget
            spec   (get-in bundle [:manifest :widgets id])]
        [:div
         [ui/html-select
          {:value (some-> id name)
           :on-change #(send! [:widget-sets.playground/widget
                               (keyword (.. % -target -value))])}
          (for [k (keys (get-in bundle [:manifest :widgets]))]
            ^{:key k} [:option {:value (name k)} (name k)])]
         (when (:dev? bundle) [tag {:minimal true :intent :warning} "dev"])
         (for [[k pspec] (:parameters spec)]
           ^{:key k}
           [:div {:class "devcon-row"}
            [:label {:class "devcon-row__key devcon-mono"} (name k)]
            [value-input pspec (get values k)
             #(send! [:widget-sets.playground/value k %])]])
         ^{:key [(:stamp bundle) id]}
         [:div {:class "devcon-frame"}
          [custom-widget/frame
           {:bundle bundle :widget-id id :values (or values {})
            :rpc    playground-rpc
            :on-emit (fn [event updates]
                       (send! [:widget-sets.playground/emitted event updates]))}]]
         [:div {:class style/small-muted} (i18n/t :ui/widget-sets-log)]
         (for [[i [event updates]] (map-indexed vector (take-last 20 log))]
           ^{:key i} [:div {:class "devcon-mono"} (pr-str event updates)])]))))

(def ^:private playground
  (machine/entering widget-sets-frame
                    (fn [nm version] [:widget-sets.playground/load nm version])
                    playground-view))

(defn- head []
  (if-let [{:keys [name description project latest]} (at :detail)]
    [page/header
     {:title    name
      :subtitle [:span.u-flex-wrap-gap-6
                 (when latest [:span {:class "devcon-mono"} latest])
                 (when project [:span (some-> (catalog/project-of project) clojure.core/name)])
                 (when description [:span description])]}
     [ui/button {:on-click #(send! [:widget-sets/delete name])}
      (i18n/t :ui/widget-sets-delete)]]
    [page/header {:title (i18n/t :ui/app-widget-sets)}]))

(defn- main-pane []
  (when-let [{:keys [name latest versions releases] :as d} (at :detail)]
    [:<>
     (if (empty? releases)
       [:div {:class style/small-muted} (i18n/t :ui/widget-sets-first-release {:name name})]
       (let [chosen  (or (some #{(at :version)} versions) latest)
             widgets (:widgets (or (some #(when (= chosen (:version %)) %) releases)
                                   (first releases)))]
         [:<>
          [ui/html-select
           {:value chosen :on-change #(send! [:widget-sets/set :version (.. % -target -value)])}
           (for [v versions] ^{:key v} [:option {:value v} v])]
          (for [[id w] widgets] ^{:key id} [widget-card id w])
          [ui/h4 (i18n/t :ui/widget-sets-playground)]
          ^{:key [name chosen]} [playground name chosen]]))
     [versions-pane d]]))

(defn- body []
  (let [problem (at :problem)]
    [:<>
     (when problem [ui/problem problem nil])
     (if (at :selected)
       [main-pane]
       [:div {:class style/small-muted} (i18n/t :ui/widget-sets-intro)])]))

(page/view! :widget-sets/list rail)
(page/view! :widget-sets/head head)
(page/view! :widget-sets/body body)

(def ^:private tree
  (page/werkzeug
   {:sidebar       :widget-sets/list
    :sidebar-label :ui/app-widget-sets
    :main          {:kind  :rows
                    :items [(page/view :widget-sets/head)
                            {:kind :flow :padding :compact :box {:grow? true}
                             :items [(page/view :widget-sets/body)]}]}}))

(def widget-sets
  (machine/entering
   widget-sets-frame [:widget-sets/enter]
   (fn [] [platform/module :widget-sets tree])))
