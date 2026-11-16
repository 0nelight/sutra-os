(ns sutra-ui.shell.builder
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui]
            [reagent.core :as r]
            [sutra-ui.shell.desktop-nav :as desktop-nav]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.graph-editor :as graph]
            [sutra-ui.forms.modals :as form-modals]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.builder.bottom-panel :as bp]
            [sutra-ui.apps.all]
            [sutra-ui.feature :as feature]
            [sutra-ui.platform :as platform]
            [sutra-ui.apphost.platform :as cplatform]
            [sutra-ui.ui.page :as page]
            [sutra-ui.shell.carbon :as carbon]
            [sutra-ui.forms.edit-modals :as edit-modals]
            [sutra.i18n.core :as i18n]
            [sutra-ui.builder.canvas-menu :refer [view-picker]]
            [sutra-ui.builder.model :as model]
            [sutra-ui.builder.staging :refer [staging-modal]]
            [sutra-ui.builder.branches :refer [branch-bar branch-taskbar]]
            [sutra-ui.builder.command-palette :refer [command-palette go-app!]]))

(defn- canvas-top-bar
  [{:keys [top-bar]}]
  [:div {:class "canvas-topbar"
         :on-click #(.stopPropagation %)
         :on-context-menu #(.preventDefault %)}
   (when top-bar [top-bar])
   [view-picker]
   [graph/layout-button]])

(defn- current-canvas [] (feature/canvas (shell/canvas-mode)))

(defn- canvas-surface
  []
  (let [{:keys [header overlays] :as canvas} (current-canvas)]
    [:<>
     (when header [header])
     [:div {:class "canvas-surface"}
      [graph/graph-canvas]
      [canvas-top-bar canvas]
      (map-indexed (fn [i c] ^{:key i} [c]) overlays)
      [bp/writeback-modal]]]))

(def ^:private canvas-detail
  (reify
    IDeref
    (-deref [_] (when-let [open? (:open? (:detail (current-canvas)))] (when (open?) [true])))
    IReset
    (-reset! [_ _] (when-let [close! (:close! (:detail (current-canvas)))] (close!)) nil)))

(page/view! :canvas/surface canvas-surface)
(page/view! :canvas/detail (fn [] (when-let [v (:view (:detail (current-canvas)))] [v])))
(page/view! :canvas/bottom (fn [] [bp/bottom-panel]))

(def ^:private canvas-tree
  (page/werkzeug {:main   (page/liste-detail {:list     :canvas/surface
                                              :detail   :canvas/detail
                                              :open     canvas-detail
                                              :detail-w 320
                                              :resize   :canvas/detail
                                              :back-label "Leinwand"})
                  :bottom :canvas/bottom}))

(defn- canvas-view [] [cplatform/module :canvas canvas-tree])

(defn- project-chip
  []
  (when-let [p (:selected-project @shell/ui-state)]
    [ui/tooltip {:content (i18n/t :ui/project-back-home)}
     [ui/button {:on-click #(shell/enter! (feature/home-nav))
                 :minimal  true
                 :small    true
                 :icon     :caret-right
                 :class    "shell-bar__project-chip"}
      (name p)]]))

(defn- app-identity
  []
  (let [crumb (feature/app-label (shell/current-app))]
    [ui/navbar-group {:class "shell-bar__brand"}
     (when (desktop-nav/desktop?)
       [ui/tooltip {:content (i18n/t :ui/nav-back)}
        [ui/button {:class    "shell-bar__back"
                    :variant  :minimal
                    :icon     :arrow-left
                    :disabled (not @desktop-nav/can-go-back?)
                    :on-click desktop-nav/back! :aria-label (i18n/t :ui/nav-back)}]])
     [ui/tooltip {:content (i18n/t :ui/brand-back-home)}
      [carbon/home {:on-click #(shell/enter! (feature/home-nav))}
       [platform/badge]
       [platform/wordmark]]]
     (when crumb
       [:<>
        [ui/navbar-divider {:class "shell-bar__crumb-sep"}]
        [:span {:class "shell-bar__crumb"} crumb]])
     [project-chip]]))

(defn- page-title
  []
  (let [ui    @shell/ui-state
        own   (feature/page-title ui (feature/current-nav ui))
        label (or (:label own) (feature/app-label (shell/current-app)))]
    [ui/tooltip {:content (or (:title own) (i18n/t :ui/mobile-areas))}
     [ui/button {:class      "shell-bar__page"
                 :minimal    true
                 :right-icon [:span {:class "shell-bar__page-caret"} "▾"]
                 :on-click   (or (:on-click own)
                                 #(shell/set-mobile-nav! true))}
      [:span {:class "shell-bar__page-label"} label]]]))

(defn- tools
  [r]
  [carbon/region :builder r
   {:on-open (fn [{:keys [object-type object-id]}]
               (shell/open-object! (keyword (name (catalog/name-of object-type))) object-id))
    :own     {:branch (when (feature/branching? (shell/current-app)) [branch-bar])}}])

(defn- top-bar
  []
  [carbon/menu-bar {:variant :builder}
   [app-identity]
   [page-title]
   [:div {:class "shell-bar__search"}
    [ui/tooltip {:content (i18n/t :ui/quicksearch-hint)}
     [ui/button {:on-click   #(shell/open-cmdk!)
                 :variant    :outlined
                 :icon       :search
                 :align-text :start
                 :end-icon   [ui/key-combo-tag {:combo   "mod+k"
                                                :minimal true
                                                :class   "shell-bar__search-kbd"}]
                 :class      "shell-bar__search-box"}
      [:span {:class "shell-bar__search-label"} (i18n/t :ui/search-placeholder)]]]]
   [ui/navbar-group {:align :end :class "shell-bar__end"}
    [tools :bar-end]]])

(defn- nav-icon-button
  [{:keys [label glyph on-activate active?]}]
  (let [a? (active?)]
    [ui/tooltip {:content label :placement :right}
     [ui/button {:on-click (fn [_] (on-activate))
                 :variant  (if a? :solid :minimal)
                 :intent   (when a? :primary)
                 :class    "rail__btn"}
      (glyph "currentColor")]]))

(defn- nav-rail []
  (let [cur-app (shell/current-app)
        navs    (feature/app-navs cur-app)]
    (if (:rail-folded? @model/ui)
      [ui/tooltip {:content (i18n/t :ui/rail-unfold) :placement :right}
       [ui/button {:class      "rail__unfold"
                   :minimal    true
                   :small      true
                   :icon       :chevron-right
                   :aria-label (i18n/t :ui/rail-unfold)
                   :on-click   #(model/send! [:builder.rail/fold false])}]]
      [:div {:class "rail"}
       [ui/tooltip {:content (i18n/t :ui/rail-fold) :placement :right}
        [ui/button {:class      "rail__fold"
                    :minimal    true
                    :small      true
                    :icon       :chevron-left
                    :aria-label (i18n/t :ui/rail-fold)
                    :on-click   #(model/send! [:builder.rail/fold true])}]]
       (for [{:keys [id glyph label-key]} (feature/apps)]
         ^{:key id}
         [nav-icon-button {:label (i18n/t label-key) :glyph glyph
                           :active? #(= id (shell/current-app))
                           :on-activate #(go-app! id)}])
       (when (> (count navs) 1)
         [:div {:key   :subnav
                :class "rail__subnav"}
          (for [n navs]
            ^{:key n}
            [nav-icon-button {:label (feature/screen-label n)
                              :glyph (:glyph (feature/screen n))
                              :active? #(= n (shell/current-nav))
                              :on-activate #(shell/go! n)}])])])))

(defn- mobile-tabbar []
  (let [cur   (shell/current-app)
        tabs  (filter :mobile-tab (feature/apps))
        rest? (not (some #(= cur (:id %)) tabs))]
    [:nav {:class "tabbar"}
     (for [{:keys [id glyph mobile-tab]} tabs]
       ^{:key id}
       [ui/button {:class    (cond-> "tabbar__btn" (= id cur) (str " tabbar__btn--on"))
                   :minimal  true
                   :active   (= id cur)
                   :on-click #(go-app! id)}
        [:span {:class "tabbar__glyph"}
         (glyph (if (= id cur) "var(--c-primary)" "var(--c-muted-2)"))]
        [:span {:class "tabbar__label"} (i18n/t mobile-tab)]])
     [ui/button {:class    (cond-> "tabbar__btn" rest? (str " tabbar__btn--on"))
                 :minimal  true
                 :active   rest?
                 :on-click #(shell/set-mobile-nav! true)}
      [:span {:class "tabbar__glyph tabbar__glyph--dots"} "⋯"]
      [:span {:class "tabbar__label"} (i18n/t :ui/mobile-more)]]]))

(defn- mobile-nav-sheet []
  (when (:mobile-nav? @shell/ui-state)
    (let [close! #(shell/set-mobile-nav! false)
          cur    (shell/current-app)
          navs   (feature/app-navs cur)]
      [overlay/modal {:on-close #(close!)
                      :class "mobilenav"
                      :header? false}
       [:<>
        (into [ui/menu {:class "mobilenav__menu"}]
              (concat
               (when (> (count navs) 1)
                 (cons [ui/menu-divider {:title (i18n/t :ui/mobile-in-app)}]
                       (for [n navs]
                         [ui/menu-item {:text (feature/screen-label n)
                                        :active (= n (shell/current-nav))
                                        :class "mobilenav__item"
                                        :on-click #(do (close!) (shell/go! n))}])))
               (cons [ui/menu-divider {:title (i18n/t :ui/mobile-areas)}]
                     (for [{:keys [id label-key]} (feature/apps)]
                       [ui/menu-item {:text (i18n/t label-key)
                                      :active (= id cur)
                                      :class "mobilenav__item"
                                      :on-click #(do (close!) (go-app! id))}]))))
        [:div {:class "mobilenav__foot"} [tools :overflow]]]])))

(defn- schema-error-banner
  []
  (when-let [{:keys [message ontology branch]} (:schema-error @auth/auth)]
    [ui/callout {:intent :danger :class "schema-error-banner"
                 :title  (i18n/t :ui/schema-load-failed)}
     [:span (i18n/t :ui/schema-load-failed-detail {:twin (str ontology "/" branch)})]
     [:code (str message)]]))

(defn- embedded?
  []
  (and (exists? js/window)
       (= "true" (.get (js/URLSearchParams. (.-search js/location)) "embedded"))))

(defonce ^:private status-shown (atom {}))

(defn- status-toast-props
  [kind msg]
  {:intent     (if (= :error kind) :danger :warning)
   :timeout    0
   :message    [:div
                [:div {:class "errtoast__message"} (str msg)]
                (when (= :error kind)
                  [ui/tooltip {:content (i18n/t :ui/copy-error)}
                   [ui/button {:on-click #(some-> js/navigator .-clipboard
                                                  (.writeText (str msg)))
                               :minimal  true
                               :small    true
                               :icon     :duplicate
                               :class    "errtoast__copy"}
                    (i18n/t :ui/copy)]])]
   :on-dismiss (fn [_] (shell/dismiss-status! kind msg))})

(defn- sync-status-toast!
  [kind]
  (let [msg (get @shell/ui-state kind)
        [shown k] (get @status-shown kind)]
    (when (not= msg shown)
      (when k (ui/dismiss! (ui/default-toaster) k))
      (swap! status-shown assoc kind
             (when msg
               [msg (ui/show! (ui/default-toaster) (status-toast-props kind msg))])))))

(defn- status-toasts []
  (r/with-let [tracks (mapv #(r/track! sync-status-toast! %) [:error :warning])]
    nil
    (finally (run! r/dispose! tracks))))

(defn- main-overlays []
  [:<>
   [edit-modals/edit-node-modal]
   [form-modals/add-object-form]
   [form-modals/add-link-form]
   (when (:connect-modal @shell/ui-state)
     [form-modals/connect-modal])
   (when-let [{:keys [kind id]} (:detail-modal @shell/ui-state)]
     (let [bottom-panel-kind? (or (panel/schema-panel-kinds kind)
                                  (#{:object-instance :link} kind))
           editing?           (get-in @shell/ui-state [:bottom-panel :editing?])
           show?              (or (not bottom-panel-kind?) editing?)
           close!             (fn []
                                (panel/set-panel-editing! false)
                                (when-not bottom-panel-kind?
                                  (shell/close-detail-modal!)))]
       (when-let [page (and show? (:render (first (feature/contributions :detail-page))))]
         [object-forms/entity-detail-modal
          [page kind id close!]
          close!])))
   (when (:staging-modal? @shell/ui-state)
     [staging-modal])
   (for [{:keys [id render]} (feature/contributions :shell-overlay)]
     ^{:key id} [render])
   [command-palette]
   [status-toasts]])

(def parts
  {:class    (fn []
               (cond-> nil
                 (:rail-folded? @model/ui) (str " shell--rail-folded")
                 (embedded?)               (str " shell--embedded")))
   :top-bar  (fn [] (when-not (embedded?) [top-bar]))
   :banner   schema-error-banner
   :rail     (fn []
               (when-not (embedded?)
                 [:<>
                  [nav-rail]
                  [mobile-tabbar]
                  [mobile-nav-sheet]]))
   :fallback canvas-view
   :overlays main-overlays
   :taskbar  (fn [] (when-not (embedded?) [branch-taskbar]))})
