(ns sutra-ui.builder.canvas-menu
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra-ui.builder.events :as ev]
            [sutra-ui.builder.model :as model]
            [sutra-ui.state.canvas :as canvas-state]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.graph :as graph-state]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]
            [sutra-ui.object.card :as oc]
            [sutra-ui.graph-editor.export :as gexport]
            [sutra-ui.graph-editor.legend :as gleg]
            [sutra-ui.feature :as feature]
            [sutra.i18n.core :as i18n]))

(defn graphs-grouped-by-area
  []
  (->> (catalog/graphs-of (shell/canvas-mode))
       (group-by :area)
       (sort-by (comp str key))))

(defn diverged?
  [view max-seq]
  (let [seen (:last-seen-action-seq view)]
    (and (some? seen) (some? max-seq) (< seen max-seq))))

(defn lens-row [{:keys [label title visible? on-click disabled?]}]
  [ui/tooltip {:content title}
   [:div
    [ui/checkbox
     {:class "lens__row"
      :style {:margin 0
              :background (if visible? "transparent" "var(--c-surface-soft)")}
      :checked (boolean visible?)
      :disabled (boolean disabled?)
      :on-change on-click
      :label-element [:span {:style {:color (if visible? "var(--c-text)" "var(--c-muted-2)")}}
                      label]}]]])

(defn lens-panel []
  (let [v        @canvas-state/current-view
        close    #(shell/set-lens-drawer! false)
        sch      @catalog/schema
        vt       (:visible-types v)
        ot-vis   (or (:object-types vt) :all)
        ct-vis   (or (:link-types vt) :all)
        show?    (fn [kind kw]
                   (let [s (case kind :object-types ot-vis :link-types ct-vis)]
                     (or (= s :all) (contains? s kw))))
        all-ots  (sort (keys (:objects sch)))
        all-cts  (sort-by #(name (:name %)) (:links sch))]
    [:div {:class "lens"}
     [:div {:class "lens__header"}
      [:div
       [:div {:class "lens__title"} (i18n/t :ui/lens)]
       [:div {:class "lens__subtitle"}
        (if v (i18n/t :ui/view-named {:name (:name v)})
            (i18n/t :ui/view-none-loaded))]]
      [ui/button {:on-click close :minimal true :small true :icon :cross
                  :aria-label (i18n/t :ui/close)}]]
     (when v
       [:div {:class "lens__status"}
        [:span {:class "u-muted"}
         (if (nil? vt) (i18n/t :ui/showing-all) (i18n/t :ui/filtered))]
        [ui/button {:on-click #(graph-state/set-visible-types! nil)
                    :disabled (nil? vt)
                    :minimal  true
                    :small    true
                    :intent   :primary}
         (i18n/t :ui/show-all)]])
     [:div {:class "lens__body"}
      [:div {:class "lens__group-label"}
       (i18n/t :ui/object-types)]
      (if (empty? all-ots)
        [:div {:class "lens__empty"}
         (i18n/t :ui/lens-none)]
        (for [kw (sort-by #(str/lower-case (oc/type-label %)) all-ots)]
          ^{:key (str "ot-" (name kw))}
          [lens-row {:label (oc/type-label kw)
                     :title (name kw)
                     :visible? (show? :object-types kw)
                     :disabled? (nil? v)
                     :on-click #(graph-state/toggle-visible-type! :object-types kw)}]))
      [:div {:class "lens__group-label"}
       (i18n/t :ui/link-types)]
      (if (empty? all-cts)
        [:div {:class "lens__empty"}
         (i18n/t :ui/lens-none)]
        (for [{cn :name :as c} all-cts]
          ^{:key (str "ct-" (name cn))}
          [lens-row {:label (oc/link-type-label c)
                     :title (name cn)
                     :visible? (show? :link-types cn)
                     :disabled? (nil? v)
                     :on-click #(graph-state/toggle-visible-type! :link-types cn)}]))]]))

(def color-by-modes
  (vec (concat [[:resource-kind        :ui/color-by-resource-kind]
                [:build-status         :ui/color-by-build-status]
                [:build-duration       :ui/color-by-build-duration]
                [:last-built           :ui/color-by-last-built]
                [:model-usage          :ui/color-by-model-usage]
                [:out-of-date          :ui/color-by-out-of-date]
                [:data-health          :ui/color-by-data-health]
                [:schedule-count       :ui/color-by-schedule-count]
                [:ontology-status      :ui/color-by-ontology-status]
                [:folder               :ui/color-by-folder]
                [:function-repo        :ui/color-by-function-repo]
                [:app-views            :ui/color-by-app-views]
                [:resource-permissions :ui/color-by-resource-permissions]]
               (for [[k
                      {:keys [desc]}] (sort-by key
                                               (dissoc colors/categorical-color-modes :project))]
                 [k desc])
               [[:scheme :ui/color-by-scheme]])))

(def color-mode-swatch
  {:resource-kind        "var(--blue3)"
   :build-status         "var(--green3)"
   :build-duration       "var(--violet3)"
   :last-built           "var(--turquoise2)"
   :model-usage          "var(--vermilion3)"
   :out-of-date          "var(--gold3)"
   :data-health          "var(--red3)"
   :schedule-count       "var(--cerulean3)"
   :ontology-status      "var(--cerulean4)"
   :folder               "var(--orange3)"
   :function-repo        "var(--turquoise3)"
   :app-views            "var(--violet4)"
   :resource-permissions "var(--gray3)"
   :scheme               "var(--indigo3)"})

(defn color-menu [v schemes mode active-id active]
  (let [close! #(shell/set-color-popover! false)
        delete (fn [s]
                 [ui/tooltip {:content (i18n/t :ui/scheme-delete)}
                  [ui/button
                   {:minimal    true
                    :small      true
                    :icon       :cross
                    :aria-label (i18n/t :ui/scheme-delete)
                    :class      "lineage-groups__delete"
                    :on-click   (fn [e]
                                  (.stopPropagation e)
                                  (model/send! [:builder.scheme/delete s]))}]])]
    (into [ui/menu]
          (concat
           [[ui/menu-divider {:title (i18n/t :ui/color-by)}]]
           (for [[k desc] color-by-modes]
             [ui/menu-item
              {:text [:<> [:div (get gleg/mode-labels k)]
                      [:div {:class "sutra-menu__sub"} (if (keyword? desc) (i18n/t desc) desc)]]
               :multiline true
               :active (= k mode)

               :on-click #(colors/set-color-mode! k)}])
           [[ui/menu-divider {:title (i18n/t :ui/color-scheme)}]]
           (if (empty? schemes)
             [[ui/menu-item {:text (if v (i18n/t :ui/color-schemes-none)
                                       (i18n/t :ui/color-schemes-need-view))
                             :disabled true}]]
             (for [s schemes
                   :let [id (str (:id s))]]
               [ui/menu-item
                {:text (str (:name s)
                            (when (:is-default s) (i18n/t :ui/scheme-default-suffix)))
                 :label (delete s)
                 :active (= id (str (or active-id (:id active))))

                 :on-click #(do (graph-state/set-active-color-scheme! id) (close!))}]))
           (when v
             [[ui/menu-divider]
              [ui/menu-item {:text (i18n/t :ui/scheme-freeze)
                             :icon :snowflake

                             :on-click #(model/send! [:builder.scheme/freeze])}]])))))

(defn color-chip []
  (let [v       @canvas-state/current-view
        schemes (:color-schemes v)
        mode    (:color-mode @shell/ui-state :resource-kind)
        active-id (:active-color-scheme-id @shell/ui-state)
        active  (or (some #(when (= (str (:id %)) (str active-id)) %) schemes)
                    (some :is-default schemes))
        mode-label (get gleg/mode-labels mode)]
    [:div {:class "lineage-groups"}
     [ui/popover
      {:is-open            (boolean (:color-popover? @shell/ui-state))
       :on-interaction     (fn [o _] (shell/set-color-popover! o))
       :placement          :bottom-end
       :popover-class-name "colormenu"
       :content            [color-menu v schemes mode active-id active]}
      [ui/tooltip {:content (i18n/t :ui/color-switch-concern)}
       [ui/button {:minimal  true
                   :small    true

                   :icon     [:span {:class "colorchip__swatch"
                                     :style {:background
                                             (or (get color-mode-swatch mode)
                                                 (when (contains? colors/categorical-color-modes
                                                                  mode)
                                                   (colors/categorical-color (name mode)))
                                                 "var(--gray5)")}}]}
        (str (i18n/t :ui/color-prefix) (or mode-label "—")
             (when (and (= mode :scheme) active) (str " · " (:name active))))]]]]))

(defn export-graph! [fmt]
  (if (gexport/export! fmt (or (some-> @canvas-state/current-view :name) "graph"))
    (ev/flash-toast! (i18n/t :ui/graph-exported {:format (str/upper-case (name fmt))}))
    (ev/flash-toast! (i18n/t :ui/graph-no-canvas))))

(defn actions-menu
  []
  [:div {:style {:position "relative" :display "inline-block"}
         :on-click #(.stopPropagation %)}
   [ui/popover
    {:is-open        (boolean (:actions-menu? @model/ui))
     :on-interaction (fn [o _] (model/send! [:builder.menu/actions o]))
     :placement      :bottom-start
     :content
     (into [ui/menu]
           [[ui/menu-item
             {:text (i18n/t :ui/save)
              :on-click #(model/send! [:builder.save/in-place
                                       (:current-view-id @shell/ui-state)])}]
            [ui/menu-item
             {:text (i18n/t :ui/save-as)
              :on-click #(model/send! [:builder.save-as/open])}]
            [ui/menu-item {:text (i18n/t :ui/graph-open)
                           :on-click #(model/send! [:builder.open-graph/open])}]
            [ui/menu-divider]
            [ui/menu-item
             {:text (i18n/t :ui/graph-copy-link)
              :on-click #(model/send! [:builder.share/link (.. js/window -location -href)])}]
            [ui/menu-item {:text (i18n/t :ui/graph-export-svg)
                           :on-click #(export-graph! :svg)}]
            [ui/menu-item {:text (i18n/t :ui/graph-export-dot)
                           :on-click #(export-graph! :dot)}]])}
    [ui/tooltip {:content (i18n/t :ui/actions-hint)}
     [ui/button {:minimal    true
                 :right-icon :caret-down}
      (i18n/t :ui/actions-menu)]]]])

(def field-label-style
  {:font-size "var(--fs-sm)" :color "var(--c-muted)" :font-weight "600"
   :margin "var(--pt-grid-size) 0 var(--sp-1)"})

(defn kompass-folder-options
  []
  (let [nodes  (kompass/kompass-node-list)
        by-rid (into {} (map (juxt :rid identity)) nodes)]
    (letfn [(path [rid]
              (if-let [f (get by-rid rid)]
                (conj (path (:parent-rid f)) (:name f))
                []))]
      (->> nodes
           (filter #(= :folder (:node-kind %)))
           (map (fn [f] {:rid (:rid f) :name (:name f)
                         :label (str/join " / " (path (:rid f)))}))
           (sort-by :label)))))

(defn save-as-modal []
  (when-let [form (:save-as @model/ui)]
    (let [close!  #(model/send! [:builder.save-as/close])
          folders (kompass-folder-options)
          by-rid  (into {} (map (juxt :rid identity)) folders)
          area    (or (:name (get by-rid (:folder-rid form))) "default")
          submit  #(model/send! [:builder.save-as/submit area])]
      [overlay/form-dialog {:on-close #(close!) :title (i18n/t :ui/graph-save-as)
                            :ok-label (i18n/t :ui/save) :on-ok submit}
       [:div {:style {:font-size "var(--fs-sm)" :color "var(--c-muted)"}}
        (i18n/t :ui/graph-save-as-hint
                {:mode (or (some-> @canvas-state/current-view :mode feature/mode->nav
                                   feature/screen-label)
                           "")})]
       [:div {:style field-label-style} (i18n/t :ui/name)]
       [ui/input-group
        {:auto-focus true
         :value (:name form)
         :on-key-down (fn [e] (when (= "Enter" (.-key e)) (submit)))
         :on-change #(model/send! [:builder.save-as/field :name (.. % -target -value)])}]
       [:div {:style field-label-style} (i18n/t :ui/location-kompass-folder)]
       [ui/html-select
        {:fill true
         :value (or (:folder-rid form) "")
         :on-change #(model/send! [:builder.save-as/field :folder-rid
                                   (not-empty (.. % -target -value))])}
        [:option {:value ""} (i18n/t :ui/folder-root)]
        (for [{:keys [rid label]} folders]
          ^{:key rid} [:option {:value rid} label])]
       (when (empty? folders)
         [:div {:style {:font-size "var(--fs-sm)" :color "var(--c-muted-2)"
                        :margin-top "var(--sp-1)"}}
          (i18n/t :ui/kompass-folders-none)])])))

(defn open-graph-modal []
  (when (:open-graph? @model/ui)
    (let [close! #(model/send! [:builder.open-graph/close])
          groups (graphs-grouped-by-area)
          open!  (fn [vid] (model/send! [:builder.open-graph/load vid]))
          rid    (:open-graph-rid @model/ui "")]
      [overlay/modal {:on-close close!
                      :title (i18n/t :ui/graph-open-title)
                      :foot [ui/button {:on-click close!} (i18n/t :ui/close)]
                      :foot-end? true}
       [:div {:class "graph-open__list"}
        (if (empty? groups)
          [ui/empty-state {:icon :graph :title (i18n/t :ui/graphs-none)}]
          (for [[area items] groups]
            ^{:key area}
            [:div
             (when area
               [:div {:style {:padding "var(--sp-2) var(--pt-grid-size)"
                              :background "var(--c-surface-soft)"
                              :font-size "var(--fs-sm)" :font-weight "600" :color "var(--c-muted-2)"
                              :text-transform "uppercase"}} area])
             (for [v (sort-by :name items)]
               ^{:key (str (:id v))}
               [:div {:class "sutra-menu-row"
                      :on-click #(open! (str (:id v)))}
                (:name v)])]))]
       [ui/section
        {:title (i18n/t :ui/frame-advanced) :collapsible true :compact true
         :collapse-props {:default-is-open false} :class "u-mt-8"}
        [ui/section-card
         [:div {:style field-label-style} (i18n/t :ui/open-by-rid)]
         [:div {:style {:display "flex" :gap "var(--sp-2)"}}
          [ui/input-group {:class "u-flex-1" :placeholder "ri.sutra.…view.…"
                           :value rid
                           :on-change #(model/send! [:builder.open-graph/rid
                                                     (.. % -target -value)])}]
          [ui/button {:on-click #(open! rid)}
           (i18n/t :ui/open)]]]]])))

(defn view-picker
  []
  (let [cur      (:current-view-id @shell/ui-state)
        view     @canvas-state/current-view
        max-seq  (:max-action-seq @sync/snapshot 0)
        groups   (graphs-grouped-by-area)
        stale?   (and view (diverged? view max-seq))]
    [:<>
     [:label {:class "shell-bar__context-label"} (i18n/t :ui/view-label)]
     [ui/html-select {:value (or cur "")
                      :on-change (fn [e]
                                   (let [v (.. e -target -value)]
                                     (when (seq v) (graph-state/load-view! v))))
                      :class "shell-bar__view-select"}
      [:option {:value ""} (i18n/t :ui/view-select)]
      (for [[area items] groups
            v (sort-by :name items)]
        ^{:key (str (:id v))}
        [:option {:value (str (:id v))}
         (cond->> (:name v) area (str area " / "))])]
     [actions-menu]
     (let [vt (:visible-types view)
           lens-on? (some? vt)]
       [ui/popover
        {:is-open        (boolean (:lens-drawer? @shell/ui-state))
         :on-interaction (fn [o _] (shell/set-lens-drawer! o))
         :placement      :bottom-start
         :content        [lens-panel]}
        [ui/tooltip {:content (i18n/t :ui/lens-hint)}
         [ui/button {:disabled (nil? view)
                     :intent   :primary
                     :outlined (not lens-on?)
                     :small    true}
          (if lens-on? (i18n/t :ui/lens-filtered) (i18n/t :ui/lens))]]])
     [color-chip]
     (when cur
       [ui/button {:on-click #(model/send! [:builder.view/delete (:name view)])
                   :minimal  true
                   :intent   :danger
                   :class    "shell-bar__drop-btn"}
        (i18n/t :ui/delete)])
     (when stale?
       [ui/tooltip {:content (i18n/t :ui/view-stale-hint
                                     {:from (:last-seen-action-seq view) :to max-seq})}
        [ui/tag {:intent :warning :minimal true}
         (i18n/t :ui/view-stale)]])
     [save-as-modal]
     [open-graph-modal]]))
