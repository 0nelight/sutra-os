(ns sutra-ui.apps.lineage.core
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.display :as display]
            [sutra-ui.ui.page :as page]
            [reagent.core :as r]
            [clojure.string :as str]
            [clojure.set]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.lineage :as lineage]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.selection :as selection]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.lineage.events]
            [sutra-ui.apps.lineage.model :as lineage-model]
            [sutra-ui.graph-editor.shared :as gs]
            [sutra-ui.graph-editor.seed :as seed]
            [sutra-ui.graph-editor.export :as gexport]
            [sutra-ui.graph-editor.legend :as gleg]
            [sutra-ui.graph-editor.kinds :as kinds]
            [sutra-ui.ui.busy :as busy]
            [sutra.viz.lineage-projection :as lp]
            [sutra.i18n.core :as i18n]))

(defn- groups-chip
  []
  (let [groups (canvas/current-color-groups)
        open?  (:groups-popover? @lineage-model/ui)]
    [:div {:class "lineage-groups"}
     [ui/popover
      {:is-open            (boolean open?)
       :on-interaction     (fn [o _] (lineage-model/edit! [] assoc :groups-popover? o))
       :placement          :bottom-end
       :popover-class-name "lineage-groups__popover"
       :content
       [:<>
        [:div {:class "lineage-groups__header"}
         (i18n/t :ui/color-groups)]
        (if (empty? groups)
          [:div {:class "lineage-groups__empty"}
           (i18n/t :ui/color-groups-empty)]
          (for [g groups]
            ^{:key (:id g)}
            [:div {:class "lineage-groups__row"}
             [ui/tooltip {:content (i18n/t :ui/recolor)}
              [:input {:type "color" :value (:color g)
                       :on-change #(let [c (.. % -target -value)]
                                     (graph/update-group! (:id g) (fn [x] (assoc x :color c))))
                       :class "lineage-groups__color"}]]
             [ui/tooltip {:content (i18n/t :ui/rename)}
              [:span
               {:class "lineage-groups__name"
                :on-click #(lineage-model/send! [:lineage.group/rename g])}
               (str (:name g) " · " (count (:members g)))]]
             [ui/tooltip
              {:content (i18n/t (if (:collapsed? g) :ui/expand :ui/collapse))}
              [ui/button {:minimal true :small true
                          :icon (if (:collapsed? g) :expand-all :collapse-all)
                          :on-click #(graph/toggle-group-collapsed! (:id g))
                          :aria-label (i18n/t (if (:collapsed? g) :ui/expand :ui/collapse))}]]
             [ui/tooltip {:content (i18n/t :ui/group-delete)}
              [ui/button
               {:minimal true :small true :icon :cross
                :on-click #(lineage-model/send! [:lineage.group/delete g])
                :aria-label (i18n/t :ui/group-delete)}]]]))]}
      [ui/tooltip {:content (i18n/t :ui/color-groups)}
       [ui/button {:small true :icon :tint}
        (str (i18n/t :ui/groups) (when (seq groups) (str " (" (count groups) ")")))]]]]))

(defn- lineage-seed-box
  []
  [seed/seed-box
   {:placeholder (i18n/t :ui/seed-placeholder)
    :roots-label (i18n/t :ui/seed-roots-label)
    :candidates  lineage-model/lineage-seed-candidates
    :roots       lineage-model/lineage-root-candidates
    :on-pick     lineage-model/seed-lineage-ref!}])

(defn- branch-modified-callout
  []
  (let [refs (lineage-model/modified-not-on-graph)]
    (when (seq refs)
      [:div {:class "lineage-branch-callout" :on-click #(.stopPropagation %)}
       [:span (i18n/t :ui/lineage-branch-modified {:n (count refs)})]
       [ui/button {:small true :intent :primary
                   :on-click #(graph/add-to-lineage-scope! refs)}
        (i18n/t :ui/lineage-branch-modified-add)]])))

(defn- color-legend
  []
  (let [inv (lineage/lineage-node-inventory @catalog/schema (canvas/lineage-scope))]
    [gleg/legend gleg/lineage-kinds
     {:counts (into {} (for [{:keys [kind inventory]} lp/node-kinds :when inventory]
                         [kind (count (get inv inventory))]))}]))

(defn- selected-lineage-node-id
  []
  (let [{:keys [kind id]} (:detail-modal @shell/ui-state)]
    (when (and kind id) (lp/node-id kind id))))

(defn- target-node-ids
  []
  (let [sel (selection/selected-node-ids)]
    (if (seq sel)
      (vec sel)
      (when-let [one (selected-lineage-node-id)] [one]))))

(defn- selected-build-refs
  [node-ids]
  (->> node-ids
       (keep lp/parse-node-id)
       (filter (comp #{:dataset :object-type :transform} first))
       distinct
       vec))

(defn- export-lineage-svg! []
  (gexport/export-svg! "lineage"))

(defn- export-lineage-dot! []
  (gexport/export-dot! "lineage"))

(defn- tb-button
  [{:keys [icon label title active? on-click disabled?]}]
  [ui/tooltip {:content title}
   [ui/button {:small true :icon icon :active active? :on-click on-click :disabled disabled?
               :aria-label title}
    label]])

(defn- tool-chip
  [_opts _items]
  (let [open? (r/atom false)]
    (fn [{:keys [disabled? wide?] :as opts} items]
      [ui/popover
       {:is-open        (and @open? (not disabled?))
        :on-interaction (fn [o _] (reset! open? (and o (not disabled?))))
        :placement      :bottom-start
        :content        (into [ui/menu {:class ["lineage-toolmenu"
                                                (when wide? "lineage-toolmenu--wide")]}]
                              (items))}
       [tb-button (assoc (dissoc opts :wide?) :active? @open?)]])))

(defn- nodes-note [n]
  (i18n/t (if (= 1 n) :ui/nodes-added-one :ui/nodes-added-many) {:n n}))

(defn- counted
  [label note n on-click]
  [ui/menu-item {:text     label
                 :label    note
                 :on-click #(on-click)
                 :class    (when (zero? n) "graph-menu__item--empty")}])

(defn- expand-chip
  [direction icon label]
  (let [ids (target-node-ids)]
    [tool-chip
     {:icon icon :label label :disabled? (empty? ids)
      :title (i18n/t :ui/expand-direction {:direction (name direction) :n (count ids)})}
     (fn []
       (for [[levels lbl] [[1 (i18n/t :ui/expand-1)] [2 (i18n/t :ui/expand-2)]
                           [:all (i18n/t :ui/expand-all)]]]
         (let [n (lineage/lineage-expand-count-of ids direction levels)]
           (counted lbl (nodes-note n) n #(graph/expand-nodes! ids direction levels)))))]))

(defn- relate-chip
  []
  (let [ids   (target-node-ids)
        many? (> (count ids) 1)
        rows  [[(i18n/t :ui/relate-between) #(lineage-model/lineage-between-count ids)
                #(lineage-model/expand-between! ids)]
               [(i18n/t :ui/relate-ancestors) #(lineage-model/lineage-common-count ids :upstream)
                #(lineage-model/expand-common! ids :upstream)]
               [(i18n/t :ui/relate-descendants)
                #(lineage-model/lineage-common-count ids :downstream)
                #(lineage-model/expand-common! ids :downstream)]]]
    [tool-chip
     {:icon :exchange :label (i18n/t :ui/lin-relate) :disabled? (not many?) :wide? true
      :title (if many?
               (i18n/t :ui/relate-selected {:n (count ids)})
               (i18n/t :ui/relate-need-two))}
     (fn []
       (for [[lbl count-fn run!] rows]
         (let [n (count-fn)]
           (counted lbl (nodes-note n) n run!))))]))

(defn- cursor-mode-chip
  []
  [ui/segmented-control
   {:small true :value (boolean (:drag-select? @lineage-model/ui))
    :on-value-change #(lineage-model/edit! [] assoc :drag-select? %)
    :options [{:value false :label "" :icon :hand :title (i18n/t :ui/cursor-pan)
               :aria-label (i18n/t :ui/cursor-pan)}
              {:value true :label "" :icon :select :title (i18n/t :ui/cursor-marquee)
               :aria-label (i18n/t :ui/cursor-marquee)}]}])

(defn- selection-chip
  []
  (let [n    (count (selection/selected-node-ids))
        rows [[(i18n/t :ui/select-all) selection/select-all-nodes!]
              [(i18n/t :ui/select-invert) lineage-model/invert-selection!]
              [(i18n/t :ui/select-parents) #(lineage-model/select-adjacent! :upstream)]
              [(i18n/t :ui/select-children) #(lineage-model/select-adjacent! :downstream)]
              [(i18n/t :ui/select-clear) selection/clear-selection!]]]
    [tool-chip
     {:icon :select :label (if (pos? n) n (i18n/t :ui/select))
      :title (i18n/t :ui/selection-tools)}
     (fn []
       (cons [ui/menu-divider
              {:title (i18n/t (if (= 1 n) :ui/nodes-selected-one :ui/nodes-selected-many)
                              {:n n})}]
             (for [[lbl f] rows] [ui/menu-item {:text lbl :on-click #(f)}])))]))

(defn- clean-chip
  []
  (let [rows [[:isolated   (i18n/t :ui/clean-isolated)]
              [:unselected (i18n/t :ui/clean-unselected)]
              [:all        (i18n/t :ui/clean-all)]]]
    [tool-chip
     {:icon :eraser :label (i18n/t :ui/lin-clean) :title (i18n/t :ui/clean-graph) :wide? true}
     (fn []
       (for [[what lbl] rows]
         (let [n (count (lineage-model/clean-candidates what))]
           (counted lbl
                    (i18n/t (if (= 1 n) :ui/nodes-removed-one :ui/nodes-removed-many) {:n n})
                    n #(lineage-model/clean-graph! what)))))]))

(defn- align-chip
  []
  (let [n     (count (selection/selected-node-ids))
        many? (> n 1)
        item  (fn [[how lbl]]
                [ui/menu-item {:text lbl :on-click #(lineage-model/align-selection! how)}])]
    [tool-chip
     {:icon :alignment-left :label (i18n/t :ui/align) :disabled? (not many?) :wide? true
      :title (if many?
               (i18n/t :ui/align-selected {:n n})
               (i18n/t :ui/align-need-two))}
     (fn []
       (concat
        [[ui/menu-divider {:title (i18n/t :ui/align)}]]
        (map item [[:left (i18n/t :ui/align-left)] [:h-center (i18n/t :ui/align-h-center)]
                   [:right (i18n/t :ui/align-right)] [:top (i18n/t :ui/align-top)]
                   [:v-middle (i18n/t :ui/align-v-middle)]
                   [:bottom (i18n/t :ui/align-bottom)]])
        [[ui/menu-divider {:title (i18n/t :ui/distribute)}]]
        (map item [[:distribute-h (i18n/t :ui/distribute-h)]
                   [:distribute-v (i18n/t :ui/distribute-v)]])))]))

(defn- history-buttons
  []
  [:<>
   [tb-button {:icon :undo :title (i18n/t :ui/undo-hint)
               :disabled? (not (lineage-model/can-undo?))
               :on-click graph/graph-undo!}]
   [tb-button {:icon :redo :title (i18n/t :ui/redo-hint)
               :disabled? (not (lineage-model/can-redo?))
               :on-click graph/graph-redo!}]])

(defn- color-layout-buttons
  []
  (let [groups (canvas/current-color-groups)
        none?  (empty? groups)
        folded? (and (seq groups) (every? :collapsed? groups))]
    [:<>
     [tb-button {:icon :grid-view :disabled? none?
                 :title (if none?
                          (i18n/t :ui/color-groups-first)
                          (i18n/t :ui/layout-by-color))
                 :on-click graph/layout-by-color!}]
     [tb-button {:icon :collapse-all :disabled? none? :active? folded?
                 :title (if none?
                          (i18n/t :ui/color-groups-first)
                          (i18n/t (if folded? :ui/color-groups-expand :ui/group-by-color)))
                 :on-click lineage-model/toggle-all-groups-collapsed!}]]))

(defn- remove-chip
  []
  (let [ids (selection/selected-node-ids)]
    [tb-button {:icon :remove :label (i18n/t :ui/lin-remove) :disabled? (empty? ids)
                :title (if (seq ids)
                         (i18n/t (if (= 1 (count ids)) :ui/remove-nodes-one :ui/remove-nodes-many)
                                 {:n (count ids)})
                         (i18n/t :ui/remove-need-selection))
                :on-click #(when (seq ids)
                             (lineage-model/remove-nodes-from-lineage-scope! (vec ids))
                             (selection/clear-selection!))}]))

(def ^:private shortcuts
  [[:ui/shortcuts-graph
    [["Click a node" :ui/sc-click-node]
     ["Ctrl/Cmd + click" :ui/sc-ctrl-click]
     ["Shift + drag" :ui/sc-shift-drag]
     ["Drag on empty canvas" :ui/sc-drag-empty]
     ["Middle-drag" :ui/sc-middle-drag]
     ["Scroll" :ui/sc-scroll]
     ["‹ / › on a node" :ui/sc-chevrons]
     ["Right-click a node" :ui/sc-right-click]]]
   [:ui/shortcuts-keys
    [["Ctrl/Cmd + A" :ui/sc-select-all]
     ["Delete / Backspace" :ui/sc-delete]
     ["Ctrl/Cmd + Z" :ui/sc-undo]
     ["Ctrl/Cmd + Shift + Z" :ui/sc-redo]
     ["Esc" :ui/sc-esc]]]
   [:ui/shortcuts-layout
    [["Ctrl/Cmd + Shift + A" :ui/ge-layout-all]
     ["L" :ui/ge-layout-nodes]
     ["Shift + L" :ui/ge-layout-hier-lr]
     ["Alt + L" :ui/ge-layout-clustered]
     ["G" :ui/ge-layout-grid]
     ["V" :ui/ge-sel-vertical]
     ["H" :ui/ge-sel-horizontal]]]])

(defn- tool-group
  [label & children]
  (into [ui/button-group {:role "group" :aria-label label :class "lineage-toolgroup"}
         [:span {:class "lineage-toolgroup__label"} label]]
        children))

(defn- toolbar-buttons
  []
  (let [tab    (:tab (:right-panel @lineage-model/ui))
        toggle (fn [t] (lineage-model/edit! [] update :right-panel
                              (fn [rp] (if (= (:tab rp) t) nil {:tab t}))))]
    [:<>
     [tool-group (i18n/t :ui/lin-group-graph)
      [cursor-mode-chip]
      [history-buttons]
      [tb-button
       {:icon :search :label (i18n/t :ui/find) :title (i18n/t :ui/find-nodes)
        :active? (= tab :search)
        :on-click #(toggle :search)}]
      [selection-chip]
      [clean-chip]
      [color-layout-buttons]
      [tb-button {:icon :download :label "SVG" :title (i18n/t :ui/graph-export-svg)
                  :on-click export-lineage-svg!}]
      [tb-button {:icon :download :label "DOT" :title (i18n/t :ui/graph-export-dot)
                  :on-click export-lineage-dot!}]
      [tb-button {:icon :key-command :title (i18n/t :ui/shortcuts)
                  :active? (boolean (:shortcuts? @lineage-model/ui))
                  :on-click #(lineage-model/edit! [] update :shortcuts? not)}]]
     [tool-group (i18n/t :ui/lin-group-selection)
      [expand-chip :upstream :arrow-up (i18n/t :ui/lin-upstream)]
      [expand-chip :downstream :arrow-down (i18n/t :ui/lin-downstream)]
      [relate-chip]
      [align-chip]
      [remove-chip]
      [tb-button
       {:icon :build :label (i18n/t :ui/builds) :title (i18n/t :ui/builds-manage)
        :active? (= tab :builds)
        :on-click #(do (toggle :builds) (lineage-status/ensure-lineage-status!))}]
      [tb-button
       {:icon :calendar :label (i18n/t :ui/schedules) :title (i18n/t :ui/schedules-manage)
        :active? (= tab :schedules)
        :on-click #(toggle :schedules)}]
      [tb-button
       {:icon :info-sign :label (i18n/t :ui/properties)
        :title (i18n/t :ui/properties-histogram)
        :active? (= tab :properties)
        :on-click #(do (toggle :properties) (lineage-status/ensure-lineage-status!))}]
      (let [{:keys [kind id]} (:detail-modal @shell/ui-state)
            n (count (:items (panel/panel :related kind id)))]
        [tb-button {:icon :duplicate
                    :label (str (i18n/t :ui/related) (when (pos? n) (str " " n)))
                    :title (if id
                             (i18n/t :ui/what-uses {:name (kinds/display-name kind id)})
                             (i18n/t :ui/related-need-selection))
                    :active? (= tab :related)
                    :on-click #(do (toggle :related)
                                   (when id (panel/load-panel! :related kind id)))}])]]))

(defn- close-panel! [] (lineage-model/edit! [] assoc :right-panel nil))

(defn- panel-header [title]
  [page/header {:title title}
   [ui/button {:minimal true :small true :icon :cross :aria-label (i18n/t :ui/close)
               :on-click close-panel!}]])

(defn- shortcuts-dialog
  []
  (let [close! #(lineage-model/edit! [] dissoc :shortcuts?)]
    [overlay/modal {:on-close #(close!)
                    :class "lineage-shortcuts"
                    :title (i18n/t :ui/shortcuts-title)}
     (for [[section rows] shortcuts]
       ^{:key section}
       [:div
        [:div {:class "u-section-label"} (i18n/t section)]
        (for [[gesture what] rows]
          ^{:key gesture}
          [:div {:class "lineage-shortcuts__row"}
           [:span {:class "lineage-shortcuts__key"} gesture]
           [:span {:class "u-muted"} (i18n/t what)]])])]))

(defn- shortcuts-modal
  []
  (when (:shortcuts? @lineage-model/ui)
    [shortcuts-dialog]))

(defn- search-panel
  []
  (let [q (r/atom "")]
    (fn []
      (let [by  (or (:find-axis @lineage-model/ui) :name)
            col? (= :columns by)
            results (->> (if col?
                           (lineage-model/lineage-column-candidates @q)
                           (let [ql (str/lower-case (str/trim @q))]
                             (filter (fn [{:keys [kind kw label]}]
                                       (some #(str/includes? (str/lower-case (str %)) ql)
                                             [label (kinds/display-name kind kw)]))
                                     (lineage-model/lineage-search-candidates ""))))
                         (take 40))]
        [:div {:class "lineage-panel"}
         [panel-header (i18n/t :ui/find-on-graph)]
         [:div {:class "lineage-panel__body"}
          [:div {:class "lineage-find__axes"}
           (for [[k label] [[:name (i18n/t :ui/by-name)] [:columns (i18n/t :ui/by-column)]]]
             ^{:key (name k)}
             [ui/button {:small true :active (= by k)
                         :on-click #(lineage-model/edit! [] assoc :find-axis k)}
              label])]
          [ui/query-list
           {:query              @q
            :on-query-change    (fn [s _] (reset! q s))
            :items              (vec results)
            :items-equal        (fn [a b] (= ((juxt :kind :kw) a) ((juxt :kind :kw) b)))
            :item-renderer
            (fn [{:keys [kind kw columns]} {:keys [handle-click modifiers ref id]}]
              [ui/menu-item
               {:id             id
                :ref            ref
                :role-structure :listoption
                :active         (:active modifiers)
                :multiline      true
                :text           [:div
                                 [:div {:class "u-flex-row-center-6"}
                                  [kinds/ident kind kw]
                                  [:span {:class "lineage-search__kind u-muted"}
                                   (kinds/label kind)]]
                                 (when (seq columns)
                                   [:div {:class "lineage-search__cols u-muted"}
                                    (str/join ", " (take 6 columns))])]
                :on-click       handle-click}])
            :no-results         [ui/menu-item
                                 {:disabled true
                                  :text     (i18n/t (cond (and col? (str/blank? @q))
                                                          :ui/find-type-a-column
                                                          col? :ui/find-no-column-match
                                                          :else :ui/find-no-node-match))}]
            :menu-props         {:class "lineage-panel__list"}
            :on-item-select     (fn [{:keys [kind kw]} _]
                                  (instances/focus-nodes! [(lp/node-id kind kw)]))
            :renderer
            (fn [{:keys [item-list query handle-query-change handle-key-down handle-key-up]}]
              [:div {:class "lineage-find"}
               [ui/input-group {:left-icon   :search
                                :placeholder (i18n/t (if col? :ui/find-by-column-hint
                                                         :ui/find-by-name-hint))
                                :value       query
                                :on-change   handle-query-change
                                :on-key-down handle-key-down
                                :on-key-up   handle-key-up}]
               item-list])}]]]))))

(def ^:private build-strategies
  [[:selected  :ui/strategy-selected  :ui/strategy-selected-desc]
   [:between   :ui/strategy-between   :ui/strategy-between-desc]
   [:ancestors :ui/strategy-ancestors :ui/strategy-ancestors-desc]])

(defn- preview-args
  [targets strategy]
  (cond-> {:only targets}
    strategy            (assoc :strategy strategy)
    (= :between strategy) (assoc :from targets)))

(defn- build-preview-body
  [{:keys [scope total]}]
  (let [{:keys [jobs levels unresolved strategy]} scope
        cost (or (:cost-next-run total) (:cost total))]
    [:div {:class "lineage-builds__section"}
     [:div {:class "lineage-builds__runs-label u-muted"}
      (str (i18n/t (if (= 1 jobs) :ui/jobs-count-one :ui/jobs-count-many) {:n jobs})
           (i18n/t :ui/strategy-suffix {:strategy (name (or strategy :default))})
           (when cost (str " · ≈ $" (.toFixed cost 2))))]
     (if (zero? (or jobs 0))
       [:div {:class "lineage-panel__empty u-muted"}
        (i18n/t :ui/build-nothing-to-do)]
       (for [[i level] (map-indexed vector levels)]
         ^{:key (str "lvl-" i)}
         [:div {:class "lineage-builds__run-row"}
          [:span {:class "u-muted"} (str "L" i)]
          [:span (str/join ", " (map name level))]]))
     (when (seq unresolved)
       [:div {:class "lineage-panel__empty lineage-status--failed"}
        (i18n/t :ui/build-unresolved {:list (str/join ", " (map name unresolved))})])]))

(declare target-names)

(defn- run-targets-label
  [{:keys [only strategy origin]}]
  (str (if (seq only)
         (target-names (map keyword only))
         (i18n/t :ui/run-targets-all))
       (when-let [strategy-label (some (fn [[k label]] (when (= (keyword strategy) k) label))
                                       build-strategies)]
         (str " · " (i18n/t strategy-label)))
       (when origin (str " · " origin))))

(def builds-panel
  (machine/entering
   lineage-model/lineage-frame [:lineage.builds/enter] [:lineage.builds/leave]
   (fn []
      (let [{:keys [busy? strategy preview]} (:builds @lineage-model/ui)
            runs    (data/run-list)
            lineage-status      @lineage-status/lineage-status
            nodes   (vals (:nodes lineage-status))
            built   (count (filter #(= "succeeded" (:build-status %)) nodes))
            stale   (count (filter :out-of-date? nodes))
            targets (lineage-model/build-targets-of (selection/selected-node-ids))
            scoped? (seq targets)
            args    (preview-args targets strategy)
            run!    #(lineage-model/send! [:lineage.builds/run %])]
        [:div {:class "lineage-panel"}
         [panel-header (i18n/t :ui/builds-manage)]
         [:div {:class "lineage-builds__section"}
          [:div {:class "lineage-builds__stats"}
           [:div [:div {:class "lineage-builds__stat-value"} built]
            [:div {:class "lineage-builds__stat-label u-muted"} (i18n/t :ui/built)]]
           [:div
            [:div {:class ["lineage-builds__stat-value" (when (pos? stale) "lineage-status")]}
             stale]
            [:div {:class "lineage-builds__stat-label u-muted"}
             (i18n/t :ui/out-of-date-caps)]]]
          (if-not scoped?
            [:div {:class "lineage-panel__empty u-muted"}
             (i18n/t :ui/build-need-selection)]
            [:div
             (into [ui/menu]
                   (for [[k label desc] build-strategies]
                     ^{:key (name k)}
                     [ui/menu-item
                      {:active    (= k strategy)
                       :multiline true
                       :on-click  #(lineage-model/send! [:lineage.builds/strategy k])
                       :text      [:<> [:div (i18n/t label)]
                                   [:div {:class "u-muted"} (i18n/t desc)]]}]))
             [:div {:class "lineage-builds__runs-label u-muted"}
              (str (i18n/t (if (= 1 (count targets)) :ui/targets-one :ui/targets-many)
                           {:n (count targets)})
                   ": " (str/join ", " (map (fn [[k kw]] (kinds/display-name k kw))
                                            (selected-build-refs
                                             (selection/selected-node-ids)))))]])]
         (when-let [pv preview]
           (if (= :loading pv)
             [:div {:class "lineage-panel__empty u-muted"} (i18n/t :ui/planning)]
             [build-preview-body pv]))
         [:div {:class "lineage-builds__section"}
          (cond
            (and scoped? (map? preview))
            [:div
             [ui/button {:intent :primary :disabled busy? :on-click (fn [_] (run! args))}
              (i18n/t (if busy? :ui/building :ui/run-build))]
             [ui/button {:disabled busy?
                         :on-click #(lineage-model/send! [:lineage.builds/cancel])}
              (i18n/t :ui/cancel)]]

            scoped?
            [ui/button
             {:intent :primary :disabled busy?
              :on-click #(lineage-model/send! [:lineage.builds/estimate args])}
             (i18n/t :ui/build-preview-next)]

            :else
            [ui/button {:intent :primary :disabled busy? :on-click (fn [_] (run! nil))}
             (i18n/t (if busy? :ui/building :ui/run-now-all))])]
         [:div {:class "lineage-builds__runs-label u-muted"}
          (i18n/t :ui/runs-recent)]
         [:div {:class "lineage-panel__list"}
          (if (seq runs)
            (for [{:keys [id kind status started-at targets]} (take 30 runs)]
              ^{:key (str id)}
              [:div {:class "lineage-builds__run"}
               [:div {:class "lineage-builds__run-row"}
                [:span (str (name (or kind "?"))
                            (when started-at
                              (str " · "
                                   (subs (str started-at) 0 (min 16 (count (str started-at)))))))]
                [:span {:class (str "u-bold lineage-status lineage-status--" status)} status]]
               (when targets
                 [:div {:class "lineage-builds__run-targets u-muted"}
                  (run-targets-label targets)])])
            [:div {:class "lineage-panel__empty u-muted"}
             (i18n/t :ui/runs-none-yet)])]]))))

(defn- schedule-targets [target]
  (into #{} (comp (map str/trim) (remove str/blank?) (map keyword))
        (str/split (str target) #",")))

(defn- target-names
  [targets]
  (let [by-kw (into {} (for [id (instances/canvas-node-ids)
                             :let [[k kw] (lp/parse-node-id id)]
                             :when (#{:dataset :object-type :transform} k)]
                         [kw k]))]
    (str/join ", " (for [kw (if (string? targets) (schedule-targets targets) targets)]
                     (kinds/display-name (get by-kw kw :dataset) kw)))))

(def schedules-panel
  (machine/entering
   lineage-model/lineage-frame [:lineage.schedules/enter] [:lineage.schedules/leave]
   (fn []
      (let [{items :items nm :name cron :cron} (:schedules @lineage-model/ui)
            field!  (fn [k] #(lineage-model/send! [:lineage.schedules/field k
                                                   (.. % -target -value)]))
            refs    (selected-build-refs (target-node-ids))
            sel     (set (map second refs))
            shown   (if (and (seq sel) (vector? items))
                      (filter #(seq (clojure.set/intersection sel (schedule-targets (:target %))))
                              items)
                      items)]
        [:div {:class "lineage-panel"}
         [panel-header (i18n/t :ui/schedules-manage)]
         [:div {:class "lineage-builds__section"}
          [:div {:class "lineage-builds__runs-label u-muted"}
           (if (seq refs)
             (i18n/t :ui/lin-schedule-targets
                     {:list (str/join ", " (map (fn [[k kw]] (kinds/display-name k kw)) refs))})
             (i18n/t :ui/lin-schedule-need-selection))]
          [ui/input-group {:value nm :placeholder (i18n/t :ui/schedule-name)
                           :on-change (field! :name)
                           :class "lineage-schedules__input" :fill true}]
          [ui/input-group {:value cron :placeholder (i18n/t :ui/cron-placeholder)
                           :on-change (field! :cron)
                           :class "lineage-schedules__input" :fill true}]
          [ui/button
           {:intent :primary :disabled (or (str/blank? nm) (empty? refs))
            :on-click #(lineage-model/send!
                        [:lineage.schedules/add
                         (str/join "," (map (comp name second) refs))])}
           (i18n/t :ui/schedule-add)]]
         [:div {:class "lineage-panel__list"}
          (cond
            (= :loading items) [busy/block {:class "lineage-panel__empty"} (i18n/t :ui/loading)]
            (empty? shown)      [:div {:class "lineage-panel__empty u-muted"}
                                 (i18n/t (if (seq sel) :ui/lin-schedules-none-selected
                                             :ui/schedules-none))]
            :else
            (for [{:keys [id name cron target last-fired-at paused scope latest-run]
                   :as sch} shown]
              ^{:key id}
              [:div {:class "lineage-schedules__row"}
               [:div {:class "u-bold"}
                name (when paused [:span {:class "u-muted"} " · ⏸"])]
               [:div
                {:class "u-muted u-truncate" :title (str target)}
                (str (or (:when sch) cron "—") " · "
                     (if (str/blank? (str target))
                       (i18n/t :ui/whole-ontology)
                       (target-names target)))]
               (when scope
                 [:div {:class "lineage-builds__stat-label u-muted"}
                  (str (i18n/t :ui/lin-scope) ": " (clojure.core/name scope))])
               (when-let [{:keys [status at]} latest-run]
                 [:div {:class (str "lineage-builds__stat-label u-muted lineage-status--"
                                   status)}
                  (str (i18n/t :ui/lin-latest-run) ": " status (when at (str " · " at)))])
               (when (and last-fired-at (not latest-run))
                 [:div {:class "lineage-builds__stat-label u-muted"}
                  (i18n/t :ui/last-fired {:at last-fired-at})])
               [:div {:class "lineage-schedules__actions"}
                [:span {:class "lineage-schedules__run"
                        :on-click #(lineage-model/send! [:lineage.schedules/run id])}
                 (i18n/t :ui/run-now)]
                [:span
                 {:class "lineage-schedules__delete"
                  :on-click #(lineage-model/send! [:lineage.schedules/delete id name])}
                 (i18n/t :ui/delete-lower)]]]))]]))))

(defn- related-panel
  []
  (let [{:keys [kind id]} (:detail-modal @shell/ui-state)
        {:keys [loading? error items]} (panel/panel :related kind id)
        filt (or (:related-filter @lineage-model/ui) "all")
        item-kinds (into (sorted-set) (map :kind) items)
        shown (if (= "all" filt) items (filter #(= filt (:kind %)) items))
        on-graph (instances/canvas-node-ids)]
    [:div {:class "lineage-panel"}
     [panel-header (str (i18n/t :ui/related-items) (when (seq items) (str " (" (count items) ")")))]
     (cond
       (nil? id)
       [:div {:class "lineage-panel__empty u-muted"}
        (i18n/t :ui/related-need-selection-dot)]

       loading?
       [busy/block {:class "lineage-panel__empty"} (i18n/t :ui/loading)]

       error
       [:div {:class "lineage-panel__empty u-muted"}
        (i18n/t :ui/related-unavailable {:error error})]

       (empty? items)
       [:div {:class "lineage-panel__empty u-muted"}
        (i18n/t :ui/related-nothing {:name (kinds/display-name kind id)})]

       :else
       [:<>
        [:div {:class "lineage-builds__section"}
         [ui/html-select {:value filt
                          :on-change #(lineage-model/edit! [] assoc :related-filter
                                             (.. % -target -value))
                          :class "lineage-schedules__input" :fill true}
          [:option {:value "all"} (i18n/t :ui/lin-related-all {:n (count items)})]
          (for [k item-kinds]
            ^{:key k}
            [:option {:value k}
             (str (kinds/label k) " (" (count (filter #(= k (:kind %)) items)) ")")])]]
        [:div {:class "lineage-panel__list"}
         (for [it shown
               :let [kind  (:kind it)
                     name* (:name it)
                     via   (:via it)
                     nid   (lp/node-id (keyword kind) (keyword name*))
                     here? (contains? on-graph nid)]]
           ^{:key (str kind "/" name* "/" via)}
           [ui/tooltip
            {:content (i18n/t (if here? :ui/node-zoom-to :ui/node-add-to-graph))}
            [:div
             {:class "panel-related__row"
              :on-click (fn [_]
                          (if here?
                            (instances/focus-nodes! [nid])
                            (do (lineage-model/seed-lineage-ref! [(keyword kind) (keyword name*)])
                                (instances/focus-nodes! [nid]))))}
             [:span {:class "panel-related__kind"
                     :title (kinds/label kind)
                     :style {:color (or (colors/node-kind-color (kinds/kind-of kind)) gs/grey)}}
              (kinds/glyph kind name*)]
             [:span {:class "u-flex-1" :title name*} (kinds/display-name kind name*)]
             [:span {:class "panel-related__via"} via]
             (when-not here?
               [ui/icon {:icon :plus :class "panel-related__add"}])]])]])]))

(defn- histo-bar [label n total color]
  [:div {:class "lineage-properties__histo-row"}
   [:div {:class "lineage-properties__histo-head"}
    [:span label] [:span n]]
   [:div {:class "lineage-properties__histo-track"}
    [:div
     {:class "lineage-properties__histo-fill"
      :style {:background color :width (str (if (pos? total) (* 100 (/ n total)) 0) "%")}}]]])

(defn- selection-histogram-block
  [facets]
  (let [total (count (selection/selected-node-ids))]
    [:div {:class "lineage-properties__block"}
     [:div {:class "lineage-properties__title"}
      (i18n/t (if (= 1 total) :ui/nodes-selected-one :ui/nodes-selected-many) {:n total})
      [ui/tooltip {:content (i18n/t :ui/copy-names-hint)}
       [:span {:class "lineage-properties__copy"
               :on-click (fn [_]
                           (some-> js/navigator .-clipboard
                                   (.writeText (lineage-model/selection-names))))}
        (i18n/t :ui/copy-names)]]]
     (when-let [{:keys [prop value ids]} (lineage-model/highlighted-bucket)]
       [:div {:class "lineage-properties__update"}
        [:span {:class "u-muted"}
         (str prop " = " value " · " (count ids))]
        [ui/button {:small true :intent :primary
                    :on-click (fn [_] (lineage-model/update-selection-to-highlight!))}
         (i18n/t :ui/update-selection)]])
     (for [[prop buckets] facets
           :when (> (count buckets) 0)]
       ^{:key prop}
       [:div
        [:div {:class "lineage-properties__histo-label u-muted"} prop]
        (for [[value ids] (take 12 buckets)
              :let [hl (lineage-model/highlighted-bucket)
                    on? (and (= prop (:prop hl)) (= value (:value hl)))]]
          ^{:key (str prop "/" value)}
          [:div {:class ["lineage-properties__histo-row"
                         (when on? "lineage-properties__histo-row--on")]
                 :title (i18n/t :ui/histogram-bucket-hint)
                 :on-click #(lineage-model/highlight-bucket! prop value ids)}
           [:div {:class "lineage-properties__histo-head"}
            [:span value] [:span (count ids)]]
           [:div {:class "lineage-properties__histo-track"}
            [:div {:class "lineage-properties__histo-fill"
                   :style {:width (str (* 100 (/ (count ids) (max 1 total))) "%")}}]]])
        (when (> (count buckets) 12)
          [:div {:class "lineage-panel__empty u-muted"}
           (i18n/t :ui/more-values {:n (- (count buckets) 12)})])])]))

(defn- producing-transforms
  [kind id]
  (if (= :transform kind)
    [id]
    (->> (:upstream (lineage/lineage-neighbors @catalog/schema (lp/node-id kind id)))
         (keep (fn [[k t]] (when (= :transform k) t)))
         distinct
         vec)))

(defn- open-in-explorer! [ot]
  (when (nil? (get @panel/panel-instances ot))
    (panel/refresh-panel-instances! ot {:limit 1000}))
  (lineage-model/edit! [] assoc :right-panel nil)
  (shell/enter! :object-workspace {:object-type ot}))

(defn- open-in-repository! [t]
  (lineage-model/edit! [] assoc :right-panel nil)
  (shell/enter! :repository {:tab :code :selected t}))

(defn- node-actions
  [kind id]
  (let [rid (get-in @catalog/canonical-schema
                    [(case kind :dataset :datasets :object-type :object-types
                           :transform :transforms :datasource :sources
                           :action :actions :application :applications nil)
                     id :id])
        producer (first (producing-transforms kind id))
        rows (cond-> []
               (= :object-type kind)
               (conj [(i18n/t :ui/open-in {:app "Object Workspace"}) #(open-in-explorer! id)]
                     [(i18n/t :ui/open-in {:app "Ontology Editor"})
                      #(do (lineage-model/edit! [] assoc :right-panel nil)
                           (catalog/open-resource! :object-types id))])
               (= :action kind)
               (conj [(i18n/t :ui/open-in {:app "Ontology Editor"})
                      #(do (lineage-model/edit! [] assoc :right-panel nil)
                           (catalog/open-resource! :actions id))])
               (= :application kind)
               (conj [(i18n/t :ui/open-in {:app "App Studio"})
                      #(do (lineage-model/edit! [] assoc :right-panel nil)
                           (catalog/open-resource! :applications id))])
               (= :datasource kind)
               (conj [(i18n/t :ui/open-in {:app "Source Manager"})
                      #(do (lineage-model/edit! [] assoc :right-panel nil)
                           (shell/enter! :source-manager))])
               producer
               (conj [(i18n/t :ui/open-in-the-repository) #(open-in-repository! producer)])
               rid
               (conj [(i18n/t :ui/copy-resource-id)
                      #(some-> js/navigator .-clipboard (.writeText (str rid)))]))]
    [:div {:class "lineage-properties__actions"}
     [:div {:class "lineage-properties__histo-label u-muted"} (i18n/t :ui/actions)]
     (into [ui/menu]
           (for [[l f d] (remove nil? rows)]
             [ui/menu-item {:text l :on-click #(f) :intent (when d :danger)}]))]))

(defn- fmt-ms
  [ms]
  (cond
    (nil? ms)    "—"
    (< ms 1000)  (str ms " ms")
    (< ms 60000) (str (.toFixed (/ ms 1000) 1) " s")
    :else        (str (js/Math.floor (/ ms 60000)) "m "
                      (js/Math.round (/ (mod ms 60000) 1000)) "s")))

(defn- kv-list
  [rows]
  [display/property-list
   (for [[label v] rows] {:prop label :label (i18n/t label) :render v})
   {:label-position :beside}])

(defn- action-properties
  [id]
  (let [kw    (keyword id)
        edits (sort (get (lineage/lineage-action-edits) kw))
        rules (get-in @catalog/canonical-schema [:actions kw :rules])]
    [:div {:class "lineage-properties__block"}
     [:div {:class "lineage-properties__title"}
      [kinds/ident :action id]
      [:span {:class "u-muted u-ml-6"}
       (kinds/label :action)]]
     [kv-list [[:ui/project (catalog/node-project :action kw)]
               [:ui/action-edits (when (seq edits) (str/join ", " (map name edits)))]
               [:ui/action-rules (str (count rules))]]]
     [node-actions :action kw]]))

(defn- application-properties
  [id]
  (let [kw   (keyword id)
        uses (sort (get (lineage/lineage-app-uses) kw))
        st   (lineage-status/node-status :application kw)]
    [:div {:class "lineage-properties__block"}
     [:div {:class "lineage-properties__title"}
      [kinds/ident :application id]
      [:span {:class "u-muted u-ml-6"}
       (kinds/label :application)]]
     [kv-list (cond-> [[:ui/project (catalog/node-project :application kw)]
                       [:ui/object-types (when (seq uses) (str/join ", " (map name uses)))]]
                (pos? (:issues st 0)) (conj [:ui/lin-open-issues (str (:issues st))]))]
     [node-actions :application kw]]))

(defn- fact-node-link
  [fact-id]
  (if-let [[k kw] (lp/parse-node-id fact-id)]
    (let [nid (lp/node-id k kw)]
      [ui/tooltip {:content (i18n/t :ui/node-zoom-to)}
       [:span {:class "lineage-properties__link"
               :on-click (fn [_]
                           (when-not (contains? (instances/canvas-node-ids) nid)
                             (lineage-model/seed-lineage-ref! [k kw]))
                           (instances/focus-nodes! [nid]))}
        (kinds/display-name k kw)]])
    [:span (str fact-id)]))

(defn- fact-node-links
  [ids]
  (when (seq ids)
    (into [:span] (interpose ", " (map fact-node-link ids)))))

(defn- prefixed [kind names] (map #(str kind "/" %) names))

(defn- freshness-cell
  [{:keys [freshness logic?]}]
  (when freshness
    [:span {:style {:color (get colors/freshness-colors freshness)}}
     (str (get colors/freshness-labels freshness freshness)
          (when (and logic? (not= "logic" freshness)) (str " · " (i18n/t :ui/lin-logic))))]))

(defn- schedules-cell
  [{:keys [schedules paused-schedules schedule-names]}]
  (str (or schedules 0)
       (when (pos? (or paused-schedules 0))
         (str " (" (i18n/t :ui/lin-paused-n {:n paused-schedules}) ")"))
       (when (seq schedule-names) (str " · " (str/join ", " schedule-names)))))

(defn- error-cell
  [{e :error}]
  (when e [:span {:class "lineage-status--failed"} (str e)]))

(defn- model-calls-cell
  [{u :model-usage}]
  (when u
    (i18n/t :ui/model-calls-breakdown {:ok (:successful u) :limited (:rate-limited u)
                                       :failed (:failed u) :attempted (:attempted u)})))

(def ^:private property-rows
  [[:ui/project            (fn [k id _] (catalog/node-project k id))                    true]
   [:ui/lin-resource       (fn [_ _ s] (:resource s))                                  false]
   [:ui/lin-repository     (fn [_ _ s] (:repository s))                                false]
   [:ui/lin-compute        (fn [_ _ s] (:compute s))                                   false]
   [:ui/lin-write-mode     (fn [_ _ s] (:write-mode s))                                false]
   [:ui/lin-storage        (fn [_ _ s] (:storage s))                                   false]
   [:ui/lin-jobspec        (fn [_ _ s] (some->> (:jobspec s) (str "JobSpec on ")))     false]
   [:ui/columns            (fn [k id _] (str (count (catalog/node-columns k id))))      true]
   [:ui/build-status       (fn [_ _ s] (:build-status s))                              true]
   [:ui/lin-freshness      (fn [_ _ s] (freshness-cell s))                             false]
   [:ui/lin-changed-inputs (fn [_ _ s] (fact-node-links (:changed s)))                 false]
   [:ui/lin-behind         (fn [_ _ s] (fact-node-links (:behind s)))                  false]
   [:ui/last-build         (fn [_ _ s] (:last-build s))                                true]
   [:ui/duration           (fn [_ _ s] (some-> (:duration-ms s) fmt-ms))               true]
   [:ui/rows               (fn [_ _ s] (some-> (:rows s) str))                         true]
   [:ui/row-delta          (fn [_ _ s] (some-> (:row-delta s) str))                    false]
   [:ui/last-write         (fn [_ _ s] (:last-write s))                                false]
   [:ui/created            (fn [_ _ s] (:created-at s))                                false]
   [:ui/transactions       (fn [_ _ s] (some-> (:transactions s) str))                 false]
   [:ui/model-calls        (fn [_ _ s] (model-calls-cell s))                           false]
   [:ui/error              (fn [_ _ s] (error-cell s))                                 false]
   [:ui/schedules          (fn [_ _ s] (schedules-cell s))                             true]
   [:ui/health             (fn [_ _ s] (some-> s :health :status name))               true]
   [:ui/lin-open-issues    (fn [_ _ s] (when (pos? (:issues s 0)) (str (:issues s))))  false]
   [:ui/lin-defines        (fn [_ _ s] (fact-node-links (prefixed "object-type" (:defines s))))
    false]
   [:ui/lin-backed-by      (fn [_ _ s] (fact-node-links (prefixed "dataset" (:backed-by s))))
    false]
   [:ui/syncs              (fn [_ _ s] (some->> (:sync-names s) seq (str/join ", ")))  false]
   [:ui/ge-trashed         (fn [_ _ s] (when (:trashed? s) [ui/icon {:icon :trash}]))  false]])

(defn- property-table
  [kind id status]
  [kv-list (for [[label cell always?] property-rows
                 :let [v (cell kind id status)]
                 :when (or always? (some? v))]
             [label v])])

(defn- properties-panel []
  (let [status    @lineage-status/lineage-status
        nodes (vals (:nodes status))
        total (count nodes)
        sel-id (selected-lineage-node-id)
        {:keys [kind id]} (:detail-modal @shell/ui-state)
        sel   (when (and kind id) (lineage-status/node-status kind id))
        facets (lineage-model/selection-histogram)
        by-status (frequencies (map #(or (:build-status %) "never") nodes))
        by-fresh  (frequencies (keep :freshness nodes))]
    [:div {:class "lineage-panel"}
     [panel-header (i18n/t :ui/properties)]
     [:div {:class "lineage-properties__body"}
      (cond
        facets [selection-histogram-block facets]

        (= :action kind)
        [action-properties id]

        (= :application kind)
        [application-properties id]

        sel
        [:div {:class "lineage-properties__block"}
         [ui/tooltip {:content sel-id}
          [:div {:class "lineage-properties__title"}
           [kinds/ident kind id]
           [:span {:class "u-muted u-ml-6"}
            (kinds/label kind)]]]
         [property-table kind id sel]
         [node-actions kind id]]

        :else
        [:div {:class "lineage-properties__block u-muted"}
         (i18n/t :ui/properties-need-selection)])
      [:div {:class "lineage-properties__histo-label u-muted"}
       (i18n/t :ui/build-status-over-nodes {:n total})]
      [histo-bar (i18n/t :ui/status-succeeded) (get by-status "succeeded" 0) total
       "var(--c-success)"]
      [histo-bar (i18n/t :ui/never-built) (get by-status "never" 0) total "var(--gray5)"]
      (for [[w label color] (rest colors/freshness-states)
            :let [n (get by-fresh w 0)]
            :when (pos? n)]
        ^{:key w} [histo-bar label n total color])]]))

(defn- right-panel []
  (case (:tab (:right-panel @lineage-model/ui))
    :search     [search-panel]
    :builds     [builds-panel]
    :schedules  [schedules-panel]
    :properties [properties-panel]
    :related    [related-panel]
    nil))

(defn top-bar
  []
  [:<>
   [toolbar-buttons]
   [groups-chip]])

(def overlays
  [lineage-seed-box branch-modified-callout color-legend shortcuts-modal])

(def detail
  {:view   right-panel
   :open?  #(some? (:tab (:right-panel @lineage-model/ui)))
   :close! close-panel!})
