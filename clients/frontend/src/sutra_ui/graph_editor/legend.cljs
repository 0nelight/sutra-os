(ns sutra-ui.graph-editor.legend
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.graph-editor.events]
            [sutra.viz.palette :as palette]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.selection :as selection]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.feature :as feature]))

(def lineage-kinds
  [:datasource :dataset :writeback :object-type :action :application])

(defn- kind-rows
  [kinds counts]
  (for [k kinds
        :let [{:keys [label color icon]} (get palette/node-kinds k)
              n (get counts k)]
        :when (and label (or (nil? counts) (pos? (or n 0))))]
    {:label (cond-> label n (str " (" n ")")) :color color :icon icon}))

(defn- scheme-rows
  []
  (let [scheme (colors/current-color-scheme)
        {:keys [kind colors]} (:rules scheme)]
    (if (and (= kind :by-type) (seq colors))
      (concat (for [[type-kw color] colors]
                {:label (name type-kw) :color color})
              [{:label "Other types (auto)" :color (colors/hash-color :other)}])
      [{:label (if scheme
                 (str "Scheme: " (or (:name scheme) "unnamed"))
                 "No scheme — types auto-coloured")
        :color (colors/hash-color :other)}])))

(defn- declared-rows
  [k none-label]
  (let [sch  @catalog/canonical-schema
        vals* (into (sorted-set)
                    (comp (mapcat vals) (keep k) (map name))
                    (vals (select-keys sch [:object-types :actions :functions
                                            :applications :link-types :transforms
                                            :datasets :agents])))]
    (cond-> (vec (for [v vals*] {:label v :color (colors/hash-color (keyword v))}))
      none-label (conj {:label none-label :color "#c5cbd3"}))))

(defn- rows-for
  [mode kinds counts]
  (case mode
    :resource-kind  (kind-rows kinds counts)
    :build-status   [{:label "Succeeded" :color (get colors/build-status-colors "succeeded")}
                     {:label "Failed"    :color (get colors/build-status-colors "failed")}
                     {:label "Running"   :color (get colors/build-status-colors "running")}
                     {:label "Waiting"   :color (get colors/build-status-colors "waiting")}
                     {:label "Skipped"   :color (get colors/build-status-colors "skipped")}
                     {:label "Aborted"   :color (get colors/build-status-colors "aborted")}
                     {:label "Never built / unknown" :color colors/build-status-none}]
    :build-duration (conj (vec (for [[_ label color] colors/duration-buckets]
                                 {:label label :color color}))
                          {:label "Running / not measured" :color colors/build-status-none})
    :last-built     (conj (vec (for [[_ label color] colors/freshness-buckets]
                                 {:label label :color color}))
                          {:label "Never built" :color colors/build-status-none})
    :model-usage    (vec (for [[_ label color] colors/model-usage-buckets]
                           {:label label :color color}))
    :out-of-date    (conj (vec (for [[_ label color] colors/freshness-states]
                                 {:label label :color color}))
                          {:label "Data source" :color (colors/node-kind-color :datasource)})
    :data-health    [{:label "Passed"  :color (get colors/health-colors "passed")}
                     {:label "Warning" :color (get colors/health-colors "warning")}
                     {:label "Failed"  :color (get colors/health-colors "failed")}
                     {:label "No checks" :color (get colors/health-colors nil)}]
    :schedule-count [{:label "0 schedules" :color (colors/schedule-color 0)}
                     {:label "1"           :color (colors/schedule-color 1)}
                     {:label "2"           :color (colors/schedule-color 2)}
                     {:label "3+"          :color (colors/schedule-color 3)}]
    :scheme         (scheme-rows)
    :ontology-status (conj (vec (for [[s c] colors/ontology-status-colors]
                                  {:label (str/capitalize s) :color c}))
                           {:label "Undeclared" :color colors/ontology-status-unknown})
    :folder          (declared-rows :project "No project")
    :function-repo   (declared-rows :repository "No repository")
    :app-views       [{:label "Published" :color (get colors/app-view-colors "published")}
                      {:label "Draft — no views by construction"
                       :color (get colors/app-view-colors "draft")}]
    :resource-permissions
    (conj (declared-rows :project nil)
          {:label "Unowned — inherits no project grant"
           :color (colors/resource-permission-color nil)})
    (or (colors/categorical-legend mode) [])))

(def mode-labels
  (merge (into {} (for [[k {:keys [label]}] colors/categorical-color-modes] [k label]))
         {:resource-kind        "Node type"
          :build-status         "Build status"
          :build-duration       "Build duration"
          :last-built           "Last built"
          :model-usage          "Model usage"
          :out-of-date          "Out of date"
          :data-health          "Data health"
          :schedule-count       "Schedules"
          :scheme               "Color scheme"
          :ontology-status      "Ontology status"
          :folder               "Folder"
          :function-repo        "Functions repository"
          :app-views            "Application views"
          :resource-permissions "Resource permissions"}))

(defn- mode-label [mode]
  (get mode-labels mode "Legend"))

(defn- color-group-rows
  []
  (let [canvas (shell/canvas-mode)
        groups (canvas/mode-color-groups canvas)]
    [:<>
     [:div {:class "lineage-legend__header"} (i18n/t :ui/ge-colors-header)]
     (for [g groups]
       ^{:key (:id g)}
       [ui/tooltip
        {:content (i18n/t :ui/ge-select-group {:n (count (:members g))})}
        [:div {:class "lineage-legend__row u-pointer"
               :on-click #(selection/select-nodes! (set (:members g)))}
         [:span {:class "lineage-legend__swatch" :style {:background (:color g)}}]
         [:span {:class "lineage-legend__icon" :style {:color (:color g)}} "◧"]
         [:span (str (:name g) " · " (count (:members g)))]]])
     [ui/tooltip {:content (i18n/t :ui/ge-add-color-hint)}
      [:div {:class "lineage-legend__row u-pointer"
             :on-click
             (fn [_]
               (let [ids (vec (selection/selected-node-ids))]
                 (db/send! [:shared.graph/new-color-group
                            (if (seq ids)
                              (i18n/t :ui/ge-add-color-for {:n (count ids)})
                              (i18n/t :ui/ge-add-color-empty))
                            (count groups) ids])))}
       [:span {:class "lineage-legend__swatch" :style {:background "transparent"
                                                       :border "1px dashed var(--c-muted-2)"}}]
       [:span {:class "lineage-legend__icon"} "＋"]
       [:span (i18n/t :ui/ge-add-color)]]]]))

(defn legend
  ([kinds] (legend kinds nil))
  ([kinds {:keys [class counts]}]
   (let [mode   (:color-mode @shell/ui-state :resource-kind)
         canvas (shell/canvas-mode)
         rows   (rows-for mode kinds counts)]
     (when (or (seq rows) (feature/capable? canvas :color-groups))
       (into [:div {:class (str "lineage-legend" (when class (str " " class)))
                    :on-click #(.stopPropagation %)}
              [:div {:class "lineage-legend__header"} (mode-label mode)]]
             (concat
              (for [{:keys [label color icon]} rows]
                ^{:key label}
                [:div {:class "lineage-legend__row"}
                 [:span {:class "lineage-legend__swatch" :style {:background color}}]
                 [:span {:class "lineage-legend__icon" :style {:color color}}
                  (or icon "")]
                 [:span label]])
              (when (feature/capable? canvas :color-groups)
                [^{:key "color-groups"} [color-group-rows]])))))))
