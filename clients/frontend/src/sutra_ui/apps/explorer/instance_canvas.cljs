(ns sutra-ui.apps.explorer.instance-canvas
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra.capps.standard-view :as sv]
            [sutra-ui.object.card :as oc]
            [sutra-ui.render-ctx :as rctx]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.selection :as selection]
            [sutra-ui.apps.explorer.model :as explorer-model]))

(defn- instance-filter-banner
  []
  (let [vis @instances/instance-visible-ids]
    (when vis
      (let [n (count vis)]
        [:div {:class "explorer-canvas__filter-banner"}
         [:span (i18n/t :ui/ox-canvas-filtered {:n n})]
         [ui/button {:minimal true :small true :on-click #(instances/clear-instance-filter!)}
          (i18n/t :ui/ox-canvas-show-all)]]))))

(defn- seed-search []
  (let [{:keys [q results busy?]} (:seed @explorer-model/ui)
        send! explorer-model/send!]
    [:div {:class "vx-panel__search"}
     [:div {:class "u-flex-gap-6"}
      [ui/input-group {:value (or q "")
                       :left-icon "search"
                       :placeholder (i18n/t :ui/ox-canvas-search)
                       :on-change #(send! [:explorer.seed/type (.. % -target -value)])
                       :on-key-down #(when (= "Enter" (.-key %))
                                       (send! [:explorer.seed/now]))
                       :class "explorer-canvas__seed-input"}]
      [ui/tooltip {:content (i18n/t :ui/ox-canvas-clear-hint)}
       [ui/button
        {:minimal true :small true
         :on-click (fn [_] (instances/clear-instances!) (send! [:explorer.seed/done false]))}
        (i18n/t :ui/ox-canvas-clear)]]]
     (when busy?
       [:div {:class "explorer-canvas__seed-busy"}
        (i18n/t :ui/ox-searching)])
     (when (and results (not busy?))
       [:div {:class "explorer-canvas__seed-results"}
        (if (empty? results)
          [:div {:class "explorer-canvas__seed-empty"}
           (i18n/t :ui/ox-no-matches)]
          (for [{:keys [type-ref row]} results]
            ^{:key (str type-ref "/" (:id row))}
            [:div {:class "sutra-menu-row"
                   :on-click (fn [_]
                               (canvas/seed-add! type-ref row)
                               (send! [:explorer.seed/done true]))}
             [:span {:class "u-bold"} (catalog/instance-display-name row)]
             [:span {:class "explorer-canvas__seed-type"}
              (oc/type-label (catalog/name-of type-ref))]]))])]))

(defn- selected-object
  []
  (let [sel (selection/selected-node-ids)]
    (when (= 1 (count sel))
      (let [id (first sel)]
        (some (fn [[t rows]]
                (some #(when (= id (str (:id %))) [t %]) rows))
              (:objects @instances/instances))))))

(defn- matches? [q {:keys [label text]}]
  (or (str/blank? q)
      (some #(str/includes? (str/lower-case (str %)) (str/lower-case q)) [label text])))

(defn- selection-card [_t _row]
  (let [q (r/atom "")]
    (fn [t row]
      (let [meta    (oc/type-meta t)
            id      (str (:id row))
            sym     (:symbol meta)
            entries (sv/property-entries row (:display meta) {} (rctx/current))]
        [:div {:class "vx-selection"}
         [:div {:class "vx-selection__head"}
          [:span {:class (str "object-instance__tile vx-selection__tile"
                              (when sym " object-instance__tile--symbol"))
                  :style {"--tile" (colors/type-color t row)}}
           (or sym (some-> (oc/type-label meta) (subs 0 1) str/upper-case))]
          [:span {:class "object-instance__text"}
           [:span {:class "vx-selection__title"} (oc/title meta row id)]
           [:span {:class "object-instance__type"} (oc/type-label meta)]]]
         [:div {:class "vx-selection__actions"}
          [ui/button {:small true :icon "document-open"
                      :on-click #(panel/open-bottom-panel! :object-instance id)}
           (i18n/t :ui/ox-canvas-open)]
          [ui/tooltip {:content (i18n/t :ui/ox-canvas-expand-hint)}
           [ui/button {:small true :icon "graph"
                       :on-click #(canvas/expand-around! id)}
            (i18n/t :ui/ox-canvas-expand)]]]
         [ui/input-group {:value @q :small true
                          :placeholder (i18n/t :ui/ox-canvas-filter)
                          :on-change #(reset! q (.. % -target -value))}]
         (into [:dl {:class "vx-selection__props"}]
               (for [{:keys [prop label text] :as e} entries
                     :when (matches? @q e)]
                 ^{:key (str prop)}
                 [:div {:class "vx-selection__prop"}
                  [:dt label]
                  [:dd (if (seq text) text "—")]]))]))))

(defn- vertex-panel []
  (let [[t row] (selected-object)]
    [:div {:class "vx-panel"
           :on-click #(.stopPropagation %)
           :on-context-menu #(.preventDefault %)}
     [seed-search]
     (when row
       ^{:key (str (:id row))} [selection-card t row])]))

(def overlays
  [vertex-panel instance-filter-banner])
