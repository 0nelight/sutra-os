(ns sutra-ui.apps.lineage.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.lineage :as lineage-state]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.lineage.graph :as lgraph]
            [sutra-ui.apps.lineage.core :as lineage]))

(def ^:private glyph
  (feature/icon
   {:stroked? false}
   [:line {:x1 5 :y1 10 :x2 10 :y2 10 :stroke "currentColor" :stroke-width 1.4}]
   [:line {:x1 10 :y1 10 :x2 15 :y2 10 :stroke "currentColor" :stroke-width 1.4}]
   [:circle {:cx 4 :cy 10 :r 2.4 :fill "none" :stroke "currentColor" :stroke-width 1.4}]
   [:rect {:x 8 :y 8 :width 4 :height 4 :fill "none" :stroke "currentColor" :stroke-width 1.4}]
   [:circle {:cx 16 :cy 10 :r 2.4 :fill "currentColor"}]))

(defn- view-breadcrumb
  []
  (when-let [v @canvas/current-view]
    (when-let [{:keys [view]} (first (feature/contributions :resource-path))]
      [view (str "view/" (:id v))])))

(add-watch canvas/current-view ::status
           (fn [_ _ before after]
             (when (and after (not= (:id before) (:id after)) (= :data-lineage (shell/current-app)))
               (lineage-status/ensure-lineage-status!))))

(feature/register-app!
 {:id :data-lineage :order 5 :label-key :ui/app-data-lineage :glyph glyph :branching? true})

(feature/register-screen!
 {:id :lineage-canvas :app :data-lineage :segment "data-lineage" :aliases ["lineage-canvas"]
  :label-key :ui/app-data-lineage :glyph glyph
  :canvas {:mode :lineage :positions :schema
           :capabilities #{:undo-redo :delete-key :remove-tool :cursor-mode
                           :color-groups :expand}
           :graph (fn [{:keys [schema positions show-fields? ctrl-held?]}]
                    [(lgraph/lineage->rf-nodes schema positions show-fields? ctrl-held?)
                     (lgraph/lineage->rf-edges schema)])
           :header view-breadcrumb :top-bar lineage/top-bar :overlays lineage/overlays
           :detail lineage/detail
           :settings (fn [{:keys [drag-select?]}] {:drag-select? drag-select?})}
  :on-enter (fn [_]
              (kompass/refresh-kompass-nodes!)
              (when @canvas/current-view (lineage-status/ensure-lineage-status!))
              (lineage-state/refresh-lineage-graph!)
              (branches/refresh-branch-changes!))})

(defn- open-in-lineage!
  [ref]
  (shell/enter! :lineage-canvas)
  (graph/add-to-lineage-scope! [ref]))

(feature/contribute!
 :resource-opener [:data-lineage :transforms]
 {:family :transforms :app :data-lineage :order 100
  :open! (fn [nm _]
           (shell/enter! :lineage-canvas)
           (panel/open-bottom-panel! :transform nm))})

(feature/contribute!
 :node-menu :open-in-lineage
 {:label-key :ui/ge-open-in-lineage
  :when (fn [{:keys [type multi? instances? mode]}]
          (and (= "objectType" type) (not multi?) (not instances?) (not= :lineage mode)))
  :run (fn [{:keys [id]}] (open-in-lineage! [:object-type (keyword id)]))})

(feature/contribute! :command :open-in-lineage {:run! open-in-lineage!})
