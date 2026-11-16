(ns sutra-ui.graph-editor.edges
  (:require [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra-ui.graph-editor.events]
            [sutra-ui.graph-editor.shared :as gs]
            [sutra-ui.state.db :as db]
            ["reactflow" :refer [BaseEdge EdgeLabelRenderer
                                 getBezierPath useStore]]
            ["react" :as react]))

(defn floating-edge-impl
  [cfg props]
  (let [source-id (.-source props)
        target-id (.-target props)
        edge-id   (.-id props)
        data      (.-data props)
        label     (.-label data)
        card      (.-cardinality ^js data)
        staged?   (.-staged? ^js data)
        source-node (useStore (react/useCallback
                               (fn [^js s] (.get (.-nodeInternals s) source-id))
                               #js [source-id]))
        target-node (useStore (react/useCallback
                               (fn [^js s] (.get (.-nodeInternals s) target-id))
                               #js [target-id]))]
    (if (and source-node target-node
             (.-width source-node) (.-width target-node))
      (let [{:keys [sx sy sp tx ty tp]} (gs/get-edge-params source-node target-node)
            [edge-path label-x label-y]
            (getBezierPath #js {:sourceX sx :sourceY sy :sourcePosition sp
                                :targetX tx :targetY ty :targetPosition tp})
            font-size (or (:font-size cfg) 11)
            small?    (= font-size 10)
            on-delete (:on-delete cfg)
            handle-delete
            (fn [e]
              (.stopPropagation e)
              (if (:confirm-delete? cfg)
                (db/send! [:shared.graph/confirm
                           {:title   (i18n/t :ui/ge-delete-link-title {:name label})
                            :body    (i18n/t :ui/om-delete-staged-body)
                            :danger? true}
                           on-delete edge-id data])
                (on-delete edge-id data)))]
        (r/as-element
         [:<>
          [:> BaseEdge
           {:path edge-path
            :markerEnd (.-markerEnd props)
            :style #js {:stroke      (if staged? gs/green (:stroke cfg))
                        :strokeWidth (if staged?
                                       (or (:staged-width cfg) 2.5)
                                       (:width cfg))
                        :strokeDasharray (if staged?
                                           (or (:staged-dasharray cfg) "8,5")
                                           (:dasharray cfg))}}]
          (when (or (seq label) (seq card) on-delete)
           [:> EdgeLabelRenderer
           [:div {:style (assoc gs/edge-label-style
                                :transform (str "translate(-50%,-50%) translate("
                                                label-x "px," label-y "px)")
                                :padding (if small? "2px 6px" "3px 8px")
                                :font-size (str font-size "px")
                                :gap (if small? "5px" "6px")
                                :box-shadow (if small?
                                              "0 1px 3px rgba(17,20,24,0.06)"
                                              "0 1px 4px rgba(17,20,24,0.08)"))}
            [:div
             [:div {:class "graph-edge__label"} label]
             (when (seq card)
               [:div {:style {:color (if small? "var(--c-muted-2)" "var(--c-muted)")
                              :font-size (str (dec font-size) "px")}}
                card])]
            (when on-delete
              [ui/tooltip {:content (i18n/t :ui/delete)}
               [:button {:on-click handle-delete
                         :style (assoc gs/edge-delete-btn-style
                                       :font-size (if small? "12px" "14px"))}
                "×"]])]])]))
      (r/as-element [:g]))))
