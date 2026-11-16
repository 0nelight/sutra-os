(ns sutra-ui.apphost.widgets.image-annotation
  (:require [sutra-ui.ui.sel :as sel]
            [reagent.core :as r]
            [sutra.capps.image-annotation :as ia]
            [sutra.capps.variables :refer [deref*]]
            [sutra-ui.apphost.events :refer [on-click-fn]]))

(defn- point
  [^js e size]
  (let [^js r (.getBoundingClientRect (.-currentTarget e))
        sx    (/ (:w size) (max 1 (.-width r)))
        sy    (/ (:h size) (max 1 (.-height r)))]
    (ia/clamp-point [(* sx (- (.-clientX e) (.-left r))) (* sy (- (.-clientY e) (.-top r)))]
                    size)))

(defn- shape-rect
  [{:keys [row box color active?]} on-pick]
  (let [{:keys [x1 y1 x2 y2]} box]
    [:rect {:key          (str (:id row) "/" x1 "/" y1)
            :class        (cond-> "capp-img-anno__box" active? (str " capp-img-anno__box--on"))
            :x            x1
            :y            y1
            :width        (- x2 x1)
            :height       (- y2 y1)
            :stroke       color
            :fill         color
            :vector-effect "non-scaling-stroke"
            :on-pointer-down #(.stopPropagation %)
            :on-click     #(on-pick row)}]))

(defn image-annotation
  [_opts]
  (let [size  (r/atom nil)
        state (r/atom nil)]
    (fn [{:keys [src sel active rows box-prop color-prop on-create height class env]}]
      (let [ctx    (:ev-ctx env)
            create (on-click-fn on-create ctx)
            event! (fn [ev]
                     (let [next (swap! state ia/draw ev)]
                       (when-let [box (:done next)]
                         (reset! state nil)
                         (sel/wr! sel box)
                         (when create (create {:box box :values {:box box}})))))
            shapes (ia/shapes {:rows rows :box-prop box-prop :color-prop color-prop
                               :active (deref* active)})
            drawn  (ia/parse-box (deref* sel))
            live   (ia/drawing-box @state)]
        [:div {:class (cond-> "capp-img-anno" class (str " " class))
               :style (when height {:max-height height :overflow "auto"})}
         [:img {:src      src
                :on-load  (fn [^js e]
                            (reset! size {:w (.. e -target -naturalWidth)
                                          :h (.. e -target -naturalHeight)}))}]
         (when-let [{:keys [w h] :as s} @size]
           (into [:svg.capp-img-anno__layer
                  {:view-box        (str "0 0 " w " " h)
                   :preserve-aspect-ratio "none"
                   :on-pointer-down (fn [^js e]
                                      (.setPointerCapture (.-currentTarget e) (.-pointerId e))
                                      (event! [:start (point e s)]))
                   :on-pointer-move (fn [^js e]
                                      (when @state (event! [:move (point e s)])))
                   :on-pointer-up   (fn [^js e]
                                      (when @state (event! [:finish (point e s)])))}]
                 (concat
                  (map #(shape-rect % (fn [row] (sel/wr! active row))) shapes)
                  (when (and drawn (nil? live))
                    (let [{:keys [x1 y1 x2 y2]} drawn]
                      [^{:key "current"}
                       [:rect.capp-img-anno__draft {:x x1 :y y1 :width (- x2 x1) :height (- y2 y1)
                                                    :vector-effect "non-scaling-stroke"}]]))
                  (when live
                    [^{:key "live"}
                     [:rect.capp-img-anno__draft
                      (assoc live :vector-effect "non-scaling-stroke")]]))))]))))
