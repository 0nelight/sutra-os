(ns sutra-ui.ui.chart
  (:require [clojure.walk :as walk]
            [reagent.core :as r]
            [sutra.capps.chart-spec :as spec]
            [sutra.types.decimal :as d]
            [sutra-ui.ui.vendor :as vendor]))

(defn- ->js
  [x]
  (clj->js (walk/postwalk #(if (d/exact? %) (d/plain %) %) x)))

(defn draw!
  [node {:keys [traces layout] :as plot}]
  (when node
    (if-let [P (.-Plotly js/window)]
      (.react P node (->js traces) (->js layout)
              #js {:displayModeBar false :responsive true})
      (.then (vendor/load! :plotly) (fn [_] (draw! node plot))))))

(defn plotly-node
  ([class spec-of initial] (plotly-node class spec-of initial nil))
  ([class spec-of initial after]
   (let [!node (atom nil)
         !spec (atom (spec-of initial))
         !cfg  (atom initial)
         done! (fn []
                 (let [drawn (draw! @!node @!spec)]
                   (when after
                     (if drawn
                       (.then drawn (fn [_] (after @!node @!cfg)))
                       (after @!node @!cfg)))))]
     (r/create-class
      {:display-name        (str "capp-" class)
       :component-did-mount  (fn [_] (done!))
       :component-did-update (fn [_] (done!))
       :reagent-render
       (fn [cfg]
         (reset! !cfg cfg)
         (reset! !spec (spec-of cfg))
         [:div {:class (str "capp-plot capp-" class)
                :style (when-let [h (:height cfg)] {:height (if (number? h) (str h "px") h)})
                :ref   (fn [el] (when el (reset! !node el)))}])}))))

(defn- on-point!
  [^js node cfg]
  (when (and node (.-on node))
    (set! (.-sutraCfg node) cfg)
    (when-not (.-sutraPoint node)
      (set! (.-sutraPoint node) true)
      (.on node "plotly_click"
           (fn [^js e]
             (when-let [^js p (some-> e .-points (aget 0))]
               (let [{:keys [rows on-point]} (.-sutraCfg node)
                     i (or (.-pointIndex p) (.-pointNumber p))]
                 (when-let [row (and (number? i) (nth (vec rows) i nil))]
                   (on-point row)))))))))

(defn chart
  [cfg]
  (plotly-node "chart" #(spec/chart->spec (cond-> % (:on-point %) (assoc :sort? false)))
               cfg (when (:on-point cfg) on-point!)))
