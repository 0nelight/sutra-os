(ns sutra-ui.apphost.charts
  (:require [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            [sutra.kit.coerce :refer [->kw]]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [numeric-input]]
            [sutra.capps.chart-spec :as spec]
            [sutra.capps.object-filter :as of]
            [sutra-ui.apphost.interop :refer [->js]]
            [sutra-ui.ui.chart :as chart]
            [sutra-ui.api :as api]
            [sutra-ui.apphost.frame :as fr]))

(defn- filter-handle
  [cfg layer]
  (or (:selection-filter (nth (vec (:layers cfg)) layer nil))
      (:selection-filter cfg)))

(defn- write-selection!
  [cfg ^js point]
  (let [meta (some-> point .-data .-meta (js->clj :keywordize-keys true))
        pie? (= "pie" (some-> point .-data .-type))
        sel  (spec/selection meta {:x (.-x point) :label (.-label point)
                                   :custom (when pie? (.-customdata point))}
                             pie?)]
    (when-let [h (and sel (filter-handle cfg (or (:layer sel) 0)))]
      (wr! h (of/toggle-value (rd h) {:prop (->kw (:prop sel))} (:value sel) false)))))

(defn attach-select!
  [^js node {:keys [selected] :as cfg}]
  (when node (set! (.-sutraCfg node) cfg))
  (when (and node (.-on node) (not (.-sutraSelect node))
             (or selected (:selection-filter cfg) (some :selection-filter (:layers cfg))))
    (set! (.-sutraSelect node) true)
    (.on node "plotly_click"
         (fn [^js e]
           (when-let [^js p (some-> e .-points (aget 0))]
             (let [cfg  (.-sutraCfg node)
                   cd   (.-customdata p)
                   row  (when (number? cd) (nth (vec (:rows cfg)) cd nil))
                   sel  (:selected cfg)]
               (write-selection! cfg p)
               (when sel
                 (if row
                   (wr! sel [row])
                   (let [x (.-x p)]
                     (wr! sel (when-not (= x (rd sel)) x)))))))))))

(defn- plot-near
  [^js e]
  (some-> e .-currentTarget (.closest ".capp-chart-frame") (.querySelector ".js-plotly-plot")))

(defn- download!
  [e opts]
  (when-let [node (plot-near e)]
    (.downloadImage (.-Plotly js/window) node
                    (->js (merge {:format "png" :filename "chart"} opts)))))

(defn- copy!
  [e]
  (when-let [node (plot-near e)]
    (-> (.toImage (.-Plotly js/window) node (->js {:format "png"}))
        (.then api/fetch-url)
        (.then #(.blob %))
        (.then (fn [blob]
                 (.write (.-clipboard js/navigator)
                         #js [(js/ClipboardItem. (js-obj "image/png" blob))]))))))

(defn- export-bar
  []
  (let [custom (r/atom nil)]
    (fn []
      [:div.capp-chart-export
       [ui/button {:intent :primary :on-click #(download! % {})} "PNG"]
       [ui/button {:intent :primary :on-click copy!} "Kopieren"]
       [ui/button {:intent :primary
                   :on-click #(swap! custom (fn [c] (when-not c {:width 1200 :height 800})))}
        "Eigenes Format"]
       (when-let [{:keys [width height]} @custom]
         [:span.capp-chart-export__custom
          [numeric-input {:small true :button-position :none :aria-label "Breite"
                          :value (if (js/isNaN width) "" width)
                          :on-value-change #(swap! custom assoc :width (js/parseInt %2))}]
          "×"
          [numeric-input {:small true :button-position :none :aria-label "Höhe"
                          :value (if (js/isNaN height) "" height)
                          :on-value-change #(swap! custom assoc :height (js/parseInt %2))}]
          [ui/button {:intent   :primary
                      :on-click #(download! % {:width width :height height})}
           "Laden"]])])))

(defn chart
  [initial]
  (chart/plotly-node "chart" spec/chart->spec initial attach-select!))

(defn- series-reads
  [{:keys [layers]}]
  (keep-indexed (fn [i {:keys [time-series]}] (when time-series [i time-series])) layers))

(defn- with-series
  [cfg answers]
  (update cfg :layers
          (fn [ls] (vec (map-indexed (fn [i l] (cond-> l (get answers i)
                                                       (assoc :series-data (get answers i))))
                                     ls)))))

(defn- series-chart
  [_ reads env]
  (let [answers (r/atom {})]
    (doseq [[i spec] reads]
      ((:series! env) spec (fr/kept (fn [a] (swap! answers assoc i a)))))
    (fn [cfg _ _] [chart (with-series cfg @answers)])))

(defn chart-frame
  [{:keys [export?] :as cfg} env]
  (let [reads (series-reads cfg)
        body  (if (seq reads)
                ^{:key (pr-str reads)} [series-chart cfg reads env]
                [chart cfg])]
    (if export?
      [:div.capp-chart-frame [export-bar] body]
      body)))
