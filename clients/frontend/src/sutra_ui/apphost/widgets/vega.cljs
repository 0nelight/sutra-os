(ns sutra-ui.apphost.widgets.vega
  (:require [sutra-ui.ui.sel :as sel :refer [wr!]]
            [reagent.core :as r]
            [sutra.kit.coerce :refer [->kw]]
            [sutra-ui.ui.core :as ui :refer [checkbox segmented-control]]
            [sutra.capps.vega-spec :as vs]
            [sutra-ui.apphost.interop :refer [->js]]
            [sutra-ui.apphost.toast :refer [toast!]]
            [sutra-ui.ui.vendor :as vendor]))

(defn- listen!
  [^js view selections]
  (doseq [{:keys [param output]} selections :when output]
    (try
      (.addSignalListener view (name param)
                          (fn [_ value]
                            (wr! output
                                 (vs/selection->filter
                                  (js->clj value :keywordize-keys true)))))
      (catch :default e
        (js/console.warn "[vega] no such selection parameter" (name param) e)))))

(defn- finalize!
  [!result]
  (when-let [^js result @!result] (.finalize result)))

(defn- embed!
  [^js node spec {:keys [grammar selections]} !result]
  (finalize! !result)
  (reset! !result nil)
  (-> (vendor/load! :vega)
      (.then (fn [embed]
               (embed node (->js spec)
                      (->js {:mode (if (= :vega (->kw grammar)) "vega" "vega-lite")
                             :actions false}))))
      (.then (fn [^js result]
               (when result
                 (reset! !result result)
                 (listen! (.-view result) selections))))
      (.catch (fn [e] (toast! (str "Vega: " (.-message e)))))))

(defn- plot
  [_ _]
  (let [!node   (atom nil)
        !result (atom nil)
        !drawn  (atom nil)
        draw!   (fn [spec cfg]
                  (when (and @!node (not= spec @!drawn))
                    (reset! !drawn spec)
                    (embed! @!node spec cfg !result)))]
    (r/create-class
     {:display-name "capp-vega"
      :component-did-mount  (fn [this] (let [[_ spec cfg] (r/argv this)] (draw! spec cfg)))
      :component-did-update (fn [this] (let [[_ spec cfg] (r/argv this)] (draw! spec cfg)))
      :component-will-unmount (fn [_] (finalize! !result))
      :reagent-render
      (fn [_ {:keys [height]}]
        [:div.capp-vega {:style (when height {:height height})
                         :ref   #(when % (reset! !node %))}])})))

(defn- preview
  [_]
  (let [cut? (r/atom true)]
    (fn [spec]
      (let [text (js/JSON.stringify (->js (if @cut? (vs/truncated spec 5) spec)) nil 2)]
        [:div.capp-vega-preview
         [checkbox {:checked @cut? :label "Daten kürzen" :on-change #(swap! cut? not)}]
         [ui/button {:intent :primary
                     :on-click #(.writeText (.-clipboard js/navigator) text)}
          "Kopieren"]
         [:pre text]]))))

(defn vega-chart
  [_ _]
  (let [tab (r/atom :chart)]
    (fn [{:keys [preview?] :as cfg} _]
      (let [{:keys [spec error]} (vs/complete cfg)]
        (if error
          [ui/problem "Vega-Spezifikation" error]
          [:div.capp-vega-frame
           (when preview?
             [segmented-control {:class           "capp-segmented capp-vega-tabs"
                                 :inline          true
                                 :small           true
                                 :value           @tab
                                 :options         [{:label "Diagramm" :value :chart}
                                                   {:label "Vorschau" :value :preview}]
                                 :on-value-change (fn [v _] (reset! tab v))}])
           (if (and preview? (= :preview @tab))
             [preview spec]
             [plot spec cfg])])))))

(def widgets
  {:vega-chart (fn [cfg env] [vega-chart cfg env])})
