(ns sutra-ui.ui.vendor
  (:require [shadow.loader :as loader]))

(def ^:private libs
  {:maplibre {:css     ["/js/maplibre/maplibre-gl.css"]
              :scripts [{:src "/js/maplibre/boot.mjs" :module? true}]
              :global  "maplibregl"}
   :vega     {:scripts (mapv (fn [f] {:src (str "/js/vega/" f)})
                             ["vega.min.js" "vega-lite.min.js" "vega-embed.min.js"])
              :global  "vegaEmbed"}
   :pdfjs    {:scripts [{:src "/js/pdfjs/boot.mjs" :module? true}]
              :global  "pdfjsLib"}
   :plotly   {:shadow-module "plotly"
              :global        "Plotly"}})

(defonce ^:private loading (atom {}))

(defn- nonce []
  (some-> (.querySelector js/document "script[nonce]") .-nonce not-empty))

(defn- stylesheet! [href]
  (let [el (.createElement js/document "link")]
    (set! (.-rel el) "stylesheet")
    (set! (.-href el) href)
    (.appendChild (.-head js/document) el)))

(defn- script! [{:keys [src module?]}]
  (js/Promise.
   (fn [ok fail]
     (let [el (.createElement js/document "script")]
       (when module? (set! (.-type el) "module"))
       (when-let [n (nonce)] (set! (.-nonce el) n))
       (set! (.-src el) src)
       (set! (.-onload el) ok)
       (set! (.-onerror el) #(fail (js/Error. (str "not loaded: " src))))
       (.appendChild (.-head js/document) el)))))

(defn loaded
  [lib]
  (some-> js/window (aget (:global (libs lib)))))

(defn load!
  [lib]
  (let [{:keys [css scripts shadow-module global]} (libs lib)]
    (if-let [g (loaded lib)]
      (js/Promise.resolve g)
      (or (get @loading lib)
          (let [p (-> (if shadow-module
                        (try (loader/load shadow-module)
                             (catch :default e
                               (js/console.warn "[vendor] no module loader:" shadow-module e)
                               (js/Promise.resolve nil)))
                        (do (run! stylesheet! css)
                            (reduce (fn [p s] (.then p #(script! s)))
                                    (js/Promise.resolve nil) scripts)))
                      (.then (fn [_] (aget js/window global))))]
            (swap! loading assoc lib p)
            p)))))
