(ns sutra-ui.apphost.widgets.device
  (:require [sutra-ui.ui.display :as display]
            [sutra-ui.ui.overlay :as overlay]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.button-group :as bg]
            [sutra.capps.device :as device]))

(defn- detector
  []
  (when (exists? js/BarcodeDetector)
    (js/BarcodeDetector. #js {:formats #js ["qr_code"]})))

(defn- stop-stream! [^js stream]
  (when stream (doseq [^js t (array-seq (.getTracks stream))] (.stop t))))

(defn- scanner
  [{:keys [on-code]}]
  (let [stream (atom nil)
        timer  (atom nil)
        video  (atom nil)
        err    (r/atom nil)
        props  (atom on-code)
        scan!  (fn [^js det]
                 (when-let [^js v @video]
                   (when (>= (.-readyState v) 2)
                     (-> (.detect det v)
                         (.then (fn [found]
                                  (@props (when-let [^js code (first (array-seq found))]
                                            (.-rawValue code)))))
                         (.catch (fn [_] nil))))))]
    (r/create-class
     {:display-name "qr-scanner"
      :component-did-mount
      (fn [_]
        (if-let [det (detector)]
          (-> (js/navigator.mediaDevices.getUserMedia
               #js {:video #js {:facingMode "environment"}})
              (.then (fn [s]
                       (reset! stream s)
                       (when-let [^js v @video]
                         (set! (.-srcObject v) s)
                         (.play v))
                       (reset! timer (js/setInterval #(scan! det) 300))))
              (.catch (fn [e] (reset! err (str "Kein Kamerazugriff: " (.-message e))))))
          (reset! err "Dieser Browser kann keine QR-Codes erkennen (BarcodeDetector fehlt).")))
      :component-will-unmount
      (fn [_]
        (some-> @timer js/clearInterval)
        (stop-stream! @stream))
      :reagent-render
      (fn [{:keys [on-code on-close on-confirm pending seen]}]
        (reset! props on-code)
        [:<>
         [:video.capp-qr__video {:ref #(reset! video %) :plays-inline true :muted true}]
         (when-let [e @err] [:div.capp-qr__error e])
         (when (seq seen) [:div.capp-qr__seen (str (count seen) " erfasst")])
         [:div.capp-qr__bar
          (when pending
            [ui/button {:intent :primary :on-click on-confirm} "Code erfassen"])
          [ui/button {:intent :primary :on-click on-close} "Schließen"]]])})))

(defn qr-reader
  [_cfg]
  (let [open (r/atom false)
        st   (r/atom {:seen []})]
    (fn [{:keys [output label left-icon right-icon description disabled?] :as cfg}]
      (let [apply! (fn [{:keys [state take close?]}]
                     (reset! st state)
                     (when (and take (satisfies? IReset output))
                       (reset! output (device/output-value cfg (:seen state))))
                     (when close? (reset! open false)))]
        [:<>
         [ui/tooltip {:content description}
          [ui/button
           (merge (bg/button-props cfg)
                  {:style    (bg/button-style cfg)
                   :disabled (= :disabled (bg/state {:visible   (:visible cfg true)
                                                     :disabled? disabled?}))
                   :on-click (fn [] (reset! st {:seen []}) (reset! open true))})
           (when left-icon [:span.capp-btn__icon left-icon])
           (or label "QR-Code scannen")
           (when right-icon [:span.capp-btn__icon right-icon])]]
         (when @open
           [overlay/window {:on-close #(reset! open false) :backdrop "capp-backdrop--clear"}
            [:div.capp-qr
             [scanner {:on-code    #(apply! (device/scan-step @st cfg %))
                       :on-confirm #(apply! (device/confirm-step @st cfg))
                       :on-close   #(reset! open false)
                       :pending    (:pending @st)
                       :seen       (:seen @st)}]]])]))))

(defn current-location
  [_cfg]
  (let [here  (r/atom nil)
        err   (r/atom nil)
        watch (atom nil)
        out   (atom nil)]
    (r/create-class
     {:display-name "current-location"
      :component-did-mount
      (fn [this]
        (reset! out (:output (second (r/argv this))))
        (if (exists? js/navigator.geolocation)
          (reset!
           watch
           (js/navigator.geolocation.watchPosition
            (fn [^js p]
              (let [c   (.-coords p)
                    lat (.-latitude c)
                    lon (.-longitude c)]
                (reset! err nil)
                (reset! here [lat lon])
                (when (satisfies? IReset @out)
                  (reset! @out (device/location-string lat lon)))))
            (fn [^js e] (reset! err (.-message e)))))
          (reset! err "Dieser Browser kennt keinen Standort.")))
      :component-will-unmount
      (fn [_]
        (some-> @watch js/navigator.geolocation.clearWatch))
      :reagent-render
      (fn [{:keys [output label hide-display?]}]
        (reset! out output)
        (cond
          @err           [:div.capp-location.capp-location--denied
                          [display/badge :warning "Standort"] " " @err]
          hide-display?  nil
          :else
          [:div.capp-location
           (when (seq (str label)) [:span.capp-location__label label])
           (if-let [[lat lon] @here]
             [:span.capp-location__value (device/location-preview lat lon)]
             [:span.capp-location__value "…"])]))})))

(def widgets
  {:qr-reader        (fn [cfg _] [qr-reader cfg])
   :current-location (fn [cfg _] [current-location cfg])})
