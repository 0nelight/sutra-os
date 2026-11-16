(ns sutra-ui.apphost.widgets.recorder
  (:require [sutra-ui.ui.sel :as sel]
            [sutra-ui.ui.busy :as busy]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui]
            [sutra.capps.recorder :as rec]
            [sutra-ui.apphost.client :as client]
            [sutra-ui.apphost.frame :as frame]))

(defn- blob->b64!
  [blob on-ok on-error]
  (let [^js reader (js/FileReader.)]
    (set! (.-onload reader)
          (fn [] (on-ok (second (str/split (str (.-result reader)) #",")))))
    (set! (.-onerror reader) (fn [] (on-error "Aufnahme nicht lesbar")))
    (.readAsDataURL reader blob)))

(defn- stop-tracks!
  [^js stream]
  (doseq [^js t (array-seq (.getTracks stream))] (.stop t)))

(defn- start-recording!
  [media send!]
  (let [[mime ext :as fmt] (rec/format-for #(.isTypeSupported js/MediaRecorder %))
        devices (.-mediaDevices js/navigator)]
    (if-not (and fmt devices)
      (send! [:failed "Dieser Browser kann nicht aufnehmen."])
      (-> (.getUserMedia devices #js {:audio true})
          (.then
           (fn [stream]
             (let [^js mr (js/MediaRecorder. stream #js {:mimeType mime})
                   chunks (atom [])]
               (set! (.-ondataavailable mr) #(swap! chunks conj (.-data %)))
               (set! (.-onstop mr)
                     (fn []
                       (stop-tracks! stream)
                       (let [blob (js/Blob. (into-array @chunks) #js {:type mime})
                             nm   (rec/recording-name (.toISOString (js/Date.)) ext)]
                         (swap! media assoc :blob blob :mime mime)
                         (send! [:stopped {:url (js/URL.createObjectURL blob) :name nm}]))))
               (.start mr)
               (swap! media assoc :recorder mr)
               (send! [:started]))))
          (.catch #(send! [:failed (str "Mikrofon: " (.-message %))]))))))

(defn- upload!
  [{:keys [descriptor refresh] :as env} {:keys [action output] :as cfg} {:keys [blob mime]} nm
   send!]
  (let [f     (frame/frame-for env)
        saved (fn [hash]
                (sel/wr! output nm)
                (if action
                  (frame/request!
                   f (frame/submit-request descriptor (rec/action-params cfg hash nm)
                                           {:action   action
                                            :on-ok    (fn [d]
                                                        (when (and refresh d) (refresh d))
                                                        (send! [:saved]))
                                            :on-error #(send! [:failed %])}))
                  (send! [:saved])))]
    (blob->b64! blob
                (fn [b64]
                  (frame/request!
                   f (assoc (frame/kept #(saved (:hash %)) #(send! [:failed %]))
                            :ask [client/upload! descriptor {:bytes b64 :dateiname nm :mime mime}
                                  {}])))
                #(send! [:failed %]))))

(defn audio-recorder
  [_opts]
  (let [state (r/atom rec/initial)
        media (atom nil)
        cfg*  (atom nil)]
    (letfn [(send! [ev]
              (let [[s effects] (rec/step @state ev)]
                (reset! state s)
                (run! run-effect! effects)))
            (run-effect! [[kind x]]
              (let [{:keys [env] :as cfg} @cfg*
                    ^js mr (:recorder @media)]
                (case kind
                  :start  (start-recording! media send!)
                  :pause  (.pause mr)
                  :resume (.resume mr)
                  :stop   (.stop mr)
                  :upload (upload! env cfg @media x send!)
                  :forget (do (js/URL.revokeObjectURL x) (reset! media nil)))))]
      (fn [{:keys [class] :as cfg}]
        (reset! cfg* cfg)
        (let [{:keys [phase url name error saved]} @state
              button (fn [label ev]
                       [ui/button {:intent :primary :on-click #(send! ev)} label])]
          [:div {:class (cond-> "capp-recorder" class (str " " class))}
           (case phase
             :idle      [button "⏺ Aufnehmen" [:record]]
             :asking    [busy/inline {:class "capp__hint"} "Mikrofon wird angefragt…"]
             :recording [:<> [:span.capp-recorder__on "● Aufnahme"]
                         [button "⏸ Pause" [:pause]] [button "⏹ Stopp" [:stop]]]
             :paused    [:<> [:span.capp__hint "Pausiert"]
                         [button "⏺ Weiter" [:record]] [button "⏹ Stopp" [:stop]]]
             :stopping  [busy/inline {:class "capp__hint"} "Wird beendet…"]
             :done      [:<> [:audio {:controls true :src url}]
                         [:span.capp-recorder__name name]
                         [button "Speichern" [:save]] [button "Verwerfen" [:discard]]]
             :saving    [busy/inline {:class "capp__hint"} "Wird hochgeladen…"]
             nil)
           (when saved [:span.capp__hint (str "Gespeichert: " saved)])
           (when error [ui/problem (str error) nil])])))))
