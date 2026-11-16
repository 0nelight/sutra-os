(ns sutra-ui.apphost.widgets.media-viewer
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.media-view :as mv]
            [sutra-ui.apphost.interactions :as interactions]))

(defn- px [h] (when h (if (number? h) (str h "px") h)))

(defn- sandbox-attr
  [sandbox]
  (cond
    (or (false? sandbox) (= :none sandbox)) nil
    (string? sandbox)                       sandbox
    :else "allow-scripts allow-forms allow-popups"))

(def ^:private html-head
  (str "<meta http-equiv=\"Content-Security-Policy\" content=\"script-src 'none'\">"
       "<base target=\"_blank\">"
       "<style>body{margin:12px;font-family:system-ui,sans-serif;font-size:14px}</style>"))

(defn- fit-height!
  [cap e]
  (when-let [d (.. e -target -contentDocument)]
    (set! (.. e -target -style -height)
          (str (min cap (+ 2 (.. d -documentElement -scrollHeight))) "px"))))

(defn- html-frame
  [{:keys [html height title class]}]
  [:iframe {:class   (cond-> "capp-frame capp-frame--html" class (str " " class))
            :srcDoc  (str html-head html)
            :sandbox "allow-same-origin allow-popups allow-popups-to-escape-sandbox"
            :title   (or title "eingebetteter Inhalt")
            :style   {:width "100%" :height "120px" :border "none"}
            :on-load #(fit-height! (if (number? height) height 600) %)}])

(defn- iframe-src
  [{:keys [src height title sandbox allow class]}]
  [:iframe (cond-> {:class (cond-> "capp-frame" class (str " " class))
                    :src   src
                    :title (or title "eingebetteter Inhalt")
                    :style {:width "100%" :height (or (px height) "460px") :border "none"}}
             (sandbox-attr sandbox) (assoc :sandbox (sandbox-attr sandbox))
             allow                  (assoc :allow allow))])

(defn iframe
  [{:keys [src html] :as cfg}]
  (if (and (string? html) (not (str/blank? html)) (not src))
    [html-frame cfg]
    [iframe-src cfg]))

(defn- media-kind-of
  [{:keys [media-kind mime src]}]
  (or media-kind
      (cond
        (and mime (str/starts-with? mime "image")) :image
        (and mime (str/starts-with? mime "video")) :video
        (and mime (str/starts-with? mime "audio")) :audio
        (and mime (str/includes? mime "pdf"))      :pdf
        :else
        (let [s (str/lower-case (str src))]
          (cond
            (re-find #"\.(png|jpe?g|gif|webp|svg|avif)(\?|$)" s) :image
            (re-find #"\.(mp4|webm|mov|m4v)(\?|$)" s)            :video
            (re-find #"\.(mp3|wav|ogg|m4a)(\?|$)" s)             :audio
            (re-find #"\.pdf(\?|$)" s)                           :pdf
            :else                                                :iframe)))))

(defn- on-change
  [_v _f]
  (r/create-class
   {:component-did-mount  (fn [this] (let [[_ v f] (r/argv this)] (f v)))
    :component-did-update (fn [this [_ old _]]
                            (let [[_ v f] (r/argv this)] (when (not= v old) (f v))))
    :reagent-render       (fn [_ _] nil)}))

(defn- seek!
  [^js el t]
  (when el
    (when-let [to (mv/clamp-seek t (.-duration el))]
      (set! (.-currentTime el) to))))

(def ^:private sample-limit 240)

(defn- signature
  [^js ctx]
  (let [d (.-data (.getImageData ctx 0 0 32 18))]
    (mapv #(aget d (* 4 %)) (range (* 32 18)))))

(defn- scan-scenes!
  [src sensitivity on-done]
  (let [v      (js/document.createElement "video")
        canvas (js/document.createElement "canvas")
        ctx    (do (set! (.-width canvas) 32) (set! (.-height canvas) 18)
                   (.getContext canvas "2d" #js {:willReadFrequently true}))
        acc    (atom [])
        times  (atom nil)
        next!  (fn []
                 (if-let [t (first @times)]
                   (do (swap! times rest) (set! (.-currentTime v) t))
                   (on-done (mv/scene-times @acc sensitivity))))]
    (set! (.-muted v) true)
    (set! (.-preload v) "auto")
    (set! (.-src v) src)
    (.addEventListener v "loadedmetadata"
                       (fn []
                         (let [d (.-duration v)]
                           (reset! times (mv/frame-times {:every (max 1 (/ d sample-limit))} d))
                           (next!))))
    (.addEventListener v "seeked"
                       (fn []
                         (.drawImage ctx v 0 0 32 18)
                         (swap! acc conj [(.-currentTime v) (signature ctx)])
                         (next!)))
    (.addEventListener v "error" (fn [] (on-done [])))))

(defn- frame-strip
  [{:keys [src times on-pick]}]
  (into [:div.capp-frames]
        (map (fn [t]
               ^{:key t}
               [:button.capp-frames__frame {:type "button" :on-click #(on-pick t)}
                [:video {:src (str src "#t=" t) :preload "metadata" :muted true}]
                [:span.capp-frames__at (mv/seconds-text t)]]))
        times))

(defn- scene-frames
  [_opts]
  (let [found (r/atom nil)
        scan  (fn [[src sensitivity]]
                (reset! found nil)
                (scan-scenes! src sensitivity #(reset! found %)))]
    (fn [{:keys [src sensitivity] :as opts}]
      [:<>
       [on-change [src sensitivity] scan]
       (when @found [frame-strip (assoc opts :times @found)])])))

(def ^:private speaker-colors
  ["#2d72d2" "#29a634" "#d9822b" "#9d3f9d" "#00a396" "#db2c6f" "#7961db" "#8f7c5f"])

(defn- speaker-color
  [speakers s]
  (nth speaker-colors (mod (or (first (keep-indexed #(when (= %2 s) %1) speakers)) 0)
                           (count speaker-colors))))

(defn- diarization
  [{:keys [segs duration on-pick display]}]
  (let [speakers (mv/speakers segs)
        ms       (when (and (number? duration) (js/isFinite duration)) (* 1000 duration))
        bars     (mv/speaker-bars segs ms)
        lane     (fn [k bars]
                   (into [:div.capp-speakers__lane {:key k}]
                         (map (fn [{:keys [speaker left width start]}]
                                [:button.capp-speakers__bar
                                 {:type     "button"
                                  :title    speaker
                                  :style    {:left             (str left "%")
                                             :width            (str width "%")
                                             :background-color (speaker-color speakers speaker)}
                                  :on-click #(on-pick (/ start 1000))}]))
                         bars))]
    (if (= :player (keyword display))
      [:div.capp-speakers
       (lane "all" bars)
       (into [:div.capp-speakers__legend]
             (map (fn [s]
                    [:span.capp-speakers__name {:key s :style {:color (speaker-color speakers s)}}
                     s]))
             speakers)]
      (into [:div.capp-speakers]
            (map (fn [s]
                   [:div.capp-speakers__row {:key s}
                    [:span.capp-speakers__name s]
                    (lane s (filterv #(= s (:speaker %)) bars))]))
            speakers))))

(defn- transcript-view
  [{:keys [env transcript segs at on-pick]}]
  (let [on   (mv/active-index segs (* 1000 at))
        acts (:actions transcript)]
    (into [:div.capp-transcript]
          (map-indexed
           (fn [i {:keys [text start speaker row]}]
             ^{:key i}
             [:div {:class    (cond-> "capp-transcript__seg"
                                (= i on) (str " capp-transcript__seg--on"))
                    :on-click #(on-pick (/ start 1000))}
              [:span.capp-transcript__at (mv/seconds-text (/ start 1000))]
              (when speaker [:span.capp-transcript__speaker speaker])
              [:span.capp-transcript__text text]
              (when (seq acts)
                [interactions/bar {:env env :items acts :row row :default-icon "✎"
                                   :class "capp-transcript__actions"}])]))
          segs)))

(defn- player
  [_cfg]
  (let [el       (atom nil)
        at       (r/atom 0)
        duration (r/atom nil)
        pick     (fn [t] (seek! @el t))]
    (fn [{:keys [tag attrs seek key-frames transcript env]}]
      (let [segs (when transcript (mv/segments transcript))
            src  (:src attrs)]
        [:div.capp-player
         (when (seq segs)
           [transcript-view {:env env :transcript transcript :segs segs :at @at :on-pick pick}])
         [tag (assoc attrs
                     :ref #(reset! el %)
                     :on-time-update #(reset! at (.. % -target -currentTime))
                     :on-loaded-metadata #(reset! duration (.. % -target -duration)))]
         [on-change seek pick]
         (when (and (:speaker transcript) (:diarization? transcript) (seq segs))
           [diarization {:segs segs :duration @duration :on-pick pick
                         :display (:diarization-display transcript)}])
         (cond
           (:scenes key-frames)
           [scene-frames {:src src :sensitivity (:scenes key-frames) :on-pick pick}]
           (seq key-frames)
           [frame-strip {:src src :times (mv/frame-times key-frames @duration) :on-pick pick}])]))))

(defn media
  [{:keys [src alt height poster class seek key-frames transcript env] :as cfg}]
  (let [k (media-kind-of cfg)
        cls (fn [base] (cond-> base class (str " " class)))
        timed {:seek seek :key-frames key-frames :transcript transcript :env env}]
    (case k
      :image [:img {:class (cls "capp-media capp-media--img") :src src :alt (or alt "")
                    :style {:max-width "100%" :height (px height)}}]
      :video [player (assoc timed
                            :tag :video
                            :attrs {:class (cls "capp-media capp-media--video") :controls true
                                    :src src :poster poster
                                    :style {:width "100%" :height (px height)}})]
      :audio [player (assoc timed
                            :tag :audio
                            :attrs {:class (cls "capp-media capp-media--audio") :controls true
                                    :src src})]
      :pdf   [:iframe {:class (cls "capp-doc") :src src :title (or alt "Dokument")
                       :style {:width "100%" :height (or (px height) "70vh") :border "none"}}]
      [iframe {:src src :height height :title alt :class class}])))
