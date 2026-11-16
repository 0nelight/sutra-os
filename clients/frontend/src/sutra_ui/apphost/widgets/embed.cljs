(ns sutra-ui.apphost.widgets.embed
  (:require [sutra-ui.ui.core :as ui]
            [sutra-ui.ui.sel :as sel]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.capps.iframe-bridge :as bridge]
            [sutra.capps.variables :refer [deref*]]
            [sutra.i18n.core :as i18n]
            [sutra-ui.apphost.widgets.media-viewer :as media]))

(defn- px [h] (when h (if (number? h) (str h "px") h)))

(defn- values-of [variables] (update-vals (or variables {}) deref*))

(defn- message-of
  [^js data]
  (-> (js->clj data :keywordize-keys true)
      (assoc :undefined? (and (some? data) (undefined? (.-value data))))))

(defn- run-effect!
  [[op k v] {:keys [mode output-params variables events]} {:keys [event-fn]} height]
  (let [target (get (if (= :bidirectional mode) variables output-params) k)]
    (case op
      :set    (sel/wr! target v)
      :reset  (sel/wr! target nil)
      :event  (when-let [f (event-fn (get events k))] (f nil))
      :resize (reset! height k)
      nil)))

(defn- messaging-frame
  [_cfg _env]
  (let [el     (atom nil)
        cur    (atom nil)
        height (r/atom nil)
        last*  (atom ::nothing)
        policy (fn [cfg] (bridge/frame-policy (assoc cfg :src (bridge/frame-url cfg))
                                              (.. js/window -location -origin)))
        post!  (fn []
                 (let [[cfg] @cur
                       msg   (bridge/outbound cfg (values-of (:variables cfg)))
                       win   (some-> ^js @el .-contentWindow)]
                   (when (and win (not= msg @last*))
                     (reset! last* msg)
                     (.postMessage win (clj->js msg) (:target (policy cfg))))))
        listen (fn [^js e]
                 (let [[cfg env] @cur
                       win       (some-> ^js @el .-contentWindow)]
                   (when (and win
                              (identical? (.-source e) win)
                              (bridge/accept? (policy cfg) (.-origin e))
                              (some? (.-data e)))
                     (some-> (bridge/inbound cfg (message-of (.-data e)))
                             (run-effect! cfg env height)))))]
    (r/create-class
     {:component-did-mount    (fn [_] (.addEventListener js/window "message" listen))
      :component-did-update   (fn [_ _] (post!))
      :component-will-unmount (fn [_] (.removeEventListener js/window "message" listen))
      :reagent-render
      (fn [{:keys [title allow class] :as cfg} env]
        (reset! cur [cfg env])
        (values-of (:variables cfg))
        (let [{:keys [sandbox]} (policy cfg)]
          [:iframe (cond-> {:ref     #(reset! el %)
                            :class   (cond-> "capp-frame" class (str " " class))
                            :src     (bridge/frame-url cfg)
                            :title   (or title (i18n/t :ui/capp-iframe-title))
                            :style   {:width  "100%"
                                      :height (or (px @height) (px (:height cfg)) "460px")
                                      :border "none"}
                            :on-load (fn [_] (reset! last* ::nothing) (post!))}
                     sandbox (assoc :sandbox sandbox)
                     allow   (assoc :allow allow))]))})))

(defn iframe
  [{:keys [src html] :as cfg} env]
  (let [mode (some-> (:mode cfg) keyword)
        cfg  (assoc cfg :mode mode)]
    (cond
      (and (str/blank? (str src)) (not (str/blank? (str html))))
      [media/iframe cfg]

      (str/blank? (str src))
      [media/iframe (dissoc cfg :src)]

      (nil? (bridge/frame-url cfg))
      [ui/problem (i18n/t :ui/capp-iframe-refused) (str "app/iframe: " (pr-str src))]

      (#{:messages :bidirectional} mode)
      [messaging-frame cfg env]

      :else
      [media/iframe (assoc cfg :src (bridge/frame-url cfg))])))

(def widgets
  {:iframe (fn [cfg env] [iframe cfg env])})
