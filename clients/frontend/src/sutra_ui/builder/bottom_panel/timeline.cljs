(ns sutra-ui.builder.bottom-panel.timeline
  (:require [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.builder.bottom-panel.common :as common]
            [sutra-ui.state.data :as data]
            [sutra-ui.ui.style :as style]))

(defn runs-status
  [{:keys [show reason]}]
  [:div {:class "panel-timeline__empty u-muted"}
   (case show
     :loading (i18n/t :ui/runs-loading)
     :failed  (i18n/t :ui/runs-failed {:reason reason})
     nil)])

(def ^:private timeline-windows
  [[1 "1h"] [6 "6h"] [24 "24h"] [72 "3d"] [240 "10d"]])

(defn- ->ms
  [v]
  (cond
    (nil? v)              nil
    (instance? js/Date v) (.getTime v)
    :else (let [s (str/replace (str v) #"^(\d{4}-\d{2}-\d{2}) " "$1T")
                n (js/Date.parse s)]
            (when-not (js/isNaN n) n))))

(defn- duration-label [ms]
  (cond
    (nil? ms)      "—"
    (< ms 1000)    (str ms " ms")
    (< ms 60000)   (str (.toFixed (/ ms 1000) 1) " s")
    (< ms 3600000) (str (.toFixed (/ ms 60000) 1) " min")
    :else          (str (.toFixed (/ ms 3600000) 1) " h")))

(defn- timeline-bar
  [{:keys [id kind status started-at finished-at] :as run} t0 t1 selected-id on-pick]
  (let [span  (max 1 (- t1 t0))
        s     (or (->ms started-at) t0)
        e     (or (->ms finished-at) t1)
        left  (* 100 (/ (- (max s t0) t0) span))
        width (max 0.6 (* 100 (/ (- (min e t1) (max s t0)) span)))
        col   (case status
                "succeeded" "var(--c-success)"
                "failed"    "var(--c-danger)"
                "var(--c-warning-accent)")]
    [:div {:class "panel-gantt__row"
           :style {:cursor "pointer"
                   :background (when (= id selected-id) "var(--c-primary-bg)")}
           :on-click #(on-pick run)}
     [:div {:class "panel-gantt__label u-muted"}
      (some-> kind name)]
     [:div {:class "panel-gantt__track"}
      [ui/tooltip
       {:content (str (some-> kind name) " · " status " · "
                      (duration-label (when (and (->ms started-at) (->ms finished-at))
                                        (- (->ms finished-at) (->ms started-at)))))}
       [:div {:class "panel-gantt__bar"
              :style {:background col
                      :margin-left (str left "%")
                      :width (str width "%")}}]]]]))

(defn build-timeline-tab
  []
  (let [picked (r/atom nil)]
    (fn []
      (let [hours (or @data/runs-window-hours 6)
            seen  (rd/list-view @data/runs)
            rows  (:rows seen)
            t1    (.getTime (js/Date.))
            t0    (- t1 (* hours 3600000))
            shown (filterv (fn [r] (let [s (->ms (:started-at r))] (and s (>= s t0)))) rows)
            sel   @picked]
        [:div {:class "panel-gantt"}
         [:div {:class "panel-timeline__header u-muted"}
          (when (#{:list :empty} (:show seen))
            [:span (i18n/t :ui/builds-count {:n (count shown)})])
          (for [[h label] timeline-windows]
            ^{:key (str h)}
            [ui/button {:small true :active (= h hours) :on-click #(data/set-runs-window! h)}
             label])
          [ui/button {:icon :refresh :small true :on-click #(data/refresh-runs!)}]]
         (cond
           (#{:loading :failed} (:show seen))
           [runs-status seen]

           (empty? shown)
           [:div {:class "panel-timeline__empty u-muted"}
            (i18n/t :ui/builds-none-window {:hours hours})]

           :else
           [:div {:class "dataconn-panel__body"}
            (for [r shown]
              ^{:key (str (:id r))}
              [timeline-bar r t0 t1 (:id sel) #(reset! picked %)])])
         (when sel
           (let [s (->ms (:started-at sel))
                 e (->ms (:finished-at sel))]
             [:div {:class "panel-gantt__detail"}
              [ui/html-table {:compact true :class style/table-root}
               [:tbody
                [:tr [:td (i18n/t :ui/status)] [:td [common/status-pill (:status sel)]]]
                [:tr [:td (i18n/t :ui/kind)] [:td (str (some-> (:kind sel) name))]]
                [:tr [:td (i18n/t :ui/started)] [:td (str (:started-at sel))]]
                [:tr [:td (i18n/t :ui/finished)] [:td (str (or (:finished-at sel) "—"))]]
                [:tr [:td (i18n/t :ui/duration)]
                 [:td (duration-label (when (and s e) (- e s)))]]]]]))]))))
