(ns sutra-ui.apphost.widgets.flow
  (:require [sutra-ui.ui.display :as display]
            [sutra-ui.ui.sel :as sel :refer [rd wr!]]
            [clojure.string :as str]
            [sutra-ui.ui.core :as ui :refer [checkbox]]
            [reagent.core :as r]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.capps.flow-view :as fv]
            [sutra.capps.gantt :as gantt]))

(defn- fire!
  [ev env row]
  (when (some? ev)
    (when-let [f ((:event-fn env) ev)] (f row))))

(defn stepper
  [{:keys [completed-color active-color active on-click] :as cfg} env]
  (let [steps (fv/steps (cond-> cfg active (assoc :current (rd active))))]
    (into [:div.capp-stepper]
          (mapcat
           (fn [i {:keys [id label done? active? clickable?] :as s}]
             (let [color (cond active? active-color done? completed-color)
                   click (when (and clickable? (or active (:on-click s) on-click))
                           (fn []
                             (when active (wr! active id))
                             (fire! (or (:on-click s) on-click) env s)))
                   step  [:div {:key   (str "s" id)
                                :class (cond-> "capp-step"
                                         done?    (str " capp-step--done")
                                         active?  (str " capp-step--active")
                                         click    (str " capp-step--click"))
                                :on-click click}
                          [:div.capp-step__dot
                           {:style (when color {:border-color color :background color})}
                           (fv/step-marker s cfg)]
                          [:div.capp-step__label label]]]
               (if (pos? i)
                 [[:div.capp-step__bar {:key   (str "b" i)
                                        :class (when done? "capp-step__bar--on")}]
                  step]
                 [step])))
           (range) steps))))

(defn status-tracker
  [cfg _env]
  (into [:div.capp-status-tracker]
        (map-indexed
         (fn [i {:keys [label at state]}]
           [:div {:key i :class (str "capp-status capp-status--" (name state))}
            [:div.capp-status__dot]
            [:div.capp-status__label label]
            (when at [:div.capp-status__at at])])
         (fv/stages cfg))))

(defn- with-ontology
  [{:keys [rows prominent?] :as layer} {:keys [title-of prominent-of]}]
  (cond-> layer
    title-of   (assoc :title-of (title-of rows))
    prominent? (assoc :body (prominent-of rows))))

(defn- select-event!
  [{:keys [active on-select]} env {:keys [row] :as ev}]
  (when active (wr! active [row]))
  (fire! (if (some? (:on-select ev)) (:on-select ev) on-select) env row))

(defn- event-card
  [cfg env selected {:keys [at title body color icon no-icon? gap row layer tone] :as ev}]
  [ui/tooltip
   {:content (when (:gap-tooltip? cfg) gap)}
   [:div {:class (cond-> "capp-timeline__item capp-timeline__item--click"
                   tone (str " capp-timeline__item--" (name tone))
                   (and (:highlight? cfg) (= row selected)) (str " capp-timeline__item--on"))
          :on-click #(do (reset! (:selected-row cfg) row) (select-event! cfg env ev))}
    [:div.capp-timeline__dot {:style (when color {:background color})}
     (when (and icon (not no-icon?)) [:span.capp-timeline__icon icon])]
    [:div.capp-timeline__content
     (when (or at layer)
       [:div.capp-timeline__at at
        (when layer [:span.capp-timeline__layer (str " · " layer)])])
     [:div.capp-timeline__title {:style (when color {:color color})} title]
     (when body [:div.capp-timeline__body body])]]])

(defn- legend
  [layers hidden]
  [:div.capp-timeline__legend
   (for [[i {:keys [label color]}] (map-indexed vector layers)]
     ^{:key i}
     [checkbox {:class     "capp-timeline__legend-row"
                :checked   (not (contains? @hidden i))
                :label     [:<>
                            [:span.capp-timeline__swatch {:style {:background color}}]
                            (or label (str "Ebene " (inc i)))]
                :on-change #(swap! hidden (fn [h] (if (contains? h i) (disj h i) (conj h i))))}])])

(defn timeline
  [_ _]
  (let [hidden   (r/atom #{})
        selected (r/atom nil)]
    (fn [{:keys [items layers orientation order legend?] :as cfg} env]
      (if-not (seq layers)
        [display/timeline items]
        (let [ls  (mapv #(with-ontology % env) layers)
              evs (into (vec items)
                        (fv/timeline-events {:layers ls :order order :hidden @hidden}))
              cfg (assoc cfg :selected-row selected)]
          [:div
           (when legend? [legend ls hidden])
           (into [:div {:class (cond-> "capp-timeline"
                                 (= :horizontal (->kw orientation))
                                 (str " capp-timeline--horizontal"))}]
                 (map-indexed (fn [i ev]
                                ^{:key i} [event-card cfg env @selected ev])
                              evs))])))))

(defn action-log-timeline
  [cfg env]
  [timeline {:layers (fv/action-log-layers cfg) :legend? (next (:logs cfg))} env])

(defn- zone-of
  [{:keys [time-zone]}]
  (when-not (or (str/blank? (str time-zone)) (= :local (->kw time-zone)))
    (str time-zone)))

(defn- tick-label
  [{:keys [t]} cfg]
  (.toLocaleDateString (js/Date. t) js/undefined
                       (clj->js (cond-> {:year "2-digit" :month "short" :day "numeric"}
                                  (zone-of cfg) (assoc :timeZone (zone-of cfg))
                                  (= :hour (->kw (:grid-unit cfg))) (assoc :hour "2-digit")))))

(defn- event-tip
  [{:keys [label start end point? hover]} cfg]
  (let [show #(.toLocaleString (js/Date. %) js/undefined
                               (clj->js (cond-> {} (zone-of cfg) (assoc :timeZone (zone-of cfg)))))]
    (str/join "\n" (concat [(or label "")
                            (if point? (show start) (str (show start) " – " (show end)))]
                           (map (fn [[p v]] (str (name p) ": " v)) hover)))))

(defn- bar
  [{:keys [row x w lane label color point? on-select] :as ev} cfg env selected]
  (let [{:keys [selected-rows on-selection multi-select? selection?]} cfg
        on? (some #{row} selected)]
    [:div {:class (cond-> "capp-gantt__event"
                    point? (str " capp-gantt__event--point")
                    on?    (str " capp-gantt__event--on"))
           :style (cond-> {:left (str x "%") :top (* lane 26) :background color}
                    (not point?) (assoc :width (str w "%")))
           :title (event-tip ev cfg)
           :on-click (when selection?
                       (fn [^js e]
                         (let [picked (gantt/next-selection
                                       selected row
                                       {:multi? multi-select?
                                        :add?   (or (.-ctrlKey e) (.-metaKey e))})]
                           (when selected-rows (wr! selected-rows picked))
                           (fire! (if (some? on-select) on-select on-selection) env row))))}
     (when-not point? [:span.capp-gantt__label label])]))

(defn gantt-chart
  [{:keys [group-width selected-rows] :as cfg} env]
  (let [now     (.getTime (js/Date.))
        look-of (:look-of env)
        layers  (mapv (fn [l] (cond-> l look-of (assoc :look-of (look-of (:rows l)))))
                      (:layers cfg))
        lay     (gantt/layout (assoc cfg :layers layers :now now))
        chosen  (some-> selected-rows rd vec)
        width   (or group-width 180)]
    (if-not lay
      [ui/empty-state {:icon :calendar :title "Keine Ereignisse"}]
      [:div.capp-gantt
       [:div.capp-gantt__axis {:style {:margin-left width}}
        (for [{:keys [t x] :as tick} (:ticks lay)]
          ^{:key t} [:div.capp-gantt__tick {:style {:left (str x "%")}} (tick-label tick cfg)])]
       (for [[i {:keys [group lanes events]}] (map-indexed vector (:rows lay))]
         ^{:key i}
         [:div.capp-gantt__row {:style {:height (+ 6 (* lanes 26))}}
          [:div.capp-gantt__group {:style {:width width}} group]
          [:div.capp-gantt__track
           (for [{:keys [x] :as tick} (:ticks lay)]
             ^{:key (:t tick)} [:div.capp-gantt__grid {:style {:left (str x "%")}}])
           (when-let [n (:now-x lay)]
             [:div.capp-gantt__now {:style {:left (str n "%")}}])
           (for [[j ev] (map-indexed vector events)]
             ^{:key j} [bar ev cfg env chosen])]])])))

(def widgets
  {:stepper             (fn [cfg env] [stepper cfg env])
   :status-tracker      (fn [cfg env] [status-tracker cfg env])
   :timeline            (fn [cfg env] [timeline cfg env])
   :action-log-timeline (fn [cfg env] [action-log-timeline cfg env])
   :gantt               (fn [cfg env] [gantt-chart cfg env])})
