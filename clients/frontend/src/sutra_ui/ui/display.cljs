(ns sutra-ui.ui.display
  (:require [sutra.capps.object-display :as od]
            [sutra.kit.coerce :refer [->kw]]
            [reagent.core :as r]
            [sutra-ui.ui.core :as ui :refer [tag text-area]]
            [clojure.string :as str]
            [sutra.capps.format :as fmt]
            [sutra-ui.ui.rule-icon :as rule-icon]
            [sutra.capps.metric :as metric]))

(def ^:private accent-colors
  {:pos "var(--c-success-strong)" :neg "var(--c-danger-strong)" :muted "var(--c-muted-3)"
   :warn "var(--c-warning)"})

(defn- accent-color
  [accent]
  (if (string? accent) accent (get accent-colors (->kw accent))))

(defn kpi
  ([label value sub accent] (kpi label value sub accent nil))
  ([label value sub accent {:keys [on-click display look description secondary sparkline size]}]
   (let [tag?  (= :tag (->kw display))
         el    (if tag? :span :div)
         muted (ui/cx (ui/cls "text-muted") (ui/cls "text-small"))
         color (or (accent-color accent) (:color look))]
     [ui/card
      (cond-> {:class (ui/cx "capp-kpi" {"capp-kpi--tag" tag?}
                             (when size (str "capp-kpi--" (name (->kw size)))))
               :compact true}
        on-click (assoc :interactive true :role "button" :tab-index 0
                        :on-click (fn [_] (on-click))
                        :on-key-down #(when (= "Enter" (.-key %)) (on-click)))
        (or color (:background look))
        (assoc :style (cond-> {}
                        color              (assoc :border-top (str "3px solid " color))
                        (:background look) (assoc :background (:background look)))))
      [el {:class muted}
       label
       (when description
         [ui/tooltip {:content description} [ui/icon {:icon :info-sign :size 12}]])]
      [(if tag? :span :h3) (cond-> {:class (ui/cx "capp-kpi__value" (ui/cls "heading"))}
                             (:color look) (assoc :style {:color (:color look)}))
       (rule-icon/before (:icon look))
       (cond (= ::loading value)              [ui/spinner {:size 14 :tag-name :span}]
             (or (nil? value) (= "" value))   "—"
             :else                            value)]
      (when secondary
        [el (cond-> {:class muted}
              (:color (:look secondary)) (assoc :style {:color (:color (:look secondary))}))
         (:value secondary)])
      (when (and sub (not tag?)) [:div {:class muted} sub])
      (when (and sparkline (not tag?)) sparkline)])))

(def ^:private tone-intent
  {:ok :success :pos :success :success :success :warn :warning :warning :warning
   :danger :danger :neg :danger :info :primary :primary :primary})

(defn badge
  ([tone text] (badge tone text nil))
  ([tone text {:keys [look]}]
   (let [long? (and (string? text) (< 48 (count text)))]
     [tag (cond-> {:minimal   true
                   :intent    (get tone-intent (->kw tone))
                   :multiline long?}
            (or (:color look) (:background look))
            (assoc :style (cond-> {}
                            (:color look)      (assoc :color (:color look))
                            (:background look) (assoc :background (:background look)))))
      (when (:icon look) (str (:icon look) " "))
      text])))

(defn num-class
  [x]
  (let [n (fmt/calc x)]
    (cond (nil? n) nil (neg? n) "capp-num--neg" (pos? n) "capp-num--pos" :else nil)))

(defn bars
  [items]
  (into [:div.capp-bars]
        (map (fn [{:keys [label frac value muted?]}]
               [:div.capp-bar
                [:div {:class (cond-> "capp-bar__label" muted? (str " capp-bar__label--muted"))}
                 label]
                [ui/progress-bar
                 {:class   (cond-> "capp-bar__track" muted? (str " capp-bar__track--muted"))
                  :intent  (when-not muted? :primary)
                  :animate false
                  :stripes false
                  :value   (max 0.0 (min 1.0 (or frac 0)))}]
                [:div.capp-bar__value value]]))
        items))

(defn tabs
  [sel items]
  (let [active (if (satisfies? IDeref sel) @sel sel)]
    (into [ui/tabs {:id              (str "capp-tabs-" (hash (mapv :id items)))
                    :animate         false
                    :selected-tab-id active
                    :on-change       (fn [id] (when (satisfies? IReset sel) (reset! sel id)))}]
          (map (fn [{:keys [id label count]}]
                 [ui/tab {:id    id
                          :title label
                          :tag-content count}]))
          items)))

(defn stepper
  [steps current]
  (let [ids   (mapv :id steps)
        cur-i (or (first (keep-indexed (fn [i id] (when (= id current) i)) ids)) 0)]
    (into [:div.capp-stepper]
          (mapcat
           (fn [i {:keys [id label]}]
             (let [state (cond (< i cur-i) "capp-step--done"
                               (= i cur-i) "capp-step--active"
                               :else       nil)
                   step  [:div {:key   (str "s" id)
                                :class (cond-> "capp-step" state (str " " state))}
                          [:div.capp-step__dot (if (< i cur-i) "✓" (inc i))]
                          [:div.capp-step__label label]]]
               (if (pos? i)
                 [[:div.capp-step__bar {:key   (str "b" i)
                                        :class (when (<= i cur-i) "capp-step__bar--on")}]
                  step]
                 [step])))
           (range) steps))))

(defn kpi-row
  ([items] (kpi-row items nil))
  ([items {:keys [display label] :as opts}]
   (let [cards (into [:div {:class (metric/group-class opts)}]
                     (map-indexed (fn [i {:keys [label value sub accent] :as it}]
                                    ^{:key i}
                                    [kpi label value sub accent
                                     (assoc (select-keys it [:on-click :look :description
                                                             :secondary :sparkline])
                                            :display display)]))
                     items)]
     (if label
       [:div.capp-kpi-group [ui/h6 label] cards]
       cards))))

(defn property-list
  [entries {:keys [columns label-position]}]
  (let [below? (= :below label-position)
        cols   (if (and (number? columns) (pos? columns)) (int columns) 1)]
    (into [:dl {:class (str "capp-proplist capp-proplist--" (if below? "below" "beside")
                            (when (> cols 1) " capp-proplist--multi"))
                :style {"--capp-proplist-cols" cols}}]
          (map (fn [{:keys [prop label text prominent? look render]}]
                 ^{:key (str prop)}
                 [:div {:class (cond-> "capp-proplist__item"
                                 prominent? (str " capp-proplist__item--prominent"))}
                  [:dt.capp-proplist__label label]
                  [:dd (cond-> {:class "capp-proplist__value"}
                         (or (:color look) (:background look))
                         (assoc :style (select-keys look [:color :background])))
                   (rule-icon/before (:icon look))
                   (cond render                        render
                         (or (nil? text) (= "" text))  "—"
                         :else                         text)]]))
          entries)))

(defn timeline
  [items]
  (into [:div.capp-timeline]
        (map-indexed
         (fn [i {:keys [at title body tone layer]}]
           [:div.capp-timeline__item {:key i}
            [:div
             {:class (cond-> "capp-timeline__dot" tone (str " capp-timeline__dot--" (name tone)))}]
            [:div.capp-timeline__content
             (when (or at layer)
               [:div.capp-timeline__at at
                (when layer [:span.capp-timeline__layer (str " · " layer)])])
             [:div.capp-timeline__title title]
             (when body [:div.capp-timeline__body body])]])
         items)))

(defn comments
  [_items _opts]
  (let [draft (r/atom "")]
    (fn [items {:keys [on-submit placeholder empty error] :or {placeholder "Kommentar…"}}]
      [:div.capp-comments
       (if (seq items)
         (into [:div.capp-comments__list]
               (map-indexed
                (fn [i {:keys [author at body tone]}]
                  [:div.capp-comment {:key i}
                   [:div.capp-comment__head
                    [:span.capp-comment__author author]
                    (when at [:span.capp-comment__at at])
                    (when tone [badge tone (name tone)])]
                   [:div.capp-comment__body body]])
                items))
         [:div.capp-comments__empty (or empty "Noch keine Kommentare.")])
       (when on-submit
         [:div.capp-comments__composer
          [text-area {:class     "capp-comments__input"
                      :value     @draft
                      :placeholder placeholder
                      :on-change #(reset! draft (.. % -target -value))}]
          [ui/button
           {:intent   :primary
            :disabled (str/blank? @draft)
            :on-click (fn [] (let [t @draft]
                               (when-not (str/blank? t) (on-submit t) (reset! draft ""))))}
           "Senden"]
          (when error [:div.capp-action__error error])])])))

(defn sparkline
  [{:keys [points w h pad baseline class color stretch?] :or {w 80 h 20 pad 1}}]
  (let [{:keys [d baseline-y]} (od/sparkline points w h baseline)]
    (when d
      [:svg {:class                 (ui/cx "capp-spark" class)
             :width                 (when-not stretch? w)
             :height                (when-not stretch? h)
             :view-box              (str "0 " (- pad) " " w " " (+ h (* 2 pad)))
             :preserve-aspect-ratio (when stretch? "none")
             :role                  "img"}
       (when baseline-y
         [:line.capp-spark__baseline {:x1 0 :x2 w :y1 baseline-y :y2 baseline-y}])
       [:path.capp-spark__line {:d d :fill "none" :style (when color {:stroke color})}]])))
