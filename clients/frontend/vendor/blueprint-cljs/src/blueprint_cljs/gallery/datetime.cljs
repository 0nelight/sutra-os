(ns blueprint-cljs.gallery.datetime
  (:require [reagent.core :as r]
            [blueprint-cljs.date-input :refer [date-input]]
            [blueprint-cljs.date-picker :refer [date-picker]]
            [blueprint-cljs.date-range-input :refer [date-range-input]]
            [blueprint-cljs.date-range-picker :refer [date-range-picker]]
            [blueprint-cljs.date-utils :as du]
            [blueprint-cljs.switch :refer [switch]]
            [blueprint-cljs.time-picker :refer [time-picker]]
            [blueprint-cljs.timezone-select :refer [timezone-select]]))

(defn- row [& xs] (into [:div {:style {:display "flex" :gap 24 :align-items "flex-start"
                                       :flex-wrap "wrap"}}] xs))

(defn- col [& xs] (into [:div {:style {:display "flex" :flex-direction "column" :gap 8}}] xs))

(defn- caption [s] [:div {:class "bp-text-muted" :style {:font-size 12}} s])

(defn- show [d] (if (and d (du/valid? d)) (du/format-date d "yyyy-MM-dd HH:mm:ss") "–"))

(defn- show-range [[s e]] (str (show s) "  →  " (show e)))

(defn- toggles
  [opts keys]
  (let [o @opts]
    (into [row]
          (for [[k label] keys]
            [switch {:label label :checked (boolean (get o k))
                     :on-change #(swap! opts update k not)}]))))

(defn- date-picker-demo
  []
  (let [v    (r/atom (js/Date. 2025 0 15))
        opts (r/atom {:show-actions-bar true :highlight-current-day true})]
    (fn []
      [col
       [toggles opts [[:show-actions-bar "Actions bar"] [:shortcuts "Shortcuts"]
                      [:highlight-current-day "Highlight current day"]
                      [:reverse-month-and-year-menus "Reverse month and year"]
                      [:time "Time precision (second)"] [:de "German locale"]]]
       [row
        [col
         [date-picker (merge (dissoc @opts :time :de)
                             {:value     @v
                              :on-change #(reset! v %)
                              :min-date  (js/Date. 2015 0 1)
                              :max-date  (js/Date. 2027 11 31)
                              :locale    (if (:de @opts) "de-DE" "en-US")}
                             (when (:time @opts) {:time-precision :second}))]
         [caption (str "value: " (show @v))]]
        [col
         [caption "Uncontrolled, time precision minute, AM/PM, arrows"]
         [date-picker {:default-value     (js/Date. 2025 5 3 14 30)
                       :time-precision    :minute
                       :time-picker-props {:use-am-pm true :show-arrow-buttons true}
                       :min-date          (js/Date. 2020 0 1)
                       :max-date          (js/Date. 2026 11 31)}]]]])))

(defn- date-range-picker-demo
  []
  (let [v    (r/atom [(js/Date. 2025 0 8) (js/Date. 2025 0 21)])
        opts (r/atom {:shortcuts true :contiguous-calendar-months true})]
    (fn []
      [col
       [toggles opts [[:shortcuts "Shortcuts"] [:contiguous-calendar-months "Contiguous months"]
                      [:single-month-only "Single month only"]
                      [:allow-single-day-range "Allow single day range"]
                      [:time "Time precision"]]]
       [date-range-picker (merge (dissoc @opts :time)
                                 {:value     @v
                                  :on-change #(reset! v %)
                                  :min-date  (js/Date. 2015 0 1)
                                  :max-date  (js/Date. 2027 11 31)}
                                 (when (:time @opts) {:time-precision :minute}))]
       [caption (str "value: " (show-range @v))]])))

(defn- time-picker-demo
  []
  (let [v (r/atom (js/Date. 2025 0 1 9 41 7 250))]
    (fn []
      [col
       [row
        [col [caption "Minute (default)"]
         [time-picker {:value @v :on-change #(reset! v %)}]]
        [col [caption "Second, arrow buttons"]
         [time-picker {:value @v :on-change #(reset! v %) :precision :second
                       :show-arrow-buttons true}]]
        [col [caption "Millisecond"]
         [time-picker {:value @v :on-change #(reset! v %) :precision :millisecond}]]
        [col [caption "AM/PM"]
         [time-picker {:value @v :on-change #(reset! v %) :use-am-pm true}]]
        [col [caption "Disabled"]
         [time-picker {:value @v :disabled true :show-arrow-buttons true}]]
        [col [caption "08:00 – 17:30"]
         [time-picker {:default-value (js/Date. 2025 0 1 12 0)
                       :min-time (js/Date. 0 0 0 8 0) :max-time (js/Date. 0 0 0 17 30)}]]]
       [caption (str "value: " (show @v))]])))

(defn- date-input-demo
  []
  (let [v  (r/atom (js/Date. 2025 2 14))
        v2 (r/atom nil)]
    (fn []
      [col
       [row
        [col [caption "Controlled, default format yyyy-MM-dd"]
         [date-input {:value @v :on-change #(reset! v %) :show-actions-bar true
                      :min-date (js/Date. 2015 0 1) :max-date (js/Date. 2027 11 31)}]
         [caption (str "value: " (show @v))]]
        [col [caption "Time precision minute, German dd.MM.yyyy HH:mm"]
         [date-input {:value @v2 :on-change #(reset! v2 %) :time-precision :minute
                      :locale "de-DE" :date-fns-format "dd.MM.yyyy HH:mm"
                      :placeholder "TT.MM.JJJJ hh:mm"}]
         [caption (str "value: " (show @v2))]]
        [col [caption "Uncontrolled, shortcuts, fill"]
         [:div {:style {:width 260}}
          [date-input {:default-value (js/Date. 2024 10 2) :shortcuts true :fill true
                       :close-on-selection false}]]]
        [col [caption "Disabled"]
         [date-input {:default-value (js/Date. 2024 10 2) :disabled true}]]]])))

(defn- date-range-input-demo
  []
  (let [v (r/atom [(js/Date. 2025 3 2) (js/Date. 2025 3 9)])]
    (fn []
      [col
       [row
        [col [caption "Controlled"]
         [date-range-input {:value @v :on-change #(reset! v %)
                            :min-date (js/Date. 2015 0 1) :max-date (js/Date. 2027 11 31)}]
         [caption (str "value: " (show-range @v))]]
        [col [caption "Uncontrolled, single month, allow single day"]
         [date-range-input {:single-month-only true :allow-single-day-range true
                            :shortcuts false}]]
        [col [caption "Time precision"]
         [date-range-input {:time-precision :minute :shortcuts false}]]]])))

(defn- timezone-select-demo
  []
  (let [tz   (r/atom "Europe/Vienna")
        tz2  (r/atom nil)
        opts (r/atom {:show-local-timezone true})]
    (fn []
      [col
       [toggles opts [[:show-local-timezone "Show local timezone"] [:fill "Fill"]
                      [:disabled "Disabled"]]]
       [row
        (into [col]
              (for [f [:composite :abbreviation :offset :long-name :code]]
                [:div {:style {:display "flex" :gap 8 :align-items "center"}}
                 [:div {:style {:width 110}} [caption (name f)]]
                 [timezone-select (merge @opts {:value @tz :on-change #(reset! tz %)
                                                :value-display-format f})]]))
        [col [caption "No value, placeholder"]
         [timezone-select (merge @opts {:value @tz2 :on-change #(reset! tz2 %)})]]]
       [caption (str "value: " @tz)]])))

(def sections
  "Each: {:id kw :title str :demo (fn [] hiccup)}."
  [{:id :date-picker :title "Date picker" :demo date-picker-demo}
   {:id :date-range-picker :title "Date range picker" :demo date-range-picker-demo}
   {:id :time-picker :title "Time picker" :demo time-picker-demo}
   {:id :date-input :title "Date input" :demo date-input-demo}
   {:id :date-range-input :title "Date range input" :demo date-range-input-demo}
   {:id :timezone-select :title "Timezone select" :demo timezone-select-demo}])
