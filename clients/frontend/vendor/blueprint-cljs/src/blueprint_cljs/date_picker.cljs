(ns blueprint-cljs.date-picker
  "DatePicker: a month calendar with month/year selects, optional time picker, action bar
   and shortcuts. `[date-picker {:value d :on-change (fn [date user-change?])}]`"
  (:require [reagent.core :as r]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.date-shortcuts :as shortcuts]
            [blueprint-cljs.date-utils :as du]
            [blueprint-cljs.day-picker :refer [day-picker]]
            [blueprint-cljs.divider :refer [divider]]
            [blueprint-cljs.time-picker :refer [time-picker]]))

(def defaults
  {:can-clear-selection true :clear-button-text "Clear" :today-button-text "Today"
   :highlight-current-day false :reverse-month-and-year-menus false :shortcuts false
   :show-actions-bar false})

(def ^:private own-keys
  [:value :default-value :on-change :min-date :max-date :initial-month :locale
   :first-day-of-week :highlight-current-day :reverse-month-and-year-menus :shortcuts
   :selected-shortcut-index :on-shortcut-change :show-actions-bar :show-action-bar
   :can-clear-selection :clear-button-text :today-button-text :time-precision
   :time-picker-props :footer-element :day-picker-props :class :timezone])

(defn- with-defaults
  [p]
  (merge defaults
         {:min-date (du/default-min-date) :max-date (du/default-max-date)}
         (into {} (remove (comp nil? val)) p)
         (when (contains? p :show-action-bar) {:show-actions-bar (:show-action-bar p)})))

(defn- initial-month
  [{:keys [initial-month min-date max-date]} value]
  (let [today (js/Date.)]
    (cond
      initial-month                               initial-month
      value                                       value
      (du/day-in-range? today [min-date max-date]) today
      :else                                       (du/date-between [min-date max-date]))))

(defn date-picker
  "Props (Blueprint DatePicker, kebab-cased): `:value` (js/Date or nil; controlled when the
   key is present) / `:default-value`, `:on-change (fn [date-or-nil user-change?])`,
   `:min-date`, `:max-date`, `:initial-month`, `:locale` (default \"en-US\"),
   `:first-day-of-week`, `:highlight-current-day`, `:reverse-month-and-year-menus`,
   `:shortcuts` (true or `[{:label :date :include-time}]`), `:selected-shortcut-index`,
   `:on-shortcut-change (fn [shortcut index])`, `:show-actions-bar`, `:can-clear-selection`,
   `:clear-button-text`, `:today-button-text`, `:time-precision` (:minute :second
   :millisecond), `:time-picker-props`, `:footer-element`, `:day-picker-props`
   ({:disabled (fn [day]) :show-week-number bool :modifiers {k (fn [day])}
   :modifiers-class-names {k class}}), `:class`."
  [props]
  (let [p0    (with-defaults props)
        v0    (if (contains? props :value) (:value props) (:default-value props))
        im    (initial-month p0 v0)
        state (r/atom {:display  (du/month-start im)
                       :selected-day (some-> v0 .getDate)
                       :sc-index (or (:selected-shortcut-index props) -1)
                       :value    v0})
        cur   (atom p0)
        controlled? (fn [] (contains? (:raw @cur) :value))
        update-value!
        (fn [value user? & [skip-on-change?]]
          (let [p @cur]
            (when (and (not skip-on-change?) (:on-change p))
              ((:on-change p) value user?))
            (when-not (controlled?)
              (swap! state assoc :value value))))
        update-day!
        (fn [day]
          (swap! state assoc :display (du/month-start day) :selected-day (.getDate day)))
        valid-date-in-month
        (fn [year month]
          (let [{:keys [min-date max-date]} @cur
                sel   (:selected-day @state)
                max-d (.getDate (js/Date. year (inc month) 0))
                day   (if (nil? sel) 1 (min sel max-d))
                v     (du/date-time (js/Date. year month day 12) (:value @state))]
            (cond
              (and v (< (.getTime v) (.getTime min-date))) min-date
              (and v (> (.getTime v) (.getTime max-date))) max-date
              :else v)))
        on-month-change
        (fn [d]
          (let [nd (valid-date-in-month (.getFullYear d) (.getMonth d))]
            (when nd (swap! state assoc :display (du/month-start nd)))
            (when (some? (:value @state))
              (update-value! nd false))))
        on-day-click
        (fn [day mods _]
          (when-not (mods :disabled)
            (update-day! day)
            (update-value! (if (and (:can-clear-selection @cur) (mods :selected))
                             nil
                             (du/date-time day (:value @state)))
                           true)))
        on-today
        (fn []
          (let [t (js/Date.)]
            (update-day! t)
            (update-value! t true)))
        on-time-change
        (fn [t]
          (when-let [f (get-in @cur [:time-picker-props :on-change])] (f t))
          (update-value! (du/date-time (or (:value @state) (js/Date.)) t) true))
        on-shortcut-click
        (fn [{:keys [date-range include-time] :as sc} i]
          (let [nd (first date-range)]
            (when nd
              (update-day! nd)
              (update-value! (if include-time nd (du/date-time nd (:value @state))) true)
              (when (nil? (:selected-shortcut-index (:raw @cur)))
                (swap! state assoc :sc-index i))
              (when-let [f (:on-shortcut-change @cur)]
                (f (-> sc (dissoc :date-range) (assoc :date nd)) i)))))]
    (r/create-class
     {:display-name "DatePicker"
      :component-did-update
      (fn [this [_ old]]
        (let [new (r/props this)]
          (when (not= (:value old) (:value new))
            (if-let [v (:value new)]
              (swap! state assoc :display (du/month-start v) :selected-day (.getDate v)
                     :value v)
              (swap! state assoc :value nil)))
          (when (not= (:selected-shortcut-index old) (:selected-shortcut-index new))
            (swap! state assoc :sc-index (:selected-shortcut-index new)))))
      :reagent-render
      (fn [props]
        (let [p (assoc (with-defaults props) :raw props)
              _ (reset! cur p)
              {:keys [min-date max-date shortcuts time-precision time-picker-props
                      footer-element day-picker-props locale]} p
              {:keys [value display sc-index]} @state
              dpp day-picker-props]
          [:div (merge (bp/dom-props props own-keys)
                       {:class (bp/cx (bp/cls "datepicker")
                                      (:class props)
                                      {(bp/cls "datepicker-highlight-current-day")
                                       (:highlight-current-day p)
                                       (bp/cls "datepicker-reverse-month-and-year")
                                       (:reverse-month-and-year-menus p)})})
           (when (and shortcuts (not (false? shortcuts)))
             [:<>
              [shortcuts/shortcut-menu
               {:allow-single-day-range    true
                :max-date                  max-date
                :min-date                  min-date
                :selected-shortcut-index   sc-index
                :shortcuts                 (if (true? shortcuts)
                                             true
                                             (for [s shortcuts]
                                               (assoc s :date-range [(:date s) nil])))
                :time-precision            time-precision
                :on-shortcut-click         on-shortcut-click
                :use-single-date-shortcuts true}]
              [divider]])
           [:div {:class (bp/cls "datepicker-content")}
            [day-picker
             {:months            [display]
              :caption           :dropdown-buttons
              :from-date         min-date
              :to-date           max-date
              :locale            locale
              :first-day-of-week (:first-day-of-week p)
              :show-week-number  (:show-week-number dpp)
              :reverse-month-and-year-menus (:reverse-month-and-year-menus p)
              :disabled          (:disabled dpp)
              :class-names       (:modifiers-class-names dpp)
              :day-mods          (fn [day]
                                   (cond-> (set (for [[k f] (:modifiers dpp)
                                                      :when (f day)] k))
                                     (and value (du/same-day? value day)) (conj :selected)))
              :on-day-click      on-day-click
              :on-month-change   on-month-change}]
            (when (or time-precision time-picker-props)
              (let [apply-min (and value (du/same-day? value min-date))
                    apply-max (and value (du/same-day? value max-date))]
                [:div {:class (bp/cls "datepicker-timepicker-wrapper")}
                 [time-picker (merge {:precision time-precision
                                      :min-time  (when apply-min min-date)
                                      :max-time  (when apply-max max-date)}
                                     time-picker-props
                                     {:on-change on-time-change
                                      :value     value})]]))
            (when (:show-actions-bar p)
              [:<>
               [divider]
               [:div {:class (bp/cls "datepicker-footer")}
                [button/button {:disabled (not (du/day-in-range? (js/Date.)
                                                                 [min-date max-date]))
                                :on-click on-today
                                :text     (:today-button-text p)
                                :variant  :minimal}]
                [button/button {:disabled (not (:can-clear-selection p))
                                :on-click #(update-value! nil true)
                                :text     (:clear-button-text p)
                                :variant  :minimal}]]])
            footer-element]]))})))
