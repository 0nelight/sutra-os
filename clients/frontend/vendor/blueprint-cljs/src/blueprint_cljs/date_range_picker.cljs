(ns blueprint-cljs.date-range-picker
  "DateRangePicker: two calendars (contiguous or independent) or one, with shortcuts,
   hover preview of the range a click would make, and optional time pickers.
   `[date-range-picker {:value [start end] :on-change (fn [[start end]])}]`"
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.date-shortcuts :as shortcuts]
            [blueprint-cljs.date-utils :as du]
            [blueprint-cljs.day-picker :refer [day-picker]]
            [blueprint-cljs.divider :refer [divider]]
            [blueprint-cljs.time-picker :refer [time-picker]]))

(def defaults
  {:allow-single-day-range false :contiguous-calendar-months true :shortcuts true
   :single-month-only false :reverse-month-and-year-menus false})

(def ^:private own-keys
  [:value :default-value :on-change :on-hover-change :min-date :max-date :initial-month
   :allow-single-day-range :contiguous-calendar-months :single-month-only :shortcuts
   :selected-shortcut-index :on-shortcut-change :time-precision :time-picker-props
   :highlight-current-day :reverse-month-and-year-menus :boundary-to-modify
   :footer-element :day-picker-props :locale :first-day-of-week :class])

(def hover-class-names
  {:hovered-range       "rdp-day_hovered"
   :hovered-range-start "rdp-day_hovered_start"
   :hovered-range-end   "rdp-day_hovered_end"})

(defn- with-defaults
  [p]
  (merge defaults
         {:min-date (du/default-min-date) :max-date (du/default-max-date)}
         (into {} (remove (comp nil? val)) p)))

(defn- single-month-only?
  [{:keys [single-month-only min-date max-date]}]
  (boolean (or single-month-only (du/same-month? min-date max-date))))

(defn- prev-month-of [d] (du/add-months d -1))

(defn- initial-month
  "Blueprint `getInitialMonth` for ranges: a month in the max month shows on the right."
  [{:keys [initial-month min-date max-date] :as p} [s e]]
  (let [single? (single-month-only? p)
        today   (js/Date.)
        right   (fn [d] (if (and (not single?) (du/same-month? d max-date)) (prev-month-of d) d))]
    (cond
      initial-month (right initial-month)
      s             (right s)
      e             (if (du/same-month? e min-date) e (prev-month-of e))
      (du/day-in-range? today [min-date max-date]) (right today)
      :else         (right (du/date-between [min-date max-date])))))

(defn- range-mods
  "react-day-picker's range modifiers for `[from to]`."
  [day [from to]]
  (let [same? #(and % (du/same-day? % day))
        k     (du/day-key day)]
    (cond-> #{}
      (or (and from to (<= (min (du/day-key from) (du/day-key to)) k
                           (max (du/day-key from) (du/day-key to))))
          (and (nil? from) (same? to))
          (and (nil? to) (same? from)))
      (conj :selected)
      (same? (or from to)) (conj :range_start)
      (if from (same? (or to from)) (same? to)) (conj :range_end)
      (and from to (not (du/same-day? from to))
           (< (du/day-key from) k (du/day-key to)))
      (conj :range_middle))))

(defn- hover-mods
  [day [s e] [hs he]]
  (cond-> #{}
    (and (or s e) hs he (du/day-in-range? day [hs he] true)) (conj :hovered-range)
    (and hs (du/same-day? hs day))                          (conj :hovered-range-start)
    (and he (du/same-day? he day))                          (conj :hovered-range-end)))

;; -------------------------------------------------------- following the selection

(defn- displayed?
  [d display single?]
  (and d (or (du/same-month? d display)
             (and (not single?) (du/same-month? d (du/add-months display 1))))))

(defn next-contiguous-display
  "Blueprint `useContiguousCalendarViews`: where the left month goes after a selection."
  [display [s e] [ps _] single?]
  (let [s-m (some-> s du/month-start)
        e-m (some-> e du/month-start)]
    (cond
      (and (or (nil? s) (nil? e))
           (or (displayed? s display single?) (displayed? e display single?)))
      display

      (and (nil? s-m) e-m)
      (if (du/same-month? e-m (du/add-months display 1)) display (du/add-months e-m -1))

      (and s-m (nil? e-m))
      (if (du/same-month? s-m display) display s-m)

      (and s-m e-m)
      (cond
        (du/same-month? s-m e-m)
        (if (or (du/same-month? display s-m)
                (and (not single?) (du/same-month? (du/add-months display 1) e-m)))
          display
          s-m)
        (= (some-> ps .valueOf) (.valueOf s)) (if single? e-m (du/add-months e-m -1))
        :else s-m)

      :else display)))

(defn next-views
  "Blueprint `useNonContiguousCalendarViews`' effect: left and right months after a
   selection."
  [{:keys [left right]} [s e]]
  (let [s-m (some-> s du/month-start)
        e-m (some-> e du/month-start)
        before? (fn [a b] (< (du/month-diff b a) 0))]
    (cond
      (and (nil? s-m) e-m)
      (if (or (du/same-month? e-m left) (du/same-month? e-m right))
        {:left left :right right}
        {:left (if (before? left e-m) left (du/add-months e-m -1)) :right e-m})

      (and s-m (nil? e-m))
      (if (or (du/same-month? s-m left) (du/same-month? s-m right))
        {:left left :right right}
        {:left s-m :right (if (before? s-m right) right (du/add-months s-m 1))})

      (and s-m e-m)
      (if (du/same-month? s-m e-m)
        (if (or (du/same-month? left s-m) (du/same-month? right s-m))
          {:left left :right right}
          {:left s-m :right (du/add-months s-m 1)})
        (let [[l r] (if (du/same-month? left s-m) [left right] [s-m (du/add-months s-m 1)])]
          {:left l :right (if (du/same-month? r e-m) r e-m)}))

      :else {:left left :right right})))

(defn- bounded-views
  [left right min-date max-date]
  (let [min-v (du/month-start min-date)
        max-v (du/month-start max-date)]
    (cond
      (< (du/month-diff min-v left) 0)  [min-v (du/add-months min-v 1)]
      (> (du/month-diff max-v right) 0) [(du/add-months max-v -1) max-v]
      :else                             [left right])))

;; -------------------------------------------------------------------- component

(defn date-range-picker
  "Props (Blueprint DateRangePicker, kebab-cased): `:value [start end]` (controlled when
   present) / `:default-value`, `:on-change (fn [[start end]])`, `:on-hover-change
   (fn [range-or-nil day boundary])`, `:min-date`, `:max-date`, `:initial-month`,
   `:allow-single-day-range`, `:contiguous-calendar-months` (default true),
   `:single-month-only`, `:shortcuts` (true default, false, or `[{:label :date-range
   :include-time}]`), `:selected-shortcut-index`, `:on-shortcut-change`,
   `:time-precision`, `:time-picker-props`, `:highlight-current-day`,
   `:reverse-month-and-year-menus`, `:boundary-to-modify` (:start/:end),
   `:footer-element`, `:day-picker-props`, `:locale`, `:first-day-of-week`, `:class`."
  [props]
  (let [p0      (with-defaults props)
        v0      (or (:value props) (:default-value props) [nil nil])
        im      (du/month-start (initial-month p0 v0))
        state   (r/atom {:value    (vec v0)
                         :time     (vec v0)
                         :hover    [nil nil]
                         :sc-index (or (:selected-shortcut-index props) -1)
                         :display  im
                         :views    {:left  im
                                    :right (let [e (second v0)]
                                             (if (or (nil? e) (du/same-month? e im))
                                               (du/add-months im 1)
                                               (du/month-start e)))}})
        cur     (atom p0)
        prev-value (atom (vec v0))
        update-range!
        (fn [[s e] & [[ts te]]]
          (let [p       @cur
                [ts te] (if (some? ts) [ts te] (:time @state))
                rng     [(du/date-time s ts) (du/date-time e te)]]
            (when (nil? (:value (:raw p)))
              (swap! state assoc :time [ts te] :value rng))
            (when-let [f (:on-change p)] (f rng))))
        on-mouse-enter
        (fn [day mods _]
          (let [p @cur]
            (when-not (mods :disabled)
              (let [{:keys [range boundary]}
                    (du/next-range-state (:value @state) day (:allow-single-day-range p)
                                         (:boundary-to-modify p))]
                (swap! state assoc :hover range)
                (when-let [f (:on-hover-change p)] (f range day boundary))))))
        on-mouse-leave
        (fn [day mods _]
          (when-not (mods :disabled)
            (swap! state assoc :hover nil)
            (when-let [f (:on-hover-change @cur)] (f nil day nil))))
        on-day-click
        (fn [day mods _]
          (let [p @cur]
            (when-not (mods :disabled)
              (let [{:keys [range boundary]}
                    (du/next-range-state (:value @state) day (:allow-single-day-range p)
                                         (:boundary-to-modify p))]
                (swap! state assoc :hover range)
                (when-let [f (:on-hover-change p)] (f range day boundary))
                (update-range! range)))))
        on-shortcut-click
        (fn [{:keys [date-range include-time] :as sc} i]
          (if include-time
            (update-range! date-range date-range)
            (update-range! date-range))
          (when (nil? (:selected-shortcut-index (:raw @cur)))
            (swap! state assoc :sc-index i))
          (when-let [f (:on-shortcut-change @cur)] (f sc i)))
        on-time-change
        (fn [i t]
          (let [p     @cur
                _     (when-let [f (get-in p [:time-picker-props :on-change])] (f t))
                value (:value @state)
                other (get value (- 1 i))
                base  (cond
                        (some? (get value i))              (du/clone (get value i))
                        (nil? other)                       (js/Date.)
                        (:allow-single-day-range p)        other
                        :else (du/add-days other (if (zero? i) -1 1)))
                rng   (assoc value i (du/date-time base t))]
            (when-let [f (:on-change p)] (f rng))
            (swap! state assoc :time (assoc (:time @state) i t) :value rng)))
        follow-selection!
        (fn []
          (let [p   @cur
                v   (:value @state)
                pv  @prev-value]
            (when-not (= v pv)
              (if (or (:contiguous-calendar-months p) (single-month-only? p))
                (swap! state update :display next-contiguous-display v pv
                       (single-month-only? p))
                (swap! state update :views next-views v))
              (reset! prev-value v))))]
    (r/create-class
     {:display-name "DateRangePicker"
      :component-did-update
      (fn [this [_ old]]
        (let [new (r/props this)]
          (when (and (some? (:value old)) (some? (:value new))
                     (not (du/ranges-equal? (:value old) (:value new))))
            (swap! state assoc :value (vec (:value new))))
          (when (not= (:contiguous-calendar-months old) (:contiguous-calendar-months new))
            (let [m (du/month-start (initial-month @cur (:value @state)))]
              (swap! state assoc :display m :views {:left m :right (du/add-months m 1)})))
          (when (not= (:selected-shortcut-index old) (:selected-shortcut-index new))
            (swap! state assoc :sc-index (:selected-shortcut-index new))))
        (follow-selection!))
      :reagent-render
      (fn [props]
        (let [p (assoc (with-defaults props) :raw props)
              _ (reset! cur p)
              {:keys [min-date max-date shortcuts time-precision time-picker-props
                      footer-element day-picker-props locale contiguous-calendar-months]} p
              {:keys [value hover sc-index display views]} @state
              single?  (single-month-only? p)
              dpp      day-picker-props
              common   {:locale                       locale
                        :first-day-of-week            (:first-day-of-week p)
                        :show-week-number             (:show-week-number dpp)
                        :reverse-month-and-year-menus (:reverse-month-and-year-menus p)
                        :disabled                     (:disabled dpp)
                        :class-names                  (merge hover-class-names
                                                             (:modifiers-class-names dpp))
                        :day-mods                     (fn [day]
                                                        (into (range-mods day value)
                                                              (concat
                                                               (hover-mods day value hover)
                                                               (for [[k f] (:modifiers dpp)
                                                                     :when (f day)] k))))
                        :on-day-click                 on-day-click
                        :on-day-mouse-enter           on-mouse-enter
                        :on-day-mouse-leave           on-mouse-leave}
              precision (or time-precision (:precision time-picker-props))
              long-tp?  (or (:use-am-pm time-picker-props)
                            (contains? #{:second :millisecond} (some-> precision keyword)))]
          [:div (merge (bp/dom-props props own-keys)
                       {:class (bp/cx (bp/cls "datepicker") (bp/cls "daterangepicker")
                                      (:class props)
                                      {(bp/cls "datepicker-highlight-current-day")
                                       (:highlight-current-day p)
                                       (bp/cls "daterangepicker-contiguous")
                                       contiguous-calendar-months
                                       (bp/cls "daterangepicker-single-month") single?
                                       (bp/cls "daterangepicker-reverse-month-and-year")
                                       (:reverse-month-and-year-menus p)})})
           (when shortcuts
             [:<>
              [shortcuts/shortcut-menu
               {:allow-single-day-range  (:allow-single-day-range p)
                :max-date                max-date
                :min-date                min-date
                :on-shortcut-click       on-shortcut-click
                :selected-shortcut-index sc-index
                :shortcuts               shortcuts
                :time-precision          time-precision}]
              [divider]])
           [:div {:class (bp/cls "datepicker-content")}
            (if (or contiguous-calendar-months single?)
              [day-picker (merge common
                                 {:months          (if single?
                                                     [display]
                                                     [display (du/add-months display 1)])
                                  :caption         :dropdown-buttons
                                  :from-date       min-date
                                  :to-date         max-date
                                  :on-month-change #(swap! state assoc :display
                                                           (du/month-start %))})]
              (let [{:keys [left right]} views
                    set-views! (fn [[l r]]
                                 (swap! state assoc :views {:left l :right r}))]
                [:div {:class (bp/cls "daterangepicker-calendars")}
                 [day-picker (merge common
                                    {:key             "left"
                                     :months          [left]
                                     :caption         :blueprint
                                     :from-date       min-date
                                     :to-date         (du/month-end (prev-month-of max-date))
                                     :on-month-change
                                     (fn [d]
                                       (let [l (du/month-start d)
                                             r (if (< (du/month-diff right l) 0)
                                                 right (du/add-months l 1))]
                                         (set-views! (bounded-views l r min-date max-date))
                                         (when-let [f (get dpp :on-month-change)] (f l))))})]
                 [day-picker (merge common
                                    {:key             "right"
                                     :months          [right]
                                     :caption         :blueprint
                                     :from-date       (du/add-months min-date 1)
                                     :to-date         max-date
                                     :on-month-change
                                     (fn [d]
                                       (let [r (du/month-start d)
                                             l (if (> (du/month-diff left r) 0)
                                                 left (du/add-months r -1))]
                                         (set-views! (bounded-views l r min-date max-date))
                                         (when-let [f (get dpp :on-month-change)] (f r))))})]]))
            (when (or precision (seq time-picker-props))
              [:div {:class (bp/cx (bp/cls "daterangepicker-timepickers")
                                   {(bp/cls "daterangepicker-timepickers-stacked")
                                    (and single? long-tp?)})}
               [time-picker (merge {:precision precision} time-picker-props
                                   {:on-change #(on-time-change 0 %)
                                    :value     (first (:time @state))})]
               [time-picker (merge {:precision precision} time-picker-props
                                   {:on-change #(on-time-change 1 %)
                                    :value     (second (:time @state))})]])
            footer-element]]))})))
