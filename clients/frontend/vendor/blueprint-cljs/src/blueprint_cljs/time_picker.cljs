(ns blueprint-cljs.time-picker
  "TimePicker: hour, minute and optionally second/millisecond fields, arrow keys and
   optional arrow buttons, 12h with an AM/PM select.
   `[time-picker {:value d :on-change f :precision :second :use-am-pm true}]`"
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.date-utils :as du]
            [blueprint-cljs.html-select :refer [html-select]]
            [blueprint-cljs.icon :as icon]))

(def ^:private own-keys
  [:value :default-value :on-change :precision :use-am-pm :show-arrow-buttons
   :select-all-on-focus :min-time :max-time :disabled :auto-focus :on-focus :on-blur
   :on-key-down :on-key-up :class])

(defonce ^:private ids (atom 0))

(defn- min-time [p] (or (:min-time p) (du/default-min-time)))
(defn- max-time [p] (or (:max-time p) (du/default-max-time)))

(defn- initial-value
  [p]
  (or (:value p) (:default-value p) (min-time p)))

(defn- format-unit
  [n unit]
  (case unit
    :hour24 (str n)
    :hour12 (str (du/hour-12 n))
    (:minute :second) (du/pad n 2)
    :ms (du/pad n 3)))

(defn- full-state
  "Blueprint `getFullStateFromValue`."
  [p value]
  (let [v (du/time-in-range (or value (initial-value p)) (min-time p) (max-time p))
        hour-unit (if (:use-am-pm p) :hour12 :hour24)]
    {:hour-text   (format-unit (.getHours v) hour-unit)
     :minute-text (format-unit (.getMinutes v) :minute)
     :second-text (format-unit (.getSeconds v) :second)
     :ms-text     (format-unit (.getMilliseconds v) :ms)
     :value       v
     :pm?         (du/pm? (.getHours v))}))

(def ^:private text-key
  {:hour24 :hour-text :hour12 :hour-text :minute :minute-text :second :second-text
   :ms :ms-text})

(defn time-picker
  "Props: `:value` (js/Date, controlled) / `:default-value`, `:on-change (fn [date])`,
   `:precision` (:minute default, :second, :millisecond), `:use-am-pm`,
   `:show-arrow-buttons`, `:select-all-on-focus`, `:min-time`, `:max-time`, `:disabled`,
   `:auto-focus`, `:on-focus` `:on-blur` `:on-key-down` `:on-key-up` (fn [event unit]),
   `:class`."
  [props]
  (let [state  (r/atom (full-state props nil))
        cur    (atom props)
        uid    (swap! ids inc)
        input-id (fn [unit] (str "bp-timepicker-" (name unit) "-input-" uid))
        update-state!
        (fn [new-state]
          (let [p   @cur
                old @state
                nv  (:value new-state)
                new? (and (some? nv) (not (du/same-time? nv (:value old))))]
            (if (nil? (:value p))
              (reset! state (if new? (full-state p nv) (merge old new-state)))
              (if new?
                (reset! state (full-state p (:value old)))
                (reset! state (merge old new-state
                                     {:value (some-> (:value old) du/clone)}))))
            (when (and new? (:on-change p))
              ((:on-change p) nv))))
        update-time!
        (fn [n unit]
          (let [p     @cur
                value (or (:value @state) (initial-value p))
                nv    (du/clone value)]
            (if (du/unit-valid? unit n)
              (do (du/set-unit! unit n nv (:pm? @state))
                  (if (du/time-in-range? nv (min-time p) (max-time p))
                    (update-state! {:value nv})
                    (update-state! (full-state p value))))
              (update-state! (full-state p value)))))
        shift!
        (fn [unit amount]
          (when-not (:disabled @cur)
            (let [t (+ (du/get-unit unit (or (:value @state) (initial-value @cur))) amount)]
              (update-time! (du/wrap-unit unit t) unit))))
        arrow
        (fn [up? unit]
          (when (:show-arrow-buttons @cur)
            (let [label (str (if up? "Increase " "Decrease ")
                             (get-in du/time-units [unit :label]))]
              [:span {:key        (name unit)
                      :aria-controls (input-id unit)
                      :aria-label label
                      :tab-index  -1
                      :class      (bp/cx (bp/cls "timepicker-arrow-button")
                                         (bp/cls (str "timepicker-"
                                                      (get-in du/time-units [unit :class]))))
                      :on-click   #(shift! unit (if up? 1 -1))}
               [icon/icon {:icon (if up? :chevron-up :chevron-down) :title label}]])))
        input
        (fn [unit]
          (let [p     @cur
                text  (get @state (text-key unit))
                valid? (and (some? text) (du/unit-valid? unit (js/parseInt text 10)))
                hour? (contains? #{:hour12 :hour24} unit)]
            [:input {:aria-label (get-in du/time-units [unit :label])
                     :class      (bp/cx (bp/cls "timepicker-input")
                                        {(bp/intent-class :danger) (not valid?)}
                                        (bp/cls (str "timepicker-"
                                                     (get-in du/time-units [unit :class]))))
                     :id         (input-id unit)
                     :min        0
                     :max        (get-in du/time-units [unit :max])
                     :type       "number"
                     :role       (when (:show-arrow-buttons p) "spinbutton")
                     :value      (or text "")
                     :disabled   (:disabled p)
                     :auto-focus (and hour? (:auto-focus p))
                     :on-change  (fn [e] (swap! state assoc (text-key unit) (.. e -target -value)))
                     :on-blur    (fn [e]
                                   (update-time! (js/parseInt (.. e -target -value) 10) unit)
                                   (when-let [f (:on-blur @cur)] (f e unit)))
                     :on-focus   (fn [e]
                                   (when (:select-all-on-focus @cur) (.select (.-currentTarget e)))
                                   (when-let [f (:on-focus @cur)] (f e unit)))
                     :on-key-down (fn [e]
                                    (case (.-key e)
                                      "ArrowUp"   (do (.preventDefault e) (shift! unit 1))
                                      "ArrowDown" (do (.preventDefault e) (shift! unit -1))
                                      "Enter"     (do (.preventDefault e)
                                                      (.blur (.-currentTarget e)))
                                      nil)
                                    (when-let [f (:on-key-down @cur)] (f e unit)))
                     :on-key-up  (fn [e] (when-let [f (:on-key-up @cur)] (f e unit)))}]))
        divider (fn [text] [:span {:class (bp/cls "timepicker-divider-text")} text])]
    (r/create-class
     {:display-name "TimePicker"
      :component-did-update
      (fn [_ [_ old]]
        (let [p      @cur
              old    (or old {})
              bounds? (or (not= (:min-time old) (:min-time p))
                          (not= (:max-time old) (:max-time p)))
              value? (not= (:value old) (:value p))]
          (when (or bounds? value?)
            (let [v (if (nil? (:value p)) (initial-value p) (:value @state))
                  v (if bounds?
                      (du/time-in-range (or (:value @state) (initial-value p))
                                        (min-time p) (max-time p))
                      v)
                  v (if (and (:value p) (not (du/same-time? (:value p) (:value old))))
                      (:value p)
                      v)]
              (reset! state (full-state p v))))))
      :reagent-render
      (fn [props]
        (reset! cur props)
        (let [prec  (keyword (or (:precision props) :minute))
              ms?   (= prec :millisecond)
              secs? (or ms? (= prec :second))
              hour  (if (:use-am-pm props) :hour12 :hour24)
              units (cond-> [hour :minute] secs? (conj :second) ms? (conj :ms))]
          [:div (merge (bp/dom-props props own-keys)
                       {:class (bp/cx (bp/cls "timepicker")
                                      {(bp/cls "disabled") (:disabled props)}
                                      (:class props))})
           (into [:div {:class (bp/cls "timepicker-arrow-row")}]
                 (keep #(arrow true %) units))
           [:div {:class (bp/cls "timepicker-input-row")}
            (input hour)
            (divider ":")
            (input :minute)
            (when secs? (divider ":"))
            (when secs? (input :second))
            (when ms? (divider "."))
            (when ms? (input :ms))]
           (when (:use-am-pm props)
             [html-select
              {:class     (bp/cls "timepicker-ampm-select")
               :disabled  (:disabled props)
               :value     (if (:pm? @state) "pm" "am")
               :on-change (fn [e]
                            (let [next-pm? (= "pm" (.. e -currentTarget -value))]
                              (when (not= next-pm? (:pm? @state))
                                (let [v (or (:value @state) (initial-value @cur))
                                      h (du/meridiem-hour (.getHours v) next-pm?)]
                                  (swap! state assoc :pm? next-pm?)
                                  (update-time! h :hour24)))))}
              [:option {:value "am"} "AM"]
              [:option {:value "pm"} "PM"]])
           (into [:div {:class (bp/cls "timepicker-arrow-row")}]
                 (keep #(arrow false %) units))]))})))
