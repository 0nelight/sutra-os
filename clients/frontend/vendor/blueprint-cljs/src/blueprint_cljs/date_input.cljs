(ns blueprint-cljs.date-input
  "DateInput: a text field that parses typed dates and opens a DatePicker in a popover.
   `[date-input {:value d :on-change (fn [date-or-nil user-change?])}]`
   Values are js/Date (Blueprint 6 speaks ISO strings with a timezone; this port keeps
   the Date and leaves timezones out)."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.date-picker :refer [date-picker]]
            [blueprint-cljs.date-utils :as du]
            [blueprint-cljs.input-group :refer [input-group]]
            [blueprint-cljs.popover :as popover]))

(def invalid-date-message "Invalid date")
(def out-of-range-message "Out of range")

(def ^:private input-keys
  [:value :default-value :on-change :on-error :format-date :parse-date :date-fns-format
   :placeholder :min-date :max-date :close-on-selection :disabled :fill :input-props
   :popover-props :right-element :invalid-date-message :out-of-range-message :class])

(def ^:private focusable
  (str "a[href]:not([tabindex='-1']), button:not([disabled]):not([tabindex='-1']),"
       " input:not([disabled]):not([tabindex='-1']), select:not([disabled]):not([tabindex='-1']),"
       " textarea:not([disabled]):not([tabindex='-1']), [tabindex]:not([tabindex='-1'])"))

(defn focusable-elements
  "Keyboard-focusable elements inside `el`, without the two focus-boundary sentinels."
  [el]
  (if el
    (->> (array-seq (.querySelectorAll el focusable))
         (remove #(.hasAttribute % "data-focus-boundary"))
         vec)
    []))

(defn- with-defaults
  [p]
  (merge {:close-on-selection true :disabled false :input-props {} :popover-props {}
          :invalid-date-message invalid-date-message
          :out-of-range-message out-of-range-message
          :locale du/default-locale}
         {:min-date (du/default-min-date) :max-date (du/default-max-date)}
         (into {} (remove (comp nil? val)) p)))

(defn format-raw
  "`d` through `:format-date`, else `:date-fns-format` or the precision's default."
  [{:keys [format-date date-fns-format time-precision time-picker-props locale]} d]
  (if format-date
    (format-date d locale)
    (du/format-date d (or date-fns-format
                          (du/default-format time-precision time-picker-props)))))

(defn formatter
  "Blueprint `useDateFormatter`."
  [{:keys [min-date max-date invalid-date-message out-of-range-message] :as p}]
  (fn [d]
    (cond
      (nil? d)                                 ""
      (not (du/valid? d))                      invalid-date-message
      (du/day-in-range? d [min-date max-date]) (format-raw p d)
      :else                                    out-of-range-message)))

(defn parser
  "Blueprint `useDateParser`: nil for a message string, an invalid Date when unparsable."
  [{:keys [parse-date date-fns-format invalid-date-message out-of-range-message
           time-precision time-picker-props locale]}]
  (fn [s]
    (if (or (= s out-of-range-message) (= s invalid-date-message))
      nil
      (let [d (if parse-date
                (parse-date s locale)
                (du/parse-date s (or date-fns-format
                                     (du/default-format time-precision time-picker-props))))]
        (if (false? d) du/invalid-date d)))))

(defn- placeholder
  [{:keys [placeholder format-date parse-date date-fns-format time-precision
           time-picker-props]}]
  (if (or (some? placeholder) (and format-date parse-date))
    placeholder
    (or date-fns-format (du/default-format time-precision time-picker-props))))

(defn date-input
  "Props: `:value` (js/Date or nil; controlled when the key is present) /
   `:default-value`, `:on-change (fn [date-or-nil user-change?])`, `:on-error
   (fn [invalid-date])`, `:format-date (fn [date locale] s)` + `:parse-date
   (fn [s locale] date-or-false)` or `:date-fns-format` (yyyy MM dd HH mm ss SSS;
   default by precision, \"yyyy-MM-dd\"), `:placeholder`, `:min-date`, `:max-date`,
   `:close-on-selection` (default true), `:disabled`, `:fill`, `:input-props`,
   `:popover-props`, `:right-element`, `:time-precision`, `:time-picker-props`,
   `:invalid-date-message`, `:out-of-range-message`, `:locale`, `:class`, and every
   DatePicker prop (`:shortcuts`, `:show-actions-bar`, `:highlight-current-day`, …)."
  [props]
  (let [p0      (with-defaults props)
        state   (r/atom {:open?    false
                         :value    (if (contains? props :value) (:value props)
                                       (:default-value props))
                         :focused? false
                         :sc-index nil})
        _       (swap! state assoc :input ((formatter p0) (:value @state)))
        cur     (atom p0)
        input-el   (atom nil)
        content-el (atom nil)
        controlled? #(contains? (:raw @cur) :value)
        fmt     (fn [d] ((formatter @cur) d))
        parse   (fn [s] ((parser @cur) s))
        close!  (fn [e]
                  (when e (when-let [f (get-in @cur [:popover-props :on-close])] (f e)))
                  (swap! state assoc :open? false))
        on-date-change
        (fn [nd user? & [enter?]]
          (let [p    @cur
                prev (:value @state)]
            (if (nil? nd)
              (do (when (and (not (controlled?)) (not enter?))
                    (swap! state assoc :input ""))
                  (when-let [f (:on-change p)] (f nil user?)))
              (let [open (or (not user?)
                             (not (:close-on-selection p))
                             (and prev (or (du/month-changed? prev nd)
                                           (and (:time-precision p)
                                                (du/time-changed? prev nd)))))]
                (swap! state assoc :focused? (boolean enter?) :open? open)
                (when-not (controlled?)
                  (swap! state assoc :value nd :input (fmt nd)))
                (when-let [f (:on-change p)] (f nd user?))))))
        on-focus
        (fn [e]
          (swap! state assoc :focused? true :open? true :input (fmt (:value @state)))
          (when-let [f (get-in @cur [:input-props :on-focus])] (f e)))
        on-blur
        (fn [e]
          (let [p @cur
                {:keys [input value]} @state]
            (if (or (nil? input) (nil? value))
              (swap! state assoc :focused? false)
              (let [d (parse input)]
                (if (and (pos? (count input))
                         (not= input (fmt value))
                         (or (not (du/valid? d))
                             (not (du/day-in-range? d [(:min-date p) (:max-date p)]))))
                  (do (if (controlled?)
                        (swap! state assoc :focused? false)
                        (swap! state assoc :focused? false :value d :input nil))
                      (if (nil? d)
                        (when-let [f (:on-change p)] (f nil true))
                        (when-let [f (:on-error p)] (f d))))
                  (if (zero? (count input))
                    (swap! state assoc :focused? false :value nil :input nil)
                    (swap! state assoc :focused? false)))))
            (when-let [f (get-in p [:input-props :on-blur])] (f e))))
        on-input-change
        (fn [e]
          (let [p  @cur
                s  (.. e -target -value)
                d  (parse s)]
            (if (and (du/valid? d) (du/day-in-range? d [(:min-date p) (:max-date p)]))
              (do (if (controlled?)
                    (swap! state assoc :input s)
                    (swap! state assoc :value d :input s))
                  (when-let [f (:on-change p)] (f d true)))
              (do (when (zero? (count s))
                    (when-let [f (:on-change p)] (f nil true)))
                  (swap! state assoc :value d :input s)))
            (when-let [f (get-in p [:input-props :on-change])] (f e))))
        on-key-down
        (fn [e]
          (let [k (.-key e)]
            (cond
              (and (= k "Tab") (.-shiftKey e)) (close! e)
              (and (= k "Tab") (:open? @state))
              (when-let [el (first (focusable-elements @content-el))]
                (.focus el)
                (.preventDefault e))
              (= k "Escape") (do (swap! state assoc :open? false)
                                 (some-> @input-el .blur))
              (and (= k "Enter") (some? (:input @state)))
              (let [nd (parse (:input @state))]
                (when (du/valid? nd) (on-date-change nd true true))))
            (when-let [f (get-in @cur [:input-props :on-key-down])] (f e))))
        boundary-in
        (fn [first? e]
          (let [rel (or (.-relatedTarget e) js/document.activeElement)]
            (if (and @content-el (.contains @content-el rel))
              (do (some-> @input-el .focus)
                  (when-not first? (close! e)))
              (when-let [el ((if first? first last) (focusable-elements @content-el))]
                (.focus el)))))]
    (r/create-class
     {:display-name "DateInput"
      :component-did-update
      (fn [this [_ old]]
        (let [new (r/props this)]
          (when (and (contains? new :value) (not= (:value old) (:value new)))
            (swap! state assoc :value (:value new)))
          (when (and (contains? new :value) (not (:focused? @state)))
            (let [s (fmt (:value @state))]
              (when (not= s (:input @state)) (swap! state assoc :input s))))))
      :reagent-render
      (fn [props]
        (let [p        (assoc (with-defaults props) :raw props)
              _        (reset! cur p)
              {:keys [value focused? input open? sc-index]} @state
              {:keys [min-date max-date disabled fill input-props popover-props
                      right-element]} p
              error?   (and (some? value)
                            (or (not (du/valid? value))
                                (not (du/day-in-range? value [min-date max-date]))))
              shown    (when-not (nil? value) (fmt value))
              show-err (or (not focused?)
                           (= input (:out-of-range-message p))
                           (= input (:invalid-date-message p)))
              picker-props (apply dissoc p :raw input-keys)
              content
              [:div {:ref #(do (reset! content-el %) nil) :role "dialog"
                     :aria-label "date picker"}
               [:div {:tab-index 0 :data-focus-boundary true :on-focus #(boundary-in true %)}]
               [date-picker (merge picker-props
                                   {:min-date                min-date
                                    :max-date                max-date
                                    :on-change               on-date-change
                                    :on-shortcut-change      #(swap! state assoc :sc-index %2)
                                    :selected-shortcut-index sc-index
                                    :value                   (if error? nil value)})]
               [:div {:tab-index 0 :data-focus-boundary true :on-focus #(boundary-in false %)}]]]
          [popover/popover
           (merge (dissoc popover-props :on-close :class :popover-class-name)
                  {:is-open            (and open? (not disabled))
                   :autofocus          false
                   :enforce-focus      false
                   :class              (bp/cx (bp/cls "date-input") (:class popover-props)
                                              (:class props))
                   :content            content
                   :on-close           close!
                   :popover-class-name (bp/cx (bp/cls "date-input-popover")
                                              (:popover-class-name popover-props))
                   :render-target
                   (fn [{:keys [ref class] :as tp}]
                     [input-group
                      (merge
                       {:auto-complete "off"
                        :class         (bp/cx class (:class input-props))
                        :intent        (if (and show-err error?) :danger :none)
                        :placeholder   (placeholder p)
                        :right-element right-element
                        :type          "text"
                        :role          "combobox"}
                       (dissoc input-props :class :input-ref :on-focus :on-blur :on-change
                               :on-key-down)
                       {:aria-expanded (:is-open tp)
                        :disabled      disabled
                        :fill          fill
                        :input-ref     (fn [el]
                                         (reset! input-el el)
                                         (ref el)
                                         (when-let [f (:input-ref input-props)] (f el))
                                         nil)
                        :on-blur       on-blur
                        :on-change     on-input-change
                        :on-click      (fn [e]
                                         (.stopPropagation e)
                                         (when-let [f (:on-click input-props)] (f e)))
                        :on-focus      on-focus
                        :on-key-down   on-key-down
                        :value         (or (if focused? input shown) "")})])})]))})))
