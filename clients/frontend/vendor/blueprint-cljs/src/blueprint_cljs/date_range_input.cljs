(ns blueprint-cljs.date-range-input
  "DateRangeInput: start and end fields in a control group, a DateRangePicker in a
   popover; typing parses, hovering the calendar previews the range in the fields.
   `[date-range-input {:value [start end] :on-change (fn [[start end]])}]`"
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.date-input :as date-input]
            [blueprint-cljs.date-range-picker :refer [date-range-picker]]
            [blueprint-cljs.date-utils :as du]
            [blueprint-cljs.input-group :refer [input-group]]
            [blueprint-cljs.popover :as popover]))

(def overlapping-dates-message "Overlapping dates")

(def ^:private input-keys
  [:value :default-value :on-change :on-error :format-date :parse-date :date-fns-format
   :close-on-selection :disabled :fill :start-input-props :end-input-props :popover-props
   :select-all-on-focus :invalid-date-message :out-of-range-message
   :overlapping-dates-message :class])

(defn- with-defaults
  [p]
  (merge {:allow-single-day-range false :close-on-selection true :disabled false
          :invalid-date-message date-input/invalid-date-message
          :out-of-range-message date-input/out-of-range-message
          :overlapping-dates-message overlapping-dates-message
          :popover-props {} :select-all-on-focus false :start-input-props {}
          :end-input-props {} :locale du/default-locale}
         {:min-date (du/default-min-date) :max-date (du/default-max-date)}
         (into {} (remove (comp nil? val)) p)))

(def ^:private bkeys
  {:start {:hover :start-hover :input :start-input :focused :start-focused? :selected :start
           :index 0 :props :start-input-props}
   :end   {:hover :end-hover :input :end-input :focused :end-focused? :selected :end
           :index 1 :props :end-input-props}})

(defn- other [b] (if (= b :start) :end :start))

(defn- shift-by-arrow
  [d key]
  (case key
    "ArrowUp"    (du/add-days d -7)
    "ArrowDown"  (du/add-days d 7)
    "ArrowLeft"  (du/add-days d -1)
    "ArrowRight" (du/add-days d 1)
    d))

(defn date-range-input
  "Props: `:value [start end]` (controlled when present) / `:default-value`,
   `:on-change (fn [[start end]])`, `:on-error (fn [[start end]])`, `:format-date` +
   `:parse-date` or `:date-fns-format`, `:min-date`, `:max-date`,
   `:allow-single-day-range`, `:close-on-selection` (default true), `:disabled`, `:fill`,
   `:start-input-props`, `:end-input-props`, `:popover-props`, `:select-all-on-focus`,
   `:invalid-date-message`, `:out-of-range-message`, `:overlapping-dates-message`,
   `:time-precision`, `:time-picker-props`, `:locale`, `:class`, and every
   DateRangePicker prop (`:shortcuts` default true, `:contiguous-calendar-months`,
   `:single-month-only`, …)."
  [props]
  (let [[s0 e0] (or (:value props) (:default-value props) [nil nil])
        state  (r/atom {:start s0 :end e0 :open? false :sc-index -1})
        cur    (atom (with-defaults props))
        els    (atom {})
        controlled? #(some? (:value (:raw @cur)))
        valid-in-range?
        (fn [d] (let [{:keys [min-date max-date]} @cur]
                  (and (du/valid? d) (du/day-in-range? d [min-date max-date]))))
        fmt    (fn [d] (if (valid-in-range? d) (date-input/format-raw @cur d) ""))
        fmt-string
        (fn [d ignore-range?]
          (let [{:keys [min-date max-date invalid-date-message out-of-range-message]} @cur]
            (cond
              (nil? d)            ""
              (not (du/valid? d)) invalid-date-message
              (or ignore-range? (du/day-in-range? d [min-date max-date]))
              (date-input/format-raw @cur d)
              :else               out-of-range-message)))
        parse  (fn [s] (when (some? s) ((date-input/parser @cur) s)))
        selected-value (fn [b] (get @state (get-in bkeys [b :selected])))
        overlap?
        (fn [d b]
          (let [od (selected-value (other b))]
            (if (or (nil? d) (nil? od) (not (du/valid? d)) (not (du/valid? od)))
              false
              (let [single? (:allow-single-day-range @cur)
                    same    (du/same-day? d od)]
                (if (= b :start)
                  (or (> (.getTime d) (.getTime od)) (and (not single?) same))
                  (or (< (.getTime d) (.getTime od)) (and (not single?) same)))))))
        end-overlaps? (fn [d b] (and (= b :end) (overlap? d b)))
        next-valid?   (fn [d b] (and (valid-in-range? d) (not (overlap? d b))))
        range-for     (fn [d b] (if (= b :start) [d (selected-value :end)]
                                    [(selected-value :start) d]))
        selected-range
        (fn []
          (let [[s e] (if (controlled?) (:value (:raw @cur)) [(:start @state) (:end @state)])
                e     (if (overlap? s :start) nil e)]
            (mapv #(when (valid-in-range? %) %) [s e])))
        on-change! (fn [rng] (when-let [f (:on-change @cur)] (f rng)))
        on-picker-change
        (fn [[s e :as rng] & [enter?]]
          (when (:open? @state)
            (let [p   @cur
                  tp? (some? (:time-precision p))
                  st
                  (cond
                    (nil? s)
                    (merge {:start-hover nil}
                           (if tp?
                             {:start-focused? false :end-focused? false :boundary :start}
                             {:start-focused? true :end-focused? false}))
                    (nil? e)
                    (merge {:end-hover nil}
                           (if tp?
                             {:start-focused? false :end-focused? false :boundary :end}
                             {:start-focused? false :end-focused? true}))
                    (:close-on-selection p)
                    (merge {:open? (if (not tp?)
                                     false
                                     (let [fb (du/today-at-midnight)
                                           [cs ce] (selected-range)]
                                       (not (and (du/same-time? (or cs fb) s)
                                                 (du/same-time? (or ce fb) e)))))
                            :start-focused? false}
                           (if (and (not tp?) enter?)
                             {:end-focused? true}
                             {:end-focused? false :boundary :end}))
                    (= (:last-focused @state) :start)
                    (if tp?
                      {:start-focused? false :end-focused? false :boundary :start}
                      {:start-focused? true :end-focused? false})
                    (not tp?) {:start-focused? false :end-focused? true}
                    :else {:start-focused? false :end-focused? false :boundary :end})]
              (swap! state merge
                     {:end-input (fmt e) :start-input (fmt s) :sc-index -1
                      :hover-focus? false}
                     st
                     (when (false? (:open? st)) {:start-hover nil :end-hover nil})
                     (when-not (controlled?) {:start s :end e}))
              (on-change! rng))))
        on-picker-hover
        (fn [hovered _day boundary]
          (when (:open? @state)
            (if (nil? hovered)
              (let [end? (= (:boundary @state) :end)]
                (swap! state assoc :start-hover nil :end-hover nil :end-focused? end?
                       :start-focused? (not end?) :last-focused (:boundary @state)))
              (let [[hs he] hovered
                    sf (if boundary (= boundary :start) (:start-focused? @state))
                    ef (if boundary (= boundary :end) (:end-focused? @state))]
                (swap! state assoc :start-hover (fmt hs) :end-hover (fmt he)
                       :start-focused? sf :end-focused? ef
                       :last-focused (if sf :start :end)
                       :select? (:select-all-on-focus @cur)
                       :hover-focus? true)))))
        on-focus
        (fn [b]
          (let [k     (bkeys b)
                v     (if (controlled?)
                        (get (:value (:raw @cur)) (:index k))
                        (get @state (:selected k)))]
            (swap! state assoc (:input k) (fmt-string v true) (:focused k) true
                   :boundary (if (:hover-focus? @state) (:boundary @state) b)
                   :open? true :last-focused b :select? (:select-all-on-focus @cur)
                   :hover-focus? false)))
        on-blur
        (fn [b]
          (let [k  (bkeys b)
                s  (get @state (:input k))
                d  (parse s)]
            (swap! state assoc (:focused k) false :select? false)
            (cond
              (or (nil? s) (zero? (count s)))
              (if (controlled?)
                (swap! state assoc (:input k)
                       (fmt-string (get (:value (:raw @cur)) (:index k)) false))
                (swap! state assoc (:input k) nil (:selected k) nil))
              (not (next-valid? d b))
              (do (when-not (controlled?)
                    (swap! state assoc (:input k) nil (:selected k) d))
                  (when-let [f (:on-error @cur)] (f (range-for d b)))))))
        on-input-change
        (fn [b e]
          (let [k (bkeys b)
                s (.. e -target -value)
                d (parse s)]
            (swap! state assoc :select? false)
            (cond
              (zero? (count s))
              (do (swap! state assoc (:input k) "")
                  (when-not (controlled?) (swap! state assoc (:selected k) nil))
                  (on-change! (range-for nil b)))
              (valid-in-range? d)
              (do (swap! state assoc (:hover k) nil (:input k) s)
                  (when-not (controlled?) (swap! state assoc (:selected k) d))
                  (when (next-valid? d b) (on-change! (range-for d b))))
              :else
              (swap! state assoc (:input k) s (:hover k) nil))))
        on-arrow
        (fn [b e]
          (let [p  @cur
                el (get @els b)]
            (when (and (:open? @state) el (zero? (.-selectionStart el))
                       (= (.-selectionEnd el) (count (.-value el))))
              (let [[s e*] (selected-range)
                    init   (if (= b :start) s e*)
                    single? (:allow-single-day-range p)
                    shifted
                    (if init
                      (let [rel (shift-by-arrow init (.-key e))
                            as  (when s (if single? s (du/add-days s 1)))
                            ae  (when e* (if single? e* (du/add-days e* -1)))]
                        (if (= b :start) (du/clamp-date rel nil ae) (du/clamp-date rel as nil)))
                      (let [ob (if (= b :start) e* s)
                            fwd? (contains? #{"ArrowRight" "ArrowDown"} (.-key e))]
                        (cond (nil? ob) (du/today-at-midnight)
                              (= fwd? (= b :end)) (shift-by-arrow ob (.-key e)))))]
                (when shifted
                  (.preventDefault e)
                  (let [d (du/clamp-date shifted (:min-date p) (:max-date p))
                        k (bkeys b)]
                    (swap! state assoc (:input k) (fmt d) :sc-index -1 :select? true)
                    (when-not (controlled?) (swap! state assoc (:selected k) d))
                    (on-change! (range-for d b))))))))
        on-key-down
        (fn [b e]
          (let [key   (.-key e)
                shift (.-shiftKey e)
                lf    (:last-focused @state)]
            (cond
              (contains? #{"ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight"} key) (on-arrow b e)
              (= key "Escape")
              (do (some-> (:start @els) .blur) (some-> (:end @els) .blur)
                  (swap! state assoc :end-focused? false :open? false :start-focused? false))
              (= key "Tab")
              (cond
                (and (= lf :start) (not shift))
                (do (.preventDefault e)
                    (swap! state assoc :start-focused? false :end-focused? true
                           :hover-focus? false))
                (and (= lf :end) shift)
                (do (.preventDefault e)
                    (swap! state assoc :start-focused? true :end-focused? false
                           :hover-focus? false))
                :else (swap! state assoc :start-focused? false :end-focused? false
                             :open? false :hover-focus? false))
              (and (= lf :start) (= key "Enter"))
              (on-picker-change [(parse (:start-input @state)) (:end @state)] true)
              (and (= lf :end) (= key "Enter"))
              (on-picker-change [(:start @state) (parse (:end-input @state))] true))))
        display-string
        (fn [b]
          (let [k (bkeys b)
                {:keys [hover input focused selected]} k
                st @state]
            (cond
              (some? (get st hover))     (get st hover)
              (get st focused)           (or (get st input) "")
              (nil? (get st selected))   ""
              (end-overlaps? (get st selected) b) (:overlapping-dates-message @cur)
              :else (fmt-string (get st selected) false))))
        error?
        (fn [b]
          (let [k  (bkeys b)
                st @state
                s  (get st (:input k))]
            (if (or (some? (get st (:hover k))) (nil? s) (zero? (count s)))
              false
              (let [v (if (get st (:focused k)) (parse s) (get st (:selected k)))]
                (boolean (and (some? v) (or (not (valid-in-range? v))
                                            (end-overlaps? v b))))))))
        placeholder
        (fn [b]
          (let [k  (bkeys b)
                ip (get @cur (:props k))]
            (cond
              (some? (:placeholder ip)) (:placeholder ip)
              (= b :start) (if (get @state (:focused k))
                             (fmt-string (:min-date @cur) false) "Start date")
              :else        (if (get @state (:focused k))
                             (fmt-string (:max-date @cur) false) "End date"))))
        sync-focus!
        (fn []
          (let [st @state
                active js/document.activeElement]
            (cond
              (and (:start-focused? st) (:start @els) (not= active (:start @els)))
              (.focus (:start @els))
              (and (:end-focused? st) (:end @els) (not= active (:end @els)))
              (.focus (:end @els)))
            (when (:select? st)
              (cond
                (:start-focused? st) (some-> (:start @els) .select)
                (:end-focused? st)   (some-> (:end @els) .select)))))
        input-for
        (fn [b]
          (let [k  (bkeys b)
                ip (get @cur (:props k))]
            [input-group
             (merge {:auto-complete "off"
                     :disabled      (if (some? (:disabled ip)) (:disabled ip)
                                        (:disabled @cur))
                     :fill          (:fill @cur)}
                    (dissoc ip :input-ref :on-blur :on-change :on-click :on-focus
                            :on-key-down :on-mouse-down :disabled)
                    {:intent       (if (error? b) :danger (:intent ip))
                     :input-ref    (fn [el]
                                     (swap! els assoc b el)
                                     (when-let [f (:input-ref ip)] (f el))
                                     nil)
                     :on-blur      (fn [e] (on-blur b) (when-let [f (:on-blur ip)] (f e)))
                     :on-change    (fn [e] (on-input-change b e)
                                     (when-let [f (:on-change ip)] (f e)))
                     :on-click     (fn [e] (.stopPropagation e)
                                     (when-let [f (:on-click ip)] (f e)))
                     :on-focus     (fn [e] (on-focus b) (when-let [f (:on-focus ip)] (f e)))
                     :on-key-down  (fn [e] (on-key-down b e)
                                     (when-let [f (:on-key-down ip)] (f e)))
                     :on-mouse-down (fn [e] (swap! state assoc :hover-focus? false)
                                      (when-let [f (:on-mouse-down ip)] (f e)))
                     :placeholder  (placeholder b)
                     :value        (display-string b)})]))]
    (r/create-class
     {:display-name "DateRangeInput"
      :component-did-update
      (fn [this [_ old]]
        (sync-focus!)
        (let [new (r/props this)]
          (when (not= (:value old) (:value new))
            (let [[s e] (or (:value new) (:default-value new) [nil nil])]
              (swap! state assoc :start s :end e)))))
      :reagent-render
      (fn [props]
        (let [p (assoc (with-defaults props) :raw props)
              _ (reset! cur p)
              {:keys [popover-props fill]} p
              st @state
              picker-props (apply dissoc p :raw :locale input-keys)]
          [popover/popover
           (merge {:placement "bottom-start"}
                  (dissoc popover-props :on-close :class :popover-class-name)
                  {:is-open            (:open? st)
                   :autofocus          false
                   :enforce-focus      false
                   :class              (bp/cx (bp/cls "date-range-input")
                                              (:class popover-props) (:class props))
                   :content            [date-range-picker
                                        (merge picker-props
                                               {:boundary-to-modify      (:boundary st)
                                                :locale                  (:locale p)
                                                :on-change               on-picker-change
                                                :on-hover-change         on-picker-hover
                                                :on-shortcut-change
                                                #(swap! state assoc :sc-index %2)
                                                :selected-shortcut-index (:sc-index st)
                                                :value                   (selected-range)})]
                   :on-close           (fn [e]
                                         (swap! state assoc :open? false)
                                         (when-let [f (:on-close popover-props)] (f e)))
                   :popover-class-name (bp/cx (bp/cls "date-range-input-popover")
                                              (:popover-class-name popover-props))
                   :render-target
                   (fn [{:keys [ref class]}]
                     [(keyword (or (:target-tag-name popover-props) :div))
                      {:class (bp/cx (bp/cls "control-group") class
                                     {(bp/cls "fill") fill})
                       :ref   ref}
                      (input-for :start)
                      (input-for :end)])})]))})))
