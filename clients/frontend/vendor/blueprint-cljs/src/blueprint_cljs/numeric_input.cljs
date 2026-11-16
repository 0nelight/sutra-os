(ns blueprint-cljs.numeric-input
  "NumericInput: an InputGroup with increment/decrement buttons, arrow keys
   (Shift = `:major-step-size`, Alt = `:minor-step-size`), clamping to `:min`/`:max`
   and press-and-hold. Locale-free: values are JS number strings.
   `[numeric-input {:value v :on-value-change (fn [n s input-el]) :min 0 :max 100}]`
   Props: `:button-position` (:right default, :left, :none), `:step-size` 1,
   `:minor-step-size` 0.1, `:major-step-size` 10, `:clamp-value-on-blur`,
   `:select-all-on-focus`, `:select-all-on-increment`, `:allow-numeric-characters-only`
   (default true), `:on-button-click` (fn [n s]), `:default-value`, `:intent`,
   `:size`/`:large`/`:small`, `:fill`, `:disabled`, `:read-only`, `:left-icon`,
   `:left-element`, `:right-element`, `:input-class-name`, `:input-ref`; the rest goes to
   the `<input>`."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.control-group :as control-group]
            [blueprint-cljs.input-group :as input-group]))

(def ^:private continuous-change-delay 300)
(def ^:private continuous-change-interval 100)

(defn count-decimal-places
  [n]
  (if-not (js/isFinite n)
    0
    (loop [e 1 p 0]
      (if (= (/ (js/Math.round (* n e)) e) n) p (recur (* e 10) (inc p))))))

(defn- numeric?
  [s]
  (and (some? s) (>= (+ (- (js/Number s) (js/parseFloat s)) 1) 0)))

(defn- to-max-precision
  [n precision]
  (let [f (js/Math.pow 10 precision)] (/ (js/Math.round (* n f)) f)))

(defn- clamp
  [n mn mx]
  (-> n (max (if (some? mn) mn ##-Inf)) (min (if (some? mx) mx ##Inf))))

(defn- num->str
  [n]
  (.toLocaleString n "en-US" #js {:useGrouping false :maximumFractionDigits 20}))

(defn round-and-clamp
  "Blueprint `roundAndClampValue`: \"\" for a non-number, else value+delta rounded to
   `precision` and clamped, as a string."
  [s precision mn mx delta]
  (if-not (numeric? s)
    ""
    (num->str (clamp (to-max-precision (+ (js/Number s) delta) precision) mn mx))))

(defn sanitize
  [s]
  (->> (str/replace (str s) #"[０-９]"
                    #(js/String.fromCharCode (- (.charCodeAt % 0) 0xfee0)))
       (filter #(re-matches #"[Ee0-9+\-.]" %))
       (apply str)))

(defn- precision-of
  [{:keys [minor-step-size step-size] :or {step-size 1}}]
  (count-decimal-places (or minor-step-size step-size)))

(def ^:private defaults
  {:allow-numeric-characters-only true :button-position :right :major-step-size 10
   :minor-step-size 0.1 :step-size 1})

(def ^:private own-keys
  [:allow-numeric-characters-only :button-position :clamp-value-on-blur :default-value
   :major-step-size :minor-step-size :on-button-click :on-value-change :select-all-on-focus
   :select-all-on-increment :step-size :value :min :max :class :fill :input-ref])

(defn numeric-input
  [& args]
  (let [[init] (c/split-args args)
        st     (r/atom {:value      (str (or (:value init) (:default-value init) ""))
                        :prev-min   (:min init)
                        :prev-max   (:max init)})
        node   (atom nil)
        delta  (atom 0)
        timer  (atom nil)
        ival   (atom nil)
        pasted (atom false)
        select-after? (atom false)
        props* (atom nil)]
    (letfn [(derive! [{:keys [min max value] :as p}]
              (let [{:keys [prev-min prev-max] cur :value} @st
                    v         (if (some? value) (str value) cur)
                    sanitized (if (= v "") "" (round-and-clamp v (precision-of p) min max 0))
                    bounds?   (or (not= min prev-min) (not= max prev-max))]
                (swap! st assoc :prev-min min :prev-max max
                       :value (if (and bounds? (not= sanitized cur)) sanitized v))))
            (next-value! [s]
              (let [{:keys [value on-value-change]} @props*]
                (when (nil? value) (swap! st assoc :value s))
                (when on-value-change (on-value-change (js/Number s) s @node))))
            (increment! [d]
              (let [p   @props*
                    cur (let [v (:value @st)] (if (= v "") "0" v))
                    nv  (round-and-clamp cur (precision-of p) (:min p) (:max p) d)]
                (when (not= nv (:value @st))
                  (next-value! nv)
                  (reset! select-after? (boolean (:select-all-on-increment p))))
                nv))
            (step-delta [direction e]
              (let [{:keys [major-step-size minor-step-size step-size]} @props*]
                (reset! delta
                        (* direction
                           (cond (and (.-shiftKey e) major-step-size) major-step-size
                                 (and (.-altKey e) minor-step-size) minor-step-size
                                 :else step-size)))))
            (stop! []
              (reset! delta 0)
              (some-> @timer js/clearTimeout)
              (some-> @ival js/clearInterval)
              (js/document.removeEventListener "mouseup" stop!))
            (continuous! []
              (let [{:keys [min max on-button-click]} @props*
                    n (js/Number (:value @st))]
                (if (and (or (some? min) (some? max))
                         (or (<= n (if (some? min) min ##-Inf))
                             (>= n (if (some? max) max ##Inf))))
                  (stop!)
                  (let [nv (increment! @delta)]
                    (when on-button-click (on-button-click (js/Number nv) nv))))))
            (start! []
              (js/document.addEventListener "mouseup" stop!)
              (reset! timer (js/setTimeout
                             #(reset! ival (js/setInterval continuous!
                                                           continuous-change-interval))
                             continuous-change-delay)))
            (button-click! [direction e]
              (let [nv (increment! (step-delta direction e))]
                (when-let [f (:on-button-click @props*)] (f (js/Number nv) nv))))
            (button-handlers [direction]
              {:on-mouse-down (fn [e]
                                (when (zero? (.-button e))
                                  (button-click! direction e)
                                  (start!)))
               :on-key-down   (fn [e]
                                (when (c/keyboard-click? e)
                                  (.preventDefault e)
                                  (button-click! direction e)))})
            (buttons [{:keys [intent max min disabled read-only]}]
              (let [v         (:value @st)
                    disabled? (or disabled read-only)]
                ^{:key "button-group"}
                [button/button-group {:class (bp/cls "fixed") :vertical true}
                 [button/button (merge {:aria-label "increment"
                                        :disabled   (or disabled?
                                                        (and (some? max) (not= v "")
                                                             (>= (js/Number v) max)))
                                        :icon       :chevron-up
                                        :intent     intent}
                                       (button-handlers 1))]
                 [button/button (merge {:aria-label "decrement"
                                        :disabled   (or disabled?
                                                        (and (some? min) (not= v "")
                                                             (<= (js/Number v) min)))
                                        :icon       :chevron-down
                                        :intent     intent}
                                       (button-handlers -1))]]))]
      (r/create-class
       {:display-name "NumericInput"
        :component-will-unmount stop!
        :component-did-update
        (fn []
          (when (and @select-after? @node)
            (.select @node)))
        :reagent-render
        (fn [& args]
          (let [[props] (c/split-args args)
                p (merge defaults props)
                {:keys [allow-numeric-characters-only button-position class
                        clamp-value-on-blur disabled fill input-ref max min on-blur on-focus
                        on-key-down on-key-press on-paste read-only select-all-on-focus]} p]
            (reset! props* p)
            (derive! p)
            (let [v      (:value @st)
                  btns   (when (not= (name button-position) "none") (buttons p))
                  input
                  [input-group/input-group
                   (merge
                    {:auto-complete "off"
                     :role          (when allow-numeric-characters-only "spinbutton")}
                    (bp/dom-props p (into own-keys (keys defaults)))
                    {:aria-valuemax  max
                     :aria-valuemin  min
                     :aria-valuenow  (when (not= v "") (js/Number v))
                     :input-ref      (fn [n] (reset! node n) (when input-ref (input-ref n)))
                     :value          v
                     :on-focus       (fn [e]
                                       (reset! select-after? (boolean select-all-on-focus))
                                       (when on-focus (on-focus e)))
                     :on-blur        (fn [e]
                                       (reset! select-after? false)
                                       (when clamp-value-on-blur
                                         (next-value! (round-and-clamp
                                                       (.. e -target -value) (precision-of p)
                                                       min max 0)))
                                       (when on-blur (on-blur e)))
                     :on-key-down    (fn [e]
                                       (when-not (or disabled read-only)
                                         (when-let [d (case (.-key e)
                                                        "ArrowUp" 1 "ArrowDown" -1 nil)]
                                           (.preventDefault e)
                                           (increment! (step-delta d e))))
                                       (when on-key-down (on-key-down e)))
                     :on-key-press   (fn [e]
                                       (let [k (.-key e)]
                                         (when (and allow-numeric-characters-only
                                                    k (= 1 (count k))
                                                    (not (or (.-ctrlKey e) (.-altKey e)
                                                             (.-metaKey e)))
                                                    (not (re-matches #"[Ee0-9+\-.]" k)))
                                           (.preventDefault e)))
                                       (when on-key-press (on-key-press e)))
                     :on-paste       (fn [e]
                                       (reset! pasted true)
                                       (when on-paste (on-paste e)))
                     :on-change      nil
                     :on-value-change
                     (fn [s _]
                       (let [s (if (and allow-numeric-characters-only @pasted)
                                 (do (reset! pasted false) (sanitize s))
                                 s)]
                         (next-value! s)
                         (reset! select-after? false)))})]]
              [control-group/control-group
               {:class (bp/cx (bp/cls "numeric-input") (c/size-class p) class) :fill fill}
               (when (= (name button-position) "left") btns)
               input
               (when (= (name button-position) "right") btns)])))}))))
