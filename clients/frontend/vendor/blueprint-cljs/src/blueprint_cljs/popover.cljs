(ns blueprint-cljs.popover
  "Popover: floating content tethered to a target, positioned with @floating-ui/dom.
   Interaction kinds :click, :click-target-only, :hover, :hover-target-only. A click on an
   element with class `bp-popover-dismiss` inside the content closes the popover (and its
   parents, unless they have `:capture-dismiss`); `bp-popover-dismiss-override` stops it."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            ["@floating-ui/dom" :as fui]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.overlay :as overlay]))

(def dismiss-class (bp/cls "popover-dismiss"))
(def dismiss-override-class (bp/cls "popover-dismiss-override"))

(def arrow-svg-size 30)
(def ^:private arrow-spacing 4)
(def ^:private shadow-path
  (str "M8.11 6.302c1.015-.936 1.887-2.922 1.887-4.297v26c0-1.378-.868-3.357-1.888-4.297"
       "L.925 17.09c-1.237-1.14-1.233-3.034 0-4.17L8.11 6.302z"))
(def ^:private fill-path
  (str "M8.787 7.036c1.22-1.125 2.21-3.376 2.21-5.03V0v30-2.005c0-1.654-.983-3.9-2.21-5.03"
       "l-7.183-6.616c-.81-.746-.802-1.96 0-2.7l7.183-6.614z"))

(defn base-placement [placement] (first (str/split (name placement) #"-")))

(defn- opposite [side]
  (case side "top" "bottom" "left" "right" "bottom" "top" "left"))

(defn- vertical-side? [side] (contains? #{"left" "right"} side))

(defn- alignment [placement]
  (case (second (str/split (name placement) #"-")) "start" "left" "end" "right" "center"))

(defn- arrow-angle [placement]
  (case (base-placement placement) "top" -90 "left" 180 "bottom" 90 0))

(defn transform-origin
  "Blueprint `getTransformOrigin`: towards the arrow when there is one."
  [placement arrow-xy]
  (let [side (base-placement placement)]
    (if-let [{:keys [x y]} arrow-xy]
      (if (vertical-side? side)
        (str (opposite side) " " (+ (or y 0) (/ arrow-svg-size 2)) "px")
        (str (+ (or x 0) (/ arrow-svg-size 2)) "px " (opposite side)))
      (if (vertical-side? side)
        (str (opposite side) " " (alignment placement))
        (str (alignment placement) " " (opposite side))))))

(defn popover-arrow
  [placement ref]
  [:div {:class (bp/cls "popover-arrow") :aria-hidden true :ref ref}
   [:svg {:viewBox (str "0 0 " arrow-svg-size " " arrow-svg-size)
          :style   {:transform (str "rotate(" (arrow-angle placement) "deg)")}}
    [:path {:class (bp/cls "popover-arrow-border") :d shadow-path}]
    [:path {:class (bp/cls "popover-arrow-fill") :d fill-path}]]])

(defn- hover-kind? [kind] (contains? #{:hover :hover-target-only} kind))

(def defaults
  {:interaction-kind :click :placement :auto :hover-open-delay 150 :hover-close-delay 300
   :target-tag-name :span :use-portal true :inherit-dark-theme true :open-on-target-focus true
   :position-strategy :absolute :transition-duration 300 :minimal false
   :should-return-focus-on-close false :lazy true})

(defn- middleware
  [{:keys [placement modifiers match-target-width arrow-offset]} arrow? arrow-el]
  (let [{:keys [offset flip shift arrow size] :as m} modifiers
        on?  (fn [k] (not (false? (get m k))))
        opts (fn [x] (clj->js (if (map? x) x {})))]
    (cond-> []
      (and (on? :offset) (or arrow? (map? offset)))
      (conj (fui/offset (clj->js (merge (when arrow? {:mainAxis (or arrow-offset
                                                                    (/ arrow-svg-size 2))})
                                        (when (map? offset) offset)))))
      (= :auto (keyword placement))
      (conj (fui/autoPlacement (opts (:auto-placement m))))
      (and (not= :auto (keyword placement)) (on? :flip))
      (conj (fui/flip (opts flip)))
      (on? :shift)
      (conj (fui/shift (opts shift)))
      match-target-width
      ;; the room left on its side goes to the menu's max-height: an iPhone keyboard takes
      ;; half the screen, and a list cut by it hides its lower rows
      (conj (fui/size (clj->js (merge {:padding 8}
                                      (when (map? size) size)
                                      {:apply (fn [^js o]
                                                (let [st (.. o -elements -floating -style)]
                                                  (set! (.-width st)
                                                        (str (.. o -rects -reference -width)
                                                             "px"))
                                                  (.setProperty
                                                   st "--bp-popover-room"
                                                   (str (max 80 (.-availableHeight o))
                                                        "px"))))}))))
      (and arrow? arrow-el (on? :arrow))
      (conj (fui/arrow (clj->js (merge {:padding 4} (when (map? arrow) arrow)
                                       {:element arrow-el})))))))

(def ^:private own-keys
  [:content :placement :position :interaction-kind :is-open :default-is-open :on-interaction
   :on-opening :on-opened :on-closing :on-closed :on-close :minimal :match-target-width
   :disabled :fill :use-portal :portal-container :portal-class-name :autofocus
   :enforce-focus :can-escape-key-close :has-backdrop :backdrop-props :hover-open-delay
   :hover-close-delay :popover-class-name :class :target-tag-name :target-props
   :position-strategy :modifiers :should-return-focus-on-close :render-target
   :inherit-dark-theme :open-on-target-focus :capture-dismiss :transition-duration :lazy
   :arrow-offset :popover-ref :popup-kind])

(defn popover
  "Props as Blueprint's Popover, kebab-cased: `:content`, `:placement` (\"bottom-start\",
   :auto …; `:position` is an alias), `:interaction-kind`, `:is-open` (controlled when
   present) / `:default-is-open`, `:on-interaction (fn [next-open? event])`, `:on-close`,
   `:on-opening` `:on-opened` `:on-closing` `:on-closed`, `:minimal`, `:match-target-width`,
   `:disabled`, `:fill`, `:use-portal`, `:portal-container`, `:portal-class-name`,
   `:autofocus`, `:enforce-focus`, `:can-escape-key-close`, `:has-backdrop`,
   `:backdrop-props`, `:hover-open-delay`, `:hover-close-delay`, `:popover-class-name`,
   `:class` (on the target wrapper), `:target-tag-name`, `:target-props`,
   `:position-strategy` :absolute|:fixed, `:modifiers` {:offset {..}|false :flip {..}|false
   :shift {..}|false :arrow {..}|false :size {..} :auto-placement {..}} (floating-ui
   options), `:should-return-focus-on-close`, `:capture-dismiss`, `:inherit-dark-theme`,
   `:open-on-target-focus`, `:popover-ref (fn [el])`, `:transition-duration`, `:lazy`,
   `:render-target (fn [{:keys [ref class is-open on-click on-mouse-enter …]}] hiccup)`.
   The single child is the target."
  [_props & _children]
  (let [state       (r/atom {:open? nil :placement nil})
        cur         (atom {})
        target-el   (atom nil)
        float-el    (atom nil)
        popover-el  (atom nil)
        arrow-el    (atom nil)
        cleanup     (atom nil)
        timer       (atom nil)
        mouse-in?   (atom false)
        esc-close?  (atom false)
        lost-focus-on-page? (atom true)
        props       (fn [] @cur)
        controlled? (fn [] (some? (:is-open (props))))
        open?       (fn []
                      (let [p (props)]
                        (and (not (:disabled p))
                             (boolean (if (controlled?) (:is-open p) (:open? @state))))))
        arrow?      (fn [] (let [p (props)]
                             (and (not (:minimal p))
                                  (not (false? (get-in p [:modifiers :arrow]))))))
        update-position!
        (fn []
          (let [t @target-el f @float-el p (props)]
            (when (and t f)
              (-> (fui/computePosition
                   t f
                   #js {:placement (let [pl (name (:placement p))]
                                     (if (= pl "auto") "bottom" pl))
                        :strategy  (name (:position-strategy p))
                        :middleware (clj->js (middleware p (arrow?) @arrow-el))})
                  (.then
                   (fn [^js res]
                     (when (identical? f @float-el)
                       (let [pl   (.-placement res)
                             axy  (some-> (.. res -middlewareData -arrow)
                                          (#(hash-map :x (.-x %) :y (.-y %))))
                             side (base-placement pl)
                             s    (.-style f)
                             off  (- (/ arrow-svg-size 2) arrow-spacing)]
                         (set! (.-position s) (.-strategy res))
                         (set! (.-left s) (str (.-x res) "px"))
                         (set! (.-top s) (str (.-y res) "px"))
                         (when-let [a @arrow-el]
                           (let [as (.-style a)]
                             (set! (.-left as) (if (some? (:x axy)) (str (:x axy) "px") ""))
                             (set! (.-top as) (if (some? (:y axy)) (str (:y axy) "px") ""))
                             (set! (.-right as) "")
                             (set! (.-bottom as) "")
                             (case side
                               "top"    (set! (.-bottom as) (str (- off) "px"))
                               "bottom" (set! (.-top as) (str (- off) "px"))
                               "left"   (set! (.-right as) (str (- off) "px"))
                               (set! (.-left as) (str (- off) "px")))))
                         (when-let [pe @popover-el]
                           (set! (.. pe -style -transformOrigin)
                                 (transform-origin pl (when (and (arrow?) @arrow-el) axy))))
                         (when (not= pl (:placement @state))
                           (swap! state assoc :placement pl))))))))))
        bound       (atom nil)
        ;; autoUpdate follows the target and floating element mounted NOW: a target that
        ;; is replaced while open (its parent remounted it) must take the popover along
        bind!
        (fn []
          (let [t @target-el f @float-el [bt bf] @bound]
            (when-not (and (identical? t bt) (identical? f bf))
              (when @cleanup (@cleanup) (reset! cleanup nil))
              (reset! bound [t f])
              (when (and t f)
                (reset! cleanup (fui/autoUpdate t f update-position!))))))
        float-ref
        (fn [el]
          (reset! float-el el)
          (bind!)
          nil)
        target-ref
        (fn [el]
          (reset! target-el el)
          (when el (bind!))
          nil)
        clear-timer! (fn [] (some-> @timer js/clearTimeout) (reset! timer nil))
        set-open!
        (fn set-open!
          ([open e] (set-open! open e nil))
          ([open e delay]
           (clear-timer!)
           (if (and delay (pos? delay))
             (reset! timer (js/setTimeout #(set-open! open e nil) delay))
             (let [p (props)]
               (when-not open
                 (reset! esc-close? (and e (= "Escape" (.-key e)))))
               (if (controlled?)
                 (when-let [f (:on-interaction p)] (f open e))
                 (swap! state assoc :open? open))
               (when (and (not open) (:on-close p)) ((:on-close p) e))))))
        in-popover? (fn [el] (boolean (and el @popover-el (.contains @popover-el el))))
        on-mouse-enter
        (fn [e]
          (reset! mouse-in? true)
          (let [p (props)]
            (if (and (not (:use-portal p))
                     (in-popover? (.-target e))
                     (= :hover-target-only (:interaction-kind p))
                     (not (:open-on-target-focus p)))
              nil
              (when-not (:disabled p)
                (set-open! true e (:hover-open-delay p))))))
        on-mouse-leave
        (fn [e]
          (reset! mouse-in? false)
          (js/setTimeout
           #(when-not @mouse-in?
              (set-open! false e (:hover-close-delay (props))))
           0))
        on-target-focus
        (fn [e]
          (when (and (:open-on-target-focus (props)) (hover-kind? (:interaction-kind (props))))
            (when-not (and (nil? (.-relatedTarget e)) (not @lost-focus-on-page?))
              (on-mouse-enter e))))
        on-target-blur
        (fn [e]
          (when (and (:open-on-target-focus (props)) (hover-kind? (:interaction-kind (props))))
            (let [rel (.-relatedTarget e)]
              (when-not (and rel (or (= rel @popover-el) (in-popover? rel)))
                (on-mouse-leave e))))
          (reset! lost-focus-on-page? (some? (.-relatedTarget e))))
        on-target-click
        (fn [e]
          (let [p (props)]
            (when-not (or (:disabled p) (in-popover? (.-target e)))
              (set-open! (not (open?)) e))))
        on-popover-click
        (fn [e]
          (let [t        (.-target e)
                pop      (.closest t (str "." (bp/cls "popover")))
                self?    (identical? pop @popover-el)
                capture? (boolean (some-> pop .-classList
                                          (.contains (bp/cls "popover-capturing-dismiss"))))
                d-el     (.closest t (str "." dismiss-class ", ." dismiss-override-class))
                dismiss? (boolean (some-> d-el .-classList (.contains dismiss-class)))
                disabled? (some? (.closest t (str ":disabled, ." (bp/cls "disabled"))))]
            (when (and dismiss? (not disabled?) (or (not capture?) self?))
              (set-open! false e))))
        on-overlay-close
        (fn [e]
          (let [t   @target-el
                tgt (when e (if (and (.-composed e) (.-composedPath e))
                              (aget (.composedPath e) 0)
                              (.-target e)))
                kb? (instance? js/KeyboardEvent e)]
            (when (and t e (or kb? (not (.contains t tgt))))
              (set-open! false e))))]
    (r/create-class
     {:display-name "blueprint-cljs.popover"
      :component-did-mount
      (fn [_] (swap! state assoc :open? (boolean (:default-is-open (props)))))
      :component-did-update
      (fn [_ _]
        (when (and @float-el (open?)) (update-position!)))
      :component-will-unmount
      (fn [_]
        (clear-timer!)
        (when @cleanup (@cleanup) (reset! cleanup nil))
        (reset! bound nil))
      :reagent-render
      (fn [p & children]
        (let [[p children] (if (map? p) [p children] [{} (cons p children)])
              p     (merge defaults (into {} (remove (comp nil? val)) p)
                           (when-let [pos (:position p)]
                             (when-not (:placement p) {:placement pos})))
              _     (reset! cur p)
              _     (:open? @state)
              open  (open?)
              kind  (keyword (:interaction-kind p))
              hover (hover-kind? kind)
              ctrl  (controlled?)
              pl    (or (:placement @state)
                        (let [x (name (:placement p))] (if (= x "auto") "bottom" x)))
              side  (base-placement pl)
              child (first (keep identity children))
              target-tag (if (:fill p) :div (keyword (:target-tag-name p)))
              target-handlers
              (if hover
                {:on-blur on-target-blur :on-focus on-target-focus
                 :on-mouse-enter on-mouse-enter :on-mouse-leave on-mouse-leave}
                {:on-click on-target-click})
              target-class (bp/cx (:class p) (bp/cls "popover-target")
                                  {(bp/cls "popover-open") open
                                   (bp/cls "active") (and open (not ctrl) (not hover))
                                   (bp/cls "fill") (:fill p)})
              active?      (and open (not ctrl) (not hover))
              child-attrs  (cond-> {:class (bp/cx {(bp/cls "active") active?
                                                   (bp/cls "fill")   (:fill p)})}
                             (not hover) (assoc :aria-expanded open)
                             (not= kind :hover-target-only)
                             (assoc :aria-haspopup (or (:popup-kind p) "menu")))
              target
              (if-let [rt (:render-target p)]
                (rt (merge target-handlers child-attrs
                           {:ref target-ref :class (bp/cx target-class (:class child-attrs))
                            :is-open open}))
                (when child
                  [target-tag (merge (:target-props p) target-handlers
                                     {:class target-class :ref target-ref}
                                     (bp/dom-props p own-keys))
                   (overlay/merge-child-props child child-attrs)]))
              content (:content p)
              empty?  (or (nil? content) (= "" content))
              popover-handlers
              (cond-> {:on-click    on-popover-click
                       :on-key-down #(when (contains? #{"Enter" " "} (.-key %))
                                       (on-popover-click %))}
                (or (= kind :hover) (and (not (:use-portal p)) (= kind :hover-target-only)))
                (assoc :on-mouse-enter on-mouse-enter :on-mouse-leave on-mouse-leave))]
          (if empty?
            target
            [:<>
             target
             [overlay/overlay
              {:is-open                      open
               :autofocus                    (if (some? (:autofocus p))
                                               (:autofocus p) (not hover))
               :enforce-focus                (if (some? (:enforce-focus p))
                                               (:enforce-focus p) (not hover))
               :backdrop-class-name          (bp/cls "popover-backdrop")
               :backdrop-props               (:backdrop-props p)
               :can-escape-key-close         (:can-escape-key-close p)
               :can-outside-click-close      (= kind :click)
               :has-backdrop                 (boolean (:has-backdrop p))
               :lazy                         (:lazy p)
               :on-close                     on-overlay-close
               :on-opening                   (:on-opening p)
               :on-opened                    (fn [el]
                                              (update-position!)
                                              (when-let [f (:on-opened p)] (f el)))
               :on-closing                   (:on-closing p)
               :on-closed                    (:on-closed p)
               :transition-name              (bp/cls "popover")
               :transition-duration          (:transition-duration p)
               :use-portal                   (:use-portal p)
               :portal-container             (:portal-container p)
               :portal-class-name            (:portal-class-name p)
               :dark-source                  (when (:inherit-dark-theme p) #(deref target-el))
               :should-return-focus-on-close (and (not hover)
                                                  (or @esc-close?
                                                      (:should-return-focus-on-close p)))}
              [:div (merge popover-handlers
                           {:class (bp/cls "popover-transition-container")
                            :style {:position (name (:position-strategy p)) :left 0 :top 0}
                            :ref   float-ref})
               [:div {:class (bp/cx (bp/cls "popover")
                                    {(bp/cls "minimal") (:minimal p)
                                     (bp/cls "popover-capturing-dismiss") (:capture-dismiss p)
                                     (bp/cls "popover-match-target-width")
                                     (:match-target-width p)}
                                    (bp/cls (str "popover-content-placement-" side))
                                    (bp/cls (str "popover-placement-" side))
                                    (:popover-class-name p))
                      :ref   (fn [el]
                               (reset! popover-el el)
                               (when-let [f (:popover-ref p)] (f el))
                               nil)}
                (when (arrow?) [popover-arrow pl #(do (reset! arrow-el %) nil)])
                [:div {:class (bp/cls "popover-content")} content]]]]])))})))
