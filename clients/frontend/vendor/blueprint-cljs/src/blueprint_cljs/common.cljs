(ns blueprint-cljs.common
  "Class and prop helpers Blueprint keeps in `common/classes.ts` and
   `accessibility/useInteractiveAttributes.ts`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]))

(defn split-args
  "A component's argv tail as [props children]; the props map may be left out."
  [args]
  (if (map? (first args))
    [(first args) (rest args)]
    [{} args]))

(defn size-class
  "Blueprint `sizeClass`: `:size` wins, else the legacy `:large`/`:small` booleans."
  [{:keys [size large small]}]
  (case (some-> size name)
    "small" (bp/cls "small")
    "large" (bp/cls "large")
    (cond large (bp/cls "large")
          small (bp/cls "small"))))

(defn variant-class
  "Blueprint `variantClass`: `:variant` wins, else the legacy `:minimal`/`:outlined`."
  [{:keys [variant minimal outlined]}]
  (case (some-> variant name)
    "outlined" (bp/cls "outlined")
    "minimal"  (bp/cls "minimal")
    (cond outlined (bp/cls "outlined")
          minimal  (bp/cls "minimal"))))

(defn large?
  [{:keys [size large]}]
  (or (= "large" (some-> size name)) (boolean large)))

(defn elevation-class
  [elevation]
  (when (some? elevation) (bp/cls (str "elevation-" elevation))))

(defn alignment-class
  "Blueprint accepts :left/:start and :right/:end; :center has no class."
  [align]
  (case (some-> align name)
    ("left" "start") (bp/cls "align-left")
    ("right" "end")  (bp/cls "align-right")
    nil))

(defn keyboard-click?
  [e]
  (contains? #{"Enter" " "} (.-key e)))

(defn interactive-state
  "State for `interactive-attrs`, held by a form-2 component."
  []
  {:active? (r/atom false) :key (atom nil) :el (atom nil)})

(defn interactive-attrs
  "Blueprint `useInteractiveAttributes`: Enter/Space show the element active while held
   and click it on release. Returns [active? attrs]."
  [interactive? props {:keys [active? key el]}
   {:keys [default-tab-index disabled-tab-index] :or {disabled-tab-index -1}}]
  (let [{:keys [active on-click on-focus on-key-down on-key-up on-blur ref tab-index]
         :or   {tab-index default-tab-index}} props]
    [(boolean (and interactive? (or active @active?)))
     {:on-blur     (fn [e] (reset! active? false) (when on-blur (on-blur e)))
      :on-click    (when interactive? on-click)
      :on-focus    (when interactive? on-focus)
      :on-key-down (fn [e]
                     (when (keyboard-click? e)
                       (.preventDefault e)
                       (when (not= (.-key e) @key) (reset! active? true)))
                     (reset! key (.-key e))
                     (when on-key-down (on-key-down e)))
      :on-key-up   (fn [e]
                     (when (keyboard-click? e)
                       (reset! active? false)
                       (some-> @el .click))
                     (reset! key nil)
                     (when on-key-up (on-key-up e)))
      :ref         (fn [node] (reset! el node) (when ref (ref node)))
      :tab-index   (if interactive? tab-index disabled-tab-index)}]))

(def interactive-keys
  [:active :on-click :on-focus :on-key-down :on-key-up :on-blur :ref :tab-index])

(defn empty-node?
  [x]
  (or (nil? x) (false? x) (= "" x) (and (sequential? x) (not (keyword? (first x)))
                                        (every? empty-node? x))))
