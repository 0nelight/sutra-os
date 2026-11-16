(ns blueprint-cljs.control-card
  "ControlCard and its kinds: a Card whose whole surface toggles a checkbox, radio or
   switch. `[checkbox-card {:label \"Email\" :checked v :on-change f}]`,
   `[radio-card {:value \"a\"} \"Option A\"]` (inside radio-group), `[switch-card …]`.
   Props: `:label` (or children), `:checked`, `:default-checked`, `:on-change`, `:value`,
   `:disabled`, `:align-indicator` (:end default; checkbox/radio :start),
   `:show-as-selected-when-checked` (true), `:input-props`, `:input-ref`; the rest goes to
   the card (`:elevation`, `:compact`, `:class`, …)."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.checkbox :as checkbox]
            [blueprint-cljs.radio :as radio]
            [blueprint-cljs.switch :as switch]))

(defn control-card
  "`:control-kind` :checkbox, :radio or :switch."
  [& args]
  (let [[init] (c/split-args args)
        local  (r/atom (boolean (:default-checked init)))]
    (fn [& args]
      (let [[{:keys [align-indicator checked class control-kind disabled elevation
                     input-props input-ref label on-change show-as-selected-when-checked
                     value compact]
              :or   {align-indicator :end show-as-selected-when-checked true}
              :as   props}
             children] (c/split-args args)
            checked? (if (some? checked) checked @local)
            label-el [:div {:class (bp/cls "control-card-label")}
                      (if (seq children) (into [:<>] children) label)]
            ctl      (merge {:align-indicator align-indicator
                             :checked         checked?
                             :disabled        disabled
                             :inline          true
                             :input-ref       input-ref
                             :label-element   label-el
                             :on-change       (fn [e]
                                                (reset! local (.. e -target -checked))
                                                (when on-change (on-change e)))
                             :value           value}
                            input-props)]
        [:div (merge (bp/dom-props props [:align-indicator :checked :class :control-kind
                                          :default-checked :disabled :elevation :input-props
                                          :input-ref :label :on-change :value :compact
                                          :show-as-selected-when-checked :name :inline])
                     {:class (bp/cx (bp/cls "card")
                                    (bp/cls (str "elevation-" (or elevation 0)))
                                    {(bp/cls "interactive") (not disabled)
                                     (bp/cls "compact")     compact
                                     (bp/cls "selected")    (and show-as-selected-when-checked
                                                                 checked?)}
                                    (bp/cls "control-card")
                                    class)})
         (case (some-> control-kind name)
           "switch"   [switch/switch ctl]
           "checkbox" [checkbox/checkbox ctl]
           "radio"    [radio/radio (assoc ctl :name (:name props))]
           label-el)]))))

(defn checkbox-card
  [& args]
  (let [[props children] (c/split-args args)]
    (into [control-card (merge {:align-indicator :start} props
                               {:class        (bp/cx (:class props)
                                                     (bp/cls "checkbox-control-card"))
                                :control-kind :checkbox})]
          children)))

(defn radio-card
  [& args]
  (let [[props children] (c/split-args args)]
    (into [control-card (merge {:align-indicator :start} props
                               {:class        (bp/cx (:class props)
                                                     (bp/cls "radio-control-card"))
                                :control-kind :radio})]
          children)))

(defn switch-card
  [& args]
  (let [[props children] (c/split-args args)]
    (into [control-card (merge props
                               {:class        (bp/cx (:class props)
                                                     (bp/cls "switch-control-card"))
                                :control-kind :switch})]
          children)))
