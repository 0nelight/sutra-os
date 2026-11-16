(ns blueprint-cljs.switch
  "Switch. `[switch {:label \"Public\" :checked v :on-change f}]`; `:inner-label` and
   `:inner-label-checked` print text inside the track."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.control :as control]))

(defn- inner
  [k text]
  ^{:key k}
  [:div {:class (bp/cls "control-indicator-child")}
   [:div {:class (bp/cls "switch-inner-text")} text]])

(defn switch
  [& args]
  (let [[{:keys [inner-label inner-label-checked] :as props} children] (c/split-args args)]
    [control/control
     (-> (dissoc props :inner-label :inner-label-checked)
         (assoc :type "checkbox"
                :type-class (bp/cls "switch")
                :indicator-children
                (when (or inner-label inner-label-checked)
                  [(inner "checked" (or inner-label-checked inner-label))
                   (inner "unchecked" inner-label)])))
     children]))
