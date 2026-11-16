(ns blueprint-cljs.progress-bar
  (:require [blueprint-cljs.core :as bp]))

(defn progress-bar
  "Props: `:value` 0–1 (nil = indeterminate), `:intent`, `:animate` and `:stripes`
   (both default true)."
  ([] (progress-bar {}))
  ([{:keys [animate stripes intent value class] :or {animate true stripes true} :as props}]
   (let [percent (when (some? value) (* 100 (-> value (max 0) (min 1))))]
     [:div (merge (bp/dom-props props [:animate :stripes :intent :value :class])
                  {:aria-valuemax 100
                   :aria-valuemin 0
                   :aria-valuenow (some-> percent js/Math.round)
                   :role          "progressbar"
                   :class         (bp/cx (bp/cls "progress-bar") (bp/intent-class intent)
                                         {(bp/cls "no-animation") (not animate)
                                          (bp/cls "no-stripes")   (not stripes)}
                                         class)})
      [:div {:class (bp/cls "progress-meter")
             :style {:width (when percent (str percent "%"))}}]])))
