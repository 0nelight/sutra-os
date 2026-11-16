(ns blueprint-cljs.slider
  "Slider: one handle; the track fills from `:initial-value` (0) to the value in `:intent`
   (:primary). `[slider {:min 0 :max 10 :value v :on-change f :on-release f}]`
   Takes every MultiSlider prop; `:handle-html-props` go to the handle."
  (:require [blueprint-cljs.common :as c]
            [blueprint-cljs.multi-slider :as ms]))

(defn slider
  [& args]
  (let [[{:keys [initial-value intent value on-change on-release handle-html-props]
          :or   {initial-value 0 intent :primary value 0}
          :as   props}] (c/split-args args)]
    [ms/multi-slider
     (assoc (dissoc props :initial-value :intent :value :on-change :on-release
                    :handle-html-props)
            :handles [{:value         value
                       :intent-after  (when (< value initial-value) intent)
                       :intent-before (when (> value initial-value) intent)
                       :on-change     on-change
                       :on-release    on-release
                       :html-props    handle-html-props}
                      {:value initial-value :interaction-kind :none}])]))
