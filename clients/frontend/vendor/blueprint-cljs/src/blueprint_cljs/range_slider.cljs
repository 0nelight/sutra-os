(ns blueprint-cljs.range-slider
  "RangeSlider: two handles, the range between them filled in `:intent` (:primary).
   `[range-slider {:min 0 :max 10 :value [2 7] :on-change (fn [[lo hi]])}]`
   Takes every MultiSlider prop; `:handle-html-props` {:start {…} :end {…}}."
  (:require [blueprint-cljs.common :as c]
            [blueprint-cljs.multi-slider :as ms]))

(defn range-slider
  [& args]
  (let [[{:keys [value intent handle-html-props]
          :or   {value [0 10] intent :primary}
          :as   props}] (c/split-args args)]
    [ms/multi-slider
     (assoc (dissoc props :value :intent :handle-html-props)
            :handles [{:value        (first value)
                       :type         :start
                       :intent-after intent
                       :html-props   (:start handle-html-props)}
                      {:value      (second value)
                       :type       :end
                       :html-props (:end handle-html-props)}])]))
