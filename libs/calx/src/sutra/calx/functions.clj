(ns sutra.calx.functions
  (:require [sutra.calx.fn.logical :as logical]
            [sutra.calx.fn.lookup :as lookup]
            [sutra.calx.fn.math :as math]
            [sutra.calx.fn.engineering :as engineering]
            [sutra.calx.fn.date :as date]
            [sutra.calx.fn.financial :as financial]
            [sutra.calx.fn.text :as text]
            [sutra.calx.fn.info :as info]
            [sutra.calx.fn.stats :as stats]))

(set! *warn-on-reflection* true)

(def registry
  (merge logical/functions
         lookup/functions
         math/functions
         engineering/functions
         date/functions
         financial/functions
         text/functions
         info/functions
         stats/functions))

(defn function-names [] (sort (keys registry)))
