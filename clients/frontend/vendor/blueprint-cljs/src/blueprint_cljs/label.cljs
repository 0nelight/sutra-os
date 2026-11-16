(ns blueprint-cljs.label
  "Label: Blueprint's CSS-only `.bp-label` around a field.
   `[label {:inline true} \"Name\" [:input.bp-input]]`; `:disabled`, `:label-info`."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(defn label
  [& args]
  (let [[{:keys [class disabled inline label-info] :as props} children] (c/split-args args)]
    (into [:label (merge (bp/dom-props props [:class :disabled :inline :label-info])
                         {:class (bp/cx (bp/cls "label")
                                        {(bp/cls "disabled") disabled
                                         (bp/cls "inline")   inline}
                                        class)})]
          (concat (take 1 children)
                  [(when label-info
                     [:span {:class (bp/cls "text-muted")} " " label-info])]
                  (drop 1 children)))))
