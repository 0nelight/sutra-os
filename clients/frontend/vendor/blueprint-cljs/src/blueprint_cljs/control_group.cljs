(ns blueprint-cljs.control-group
  "ControlGroup: inputs, buttons and selects joined edge to edge.
   `[control-group {:fill true :vertical false} [html-select …] [input-group …] [button …]]`"
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(defn control-group
  [& args]
  (let [[{:keys [class fill vertical] :as props} children] (c/split-args args)]
    (into [:div (merge {:role "group"}
                       (bp/dom-props props [:class :fill :vertical])
                       {:class (bp/cx (bp/cls "control-group")
                                      {(bp/cls "fill")     fill
                                       (bp/cls "vertical") vertical}
                                      class)})]
          children)))
