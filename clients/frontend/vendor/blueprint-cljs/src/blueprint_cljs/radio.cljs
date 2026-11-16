(ns blueprint-cljs.radio
  "Radio. `[radio {:label \"One\" :value \"1\" :checked v :on-change f}]`."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.control :as control]))

(defn radio
  [& args]
  (let [[props children] (c/split-args args)]
    [control/control (assoc props :type "radio" :type-class (bp/cls "radio")) children]))
