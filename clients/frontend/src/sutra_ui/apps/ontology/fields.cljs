(ns sutra-ui.apps.ontology.fields
  (:require [sutra-ui.ui.inputs :as inputs]))

(defn via
  [value write!]
  (reify
    IDeref (-deref [_] value)
    IReset (-reset! [_ v] (write! v) v)))

(defn pick
  ([value items on-change] (pick value items on-change nil))
  ([value items on-change opts]
   [inputs/select-input nil
    (merge {:value (or value "") :items items :strict? true
            :on-change (fn [v _] (on-change (if (nil? v) "" v)))}
           opts)]))

(defn named
  [xs]
  (for [x xs] {:value (name x) :label (name x)}))

(defn pairs
  [opts]
  (for [[v l] opts] {:value v :label l}))
