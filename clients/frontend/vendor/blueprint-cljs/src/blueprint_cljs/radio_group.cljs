(ns blueprint-cljs.radio-group
  "RadioGroup: one name and one selected value over its radios.
   `[radio-group {:label \"Size\" :selected-value v :on-change f
                  :options [{:label \"S\" :value \"s\"} \"m\"]}]`
   or radios as children (`[radio/radio {:value …}]`, `[radio-card …]`), which get the
   group's name, `:checked`, `:disabled`, `:inline` and `:on-change`."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.radio :as radio]))

(defonce ^:private counter (atom 0))

(defn- radio-props
  [{:keys [selected-value disabled inline on-change]} group-name option]
  (let [{:keys [class value] option-disabled :disabled} option]
    {:checked   (= value selected-value)
     :class     class
     :disabled  (or option-disabled disabled)
     :inline    inline
     :name      group-name
     :on-change on-change
     :value     value}))

(defn- radio-child?
  [x]
  (and (vector? x) (map? (second x)) (contains? (second x) :value)))

(defn radio-group
  [& _]
  (let [auto-name (str "Blueprint.RadioGroup-" (swap! counter inc))
        label-id  (str "bp-radio-group-label-" @counter)]
    (fn [& args]
      (let [[{:keys [class label name options] :as props} children] (c/split-args args)
            group-name (or name auto-name)]
        (into [:div (merge (bp/dom-props props [:class :label :name :options :selected-value
                                                :disabled :inline :on-change])
                           {:role            "radiogroup"
                            :aria-labelledby (when label label-id)
                            :class           (bp/cx (bp/cls "radio-group") class)})
               (when label [:label {:class (bp/cls "label") :id label-id} label])]
              (if (sequential? options)
                (for [o options
                      :let [o (if (map? o) o {:value o})]]
                  ^{:key (str (:value o))}
                  [radio/radio (assoc (radio-props props group-name o)
                                      :label-element (or (:label o) (:value o)))])
                (for [[i ch] (map-indexed vector children)]
                  (if (radio-child? ch)
                    (with-meta
                      (update ch 1 merge (radio-props props group-name (second ch)))
                      {:key i})
                    ch))))))))
