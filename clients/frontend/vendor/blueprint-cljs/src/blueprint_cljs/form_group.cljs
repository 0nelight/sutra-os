(ns blueprint-cljs.form-group
  "FormGroup: label, sub-label, content and helper text around a field.
   `[form-group {:label \"Name\" :label-for \"n\" :label-info \"(required)\"
                 :helper-text \"…\" :intent :danger} [input-group …]]`"
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(defn form-group
  [& args]
  (let [[{:keys [class content-class-name disabled fill helper-text inline intent label
                 label-for label-info style sub-label]
          :as props}
         children] (c/split-args args)]
    [:div (merge (bp/dom-props props [:class :content-class-name :disabled :fill :helper-text
                                      :inline :intent :label :label-for :label-info
                                      :sub-label])
                 {:class (bp/cx (bp/cls "form-group") (bp/intent-class intent)
                                {(bp/cls "disabled") disabled
                                 (bp/cls "fill")     fill
                                 (bp/cls "inline")   inline}
                                class)
                  :style style})
     (when label
       [:label {:class (bp/cls "label") :html-for label-for}
        label " " [:span {:class (bp/cls "text-muted")} label-info]])
     (when sub-label [:div {:class (bp/cls "form-group-sub-label")} sub-label])
     (into [:div {:class (bp/cx (bp/cls "form-content") content-class-name)}]
           (concat children
                   [(when helper-text
                      [:div {:class (bp/cls "form-helper-text")} helper-text])]))]))
