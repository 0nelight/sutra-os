(ns blueprint-cljs.html-select
  "HTMLSelect: a styled native `<select>`.
   `[html-select {:options [\"a\" {:value \"b\" :label \"Bee\" :disabled true}]
                  :value v :on-change f}]`
   Props: `:fill`, `:minimal`, `:large`/`:size`, `:disabled`, `:icon-name`
   (:double-caret-vertical default, or :caret-down), `:icon-props`; the rest goes to the
   `<select>`. Children are extra `<option>`s."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.icon :as icon]))

(defn html-select
  [& args]
  (let [[{:keys [class disabled fill icon-name icon-props minimal options]
          :or   {icon-name :double-caret-vertical}
          :as   props}
         children] (c/split-args args)]
    [:div {:class (bp/cx (bp/cls "html-select")
                         {(bp/cls "disabled") disabled
                          (bp/cls "fill")     fill
                          (bp/cls "large")    (c/large? props)
                          (bp/cls "minimal")  minimal}
                         class)}
     (into [:select (merge (bp/dom-props props [:class :fill :icon-name :icon-props :minimal
                                                :options :large :size :small])
                           {:multiple false})]
           (concat
            (for [o options
                  :let [{:keys [value label] :as o} (if (map? o) o {:value o})]]
              ^{:key (str value)}
              [:option (merge (dissoc o :label) {:value value}) (or label value)])
            children))
     [icon/icon (merge {:icon  (if (= (name icon-name) "caret-down")
                                 :caret-down :double-caret-vertical)
                        :title "Open dropdown"}
                       icon-props)]]))
