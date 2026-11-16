(ns blueprint-cljs.callout
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.icon :as icon]))

(def ^:private intent-icons
  "Blueprint 6 takes these from its \"next\" icon set; the classic set's equivalents."
  {"danger"  :error
   "primary" :info-sign
   "warning" :warning-sign
   "success" :tick})

(defn- callout-icon
  [props intent]
  (let [i (:icon props)]
    (cond
      (and (contains? props :icon) (or (nil? i) (false? i))) nil
      (some? i) (icon/render-icon i {:aria-hidden true :tab-index -1})
      :else     (some-> (get intent-icons (some-> intent name))
                        (icon/render-icon {:aria-hidden true :tab-index -1})))))

(defn callout
  "Props: `:intent` (picks a default icon), `:icon` (false/nil = none), `:title`,
   `:compact`, `:minimal`."
  [& args]
  (let [[{:keys [intent title compact minimal class] :as props} children] (c/split-args args)
        ic (callout-icon props intent)]
    (into [:div (merge (bp/dom-props props [:intent :icon :title :compact :minimal :class])
                       {:class (bp/cx (bp/cls "callout") (bp/intent-class intent) class
                                      {(bp/cls "callout-has-body-content")
                                       (not (every? c/empty-node? children))
                                       (bp/cls "callout-icon") (some? ic)
                                       (bp/cls "compact")      compact
                                       (bp/cls "minimal")      minimal})})
           ic
           (when title [:h5 {:class (bp/cls "heading")} title])]
          children)))
