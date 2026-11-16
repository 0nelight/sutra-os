(ns blueprint-cljs.text
  "Text: optionally ellipsized; when the text overflows it shows itself as `title`."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(defn text
  "Props: `:ellipsize`, `:tag-name` (default :div), `:title`, `:class`."
  [& _]
  (let [el       (atom nil)
        overflow (r/atom nil)
        measure! (fn [this]
                   (when-let [n @el]
                     (let [[props] (c/split-args (rest (r/argv this)))
                           v       (when (and (:ellipsize props)
                                              (> (.-scrollWidth n) (.-clientWidth n)))
                                     (.-textContent n))]
                       (when (not= v @overflow) (reset! overflow v)))))]
    (r/create-class
     {:display-name         "Text"
      :component-did-mount  measure!
      :component-did-update measure!
      :reagent-render
      (fn [& args]
        (let [[{:keys [ellipsize tag-name title] :or {tag-name :div} :as props} children]
              (c/split-args args)]
          (into [tag-name
                 (merge (bp/dom-props props [:ellipsize :tag-name :title :class :ref])
                        {:class (bp/cx {(bp/cls "text-overflow-ellipsis") ellipsize}
                                       (:class props))
                         :ref   (fn [n] (reset! el n) (when-let [f (:ref props)] (f n)))
                         :title (or title @overflow)})]
                children)))})))
