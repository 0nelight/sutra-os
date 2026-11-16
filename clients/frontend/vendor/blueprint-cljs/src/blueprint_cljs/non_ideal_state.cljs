(ns blueprint-cljs.non-ideal-state
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.icon :as icon]))

(def icon-size-standard 48)
(def icon-size-small 32)
(def icon-size-extra-small 20)

(defn non-ideal-state
  "Empty/error placeholder. Props: `:icon`, `:icon-size` (48/32/20), `:icon-muted`
   (default true), `:title`, `:description`, `:action`, `:layout` (:vertical :horizontal)."
  [& args]
  (let [[{:keys [action description icon-muted icon-size layout title class]
          :or   {icon-muted true icon-size icon-size-standard layout :vertical}
          :as   props} children] (c/split-args args)]
    (into [:div {:class (bp/cx (bp/cls "non-ideal-state")
                               (bp/cls (str "non-ideal-state-" (name layout))) class)}
           (when (some? (:icon props))
             [:div {:class (bp/cls "non-ideal-state-visual")
                    :style {:font-size (str icon-size "px") :line-height (str icon-size "px")}}
              (icon/render-icon (:icon props)
                                {:class       (bp/cx {(bp/cls "icon-muted") icon-muted})
                                 :size        icon-size
                                 :aria-hidden true
                                 :tab-index   -1})])
           (when (or (some? title) (some? description))
             [:div {:class (bp/cls "non-ideal-state-text")}
              (when title [:h4 {:class (bp/cls "heading")} title])
              (when description
                (if (string? description) [:div description] description))])
           action]
          children)))
