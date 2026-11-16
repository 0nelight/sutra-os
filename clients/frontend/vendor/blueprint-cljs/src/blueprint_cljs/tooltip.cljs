(ns blueprint-cljs.tooltip
  "Tooltip: a hover popover with dark (in dark theme: light) text bubble."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.popover :as popover]))

(def tooltip-arrow-svg-size 22)

(defn tooltip
  "Props: `:content`, `:intent`, `:compact`, `:placement`/`:position`, `:interaction-kind`
   (:hover-target-only default, or :hover), `:hover-open-delay` (100),
   `:hover-close-delay` (0), `:transition-duration` (100), `:minimal`, `:disabled`,
   `:is-open`/`:default-is-open`, `:popover-class-name`, `:class`, `:target-tag-name`, and
   any other popover prop. The single child is the target."
  [props & children]
  (let [[props children] (if (map? props) [props children] [{} (cons props children)])
        {:keys [intent compact popover-class-name]} props]
    (into [popover/popover
           (merge {:interaction-kind    :hover-target-only
                   :hover-open-delay    100
                   :hover-close-delay   0
                   :transition-duration 100
                   :placement           :top}
                  (dissoc props :intent :compact :popover-class-name)
                  {:autofocus          false
                   :enforce-focus      false
                   :lazy               true
                   :arrow-offset       (/ tooltip-arrow-svg-size 2)
                   :popup-kind         "tooltip"
                   :popover-class-name (bp/cx (bp/cls "tooltip") (bp/intent-class intent)
                                              {(bp/cls "compact") compact}
                                              popover-class-name)})]
          children)))
