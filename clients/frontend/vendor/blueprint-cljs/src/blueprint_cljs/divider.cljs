(ns blueprint-cljs.divider
  (:require [blueprint-cljs.core :as bp]))

(defn divider
  "A thin line between items. Props: `:compact`, `:tag-name` (default :div)."
  ([] (divider {}))
  ([{:keys [compact tag-name class] :or {tag-name :div} :as props}]
   [tag-name (merge (bp/dom-props props [:compact :tag-name :class])
                    {:class (bp/cx (bp/cls "divider") {(bp/cls "compact") compact} class)})]))
