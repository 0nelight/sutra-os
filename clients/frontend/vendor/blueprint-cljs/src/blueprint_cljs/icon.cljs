(ns blueprint-cljs.icon
  "Icon: an inline SVG from the path registry. `blueprint-cljs.icons.all` fills the
   registry; until it is loaded an icon keeps its box, so nothing shifts."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]))

(defonce registry (r/atom {16 {} 20 {}}))

(defn register-paths!
  "`size` 16 or 20, `paths` a map icon-name string → vector of path `d` strings."
  [size paths]
  (swap! registry update size merge paths))

(defn icon
  "Props: `:icon` keyword/string name, `:size` px (default 16; ≥ 20 uses the 20px set),
   `:intent`, `:color`, `:title`, `:class`, `:tag-name`; the rest goes to the element."
  [{:keys [size intent color title tag-name] icon-name :icon :as props
    :or {size 16 tag-name :span}}]
  (when icon-name
    (let [set-size (if (>= size 20) 20 16)
          paths    (get-in @registry [set-size (name icon-name)])]
      [tag-name (merge (bp/dom-props props [:icon :size :intent :color :title :tag-name])
                       {:class       (bp/cx (bp/cls "icon") (bp/intent-class intent)
                                            (bp/cls (str "icon-" (name icon-name)))
                                            (:class props))
                        :aria-hidden (when-not title true)})
       [:svg {:data-icon (name icon-name) :width size :height size
              :viewBox (str "0 0 " set-size " " set-size) :fill color :role "img"}
        (when title [:title title])
        (for [[i d] (map-indexed vector paths)]
          ^{:key i} [:path {:d d :fill-rule "evenodd"}])]])))

(defn render-icon
  "What a component's `:icon`/`:right-icon` prop renders: nil for nil, an icon for a
   keyword/string, a given hiccup element as is."
  ([x] (render-icon x {}))
  ([x props]
   (cond
     (nil? x)                     nil
     (or (keyword? x) (string? x)) [icon (assoc props :icon x)]
     :else                        x)))
