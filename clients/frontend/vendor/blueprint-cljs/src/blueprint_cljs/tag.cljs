(ns blueprint-cljs.tag
  "Tag and CompoundTag. `[tag {:intent :primary :on-remove f} \"label\"]`."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.text :as text]))

(def ^:private own-keys
  (into [:end-icon :fill :icon :intent :interactive :large :minimal :multiline :on-remove
         :right-icon :round :size :html-title :class :left-content :variant]
        c/interactive-keys))

(defn- remove-button
  [{:keys [on-remove tab-index] :as props}]
  [:button {:aria-label "Remove tag"
            :type       "button"
            :class      (bp/cls "tag-remove")
            :on-click   (fn [e] (on-remove e props))
            :tab-index  tab-index}
   [icon/icon {:icon :small-cross :size (if (c/large? props) 20 16)}]])

(defn- tag-class
  [{:keys [intent fill minimal round class] :as props} active? interactive? & more]
  (apply bp/cx (bp/cls "tag")
         (concat more
                 [(bp/intent-class intent)
                  (c/size-class (dissoc props :small))
                  {(bp/cls "active")      active?
                   (bp/cls "fill")        fill
                   (bp/cls "interactive") interactive?
                   (bp/cls "minimal")     (or minimal (= "minimal" (some-> (:variant props) name)))
                   (bp/cls "round")       round}
                  class])))

(defn tag
  "Props: `:intent`, `:icon`, `:right-icon`/`:end-icon`, `:minimal`, `:round`, `:fill`,
   `:size`/`:large`, `:interactive` (default: when `:on-click` is set), `:multiline`,
   `:on-remove` (fn [e props]), `:html-title`, `:active`."
  [& _]
  (let [state (c/interactive-state)]
    (fn [& args]
      (let [[props children] (c/split-args args)
            props            (update props :tab-index #(if (nil? %) 0 %))
            interactive?     (boolean (if (contains? props :interactive)
                                        (:interactive props)
                                        (:on-click props)))
            [active? attrs]  (c/interactive-attrs interactive? props state
                                                  {:default-tab-index 0
                                                   :disabled-tab-index nil})]
        [:span (merge (bp/dom-props props own-keys)
                      attrs
                      {:class (tag-class props active? interactive?)
                       :role  (when interactive? "button")})
         (icon/render-icon (:icon props))
         (when-not (every? c/empty-node? children)
           (into [text/text {:class     (bp/cls "fill")
                             :ellipsize (not (:multiline props))
                             :tag-name  :span
                             :title     (:html-title props)}]
                 children))
         (icon/render-icon (or (:end-icon props) (:right-icon props)))
         (when (fn? (:on-remove props)) [remove-button props])]))))

(defn compound-tag
  "Tag in two halves: `:left-content` on the left, children on the right."
  [& args]
  (let [[{:keys [interactive tab-index left-content active] :or {tab-index 0} :as props}
         children] (c/split-args args)]
    [:span (merge (bp/dom-props props own-keys)
                  {:class     (tag-class props active interactive (bp/cls "compound-tag"))
                   :tab-index (when interactive tab-index)})
     [:span {:class (bp/cls "compound-tag-left")}
      (icon/render-icon (:icon props))
      [text/text {:class    (bp/cx (bp/cls "compound-tag-left-content") (bp/cls "fill"))
                  :tag-name :span}
       left-content]]
     [:span {:class (bp/cls "compound-tag-right")}
      (when-not (every? c/empty-node? children)
        (into [text/text {:class    (bp/cx (bp/cls "compound-tag-right-content")
                                           (bp/cls "fill"))
                          :tag-name :span}]
              children))
      (icon/render-icon (or (:end-icon props) (:right-icon props)))
      (when (fn? (:on-remove props)) [remove-button props])]]))
