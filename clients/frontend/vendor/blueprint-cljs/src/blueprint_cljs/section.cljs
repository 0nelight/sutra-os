(ns blueprint-cljs.section
  "Section (a card with a header, optionally collapsible) and SectionCard."
  (:require [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]
            [blueprint-cljs.card :as card]
            [blueprint-cljs.collapse :as collapse]
            [blueprint-cljs.icon :as icon]))

(defonce ^:private ids (atom 0))

(defn- unique-id [prefix] (str prefix "-" (swap! ids inc)))

(defn section
  "Props: `:title`, `:subtitle`, `:icon`, `:right-element`, `:compact`, `:elevation` 0/1,
   `:collapsible`, `:collapse-props` {:is-open :default-is-open :on-toggle
   :keep-children-mounted :transition-duration}, `:title-renderer` (tag keyword or
   component, default :h6)."
  [& _]
  (let [collapsed? (r/atom nil)
        section-id (unique-id "section")
        title-id   (unique-id "section-title")]
    (fn [& args]
      (let [[{:keys [collapse-props collapsible compact elevation right-element subtitle
                     title title-renderer class]
              :or   {elevation 0 title-renderer :h6}
              :as   props} children] (c/split-args args)
            controlled? (some? (:is-open collapse-props))
            _           (when (nil? @collapsed?)
                          (reset! collapsed? (not (get collapse-props :default-is-open true))))
            collapsed   (if controlled? (not (:is-open collapse-props)) @collapsed?)
            toggle!     (fn [_]
                          (if controlled?
                            (when-let [f (:on-toggle collapse-props)] (f))
                            (swap! collapsed? not)))]
        [card/card
         (merge {:aria-labelledby (when title title-id)}
                (bp/dom-props props [:collapse-props :collapsible :compact :elevation :icon
                                     :right-element :subtitle :title :title-renderer :class])
                {:class     (bp/cx class (bp/cls "section")
                                   {(bp/cls "compact")           compact
                                    (bp/cls "section-collapsed") (or (and collapsible collapsed)
                                                                     (every? c/empty-node?
                                                                             children))})
                 :elevation elevation
                 :id        section-id})
         (when title
           [:div {:class    (bp/cx (bp/cls "section-header")
                                   {(bp/cls "interactive") collapsible})
                  :on-click (when collapsible toggle!)}
            [:div {:class (bp/cls "section-header-left")}
             (when (:icon props)
               (icon/render-icon (:icon props) {:aria-hidden true :tab-index -1
                                                :class (bp/cls "text-muted")}))
             [:div
              [title-renderer {:class (bp/cx (bp/cls "heading") (bp/cls "section-header-title"))
                               :id    title-id}
               title]
              (when subtitle
                [:div {:class (bp/cx (bp/cls "text-muted") (bp/cls "section-header-sub-title"))}
                 subtitle])]]
            (when (or right-element collapsible)
              [:div {:class (bp/cls "section-header-right")}
               right-element
               (when collapsible
                 [:span {:role          "button"
                         :tab-index     0
                         :aria-pressed  collapsed
                         :aria-expanded collapsed
                         :aria-controls section-id
                         :aria-label    (if collapsed "expand section" "collapse section")
                         :on-key-down   (fn [e]
                                          (when (c/keyboard-click? e)
                                            (.preventDefault e)
                                            (.click (.-currentTarget e))))
                         :class         (bp/cx (bp/cls "text-muted")
                                               (bp/cls "section-header-collapse-caret"))}
                  [icon/icon {:icon (if collapsed :chevron-down :chevron-up)}]])])])
         (if collapsible
           (into [collapse/collapse (-> collapse-props
                                        (dissoc :default-is-open :on-toggle)
                                        (assoc :is-open (not collapsed)))]
                 children)
           (into [:<>] children))]))))

(defn section-card
  "A padded block inside a Section. Props: `:padded` (default true)."
  [& args]
  (let [[{:keys [padded class] :or {padded true} :as props} children] (c/split-args args)]
    (into [:div (merge (bp/dom-props props [:padded :class])
                       {:class (bp/cx (bp/cls "section-card") {(bp/cls "padded") padded}
                                      class)})]
          children)))
