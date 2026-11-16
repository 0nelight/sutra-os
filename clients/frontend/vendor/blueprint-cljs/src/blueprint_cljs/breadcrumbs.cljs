(ns blueprint-cljs.breadcrumbs
  "Breadcrumbs: `[breadcrumbs {:items [{:text \"Users\" :href \"#\"} … {:text \"Janet\"}]}]`.
   What does not fit collapses into a popover menu."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.menu :as menu]
            [blueprint-cljs.overflow-list :as ol]
            [blueprint-cljs.popover :as popover]))

(defn breadcrumb
  "Props: `:text`, `:href`, `:target`, `:on-click`, `:on-focus`, `:icon`, `:icon-title`,
   `:current`, `:disabled`, `:class`; children follow the text."
  [{:keys [text href target on-click on-focus icon icon-title current disabled class]}
   & children]
  (let [classes (bp/cx (bp/cls "breadcrumb")
                       {(bp/cls "breadcrumb-current") current
                        (bp/cls "disabled")           disabled}
                       class)
        ic      (when (some? icon) (icon/render-icon icon {:title icon-title}))]
    (if (and (nil? href) (nil? on-click))
      (into [:span {:class classes} ic text] children)
      (into [:a {:class     classes
                 :href      href
                 :on-click  (when-not disabled on-click)
                 :on-focus  (when-not disabled on-focus)
                 :tab-index (when-not disabled 0)
                 :target    target}
             ic text]
            children))))

(defn- overflow-menu
  [items collapse-from popover-props overflow-button-props]
  (let [ordered (if (= collapse-from :start) (reverse items) items)]
    [:li
     [popover/popover
      (merge {:content   (into [menu/menu {}]
                               (map-indexed
                                (fn [i {:keys [href on-click text icon target]}]
                                  ^{:key i}
                                  [menu/menu-item {:disabled (and (nil? href) (nil? on-click))
                                                   :href     href
                                                   :target   target
                                                   :icon     icon
                                                   :on-click on-click
                                                   :text     text}])
                                ordered))
              :disabled  (empty? ordered)
              :placement (if (= collapse-from :end) "bottom-end" "bottom-start")}
             popover-props)
      [:span (merge {:aria-label "collapsed breadcrumbs" :role "button" :tab-index 0}
                    overflow-button-props
                    {:class (bp/cx (bp/cls "breadcrumbs-collapsed")
                                   (:class overflow-button-props))})]]]))

(defn breadcrumbs
  "Props: `:items` (breadcrumb prop maps, the last one is current), `:collapse-from`
   `:start` (default) | `:end`, `:min-visible-items` (0), `:breadcrumb-renderer`
   `(fn [props])`, `:current-breadcrumb-renderer` `(fn [props])`,
   `:overflow-button-props`, `:overflow-list-props`, `:popover-props`, `:class`."
  [{:keys [items collapse-from min-visible-items breadcrumb-renderer
           current-breadcrumb-renderer overflow-button-props overflow-list-props
           popover-props class]
    :or   {collapse-from :start min-visible-items 0}}]
  (let [items (vec items)
        last-i (dec (count items))]
    [ol/overflow-list
     (merge {:collapse-from         collapse-from
             :min-visible-items     min-visible-items
             :tag-name              :ol
             :navigable             true
             :navigation-aria-label "Breadcrumb"}
            overflow-list-props
            {:class                 (bp/cx (bp/cls "breadcrumbs") (:class overflow-list-props)
                                           class)
             :items                 (map-indexed vector items)
             :overflow-renderer     (fn [overflow]
                                      (overflow-menu (map second overflow) collapse-from
                                                     popover-props overflow-button-props))
             :visible-item-renderer (fn [[i props] _]
                                      (let [current? (= i last-i)]
                                        [:li
                                         (cond
                                           (and current? current-breadcrumb-renderer)
                                           (current-breadcrumb-renderer props)

                                           breadcrumb-renderer
                                           (breadcrumb-renderer props)

                                           :else
                                           [breadcrumb (merge {:current current?} props)])]))})]))
