(ns sutra-ui.apphost.widgets.nav
  (:require [sutra-ui.ui.overlay :as overlay]
            [reagent.core :as r]
            [sutra.capps.nav-bar :as nav]
            [sutra.capps.variables :refer [deref*]]
            [sutra-ui.ui.core :as ui]))

(defn- fire! [env on-click] (when-let [f (and (seq on-click) ((:event-fn env) on-click))] (f nil)))

(defn tabs
  [{:keys [sel items] :as cfg} env]
  (let [items    (vec items)
        active   (nav/selected-tab items (deref* sel) deref*)
        shown    (filterv #(not= :hidden (nav/tab-state %)) items)]
    [:div {:class "capp-tabs-frame" :style (nav/tabs-style cfg)}
     (into [ui/tabs
            {:id              (str "capp-tabs-" (hash (mapv :id shown)))
             :class           (nav/tabs-class cfg)
             :animate         false
             :selected-tab-id (some #(when (nav/same-id? (:id %) active) (:id %)) shown)
             :on-change       (fn [id]
                                (when (satisfies? IReset sel) (reset! sel id))
                                (fire! env (some #(when (= id (:id %)) (:on-click %)) shown)))}]
           (map (fn [{:keys [id label count badge icon] :as it}]
                  (let [badge (if (some? badge) badge count)]
                    [ui/tab
                     {:id       id
                      :disabled (= :disabled (nav/tab-state it))
                      :title    [:<> (when icon [:span.capp-tab__icon icon]) label]
                      :tag-content badge}])))
           shown)]))

(defn- set-tab-identity!
  [{:keys [title icon]}]
  (when (seq (str title)) (set! (.-title js/document) (str title)))
  (when (seq (str icon))
    (when-let [link (or (.querySelector js/document "link[rel~='icon']")
                        (let [l (.createElement js/document "link")]
                          (set! (.-rel l) "icon")
                          (.appendChild (.-head js/document) l)
                          l))]
      (set! (.-href link) (nav/favicon-href icon)))))

(defn- item-body
  [{:keys [icon label]} hide-text?]
  [:<>
   (when icon [:span.capp-navbar__icon icon])
   (when-not hide-text? [:span.capp-navbar__label label])])

(defn mobile-navbar
  [_cfg _env]
  (let [lit  (atom nil)
        open (r/atom nil)]
    (r/create-class
     {:display-name "mobile-navbar"
      :component-did-mount  (fn [this] (set-tab-identity! (second (r/argv this))))
      :component-did-update (fn [this] (set-tab-identity! (second (r/argv this))))
      :reagent-render
      (fn [{:keys [items height hide-text?]} env]
        (let [items  (nav/bar-items items)
              here   (nav/lit-index items (deref* (nav/bar-page-var items)) @lit)
              h      (or height 50)]
          (reset! lit here)
          [:<>
           [:div.capp-navbar__spacer {:style {:height (str h "px")}}]
           (into
            [:nav.capp-navbar {:style {:height (str h "px")}}]
            (map-indexed
             (fn [i {:keys [on-click] sub :items :as it}]
               ^{:key i}
               [:div.capp-navbar__slot
                [:button
                 {:type     "button"
                  :class    (cond-> "capp-navbar__item" (= i here) (str " capp-navbar__item--on"))
                  :on-click (fn []
                              (if (seq sub)
                                (swap! open #(when-not (= % i) i))
                                (do (reset! open nil) (fire! env on-click))))}
                 [item-body it hide-text?]]
                (when (and (seq sub) (= i @open))
                  [overlay/popover {:place :above :on-close #(reset! open nil)}
                   (into [:div.capp-navbar__submenu]
                         (map-indexed
                          (fn [j s]
                            ^{:key j}
                            [:button.capp-navbar__subitem
                             {:type     "button"
                              :on-click (fn [] (reset! open nil) (fire! env (:on-click s)))}
                             [item-body s false]]))
                         sub)])]))
            items)]))})))

(def widgets
  {:tabs          (fn [cfg env] [tabs cfg env])
   :mobile-navbar (fn [cfg env] [mobile-navbar cfg env])})
