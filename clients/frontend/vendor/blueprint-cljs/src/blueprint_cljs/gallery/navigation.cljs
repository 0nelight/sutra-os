(ns blueprint-cljs.gallery.navigation
  (:require [reagent.core :as r]
            [blueprint-cljs.icons.all]
            [blueprint-cljs.breadcrumbs :as bc]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.navbar :as nb]
            [blueprint-cljs.tabs :as tabs]
            [blueprint-cljs.overflow-list :as ol]
            [blueprint-cljs.resize-sensor :as rs]
            [blueprint-cljs.tree :as tree]
            [blueprint-cljs.panel-stack :as ps]))

(defn- caption [s] [:div {:style {:font-weight 600 :margin "8px 0 4px"}} s])

(defn- navbar-demo
  []
  [:div {:style {:display "flex" :flex-direction "column" :gap 16 :width "100%"}}
   [nb/navbar
    [nb/navbar-group {:align :start}
     [nb/navbar-heading "Blueprint"]
     [nb/navbar-divider]
     [button/button {:variant :minimal :icon :home :text "Home"}]
     [button/button {:variant :minimal :icon :document :text "Files"}]]
    [nb/navbar-group {:align :end}
     [button/button {:variant :minimal :icon :user}]
     [button/button {:variant :minimal :icon :notifications}]
     [button/button {:variant :minimal :icon :cog}]]]
   [nb/navbar {:class "bp-dark"}
    [nb/navbar-group
     [nb/navbar-heading "Dark navbar"]
     [nb/navbar-divider]
     [button/button {:variant :minimal :icon :home :text "Home"}]]]])

(defn- tabs-demo
  []
  (let [sel (r/atom :ng)]
    (fn []
      [:div {:style {:width "100%"}}
       (caption "Controlled, animated, with tag and expander")
       [tabs/tabs {:id :demo :selected-tab-id @sel :on-change (fn [n _ _] (reset! sel n))}
        [tabs/tab {:id :ng :title "Angular" :panel [:p "Angular panel"]}]
        [tabs/tab {:id :mb :title "Ember" :icon :tree :panel [:p "Ember panel"]}]
        [tabs/tab {:id :rx :title "React" :tag-content 12 :panel [:p "React panel"]}]
        [tabs/tab {:id :bb :title "Backbone" :disabled true :panel [:p "Backbone"]}]
        [tabs/tabs-expander]
        [:input {:class "bp-input" :placeholder "Search…" :style {:align-self "center"}}]]
       (caption "Large, uncontrolled")
       [tabs/tabs {:id :large :large true}
        [tabs/tab {:id :a :title "First" :panel [:p "First panel"]}]
        [tabs/tab {:id :b :title "Second" :panel [:p "Second panel"]}]]
       (caption "Vertical")
       [tabs/tabs {:id :vert :vertical true :default-selected-tab-id :two}
        [tabs/tab {:id :one :title "One" :panel [:p "Panel one"]}]
        [tabs/tab {:id :two :title "Two" :panel [:p "Panel two"]}]
        [tabs/tab {:id :three :title "Three" :panel [:p "Panel three"]}]]])))

(def ^:private crumbs
  ["All files" "Users" "Janet" "Photos" "Wednesday" "image.jpg"])

(defn- overflow-demo
  []
  (let [width (r/atom 60)]
    (fn []
      [:div {:style {:width "100%"}}
       [:label "Width " [:input {:type "range" :min 10 :max 100 :value @width
                                 :on-change #(reset! width (js/parseInt (.. % -target -value)))}]
        (str " " @width "%")]
       (for [from [:start :end]]
         ^{:key from}
         [:div
          (caption (str "Collapse from " (name from)))
          [:div {:style {:width (str @width "%") :border "1px dashed #8f99a8" :padding 4}}
           [ol/overflow-list
            {:items                 crumbs
             :collapse-from         from
             :min-visible-items     1
             :overflow-renderer     (fn [items]
                                      [:span {:class "bp-tag" :style {:margin "0 4px"}}
                                       (str "+" (count items))])
             :visible-item-renderer (fn [item _]
                                      [:span {:style {:margin "0 6px" :white-space "nowrap"}}
                                       item])}]]])])))

(def ^:private crumb-items
  [{:href "#breadcrumbs" :icon :folder-close :text "All files"}
   {:href "#breadcrumbs" :icon :folder-close :text "Users"}
   {:href "#breadcrumbs" :icon :folder-close :text "Janet"}
   {:href "#breadcrumbs" :icon :folder-close :text "Photos"}
   {:href "#breadcrumbs" :icon :folder-close :text "Wednesday"}
   {:icon :document :text "image.jpg"}])

(defn- breadcrumbs-demo
  []
  (let [width (r/atom 45)]
    (fn []
      [:div {:style {:width "100%"}}
       [:label "Width " [:input {:type "range" :min 10 :max 100 :value @width
                                 :on-change #(reset! width (js/parseInt (.. % -target -value)))}]
        (str " " @width "%")]
       (caption "Collapse from start")
       [:div {:style {:width (str @width "%")}}
        [bc/breadcrumbs {:items crumb-items}]]
       (caption "Collapse from end, popover open")
       [:div {:style {:width (str @width "%")}}
        [bc/breadcrumbs {:items         crumb-items
                         :collapse-from :end
                         :popover-props {:default-is-open true}}]]
       (caption "Plain breadcrumb states")
       [:div {:style {:display "flex" :gap 16}}
        [bc/breadcrumb {:text "Link" :href "#breadcrumbs"}]
        [bc/breadcrumb {:text "Disabled" :href "#breadcrumbs" :disabled true}]
        [bc/breadcrumb {:text "Current" :current true}]]])))

(defn- resize-demo
  []
  (let [size (r/atom nil)]
    (fn []
      [rs/resize-sensor
       {:on-resize (fn [entries]
                     (let [rect (.-contentRect (first entries))]
                       (reset! size [(js/Math.round (.-width rect))
                                     (js/Math.round (.-height rect))])))}
       [:textarea {:class "bp-input"
                   :style {:resize "both" :width 240 :height 80}
                   :value (str "Resize me: " (first @size) " × " (second @size))
                   :read-only true}]])))

(def ^:private initial-tree
  [{:id 0 :has-caret true :icon :folder-close :label "Folder 0"}
   {:id 1 :icon :folder-close :is-expanded true :label "Folder 1"
    :child-nodes
    [{:id 2 :icon :document :label "Item 0" :secondary-label [button/button
                                                              {:variant :minimal
                                                               :size :small
                                                               :icon :eye-open}]}
     {:id 3 :icon [:span {:class "bp-icon"} "★"] :label "Organic meditation gluten-free"}
     {:id 4 :has-caret true :icon :folder-close :label "Folder 2"
      :child-nodes [{:id 5 :label "No-Icon Item"}
                    {:id 6 :icon :tag :label "Item 1"}
                    {:id 7 :has-caret true :icon :folder-close :label "Folder 3"
                     :child-nodes [{:id 8 :icon :document :label "Item 0"}
                                   {:id 9 :icon :tag :label "Item 1"}]}]}]}
   {:id 10 :has-caret true :icon :folder-close :label "Super secret files" :disabled true}])

(defn- tree-demo
  []
  (let [contents (r/atom initial-tree)
        set-exp  (fn [v] (fn [_ path _] (swap! contents tree/update-node path
                                               assoc :is-expanded v)))
        click    (fn [node path e]
                   (let [was (:is-selected node)]
                     (when-not (.-shiftKey e)
                       (swap! contents tree/map-nodes #(assoc % :is-selected false)))
                     (swap! contents tree/update-node path assoc :is-selected (not was))))]
    (fn []
      [:div {:style {:display "flex" :gap 24 :width "100%"}}
       [:div {:style {:width 320}}
        (caption "Default")
        [tree/tree {:contents         @contents
                    :on-node-click    click
                    :on-node-expand   (set-exp true)
                    :on-node-collapse (set-exp false)}]]
       [:div {:style {:width 320}}
        (caption "Compact")
        [tree/tree {:contents         @contents
                    :compact          true
                    :on-node-click    click
                    :on-node-expand   (set-exp true)
                    :on-node-collapse (set-exp false)}]]])))

(declare demo-panel)

(defn- demo-panel-view
  [{:keys [depth open-panel close-panel]}]
  [:div {:style {:padding 12 :display "flex" :flex-direction "column" :gap 8}}
   [:p (str "This is panel " depth ".")]
   [button/button {:intent :primary :text "Open new panel"
                   :on-click #(open-panel (demo-panel (inc depth)))}]
   (when (> depth 1)
     [button/button {:text "Close this panel" :on-click close-panel}])])

(defn- demo-panel
  [depth]
  {:title (str "Panel " depth) :props {:depth depth} :render-panel demo-panel-view})

(defn- panel-stack-demo
  []
  [:div {:style {:display "flex" :gap 24}}
   [:div
    (caption "Uncontrolled")
    [ps/panel-stack {:initial-panel (demo-panel 1)
                     :class         "bp-gallery__panel-stack"}]]
   [:div
    (caption "All panels rendered, no header")
    [ps/panel-stack {:initial-panel            (demo-panel 1)
                     :render-active-panel-only false
                     :show-panel-header        false
                     :class                    "bp-gallery__panel-stack"}]]])

(def sections
  "Each: {:id kw :title str :demo (fn [] hiccup)}."
  [{:id :navbar :title "Navbar" :demo navbar-demo}
   {:id :tabs :title "Tabs" :demo tabs-demo}
   {:id :breadcrumbs :title "Breadcrumbs" :demo breadcrumbs-demo}
   {:id :overflow-list :title "Overflow list" :demo overflow-demo}
   {:id :resize-sensor :title "Resize sensor" :demo resize-demo}
   {:id :tree :title "Tree" :demo tree-demo}
   {:id :panel-stack :title "Panel stack" :demo panel-stack-demo}])
