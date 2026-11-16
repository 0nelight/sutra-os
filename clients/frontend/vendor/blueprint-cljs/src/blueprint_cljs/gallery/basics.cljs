(ns blueprint-cljs.gallery.basics
  (:require [reagent.core :as r]
            [blueprint-cljs.button :as b]
            [blueprint-cljs.callout :refer [callout]]
            [blueprint-cljs.card :refer [card card-list]]
            [blueprint-cljs.collapse :refer [collapse]]
            [blueprint-cljs.divider :refer [divider]]
            [blueprint-cljs.entity-title :refer [entity-title]]
            [blueprint-cljs.link :refer [link]]
            [blueprint-cljs.non-ideal-state :refer [non-ideal-state]]
            [blueprint-cljs.progress-bar :refer [progress-bar]]
            [blueprint-cljs.section :refer [section section-card]]
            [blueprint-cljs.skeleton :as skeleton]
            [blueprint-cljs.spinner :as spinner]
            [blueprint-cljs.tag :refer [tag compound-tag]]
            [blueprint-cljs.text :as text]))

(def ^:private intents [:none :primary :success :warning :danger])

(defn- row [& children]
  (into [:div {:style {:display "flex" :gap 8 :align-items "center" :flex-wrap "wrap"}}]
        children))

(defn- label [s] [:div {:style {:font-size 12 :opacity 0.7 :margin-top 4}} s])

(defn- button-demo []
  [:div
   (for [v [:solid :minimal :outlined]]
     ^{:key v}
     [:div
      [label (str "variant " (name v))]
      (into [row]
            (for [i intents]
              [b/button {:intent i :variant v :icon :refresh} (name i)]))])
   [label "sizes"]
   [row
    [b/button {:size :small :icon :add} "Small"]
    [b/button {:icon :add} "Medium"]
    [b/button {:size :large :icon :add} "Large"]
    [b/button {:icon :cog :aria-label "Settings"}]
    [b/button {:right-icon :caret-down} "Right icon"]]
   [label "states"]
   [row
    [b/button {:disabled true} "Disabled"]
    [b/button {:intent :primary :disabled true} "Disabled primary"]
    [b/button {:active true} "Active"]
    [b/button {:loading true :intent :primary} "Loading"]
    [b/anchor-button {:href "#button" :icon :link} "Anchor button"]]
   [label "fill + align-text"]
   [:div {:style {:width 300}}
    [b/button {:fill true :align-text :left :icon :document :right-icon :caret-down}
     "Left-aligned fill"]]
   [label "ellipsize-text"]
   [:div {:style {:width 160}}
    [b/button {:ellipsize-text true :fill true}
     "A very long label that will not fit"]]])

(defn- button-group-demo []
  [:div
   [label "default"]
   [b/button-group
    [b/button {:icon :database} "Queries"]
    [b/button {:icon :function} "Functions"]
    [b/button {:right-icon :caret-down} "Options"]]
   [label "minimal"]
   [b/button-group {:variant :minimal}
    [b/button {:icon :database} "Queries"]
    [b/button {:icon :function :active true} "Functions"]
    [b/button {:icon :cog}]]
   [label "outlined, large"]
   [b/button-group {:variant :outlined :size :large}
    [b/button "One"] [b/button "Two"] [b/button "Three"]]
   [label "vertical, align left"]
   [b/button-group {:vertical true :align-text :left}
    [b/button {:icon :document} "Document"]
    [b/button {:icon :folder-close} "Folder with a long name"]]
   [label "fill"]
   [:div {:style {:width 400}}
    [b/button-group {:fill true}
     [b/button {:intent :primary} "A"] [b/button "B"] [b/button "C"]]]])

(defn- tag-demo []
  (let [tags (r/atom ["London" "New York" "Paris" "Tokyo"])]
    (fn []
      [:div
       [label "intents"]
       (into [row] (for [i intents] [tag {:intent i} (name i)]))
       [label "minimal"]
       (into [row] (for [i intents] [tag {:intent i :minimal true} (name i)]))
       [label "round, large, icons, interactive"]
       [row
        [tag {:round true} "round"]
        [tag {:size :large :icon :user :intent :primary} "large"]
        [tag {:icon :tick :right-icon :caret-down :intent :success} "icons"]
        [tag {:interactive true} "interactive"]
        [tag {:on-click #(js/console.log "clicked") :minimal true :intent :primary}
         "on-click"]]
       [label "removable"]
       (into [row]
             (for [t @tags]
               [tag {:on-remove (fn [_ _] (swap! tags #(vec (remove #{t} %))))} t]))
       [label "multiline vs ellipsized (max-width 120)"]
       [row
        [tag {:style {:max-width 120}} "A long tag label that is cut off"]
        [tag {:style {:max-width 120} :multiline true} "A long tag label that wraps"]]
       [label "fill"]
       [:div {:style {:width 300}} [tag {:fill true :intent :primary} "fill"]]])))

(defn- compound-tag-demo []
  [:div
   (into [row]
         (for [i intents]
           [compound-tag {:intent i :left-content "Key"} (name i)]))
   [label "minimal, round, large, icons, removable"]
   [row
    [compound-tag {:minimal true :intent :primary :left-content "Owner"} "Ana"]
    [compound-tag {:round true :left-content "Env"} "prod"]
    [compound-tag {:size :large :icon :person :left-content "User" :on-remove (fn [_ _])}
     "admin"]]])

(defn- card-demo []
  [:div {:style {:display "flex" :flex-direction "column" :gap 12 :width 420}}
   (for [e (range 5)]
     ^{:key e}
     [card {:elevation e} [:h5.bp-heading (str "Elevation " e)] [:p "Card content"]])
   [card {:interactive true} [:h5.bp-heading "Interactive"] [:p "Hover me"]]
   [card {:interactive true :selected true} [:h5.bp-heading "Selected"]]
   [card {:compact true} "Compact"]])

(defn- card-list-demo []
  [:div {:style {:display "flex" :gap 16 :flex-wrap "wrap"}}
   [:div {:style {:width 260}}
    [label "bordered"]
    [card-list
     [card {:interactive true} "Apples"]
     [card {:interactive true :selected true} "Bananas"]
     [card {:interactive true} "Cherries"]]]
   [:div {:style {:width 260}}
    [label "compact, not bordered"]
    [card-list {:compact true :bordered false}
     [card "Alpha"] [card "Beta"] [card "Gamma"]]]])

(defn- callout-demo []
  [:div {:style {:display "flex" :flex-direction "column" :gap 12 :width 520}}
   (for [i intents]
     ^{:key i}
     [callout {:intent i :title (str "Callout " (name i))}
      "Long-form information that adds context."])
   [callout {:icon :lightbulb :title "Custom icon"} "With an explicit icon."]
   [callout {:intent :primary :compact true} "Compact, no title."]
   [callout {:intent :warning :minimal true :title "Minimal"} "No background."]
   [callout {:intent :danger :icon false} "Intent without icon."]])

(defn- divider-demo []
  [:div
   [row [b/button {:variant :minimal} "File"] [divider]
    [b/button {:variant :minimal} "Edit"] [divider]
    [b/button {:variant :minimal} "View"]]
   [label "horizontal"]
   [:div {:style {:width 300}} "Above" [divider] "Below" [divider {:compact true}] "Compact"]])

(defn- spinner-demo []
  [:div
   [label "sizes"]
   [row [spinner/spinner {:size spinner/size-small}]
    [spinner/spinner]
    [spinner/spinner {:size spinner/size-large}]]
   [label "intents with value"]
   (into [row] (for [[i v] (map vector intents [0.1 0.3 0.5 0.7 0.9])]
                 [spinner/spinner {:intent i :value v :size 40}]))])

(defn- progress-bar-demo []
  [:div {:style {:display "flex" :flex-direction "column" :gap 12 :width 400}}
   [progress-bar]
   (for [[i v] (map vector intents [0.2 0.4 0.6 0.8 1])]
     ^{:key i} [progress-bar {:intent i :value v}])
   [progress-bar {:intent :primary :value 0.5 :stripes false}]
   [progress-bar {:intent :success :animate false}]])

(defn- skeleton-demo []
  (let [loading? (r/atom true)]
    (fn []
      [:div {:style {:width 420}}
       [b/button {:on-click #(swap! loading? not) :style {:margin-bottom 12}}
        (if @loading? "Show content" "Show skeleton")]
       [card
        [:h5 {:class (skeleton/skeleton @loading? "bp-heading")} "Card heading"]
        [:p {:class (skeleton/skeleton @loading?)}
         "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque eget tortor felis."]
        [b/button {:class (skeleton/skeleton @loading?) :icon :add} "Submit"]]])))

(defn- non-ideal-state-demo []
  [:div {:style {:display "flex" :flex-direction "column" :gap 24 :width 520}}
   [non-ideal-state {:icon        :search
                     :title       "No search results"
                     :description "Your search didn't match any files. Try something else."
                     :action      [b/button {:outlined true :icon :plus :text "New file"}]}]
   [non-ideal-state {:icon :error :icon-size 32 :layout :horizontal
                     :title "Horizontal" :description "Small icon, horizontal layout."}]])

(defn- collapse-demo []
  (let [open? (r/atom false)]
    (fn []
      [:div {:style {:width 420}}
       [b/button {:on-click #(swap! open? not)} (if @open? "Hide" "Show") " build logs"]
       [collapse {:is-open @open?}
        [:pre {:style {:margin-top 8}}
         "[11:53:30] Finished 'typescript-bundle-blueprint' after 769 ms\n"
         "[11:53:30] Starting 'typescript-typings-blueprint'...\n"
         "[11:53:31] Finished 'typescript-typings-blueprint' after 1.2 s"]]
       [:p "Content below the collapse."]])))

(defn- section-demo []
  [:div {:style {:display "flex" :flex-direction "column" :gap 16 :width 520}}
   [section {:title "Section" :subtitle "With subtitle" :icon :document
             :right-element [b/button {:variant :minimal :icon :edit :aria-label "edit"}]}
    [section-card "First card"]
    [section-card "Second card"]]
   [section {:title "Collapsible" :collapsible true :elevation 1}
    [section-card "Click the header to collapse."]]
   [section {:title "Starts collapsed" :collapsible true
             :collapse-props {:default-is-open false}}
    [section-card "Hidden at first."]]
   [section {:title "Compact" :compact true}
    [section-card {:padded false} "Unpadded card"]]])

(defn- entity-title-demo []
  [:div {:style {:display "flex" :flex-direction "column" :gap 16 :width 420}}
   [entity-title {:icon :folder-close :title "Folder"}]
   [entity-title {:icon :person :title "Jane Doe" :subtitle "Engineering"
                  :tags [tag {:minimal true :intent :success} "active"]}]
   [entity-title {:icon :document :title "Linked title" :title-url "#entity-title"}]
   (for [h [:h1 :h3 :h5]]
     ^{:key h}
     [entity-title {:heading h :icon :cube :title (str "Heading " (name h))}])
   [:div {:style {:width 180}}
    [entity-title {:ellipsize true :icon :document
                   :title "A very long entity title that is cut off"}]]
   [entity-title {:loading true :icon :document :title "Loading" :subtitle "subtitle"}]])

(defn- link-demo []
  [:div
   [row [link {:href "#link"} "Default link"]
    [link {:href "#link" :underline :hover} "Underline on hover"]
    [link {:href "#link" :underline :none} "No underline"]]
   [label "colors"]
   (into [row] (for [i [:primary :success :warning :danger]]
                 [link {:href "#link" :color i} (name i)]))
   [:p {:style {:color "teal"}} "Inherits: " [link {:href "#link" :color :inherit} "teal link"]]])

(defn- text-demo []
  [:div {:style {:width 260}}
   [label "ellipsize (hover shows the full text as title)"]
   [text/text {:ellipsize true}
    "This is a long text that overflows its container and gets an ellipsis."]
   [label "plain"]
   [text/text "This is a long text that wraps normally across lines inside the box."]])

(def sections
  "Each: {:id kw :title str :demo (fn [] hiccup)}."
  [{:id :button :title "Button" :demo button-demo}
   {:id :button-group :title "ButtonGroup" :demo button-group-demo}
   {:id :tag :title "Tag" :demo tag-demo}
   {:id :compound-tag :title "CompoundTag" :demo compound-tag-demo}
   {:id :card :title "Card" :demo card-demo}
   {:id :card-list :title "CardList" :demo card-list-demo}
   {:id :callout :title "Callout" :demo callout-demo}
   {:id :divider :title "Divider" :demo divider-demo}
   {:id :spinner :title "Spinner" :demo spinner-demo}
   {:id :progress-bar :title "ProgressBar" :demo progress-bar-demo}
   {:id :skeleton :title "Skeleton" :demo skeleton-demo}
   {:id :non-ideal-state :title "NonIdealState" :demo non-ideal-state-demo}
   {:id :collapse :title "Collapse" :demo collapse-demo}
   {:id :section :title "Section" :demo section-demo}
   {:id :entity-title :title "EntityTitle" :demo entity-title-demo}
   {:id :link :title "Link" :demo link-demo}
   {:id :text :title "Text" :demo text-demo}])
