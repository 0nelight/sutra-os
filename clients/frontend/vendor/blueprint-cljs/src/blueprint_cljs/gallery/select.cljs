(ns blueprint-cljs.gallery.select
  (:require [reagent.core :as r]
            [blueprint-cljs.button :as button]
            [blueprint-cljs.menu :as menu]
            [blueprint-cljs.input-group :as input-group]
            [blueprint-cljs.query-list :as ql]
            [blueprint-cljs.select :refer [select]]
            [blueprint-cljs.suggest :refer [suggest]]
            [blueprint-cljs.multi-select :refer [multi-select]]
            [blueprint-cljs.omnibar :refer [omnibar]]
            [blueprint-cljs.gallery.films :as films]))

(defn- row [& xs] (into [:div {:style {:display "flex" :gap 16 :align-items "flex-start"
                                       :flex-wrap "wrap"}}] xs))

(defn- col [w & xs] (into [:div {:style {:display "flex" :flex-direction "column"
                                         :gap 8 :width w}}] xs))

(defn- caption [s] [:div {:class "bp-text-muted" :style {:font-size 12}} s])

(def ^:private no-results
  [menu/menu-item {:disabled true :text "No results." :role-structure :listoption}])

(defn- film-select
  [{:keys [open? opts]}]
  (let [sel   (r/atom (first films/top-100-films))
        items (r/atom films/top-100-films)]
    (fn [{:keys [label]}]
      [col 260
       (caption label)
       [select (merge {:items          @items
                       :item-predicate films/filter-film
                       :item-renderer  films/render-film
                       :items-equal    films/films-equal?
                       :no-results     no-results
                       :on-item-select (fn [film _]
                                         (when-not (some #(films/films-equal? % film) @items)
                                           (swap! items conj film))
                                         (reset! sel film))
                       :popover-props  {:minimal true :is-open open?}}
                      opts)
        [button/button {:text     (if @sel (:title @sel) "(No selection)")
                        :end-icon :double-caret-vertical
                        :disabled (:disabled opts)}]]])))

(defn- select-demo
  []
  [row
   [film-select {:label "Filterable (default)"}]
   [film-select {:label "Not filterable" :opts {:filterable false}}]
   [film-select {:label "Create new items"
                 :opts  {:create-new-item-from-query films/create-film
                         :create-new-item-renderer   films/render-create-film}}]
   [film-select {:label "Disabled" :opts {:disabled true}}]
   [:div {:style {:height 380}}
    [film-select {:label "Open by default" :open? true}]]])

(defn- film-suggest
  [_]
  (let [sel (r/atom nil)]
    (fn [{:keys [label opts open?]}]
      [col 280
       (caption label)
       [suggest (merge {:items                films/top-100-films
                        :item-predicate       films/filter-film
                        :item-renderer        films/render-film
                        :items-equal          films/films-equal?
                        :input-value-renderer :title
                        :no-results           no-results
                        :selected-item        @sel
                        :on-item-select       (fn [film _] (reset! sel film))
                        :popover-props        {:minimal true :is-open open?}}
                       opts)]])))

(defn- suggest-demo
  []
  [row
   [film-suggest {:label "Suggest"}]
   [film-suggest {:label "Create new items, close on select off"
                  :opts  {:close-on-select            false
                          :create-new-item-from-query films/create-film
                          :create-new-item-renderer   films/render-create-film}}]
   [film-suggest {:label "Disabled" :opts {:disabled true}}]
   [:div {:style {:height 380}}
    [film-suggest {:label "Open by default" :open? true}]]])

(defn- film-multi-select
  [_]
  (let [selected (r/atom (vec (take 3 films/top-100-films)))]
    (fn [{:keys [label opts open?]}]
      (let [selected? (fn [f] (some #(films/films-equal? % f) @selected))]
        [col 420
         (caption label)
         [multi-select
          (merge {:items              films/top-100-films
                  :item-predicate     films/filter-film
                  :item-renderer      (fn [film props]
                                        (when (get-in props [:modifiers :matches-predicate])
                                          ^{:key (:rank film)}
                                          [menu/menu-item
                                           (assoc (films/film-item-props film props)
                                                  :role-structure :listoption
                                                  :selected (selected? film)
                                                  :should-dismiss-popover false)]))
                  :items-equal        films/films-equal?
                  :no-results         no-results
                  :selected-items     @selected
                  :tag-renderer       :title
                  :on-item-select     (fn [film _]
                                        (if (selected? film)
                                          (swap! selected
                                                 (fn [xs]
                                                   (filterv #(not (films/films-equal? % film))
                                                            xs)))
                                          (swap! selected conj film)))
                  :on-remove          (fn [_ i]
                                        (swap! selected (fn [xs] (into (subvec xs 0 i)
                                                                       (subvec xs (inc i))))))
                  :on-clear           #(reset! selected [])
                  :on-items-paste     (fn [fs] (swap! selected into (remove selected? fs)))
                  :reset-on-select    true
                  :tag-input-props    {:tag-props {:minimal true}}
                  :popover-props      {:minimal true :is-open open?}}
                 opts)]]))))

(defn- multi-select-demo
  []
  [row
   [film-multi-select {:label "MultiSelect"}]
   [film-multi-select {:label "Create new items"
                       :opts  {:create-new-item-from-query films/create-film
                               :create-new-item-renderer   films/render-create-film}}]
   [film-multi-select {:label "Custom target"
                       :opts  {:custom-target (fn [items _]
                                                [button/button
                                                 {:text     (str (count items) " selected")
                                                  :end-icon :caret-down}])}}]
   [:div {:style {:height 380}}
    [film-multi-select {:label "Open by default" :open? true}]]])

(defn- omnibar-demo
  []
  (let [open? (r/atom true)
        last  (r/atom nil)]
    (fn []
      [col 420
       (caption "The omnibar opens over the page; here it starts open. Escape closes it.")
       [button/button {:text "Click to show Omnibar" :icon :search
                       :on-click #(reset! open? true)}]
       [:div "Last selected: " (or (:title @last) "—")]
       [omnibar {:is-open                    @open?
                 :items                      films/top-100-films
                 :item-predicate             films/filter-film
                 :item-renderer              films/render-film
                 :no-results                 no-results
                 :create-new-item-from-query films/create-film
                 :create-new-item-renderer   films/render-create-film
                 :on-item-select             (fn [film _] (reset! last film) (reset! open? false))
                 :on-close                   #(reset! open? false)
                 :reset-on-select            true}]])))

(defn- query-list-demo
  []
  (let [last (r/atom nil)]
    (fn []
      [col 300
       (caption "QueryList is the engine: here its renderer draws an input and the list.")
       [ql/query-list
        {:items          (take 20 films/top-100-films)
         :item-predicate films/filter-film
         :item-renderer  films/render-film
         :no-results     no-results
         :on-item-select (fn [film _] (reset! last film))
         :menu-props     {:style {:max-height 240 :overflow "auto"}}
         :renderer
         (fn [{:keys [handle-key-down handle-key-up handle-query-change item-list query]}]
           [:div {:class "bp-card bp-elevation-1" :style {:padding 8}
                  :on-key-down handle-key-down :on-key-up handle-key-up}
            [input-group/input-group {:left-icon :search :placeholder "Filter films..."
                                      :value query :on-change handle-query-change}]
            [:div {:style {:margin-top 4}} item-list]])}]
       [:div "Selected: " (or (:title @last) "—")]])))

(def sections
  "Each: {:id kw :title str :demo (fn [] hiccup)}."
  [{:id :select       :title "Select"      :demo select-demo}
   {:id :suggest      :title "Suggest"     :demo suggest-demo}
   {:id :multi-select :title "MultiSelect" :demo multi-select-demo}
   {:id :omnibar      :title "Omnibar"     :demo omnibar-demo}
   {:id :query-list   :title "QueryList"   :demo query-list-demo}])
