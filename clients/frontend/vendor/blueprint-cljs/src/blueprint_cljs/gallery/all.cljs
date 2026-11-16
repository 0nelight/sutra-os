(ns blueprint-cljs.gallery.all
  "The gallery: every component in its variants, light and dark."
  (:require [reagent.core :as r]
            [blueprint-cljs.icons.all]
            [reagent.dom.client :as rdc]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.gallery.foundation :as foundation]
            [blueprint-cljs.gallery.basics :as basics]
            [blueprint-cljs.gallery.forms :as forms]
            [blueprint-cljs.gallery.navigation :as navigation]
            [blueprint-cljs.gallery.overlays :as overlays]
            [blueprint-cljs.gallery.select :as select]
            [blueprint-cljs.gallery.datetime :as datetime]
            [blueprint-cljs.gallery.table :as table]))

(def groups
  [["Foundation" foundation/sections]
   ["Basics" basics/sections]
   ["Forms" forms/sections]
   ["Navigation" navigation/sections]
   ["Overlays" overlays/sections]
   ["Select" select/sections]
   ["Datetime" datetime/sections]
   ["Table" table/sections]])

(defonce ^:private dark? (r/atom false))

(defn- hash-id [] (some-> (.-hash js/location) (subs 1) not-empty keyword))

(defonce ^:private current (r/atom (hash-id)))

(defn- section-view
  [{:keys [title demo]}]
  [:section.bp-gallery__section
   [:h3.bp-heading title]
   [:div.bp-gallery__demo [demo]]])

(defn root
  []
  (let [all (mapcat second groups)
        sel (or (some #(when (= (:id %) @current) %) all) (first all))]
    [:div {:class (bp/cx "bp-root bp-gallery" (when @dark? bp/dark-class))}
     [:nav.bp-gallery__nav
      [:label.bp-gallery__dark
       [:input {:type "checkbox" :checked @dark? :on-change #(swap! dark? not)}]
       " Dark"]
      (for [[g sections] groups]
        ^{:key g}
        [:div.bp-gallery__group
         [:div.bp-gallery__group-title g]
         (for [{:keys [id title]} sections]
           ^{:key id}
           [:a {:class    (bp/cx "bp-gallery__link"
                                 {"bp-gallery__link--active" (= id (:id sel))})
                :href     (str "#" (name id))
                :on-click #(reset! current id)}
            title])])]
     [:main.bp-gallery__main
      (when sel [section-view sel])]]))

(defn ^:export init
  []
  (when-let [d (some-> (.-search js/location) (js/URLSearchParams.) (.get "dark"))]
    (reset! dark? (= d "1")))
  (rdc/render (rdc/create-root (js/document.getElementById "app")) [root]))
