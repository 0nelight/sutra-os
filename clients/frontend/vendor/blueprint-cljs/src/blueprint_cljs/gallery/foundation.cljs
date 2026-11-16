(ns blueprint-cljs.gallery.foundation
  "Gallery sections for the foundation: typography, colours, elevation, icons, HTML table."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [blueprint-cljs.core :as bp]
            [blueprint-cljs.html :as html]
            [blueprint-cljs.icon :as icon]
            [blueprint-cljs.icons.all :as icons]))

(defn- typography []
  [:div {:style {:max-width 640}}
   [html/h1 "H1 heading"]
   [html/h2 "H2 heading"]
   [html/h3 "H3 heading"]
   [html/h4 "H4 heading"]
   [html/h5 "H5 heading"]
   [html/h6 "H6 heading"]
   [html/h4 {:class (bp/cls "text-muted")} "Muted heading"]
   [:p.bp-ui-text "UI text: the default font styles. "
    [:span.bp-text-muted "Muted text. "] [:span.bp-text-disabled "Disabled text. "]
    [:span.bp-text-large "Large. "] [:span.bp-text-small "Small. "]
    [:a {:href "#typography"} "A link"] "."]
   [:p.bp-monospace-text "Monospace text."]
   [:div.bp-text-overflow-ellipsis {:style {:width 240}}
    "Truncated with an ellipsis when it overflows its container."]
   [:p "Inline " [html/code "code"] " and keys " [:kbd.bp-key "Ctrl"] " " [:kbd.bp-key "K"]]
   [html/pre "(defn hello [name]\n  (str \"Hello, \" name))"]
   [html/blockquote "Blueprint is a React-based UI toolkit for the web, ported here to "
    "ClojureScript and Reagent."]
   [html/ol [:li "Item the first"] [:li "Item the second"]
    [:li "Item the third" [html/ul [:li "Item the fourth"] [:li "Item the fifth"]]]]
   [:ul.bp-list-unstyled [:li "Unstyled list item"] [:li "Another one"]]
   [:div.bp-running-text
    [:h3 "Running text"]
    [:p "Longform text with a larger line height. It styles nested "
     [:code "code"] ", " [:strong "strong"] " text and lists:"]
    [:ul [:li "First"] [:li "Second"]]
    [:hr]
    [:p "After a rule."]]
   [:p.bp-rtl "لكل لأداء بمحاولة من. مدينة الواقعة يبق أي"]])

(def ^:private scales
  ["dark-gray" "gray" "light-gray" "blue" "green" "orange" "red" "vermilion" "rose"
   "violet" "indigo" "cerulean" "turquoise" "forest" "lime" "gold" "sepia"])

(defn- swatch [v]
  [:div {:style {:width 88}}
   [:div {:style {:height 40 :border-radius 3 :background (str "var(--" v ")")
                  :box-shadow "inset 0 0 0 1px rgba(17,20,24,.1)"}}]
   [:div.bp-text-small.bp-monospace-text v]])

(defn- colors []
  [:div
   [:div {:style {:display "flex" :gap 8 :margin-bottom 8}}
    [swatch "black"] [swatch "white"]]
   (for [s scales]
     ^{:key s}
     [:div {:style {:display "flex" :gap 8 :margin-bottom 8}}
      (for [i (range 1 6)] ^{:key i} [swatch (str s i)])])
   [html/h5 "Intents"]
   [:div {:style {:display "flex" :gap 8}}
    (for [i ["primary" "success" "warning" "danger"]]
      ^{:key i} [swatch (str "pt-intent-" i)])]])

(defn- elevation []
  [:div {:style {:display "flex" :gap 24 :flex-wrap "wrap"}}
   (for [i (range 5)]
     ^{:key i}
     [:div {:class "elevation-demo"
            :style {:width 140 :height 90 :border-radius 4 :display "flex"
                    :align-items "center" :justify-content "center"
                    :background "var(--bp-surface-background-color-default-rest)"
                    :box-shadow (str "var(--bp-surface-shadow-" i ")")}}
      (str "Elevation " i)])])

(defonce ^:private query (r/atom ""))
(defonce ^:private big? (r/atom false))

(defn- icons-browser []
  (let [q    (str/lower-case @query)
        size (if @big? 20 16)
        hits (filter #(str/includes? % q) icons/names)]
    [:div {:style {:width "100%"}}
     [:div {:style {:display "flex" :gap 16 :align-items "center" :margin-bottom 12}}
      [:input {:type "search" :placeholder "Search icons…" :value @query
               :style {:height 30 :padding "0 8px" :width 260}
               :on-change #(reset! query (.. % -target -value))}]
      [:label [:input {:type "checkbox" :checked @big? :on-change #(swap! big? not)}]
       " 20px"]
      [:span.bp-text-muted (str (count hits) " of " (count icons/names))]]
     [:div {:style {:display "grid" :gap 4
                    :grid-template-columns "repeat(auto-fill, minmax(120px, 1fr))"}}
      (for [n hits]
        ^{:key n}
        [:div {:style {:display "flex" :flex-direction "column" :align-items "center"
                       :gap 6 :padding "10px 4px" :text-align "center"}
               :title n}
         [icon/icon {:icon n :size size}]
         [:span.bp-text-small.bp-text-muted {:style {:word-break "break-all"}} n]])]]))

(defn- icon-variants []
  [:div {:style {:display "flex" :gap 16 :align-items "center"}}
   [icon/icon {:icon :globe}]
   [icon/icon {:icon :globe :size 20}]
   [icon/icon {:icon :globe :size 32}]
   [icon/icon {:icon :tick-circle :intent :success :size 20}]
   [icon/icon {:icon :warning-sign :intent :warning :size 20}]
   [icon/icon {:icon :error :intent :danger :size 20}]
   [icon/icon {:icon :info-sign :intent :primary :size 20}]
   [icon/icon {:icon :heart :color "var(--rose3)" :size 20 :title "Liked"}]])

(defn- table []
  (let [rows [["Blueprint" "CSS framework and UI toolkit" "Sass, TypeScript, React"]
              ["TSLint" "Static analysis linter for TypeScript" "TypeScript"]
              ["Plottable" "Composable charting library built on top of D3" "SVG, D3"]]
        t    (fn [props]
               [html/html-table props
                [:thead [:tr [:th "Project"] [:th "Description"] [:th "Technologies"]]]
                [:tbody (for [[a b c] rows] ^{:key a} [:tr [:td a] [:td b] [:td c]])]])]
    [:div {:style {:display "flex" :flex-direction "column" :gap 24}}
     [t {}]
     [t {:bordered true :striped true}]
     [t {:compact true :interactive true}]]))

(def sections
  [{:id :typography :title "Typography" :demo typography}
   {:id :colors :title "Colors" :demo colors}
   {:id :elevation :title "Elevation" :demo elevation}
   {:id :icon :title "Icon" :demo icon-variants}
   {:id :icons :title "Icons (all)" :demo icons-browser}
   {:id :html-table :title "HTML table" :demo table}])
