(ns blueprint-cljs.navbar
  "Navbar with its compound parts: `navbar-group`, `navbar-heading`, `navbar-divider`.
   The props map may be left out."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(defn navbar
  "Props: `:fixed-to-top`; the rest goes to the `div`."
  [& args]
  (let [[{:keys [fixed-to-top] :as props} children] (c/split-args args)]
    (into [:div (merge (bp/dom-props props [:fixed-to-top])
                       {:class (bp/merge-class
                                (bp/cx (bp/cls "navbar") {(bp/cls "fixed-top") fixed-to-top})
                                props)})]
          children)))

(defn- align-class
  [align]
  (case align
    (:start :left) (bp/cls "align-left")
    (:end :right)  (bp/cls "align-right")
    nil))

(defn navbar-group
  "Props: `:align` `:start` (default) | `:end` (legacy `:left`/`:right`)."
  [& args]
  (let [[{:keys [align] :or {align :start} :as props} children] (c/split-args args)]
    (into [:div (merge (bp/dom-props props [:align])
                       {:class (bp/merge-class
                                (bp/cx (bp/cls "navbar-group") (align-class align))
                                props)})]
          children)))

(defn navbar-heading
  [& args]
  (let [[props children] (c/split-args args)]
    (into [:div (merge props {:class (bp/merge-class (bp/cls "navbar-heading") props)})]
          children)))

(defn navbar-divider
  [& args]
  (let [[props] (c/split-args args)]
    [:div (merge props {:class (bp/merge-class (bp/cls "navbar-divider") props)})]))
