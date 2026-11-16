(ns blueprint-cljs.card
  "Card and CardList."
  (:require [blueprint-cljs.core :as bp]
            [blueprint-cljs.common :as c]))

(defn card
  "Props: `:elevation` 0–4 (default 0), `:interactive`, `:selected`, `:compact`."
  [& args]
  (let [[{:keys [elevation interactive selected compact class] :or {elevation 0} :as props}
         children] (c/split-args args)]
    (into [:div (merge (bp/dom-props props [:elevation :interactive :selected :compact :class])
                       {:class (bp/cx class (bp/cls "card") (c/elevation-class elevation)
                                      {(bp/cls "interactive") interactive
                                       (bp/cls "compact")     compact
                                       (bp/cls "selected")    selected})})]
          children)))

(defn card-list
  "A list of cards in one card. Props: `:bordered` (default true), `:compact`."
  [& args]
  (let [[{:keys [bordered compact class] :or {bordered true} :as props} children]
        (c/split-args args)]
    (into [card (merge {:role "list"}
                       (bp/dom-props props [:bordered :compact :class])
                       {:elevation 0
                        :class     (bp/cx class (bp/cls "card-list")
                                          {(bp/cls "card-list-bordered") bordered
                                           (bp/cls "compact")            compact})})]
          children)))
