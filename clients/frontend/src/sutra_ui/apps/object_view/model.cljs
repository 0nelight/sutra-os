(ns sutra-ui.apps.object-view.model
  (:require [sutra-ui.apps.object-view.events]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]))

(defonce ui (shell/slice :explorer))

(defonce object-view (machine/frame ui {:name :object-view}))

(defn send! [event] (machine/dispatch event {:frame object-view}))

(defn promise!
  [event]
  (js/Promise. (fn [resolve reject] (send! (conj event {:resolve resolve :reject reject})))))

(defn complete-rows-of
  [type-kw]
  (when (and type-kw (not (contains? @panel/complete-instances type-kw)))
    (js/setTimeout #(when-not (contains? @panel/complete-instances type-kw)
                      (panel/load-complete-type! type-kw))
                   0))
  (get-in @panel/complete-instances [type-kw :rows] []))

(defn complete-links-of
  [types]
  (doseq [t types] (complete-rows-of t))
  (into [] (comp (mapcat #(get-in @panel/complete-instances [% :links])) (distinct)) types))

(defn focused-instance-id
  []
  (get-in @ui [:object-view :instance-id]))
