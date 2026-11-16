(ns sutra-ui.builder.model
  (:require [reagent.core :as r]
            [sutra-ui.builder.events]
            [sutra-ui.state.machine :as machine]))

(defonce ui (r/atom {}))

(defonce builder (machine/frame ui {:name :builder}))

(defn send! [event] (machine/dispatch event {:frame builder}))
