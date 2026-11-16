(ns sutra-ui.apps.source-manager.model
  (:require [sutra-ui.apps.source-manager.events]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]))

(defonce ui (shell/slice :source-manager))

(defonce sources (machine/frame ui {:name :source-manager}))

(defn send! [event] (machine/dispatch event {:frame sources}))
