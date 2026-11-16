(ns sutra-ui.apps.developer-apps.model
  (:require [sutra-ui.apps.developer-apps.events]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]))

(defonce ui (shell/slice :developer-apps))

(defonce console (machine/frame ui {:name :developer-apps}))

(defn send! [event] (machine/dispatch event {:frame console}))

(defonce db (machine/subscribe [:dev-apps/db] {:frame console}))
