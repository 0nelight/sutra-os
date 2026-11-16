(ns sutra-ui.apps.admin-settings.model
  (:require [sutra-ui.apps.admin-settings.events]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]))

(defonce ui (shell/slice :admin-settings))

(defonce admin (machine/frame ui {:name :admin-settings}))

(defn send! [event] (machine/dispatch event {:frame admin}))

(defn draft-at
  [event shown]
  (reify
    IDeref (-deref [_] shown)
    IReset (-reset! [_ v] (send! (conj event v)) v)))

(defonce db (machine/subscribe [:admin/db] {:frame admin}))
