(ns sutra-ui.apps.repository.model
  (:require [sutra.types.schema-reach :as reach]
            [sutra-ui.apps.repository.events]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.shell :as shell]))

(defonce ui (shell/slice :repository))

(defonce repository (machine/frame ui {:name :repository}))

(defn send! [event] (machine/dispatch event {:frame repository}))

(machine/reg-sub :repository/at (fn [db [_ k default]] (get db k default)))

(defn at
  ([k] (at k nil))
  ([k default] @(machine/subscribe [:repository/at k default] {:frame repository})))

(defn handle
  [k default event]
  (reify
    IDeref (-deref [_] (at k default))
    IReset (-reset! [_ v] (send! [event v]) v)))

(defn code-transform?
  [spec]
  (boolean (or (reach/elsewhere? spec :code) (:code spec) (:code-file spec)
               (= :python (some-> (:language spec) keyword)))))
