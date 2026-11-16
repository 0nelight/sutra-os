(ns sutra-ui.apps.explorer.model
  (:require [sutra.i18n.core :as i18n]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.shell :as shell]))

(defonce ui (shell/slice :explorer))

(defonce explorer (machine/frame ui {:name :explorer}))

(defn send! [event] (machine/dispatch event {:frame explorer}))

(defn page-info-of
  [type-kw]
  (select-keys (get @panel/panel-instances type-kw) [:total :next-offset :loading? :limit :offset]))

(defn show-set-on-graph!
  [ids]
  (shell/enter! :instance-canvas)
  (instances/set-instance-filter! ids)
  (instances/focus-nodes! (map str ids)))

(defn saved-label [entry]
  (or (not-empty (:name entry))
      (if (= "list" (some-> (:kind entry) name))
        (i18n/t :ui/ox-saved-list)
        (i18n/t :ui/ox-saved-exploration))))

(defn ensure-explorer-rows!
  [object-type]
  (when (and object-type (nil? (get @panel/panel-instances object-type)))
    (send! [:explorer.set/load object-type])))
