(ns sutra-ui.apps.ui-kit.feature
  (:require [sutra-ui.ui.busy :as busy]
            [reagent.core :as r]
            [sutra.i18n.core :as i18n]
            [sutra-ui.feature :as feature]))

(defonce load-gallery! (atom nil))

(defonce ^:private root (r/atom nil))

(defn- view
  []
  (if-let [g @root]
    [g]
    (do (some-> @load-gallery! (apply [#(reset! root %)]))
        [busy/block (i18n/t :ui/loading)])))

(feature/register-screen!
 {:id :ui-kit :app :admin-settings :hidden? true :segment "ui-kit"
  :label-key :ui/ui-kit :view view})
