(ns sutra-ui.apps.repository.ui
  (:require [sutra-ui.ui.core :as ui]))

(defn action-btn
  [{:keys [label primary? on-click disabled? title]}]
  [ui/tooltip {:content title}
   [ui/button {:small true :intent (when primary? :primary) :on-click on-click
               :disabled disabled?}
    label]])

(defn empty-state [icon title hint]
  [ui/empty-state {:icon icon :title title :hint hint}])
