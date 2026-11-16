(ns sutra-ui.object.open-in
  (:require [sutra-ui.ui.core :as ui]
            [sutra.i18n.core :as i18n]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.shell :as shell]))

(defn menu-items
  [value]
  (for [{:keys [label label-key href] v :value} (feature/open-in-targets value)]
    ^{:key (str (or label label-key))}
    [ui/menu-item {:text     (str (or label (i18n/t label-key)) " ↗")
                   :on-click #(shell/open-in-new-tab! (href v))}]))

(defn menu
  [value]
  (when-let [items (seq (menu-items value))]
    [ui/popover
     {:placement :bottom-end
      :class "explorer-action__menu"
      :content   (into [ui/menu] items)}
     [ui/tooltip {:content (i18n/t :ui/open-in-menu-hint)}
      [ui/button {:small true}
       (str (i18n/t :ui/open-in-menu) " ▾")]]]))
