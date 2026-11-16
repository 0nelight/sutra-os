(ns sutra-ui.apps.assistant.feature
  (:require [sutra-ui.feature :as feature]
            [sutra.i18n.core :as i18n]
            [sutra-ui.ui.style :as style]
            [sutra-ui.apps.assistant.core :as core]
            [sutra-ui.apps.assistant.providers :as providers]))

(def ^:private glyph
  (feature/icon
   {:width 1.6 :cap "round" :join "round"}
   [:path {:d "M10 2.5l1.6 4.3 4.4 1.7-4.4 1.7L10 14.5l-1.6-4.3L4 8.5l4.4-1.7z"}]
   [:path {:d "M15.5 13.5l.7 1.8 1.8.7-1.8.7-.7 1.8-.7-1.8-1.8-.7 1.8-.7z"}]))

(feature/register-app!
 {:id :assistant :order 10 :label-key :ui/app-assistant :glyph glyph})

(feature/register-screen!
 {:id :assistant :app :assistant :segment "assistant" :label-key :ui/app-assistant
  :glyph glyph :view core/page})

(feature/contribute!
 :admin-section :providers
 {:label-key :ui/admin-tab-providers :order 1
  :view (fn []
          [:<>
           [:div {:class [style/small-muted "u-mb-12"]} (i18n/t :ui/admin-providers-hint)]
           [providers/view {:on-change (fn [] nil)}]])})
