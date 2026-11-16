(ns sutra-ui.apps.enta-editor.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.apps.enta-editor.core :as app-builder]))

(def ^:private glyph
  (feature/icon
   {:stroked? false}
   [:rect {:x 3 :y 4 :width 14 :height 12 :rx 1.5 :fill "none" :stroke "currentColor"
           :stroke-width 1.4}]
   [:line {:x1 3 :y1 7.5 :x2 17 :y2 7.5 :stroke "currentColor" :stroke-width 1.4}]
   [:rect {:x 5 :y 9.5 :width 4 :height 4.5 :fill "currentColor"}]
   [:line {:x1 11 :y1 10.5 :x2 15 :y2 10.5 :stroke "currentColor" :stroke-width 1.2}]
   [:line {:x1 11 :y1 13 :x2 15 :y2 13 :stroke "currentColor" :stroke-width 1.2}]))

(feature/register-app!
 {:id :app-builder :order 4 :label-key :ui/app-app-builder :glyph glyph :branching? true})

(feature/register-screen!
 {:id :app-builder :app :app-builder :segment "app-builder" :label-key :ui/app-app-builder
  :glyph glyph :view app-builder/app-builder})
