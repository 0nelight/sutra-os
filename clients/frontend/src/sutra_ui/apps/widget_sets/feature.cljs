(ns sutra-ui.apps.widget-sets.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.widget-sets.core :as core]))

(def ^:private glyph
  (feature/icon
   {}
   [:rect {:x 3 :y 3 :width 6 :height 6 :rx 1}]
   [:rect {:x 11 :y 3 :width 6 :height 6 :rx 1}]
   [:rect {:x 3 :y 11 :width 6 :height 6 :rx 1}]
   [:line {:x1 14 :y1 11 :x2 14 :y2 17}]
   [:line {:x1 11 :y1 14 :x2 17 :y2 14}]))

(feature/register-app!
 {:id :widget-sets :order 9 :label-key :ui/app-widget-sets :glyph glyph})

(feature/register-screen!
 {:id :widget-sets :app :widget-sets :segment "widget-sets"
  :label-key :ui/app-widget-sets :glyph glyph :view core/widget-sets})

(feature/contribute!
 :resource-opener [:widget-sets :widget-sets]
 {:family :widget-sets :app :widget-sets
  :open!  (fn [nm _spec]
            (core/select! (str nm))
            (shell/enter! :widget-sets))})
