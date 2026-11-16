(ns sutra-ui.apps.developer-apps.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.apps.developer-apps.core :as core]))

(def ^:private glyph
  (feature/icon
   {}
   [:rect {:x 3 :y 3.5 :width 14 :height 13 :rx 1.5}]
   [:line {:x1 3 :y1 7 :x2 17 :y2 7}]
   [:circle {:cx 8.5 :cy 11.5 :r 2}]
   [:line {:x1 10.5 :y1 11.5 :x2 14.5 :y2 11.5}]
   [:line {:x1 13.5 :y1 11.5 :x2 13.5 :y2 13.5}]))

(feature/register-app!
 {:id :developer-apps :order 8 :label-key :ui/app-developer-apps :glyph glyph})

(feature/register-screen!
 {:id :developer-apps :app :developer-apps :segment "developer-apps"
  :label-key :ui/app-developer-apps :glyph glyph :view core/developer-apps})
