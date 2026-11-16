(ns sutra-ui.apps.doc-intel.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.apps.doc-intel.core :as core]))

(def ^:private glyph
  (feature/icon
   {}
   [:path {:d "M5 2.5h7l3 3v12H5z"}]
   [:line {:x1 7.5 :y1 9 :x2 12.5 :y2 9}]
   [:line {:x1 7.5 :y1 11.5 :x2 12.5 :y2 11.5}]
   [:rect {:x 7 :y 13.5 :width 6 :height 2 :rx 0.5}]))

(feature/register-app!
 {:id :document-intelligence :order 9.5 :label-key :ui/app-document-intelligence
  :glyph glyph})

(feature/register-screen!
 {:id :document-intelligence :app :document-intelligence :segment "document-intelligence"
  :label-key :ui/app-document-intelligence :glyph glyph :view core/app})
