(ns sutra-ui.apps.branching.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.apps.branching.core :as branching]))

(def ^:private glyph
  (feature/icon
   {:width 1.6 :cap "round"}
   [:circle {:cx 6 :cy 4 :r 2}]
   [:circle {:cx 6 :cy 16 :r 2}]
   [:circle {:cx 14 :cy 7 :r 2}]
   [:path {:d "M6 6v8M14 9c0 3-8 2-8 5"}]))

(feature/register-app!
 {:id :branching :order 6 :label-key :ui/app-branching :glyph glyph})

(feature/register-screen!
 {:id :branching :app :branching :segment "branching" :label-key :ui/app-branching
  :glyph glyph :view branching/root
  :on-enter (fn [_] (branching/reload!))})
