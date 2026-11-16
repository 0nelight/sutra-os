(ns sutra-ui.apps.operations.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.apps.operations.core :as ops]))

(def ^:private glyph
  (feature/icon
   {:width 1.6 :cap "round" :join "round"}
   [:polyline {:points "2,11 6,11 8,5 12,15 14,11 18,11"}]))

(feature/register-app!
 {:id :operations :order 11 :label-key :ui/app-operations :glyph glyph :branching? true})

(feature/register-screen!
 {:id :operations :app :operations :segment "operations" :label-key :ui/app-operations
  :glyph glyph :view ops/operations-app})

(feature/register-screen!
 {:id :automation-entry :app :operations :segment "automations" :hidden? true
  :label-key :ui/app-operations :glyph glyph :view ops/operations-app
  :params #{:automation}
  :parse-entry (fn [[id] _] (when (seq id) {:automation (js/decodeURIComponent id)}))
  :on-enter (fn [{:keys [automation]}] (when automation (ops/focus-automation! automation)))})
