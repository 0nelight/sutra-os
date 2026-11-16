(ns sutra-ui.apps.admin-settings.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.routing :as routing]
            [sutra-ui.apps.admin-settings.core :as core]))

(def ^:private glyph
  (feature/icon
   {:width 1.6 :cap "round" :join "round"}
   [:line {:x1 3 :y1 6 :x2 17 :y2 6}]
   [:line {:x1 3 :y1 14 :x2 17 :y2 14}]
   [:circle {:cx 8 :cy 6 :r 2}]
   [:circle {:cx 13 :cy 14 :r 2}]))

(feature/register-app!
 {:id :admin-settings :order 12 :label-key :ui/app-admin-settings :glyph glyph})

(feature/register-screen!
 {:id :admin-settings :app :admin-settings :aliases ["feedback"]
  :segment (fn [{:keys [sitzung]}] (if sitzung "feedback" "admin-settings"))
  :label-key :ui/app-admin-settings :glyph glyph :view core/admin-settings
  :params #{:sitzung}
  :tail (fn [{:keys [sitzung]}] (if sitzung [(routing/enc sitzung)] []))
  :parse-tail (fn [[id]] {:sitzung (some-> id routing/dec')})})
