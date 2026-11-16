(ns sutra-ui.apps.notebook.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.notebook.core :as notebook]))

(def ^:private glyph
  (feature/icon
   {:width 1.6 :cap "round" :join "round"}
   [:path {:d "M5 2h7l3 3v13H5z"}]
   [:polyline {:points "12,2 12,5 15,5"}]
   [:line {:x1 7.5 :y1 9 :x2 12.5 :y2 9}]
   [:line {:x1 7.5 :y1 12 :x2 12.5 :y2 12}]))

(feature/register-app!
 {:id :notebook :order 9 :label-key :ui/app-notebook :glyph glyph})

(feature/register-screen!
 {:id :notebook :app :notebook :segment "notebook" :label-key :ui/app-notebook :glyph glyph
  :view notebook/notebook-app
  :params #{:doc :read-only?}
  :tail (fn [{:keys [doc read-only?]}]
          (cond-> [] doc (conj (routing/enc doc)) (and doc read-only?) (conj "view")))
  :parse-tail (fn [[id mode]] {:doc        (some-> id routing/dec')
                               :read-only? (and (some? id) (= "view" mode))})})

(feature/contribute!
 :resource-opener [:notebook :documents]
 {:family :documents :app :notebook
  :open! (fn [nm _] (shell/enter! :notebook {:doc (str nm) :read-only? false}))})

(feature/contribute!
 :command :open-document
 {:run! (fn [doc]
          (shell/open-in-new-tab! (routing/href :notebook {:doc (str doc) :read-only? true})))})
