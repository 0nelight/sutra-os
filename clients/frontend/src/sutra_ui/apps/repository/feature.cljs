(ns sutra-ui.apps.repository.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.repository.model :as repo-model]
            [sutra-ui.apps.repository.proposals :as proposals]
            [sutra-ui.apps.repository.core :as repo]))

(def ^:private glyph
  (feature/icon
   {:width 1.6 :cap "round" :join "round"}
   [:polyline {:points "7,6 3,10 7,14"}]
   [:polyline {:points "13,6 17,10 13,14"}]
   [:line {:x1 11 :y1 5 :x2 9 :y2 15}]))

(feature/register-app!
 {:id :repository :order 7 :label-key :ui/app-repository :glyph glyph :branching? true})

(feature/register-screen!
 {:id :repository :app :repository :segment "repository" :label-key :ui/app-repository
  :glyph glyph :view repo/repository-app :params #{:tab :selected}})

(feature/register-screen!
 {:id :proposal-entry :app :repository :segment "proposals" :hidden? true
  :label-key :ui/app-repository :glyph glyph :view repo/repository-app
  :params #{:proposal}
  :parse-entry (fn [[id] _] (when (seq id) {:proposal (js/decodeURIComponent id)}))
  :on-enter (fn [{:keys [proposal]}] (when proposal (proposals/open! proposal)))})

(feature/contribute! :command :open-proposal {:run! proposals/open!})

(feature/contribute!
 :resource-opener [:repository :transforms]
 {:family :transforms :app :repository :order 0 :accepts? repo-model/code-transform?
  :open! (fn [nm _]
           (shell/go! :repository {:tab :code :selected nm}))})
