(ns sutra-ui.apps.atlas.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.apps.atlas.breadcrumb :as breadcrumb]
            [sutra-ui.apps.atlas.model :as kompass-model]
            [sutra-ui.apps.atlas.workspace :as workspace]))

(def ^:private glyph
  (feature/icon
   {}
   [:circle {:cx 10 :cy 10 :r 7}]
   [:polygon {:points "10,5 11.6,10 10,15 8.4,10" :fill "currentColor" :stroke "none"}]))

(feature/register-app!
 {:id :home :order 0 :label-key :ui/app-home :glyph glyph
  :mobile-tab :ui/mobile-tab-home})

(feature/register-screen!
 {:id :kompass :app :home :segment "kompass" :label-key :ui/nav-kompass :glyph glyph
  :view workspace/root
  :params #{:project :folder :area :dashboard-tab :portfolio}
  :tail (fn [{:keys [project folder]}]
          (cond-> []
            project (conj (routing/enc project))
            (and project folder) (conj (routing/enc folder))))
  :parse-tail (fn [[p folder]]
                {:project (some-> p routing/dec') :folder (some-> folder routing/dec')})
  :on-enter (fn [_]
              (when-let [p (:project @kompass-model/ui)]
                (ks/select-project! (keyword p)))
              (ks/refresh-kompass-index!)
              (ks/refresh!)
              (kompass-model/redeem-share-link-of-address!)
              (kompass-model/refresh-access-requests!)
              (ks/refresh-kompass-nodes!)
              (kompass-model/refresh-kompass-annotations!)
              (gov/refresh-protections!)
              (catalog/refresh-views!))})

(feature/contribute!
 :command :open-project
 {:run! (fn [pk]
          (shell/go! :kompass {:area :files :project (some-> pk name) :folder nil}))})

(feature/contribute!
 :resource-opener [:home :folders]
 {:family :folders :app :home
  :open! (fn [nm _]
           (let [node (some #(when (= (str nm) (:rid %)) %) (ks/kompass-node-list))]
             (shell/enter! :kompass {:area :files :folder (str nm)
                                     :project (some-> (:project node) catalog/project-of name)})))})

(feature/contribute!
 :resource-opener [:home :files]
 {:family :files :app :home
  :open! (fn [nm _] (js/window.open (str "/api/kompass/files/" nm)))})

(feature/contribute!
 :resource-opener [:home :graphs]
 {:family :graphs
  :app (fn [spec] (feature/app-of (feature/mode->nav (catalog/graph-mode spec))))
  :open! (fn [nm spec] (graph/load-view! (str (or (:id spec) nm))))})

(feature/contribute!
 :resource-path :kompass
 {:view breadcrumb/breadcrumb})
