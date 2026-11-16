(ns sutra-ui.apps.explorer.feature
  (:require [sutra.i18n.core :as i18n]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.explorer.address :as address]
            [sutra-ui.apps.explorer.events :as ev]
            [sutra-ui.apps.explorer.graph :as igraph]
            [sutra-ui.apps.explorer.object-set :as oset]
            [sutra-ui.object.card :as oc]
            [sutra-ui.apps.explorer.model :as explorer-model]
            [sutra-ui.apps.explorer.core :as explorer]
            [sutra-ui.apps.explorer.instance-canvas :as inst-canvas]))

(def ^:private glyph
  (feature/icon
   {}
   [:rect {:x 3 :y 3  :width 14 :height 4 :rx 1}]
   [:rect {:x 3 :y 9  :width 14 :height 4 :rx 1}]
   [:rect {:x 3 :y 15 :width 14 :height 2 :rx 1}]))

(def ^:private canvas-glyph
  (feature/icon
   {:stroked? false}
   [:line {:x1 5 :y1 5 :x2 14 :y2 5 :stroke "currentColor" :stroke-width 1.4}]
   [:line {:x1 5 :y1 5 :x2 9 :y2 14 :stroke "currentColor" :stroke-width 1.4}]
   [:line {:x1 14 :y1 5 :x2 9 :y2 14 :stroke "currentColor" :stroke-width 1.4}]
   [:circle {:cx 5 :cy 5 :r 2.4 :fill "currentColor"}]
   [:circle {:cx 14 :cy 5 :r 2.4 :fill "currentColor"}]
   [:circle {:cx 9 :cy 15 :r 2.4 :fill "currentColor"}]))

(feature/register-app!
 {:id :explorer :order 3 :label-key :ui/app-explorer :glyph glyph
  :branching? true
  :mobile-tab :ui/mobile-tab-objects})

(feature/register-screen!
 {:id :object-workspace :app :explorer :order 0 :segment "object-workspace"
  :aliases ["entities"] :label-key :ui/nav-object-workspace :glyph glyph
  :view explorer/root
  :params #{:object-type :perspective :object-set :keyword :search}
  :enter address/enter
  :tail address/tail
  :parse-tail address/parse-tail
  :parse-entry address/parse-entry
  :on-enter (fn [{t :object-type set-rid :object-set :keys [search]}]
              (instances/refresh-instance-state!)
              (ks/refresh! :explorations)
              (ks/refresh-kompass-index!)
              (ks/refresh-kompass-nodes!)
              (cond set-rid (explorer/open-saved-set! {:rid set-rid})
                    search  (oset/load-set! t)
                    t       (explorer-model/ensure-explorer-rows! t)))
  :output (fn [{t :object-type :as slice}]
            (when t (oset/set-output t (or (get-in slice [:set t]) address/blank-set))))
  :subject (fn [{t :object-type}]
             (some->> t catalog/name-of name (str "Objektliste ")))
  :page-title (fn [{t :object-type}]
                {:label    (when t (oc/type-label (oc/type-meta t) true))
                 :title    (i18n/t :ui/ox-object-types)
                 :on-click #(explorer-model/send! [:explorer/drawer nil])})})

(feature/register-screen!
 {:id :instance-canvas :app :explorer :order 1 :segment "instance-canvas"
  :label-key :ui/nav-instance-canvas :glyph canvas-glyph
  :canvas {:mode :instances :positions :instances :capabilities #{:create-nodes :connect}
           :graph (fn [{:keys [schema instances instance-positions ctrl-held?]}]
                    [(igraph/instances->rf-nodes instances instance-positions ctrl-held?)
                     (igraph/instances->rf-edges instances schema)])
           :overlays inst-canvas/overlays}
  :on-enter (fn [_] (instances/refresh-instance-state!))})

(feature/contribute!
 :open-in :object-finder
 {:accepts :object-set :order 10 :label-key :ui/app-explorer :glyph glyph
  :href ev/temp-set-href})

(feature/contribute!
 :command :open-object-set
 {:run! (fn [value] (shell/open-in-new-tab! (ev/temp-set-href value)))})
