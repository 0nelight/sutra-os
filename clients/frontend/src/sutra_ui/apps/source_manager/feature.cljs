(ns sutra-ui.apps.source-manager.feature
  (:require [sutra-ui.feature :as feature]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.source-manager.core :as core]))

(def ^:private glyph
  (feature/icon
   {}
   [:rect {:x 3 :y 4 :width 14 :height 12 :rx 1}]
   [:line {:x1 3 :y1 8 :x2 17 :y2 8}]
   [:line {:x1 3 :y1 12 :x2 17 :y2 12}]
   [:line {:x1 10 :y1 4 :x2 10 :y2 16}]))

(feature/register-app!
 {:id :source-manager :order 6 :label-key :ui/app-source-manager :glyph glyph})

(feature/register-screen!
 {:id :source-manager :app :source-manager :segment "source-manager"
  :label-key :ui/app-source-manager :glyph glyph
  :view core/source-manager-panel
  :params #{:top-tab :selected-source :wizard?}
  :enter (fn [slice {:keys [wizard?] :as params}]
           (cond-> (merge slice params) wizard? (assoc :wizard-step 0)))
  :on-enter (fn [_] (data/refresh-import-status!))})

(feature/contribute! :shell-overlay :manage-datasources {:render core/manage-datasources-modal})

(feature/contribute!
 :command :manage-datasources
 {:run! core/open-manage-datasources!})

(feature/contribute!
 :resource-opener [:source-manager :sources]
 {:family :sources :app :source-manager
  :open! (fn [nm _]
           (shell/go! :source-manager
                      {:top-tab         :sources
                       :selected-source (or (some #(when (= (name %) (name nm)) %)
                                                  (keys @data/sources))
                                            nm)}))})
