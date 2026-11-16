(ns sutra-ui.apps.object-view.registry
  (:require [sutra.i18n.core :as i18n]))

(defmulti config-form    (fn [view-type _config _on-change]  view-type))
(defmulti default-config identity)

(def view-types
  [{:view-type :attributes :label :ui/ov-widget-properties}
   {:view-type :relations  :label :ui/ov-widget-links}
   {:view-type :documents  :label :ui/ov-widget-documents :user-creatable? true}
   {:view-type :matrix     :label :ui/ov-widget-matrix :user-creatable? true}
   {:view-type :floorplan  :label :ui/ov-widget-floorplan}])

(defn user-creatable-types []
  (filter :user-creatable? view-types))

(defn label-of
  [widget]
  (or (some #(when (= (:view-type %) widget) (i18n/t (:label %))) view-types)
      (some-> widget name)))

(defmethod config-form :default [_ _ _]
  [:div {:class "explorer-registry__no-config"}
   (i18n/t :ui/ov-widget-no-config)])

(defmethod default-config :default [_] {})
