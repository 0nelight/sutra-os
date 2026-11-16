(ns sutra-ui.apps.ontology.feature
  (:require [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.feature :as feature]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.apps.ontology.graph :as ograph]
            [sutra-ui.forms.interface :as iface-forms]
            [sutra-ui.forms.link :as link-forms]
            [sutra-ui.forms.object :as object-forms]
            [sutra-ui.object.card :as oc]
            [sutra-ui.apps.ontology.canvas :as ont-canvas]
            [sutra-ui.apps.ontology.panel :as ont]))

(def ^:private glyph
  (feature/icon
   {:stroked? false}
   [:rect {:x 3 :y 4 :width 3 :height 3 :fill "currentColor"}]
   [:rect {:x 3 :y 9 :width 3 :height 3 :fill "currentColor"}]
   [:rect {:x 3 :y 14 :width 3 :height 3 :fill "currentColor"}]
   [:line {:x1 8 :y1 5.5 :x2 17 :y2 5.5 :stroke "currentColor" :stroke-width 1.6}]
   [:line {:x1 8 :y1 10.5 :x2 17 :y2 10.5 :stroke "currentColor" :stroke-width 1.6}]
   [:line {:x1 8 :y1 15.5 :x2 17 :y2 15.5 :stroke "currentColor" :stroke-width 1.6}]))

(def ^:private canvas-glyph
  (feature/icon
   {:stroked? false}
   [:line {:x1 5 :y1 5 :x2 14 :y2 5 :stroke "currentColor" :stroke-width 1.4}]
   [:line {:x1 5 :y1 5 :x2 9 :y2 14 :stroke "currentColor" :stroke-width 1.4}]
   [:line {:x1 14 :y1 5 :x2 9 :y2 14 :stroke "currentColor" :stroke-width 1.4}]
   [:circle {:cx 5 :cy 5 :r 2.4 :fill "none" :stroke "currentColor" :stroke-width 1.4}]
   [:circle {:cx 14 :cy 5 :r 2.4 :fill "none" :stroke "currentColor" :stroke-width 1.4}]
   [:circle {:cx 9 :cy 15 :r 2.4 :fill "none" :stroke "currentColor" :stroke-width 1.4}]))

(def ^:private section->detail-kind
  {:object-types       :object-type
   :object-type-groups :object-type-group
   :link-types         :link-type
   :interfaces         :interface
   :functions          :function})

(def ^:private detail-kind->section
  (into {} (map (juxt val key)) section->detail-kind))

(defn- tail
  [{:keys [section detail]}]
  (let [{:keys [kind id tab]} detail
        detail? (and id (= (detail-kind->section kind) section))]
    (cond-> []
      (and section (not= section :discover))
      (conj (name section))
      detail?
      (conj (routing/enc (routing/detail-id->str kind id)))
      (and detail? tab (not= tab :overview))
      (conj (name tab)))))

(defn- parse-tail
  [[section id tab]]
  (let [kind (section->detail-kind (keyword section))]
    {:section (if section (keyword section) :discover)
     :detail  (when (and section id)
                (cond-> {:kind kind
                         :id   (routing/str->detail-id kind (routing/dec' id))
                         :tab  :overview}
                  tab (assoc :tab (keyword tab))))}))

(feature/register-app!
 {:id :ontology :order 2 :label-key :ui/app-ontology :glyph glyph :branching? true})

(feature/register-screen!
 {:id :ontology :app :ontology :order 0 :segment "ontology" :label-key :ui/nav-ontology
  :glyph glyph :view ont/ontology-panel
  :params #{:section :detail}
  :tail tail :parse-tail parse-tail})

(feature/register-screen!
 {:id :ontology-canvas :app :ontology :order 1 :segment "ontology-canvas"
  :label-key :ui/nav-ontology-canvas :glyph canvas-glyph
  :canvas {:mode :ontology :positions :schema
           :capabilities #{:create-nodes :delete-object-type :connect :edit-schema}
           :graph (fn [{:keys [schema positions show-fields? ctrl-held?]}]
                    [(ograph/object-graph->rf-nodes schema positions show-fields? ctrl-held?)
                     (ograph/object-graph->rf-edges schema)])
           :top-bar ont-canvas/top-bar :overlays ont-canvas/overlays}})

(feature/contribute! :detail-page :ontology {:render ont/detail-page})

(defn- show-section!
  [section detail]
  (shell/go! :ontology {:section section :detail detail}))

(doseq [[family section kind] [[:object-types :object-types :object-type]
                               [:object-type-groups :object-type-groups :object-type-group]
                               [:link-types :link-types :link-type]
                               [:interfaces :interfaces :interface]
                               [:functions :functions :function]
                               [:actions :action-types :action-type]]]
  (feature/contribute!
   :resource-opener [:ontology family]
   {:family family :app :ontology
    :open! (fn [nm _] (show-section! section {:kind kind :id nm :tab :overview}))}))

(feature/contribute!
 :resource-opener [:ontology :shared-properties]
 {:family :shared-properties :app :ontology
  :open! (fn [_ _] (show-section! :shared-properties nil))})

(feature/contribute!
 :resource-opener [:ontology :default]
 {:family :default :app :ontology
  :open! (fn [_ _] (show-section! :discover nil))})

(defn- in-ontology [thunk] (fn [] (shell/go! :ontology) (thunk)))

(feature/contribute!
 :search :ontology
 {:order 10
  :entries
  (fn []
    (let [schema @catalog/schema]
      (concat
       (sort-by (comp str/lower-case :label)
                (for [k (keys (:object-types schema))]
                  {:kind (i18n/t :ui/kind-object-type) :glyph "▢" :label (oc/type-label k)
                   :api (name k)
                   :nav (in-ontology #(object-forms/open-object-detail! k))}))
       (for [c (:links schema)]
         {:kind (i18n/t :ui/kind-link) :glyph "⇄" :label (oc/link-type-label c)
          :api (name (:name c))
          :nav (in-ontology #(link-forms/open-link-detail! (catalog/link-id c)))})
       (for [[k spec] (sort-by key (:interfaces schema))]
         {:kind (i18n/t :ui/kind-interface) :glyph "◇" :label (oc/spec-label spec k)
          :api (name k)
          :nav (in-ontology #(iface-forms/open-interface-detail! k))}))))})
