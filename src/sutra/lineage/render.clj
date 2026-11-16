(ns sutra.lineage.render
  (:require [sutra.kit.coerce :refer [->kw]]
            [sutra.lineage.ports :as ports]
            [sutra.lineage.graph :as lineage]
            [sutra.viz.graph-layout :as gl]
            [sutra.viz.layouts :as layouts]
            [sutra.viz.graph-dot :as gdot]
            [sutra.viz.graph-svg :as gsvg]
            [sutra.viz.lineage-projection :as lp]))

(set! *warn-on-reflection* true)

(defn- ref-node [n]
  (-> n (update :kind ->kw) (update :name ->kw)))

(defn all-edges
  [schema]
  (into {} (for [e (lineage/edges schema)]
             [(->kw (:name e))
              {:inputs           (mapv ref-node (:inputs e))
               :outputs          (mapv ref-node (:outputs e))
               :external-systems (mapv ->kw (:external-systems e))}])))

(defn links
  [schema]
  (let [ot-name (into {} (for [[rid s] (:object-types schema)]
                           [rid (->kw (:object-type-name s))]))
        resolve-ot (fn [r] (or (get ot-name r) (->kw r)))]
    (vec (for [[_ s] (:link-types schema)]
           {:name (->kw (:link-type-name s))
            :from (resolve-ot (:from-object-type s))
            :to   (resolve-ot (:to-object-type s))}))))

(defn datasources
  [schema]
  (vec (keep #(->kw (:source-name %)) (vals (:sources schema)))))

(defn normalize-scope
  [scope]
  (into {} (for [[k v] scope]
             [(->kw k) (into #{} (keep ->kw) v)])))

(defn positions
  [saved]
  (into {} (for [{:keys [node-kind node-key x y]} saved
                 :when (and x y)]
             [(lp/node-id (->kw node-kind) node-key)
              {:x (double x) :y (double y)}])))

(defn graph-data
  ([schema scope] (graph-data schema scope nil))
  ([schema scope hidden]
   (lp/project {:links        (links schema)
                :datasources  (datasources schema)
                :all-edges    (all-edges schema)
                :action-edits (lineage/action-edits schema)
                :scope        (normalize-scope scope)
                :hidden       (into #{} (map str) hidden)})))

(defn drawing
  [{:keys [store schema scope hidden layout richtung title] saved :positions}]
  (let [{:keys [nodes edges]} (graph-data schema scope hidden)
        asked (cond-> {} layout (assoc :layout layout) richtung (assoc :richtung richtung))]
    {:title title
     :nodes (if (seq asked)
              (layouts/place (ports/viz-layouts store) nodes edges asked)
              (gl/place nodes edges (positions saved)))
     :edges (mapv #(assoc % :label (:transform %)) edges)}))

(defn graph-svg [spec] (gsvg/graph->svg (drawing spec)))

(defn graph-dot [spec] (gdot/graph->dot (drawing spec)))
