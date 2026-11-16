(ns sutra-ui.apps.ontology.graph
  (:require [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.instances :as instances]))

(defn object-graph-scope
  []
  (when (= :ontology (:mode @canvas/current-view))
    (canvas/current-visible-types)))

(defn object-graph->rf-nodes [sch positions show-fields? ctrl-held?]
  (let [staged (:object-types @instances/staged-types)
        vt     (object-graph-scope)]
    (vec
     (for [[type-kw fields] (:objects sch)
           :when (canvas/visible? vt :object-types type-kw)]
       (colors/object-type-node sch positions show-fields? ctrl-held?
                          staged type-kw fields false nil)))))

(defn object-graph->rf-edges
  [sch]
  (let [staged (:link-types @instances/staged-types)
        vt     (object-graph-scope)]
    (vec
     (for [{cn :name from :from to :to card :cardinality :as conn} (:links sch)
           :when (and (canvas/visible? vt :link-types cn)
                      (canvas/visible? vt :object-types from)
                      (canvas/visible? vt :object-types to))
           :when (not= from to)]
       {:id        (catalog/link-id conn)
        :source    (name from)
        :target    (name to)
        :type      "floatingEdge"
        :markerEnd {:type "arrowclosed"}
        :data      {:label       (name cn)
                    :cardinality (name card)
                    :staged?     (contains? staged cn)}}))))
