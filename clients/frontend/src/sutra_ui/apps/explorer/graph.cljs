(ns sutra-ui.apps.explorer.graph
  (:require [clojure.string :as str]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.colors :as colors]
            [sutra-ui.state.instances :as instances]))

(defn instance-id-visible? [id-str]
  (let [vis @instances/instance-visible-ids]
    (or (nil? vis) (contains? vis id-str))))

(defn instances->rf-nodes
  [{:keys [objects]} positions ctrl-held?]
  (let [vt  (canvas/current-visible-types)
        out (vec
             (for [[type-kw rows] objects
                   :when (canvas/visible? vt :object-types type-kw)
                   :let [meta (oc/type-meta type-kw)]
                   row rows
                   :let [id-str (str (:id row))]
                   :when (instance-id-visible? id-str)]
               {:id       id-str
                :type     "objectInstance"
                :position (get positions id-str {:x 0 :y 0})
                :data     {:label     (oc/title meta row id-str)
                           :type      (name (catalog/name-of type-kw))
                           :typeLabel (oc/type-label meta)
                           :symbol    (:symbol meta)
                           :color     (colors/type-color type-kw row)
                           :staged?   (boolean (:staged? row))
                           :ctrlHeld  (boolean ctrl-held?)}}))]
    (js/console.log
     (str "%c[rf-nodes] instances → " (count out) " nodes — positions: "
          (str/join
           ", "
           (for [n out]
             (str (subs (:id n) 0 6) "@(" (:x (:position n)) "," (:y (:position n)) ")"))))
     "color:#2d72d2")
    out))

(defn cardinality-for [view-schema conn-type-kw]
  (some->> (:links view-schema)
           (some #(when (= (:name %) conn-type-kw) %))
           :cardinality
           name))

(defn instances->rf-edges
  [{:keys [links]} view-schema]
  (let [vt  (canvas/current-visible-types)
        out (vec
             (for [c links
                   :let [conn-kw (:link-type-ref c)
                         conn-name (name (catalog/name-of conn-kw))]
                   :when (and (canvas/visible? vt :link-types conn-kw)
                              (instance-id-visible? (str (:from-id c)))
                              (instance-id-visible? (str (:to-id c))))]
               {:id        (str (:id c))
                :source    (str (:from-id c))
                :target    (str (:to-id c))
                :type      "instanceFloatingEdge"
                :data      {:label          conn-name
                            :connectionType conn-name
                            :staged?        (boolean (:staged? c))
                            :cardinality    (or (cardinality-for view-schema conn-kw) "")}}))]
    (js/console.log (str "%c[rf-edges] " (count links)
                         " links → " (count out) " edges")
                    "color:#2d72d2")
    out))
