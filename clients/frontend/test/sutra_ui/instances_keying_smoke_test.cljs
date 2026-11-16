(ns sutra-ui.instances-keying-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [sutra-ui.state.canvas :as canvas]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.apps.explorer.model :as explorer-model]
            [sutra-ui.apps.object-view.relations :as relations]))

(def ^:private widget-rid "ri.sutra.main.object-type.11111111-1111-1111-1111-111111111111")
(def ^:private maker-rid  "ri.sutra.main.object-type.22222222-2222-2222-2222-222222222222")

(defn- with-schema-index!
  []
  (reset! catalog/rid->name {widget-rid :widget maker-rid :maker}))

(defn- reset-graph! []
  (with-schema-index!)
  (reset! instances/instances {:objects {} :links []})
  (reset! canvas/instance-positions {})
  (reset! panel/panel-instances {}))

(defn- object-keys [] (set (keys (:objects @instances/instances))))

(defn- rows-of [type-kw] (get-in @instances/instances [:objects type-kw]))

(deftest seed-add-keys-by-name-not-rid
  (reset-graph!)
  (testing "a seeded object lands under its NAME, the key every consumer reads"
    (canvas/seed-add! widget-rid {:id "w1" :values {:name "one"}})
    (is (= #{:widget} (object-keys))
        "not the RID: (get-in @instances [:objects :widget]) is how the Object
         Explorer, matrix, floorplan, relations and the panel count all read")
    (is (= 1 (count (rows-of :widget))))))

(deftest seeding-an-already-loaded-object-does-not-duplicate-it
  (reset-graph!)
  (testing "the bulk-load bucket and the seed bucket are the SAME bucket"
    (reset! instances/instances {:objects {:widget [{:id "w1" :values {:name "one"}}]}
                                 :links []})
    (canvas/seed-add! widget-rid {:id "w1" :values {:name "one"}})
    (is (= #{:widget} (object-keys)) "no second bucket appears")
    (is (= 1 (count (rows-of :widget)))
        "the object is de-duped by id, because both writers agree on the key")))

(deftest seed-add-merges-alongside-existing-types
  (reset-graph!)
  (canvas/seed-add! widget-rid {:id "w1" :values {:name "one"}})
  (canvas/seed-add! maker-rid  {:id "m1" :values {:name "mk"}})
  (canvas/seed-add! widget-rid {:id "w2" :values {:name "two"}})
  (testing "each type keeps its own name-keyed bucket"
    (is (= #{:widget :maker} (object-keys)))
    (is (= #{"w1" "w2"} (set (map :id (rows-of :widget)))))
    (is (= #{"m1"} (set (map :id (rows-of :maker)))))))

(defn- graph! [objects links]
  (with-schema-index!)
  (reset! instances/instances {:objects objects :links links}))

(deftest accessors-replace-the-hand-rolled-lookups
  (graph! {:widget [{:id "w1" :values {:name "one"}}
                    {:id "w2" :values {:name "two"}}]
           :maker  [{:id "m1" :values {:name "mk"}}]}
          [{:id "e1" :link-type-ref "made-by" :from-id "w1" :to-id "m1"}
           {:id "e2" :link-type-ref "made-by" :from-id "w2" :to-id "m1"}])

  (testing "object-by-id finds across types and tags the type it found"
    (is (= "one" (get-in (instances/object-by-id "w1") [:values :name])))
    (is (= :widget (:object-type (instances/object-by-id "w1"))))
    (is (= :maker  (:object-type (instances/object-by-id "m1"))))
    (is (nil? (instances/object-by-id "nope"))))

  (testing "ids compare as strings — callers hold uuids and node-id strings alike"
    (is (some? (instances/object-by-id (str "w1")))))

  (testing "link-by-id"
    (is (= "made-by" (:link-type-ref (instances/link-by-id "e1"))))
    (is (nil? (instances/link-by-id "nope"))))

  (testing "edges-touching finds an id at EITHER end"
    (is (= #{"e1"} (set (map :id (instances/edges-touching "w1")))) "from-side")
    (is (= #{"e1" "e2"} (set (map :id (instances/edges-touching "m1")))) "to-side")
    (is (= [] (instances/edges-touching "nope"))))

  (testing "panel/rows-of serves the bounded PAGE, not the canvas graph"
    (reset! panel/panel-instances
            {:widget {:rows [{:id "p1" :values {:name "paged"}}]
                      :total 76259 :next-offset 1000}})
    (is (= #{"p1"} (set (map :id (panel/rows-of :widget))))
        "the fetched page — NOT the two rows the canvas happens to hold")
    (is (= [] (panel/rows-of :absent)) "empty-safe")
    (is (= 76259 (:total (explorer-model/page-info-of :widget)))
        "…and the page says how many exist, so a window can be labelled one")
    (is (= 1000 (:next-offset (explorer-model/page-info-of :widget)))
        "…and where the next one starts")))

(deftest a-self-link-appears-in-relations-both-ways
  (graph! {:widget [{:id "w1" :values {:name "one"}}]}
          [{:id "self" :link-type-ref "depends-on" :from-id "w1" :to-id "w1"}])
  (is (= 1 (count (instances/edges-touching "w1")))
      "one EDGE touches it")
  (let [rows (#'relations/conn-rows "w1")]
    (is (= 2 (count rows)) "…rendered as two ROWS: once per link side")
    (is (= #{:to :from} (set (map (comp :dir :side) rows))))))

(deftest a-count-does-not-depend-on-the-graph-being-loaded
  (reset-graph!)
  (reset! data/object-counts {:widget 76259 :maker 3})
  (testing "the count stands with an empty graph"
    (is (= {} (:objects @instances/instances)))
    (is (= 76259 (get @data/object-counts :widget))))
  (testing "loading one row does not move it"
    (canvas/seed-add! widget-rid {:id "w1" :values {:name "one"}})
    (is (= 1 (count (rows-of :widget))) "one row loaded")
    (is (= 76259 (get @data/object-counts :widget))
        "…of 76,259 that exist — the count is the server's, not the graph's")))

(deftest an-unresolvable-rid-falls-back-to-the-rid-not-nil
  (reset-graph!)
  (let [unknown "ri.sutra.main.object-type.99999999-9999-9999-9999-999999999999"]
    (canvas/seed-add! unknown {:id "x1" :values {:name "x"}})
    (is (= #{unknown} (object-keys)))
    (is (not (contains? (object-keys) nil)))))
