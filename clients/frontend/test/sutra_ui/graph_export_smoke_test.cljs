(ns sutra-ui.graph-export-smoke-test
  (:require [cljs.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]
            [sutra-ui.graph-editor.export :as gexport]))

(def ^:private nodes
  [{:id "datasource/mailbox" :type "datasource"
    :position {:x -420 :y -180} :width 180 :height 48
    :data {:label "mailbox"}}
   {:id "dataset/raw_messages" :type "dataset"
    :position {:x 0 :y 0} :width 180 :height 48
    :data {:label "raw_messages"}}
   {:id "auftrags-ereignis" :type "objectType"
    :position {:x 640 :y 320} :measured {:width 200 :height 60}
    :data {:label "auftrags-ereignis"}}
   {:id "dataset/hidden" :type "dataset" :hidden true
    :position {:x 9000 :y 9000} :width 180 :height 48
    :data {:label "hidden_dataset"}}])

(def ^:private edges
  [{:id "e1" :source "datasource/mailbox" :target "dataset/raw_messages"
    :data {:transform "pull-mailbox"}}
   {:id "e2" :source "dataset/raw_messages" :target "auftrags-ereignis"
    :data {:transform "classify-events"}}
   {:id "e3" :source "dataset/raw_messages" :target "dataset/gone"}])

(def ^:private svg (gexport/graph->svg (clj->js nodes) (clj->js edges) nil))
(def ^:private dot (gexport/graph->dot (clj->js nodes) (clj->js edges) "lineage"))

(defn- attr [s k]
  (second (re-find (re-pattern (str k "='([^']+)'")) s)))

(deftest bounds-cover-every-node
  (testing "viewBox spans the whole graph, not the on-screen viewport"
    (let [w (js/parseFloat (attr svg "width"))
          h (js/parseFloat (attr svg "height"))]
      (is (== w (+ (- 840 -420) 96)))
      (is (== h (+ (- 380 -180) 96)))
      (is (= (str "0 0 " w " " h) (attr svg "viewBox")))))
  (testing "a node at negative coordinates is drawn inside the canvas"
    (is (str/includes? svg "x='48' y='48'"))))

(deftest content-is-vector-not-a-live-dom-snapshot
  (is (not (str/includes? svg "foreignObject")))
  (is (not (str/includes? svg "transform='translate")))
  (is (not (str/includes? svg "scale("))))

(deftest labels-and-edges-survive
  (doseq [lbl ["mailbox" "raw_messages" "auftrags-ereignis"
               "pull-mailbox" "classify-events"]]
    (is (str/includes? svg lbl) (str "missing " lbl)))
  (testing "hidden nodes and dangling edges are dropped"
    (is (not (str/includes? svg "hidden_dataset")))
    (is (= 2 (count (re-seq #"<line " svg))))))

(deftest kind-colours-are-inlined
  (is (str/includes? svg "#238551"))
  (is (str/includes? svg "#f0b726"))
  (is (str/includes? svg "#2d72d2")))

(deftest dot-carries-the-same-graph
  (is (str/starts-with? dot "digraph \"lineage\" {"))
  (doseq [n ["datasource/mailbox" "dataset/raw_messages" "auftrags-ereignis"]]
    (is (str/includes? dot n) (str "missing " n)))
  (testing "transform names label the edges, dangling and hidden nodes are dropped"
    (is (str/includes? dot "label=\"pull-mailbox\""))
    (is (not (str/includes? dot "hidden_dataset")))
    (is (= 2 (count (re-seq #" -> " dot)))))
  (testing "the canvas positions ride along pinned, so `neato -n` matches the SVG"
    (is (str/includes? dot "pos="))))

(defn ^:export run []
  (run-tests 'sutra-ui.graph-export-smoke-test))
