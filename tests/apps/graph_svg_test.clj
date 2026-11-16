(ns ^:unit apps.graph-svg-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.lineage.render :as render]
            [sutra.viz.graph-dot :as gdot]
            [sutra.viz.graph-layout :as gl]
            [sutra.viz.graph-svg :as gsvg]
            [sutra.viz.lineage-projection :as lp]))

(def ^:private nodes
  [{:id "datasource/acars" :kind :datasource :label "acars" :x -420 :y -180}
   {:id "dataset/acars_roh" :kind :dataset :label "acars_roh" :x 0 :y 0}
   {:id "befund" :kind :object-type :label "befund"
    :x 640 :y 320 :w 200 :h 60}])

(def ^:private edges
  [{:source "datasource/acars" :target "dataset/acars_roh" :label "acars-abholen"}
   {:source "dataset/acars_roh" :target "befund" :label "befunde-klassifizieren"}
   {:source "dataset/acars_roh" :target "dataset/gone" :label "dangling"}])

(defn- kind-color
  [k]
  (:color (lp/row k)))

(defn- attr [s k]
  (second (re-find (re-pattern (str k "='([^']+)'")) s)))

(deftest svg-frames-the-whole-graph
  (let [svg (gsvg/graph->svg {:nodes nodes :edges edges})]
    (testing "the frame is the CONTENT's bounds — not a viewport, and never cropped"
      (is (= (str (+ (- 840 -420) 96)) (attr svg "width")))
      (is (= (str (+ (- 380 -180) 96)) (attr svg "height")))
      (is (= (str "0 0 " (attr svg "width") " " (attr svg "height")) (attr svg "viewBox")))
      (is (str/includes? svg "x='48' y='48'")
          "the top-left-most node sits at the padding, whatever its own coordinates"))
    (testing "vector output, so no live pan/zoom transform can ride along"
      (is (not (str/includes? svg "foreignObject")))
      (is (not (str/includes? svg "scale("))))
    (testing "kind colours are inlined (the SVG carries no stylesheet)"
      (doseq [k [:datasource :dataset :object-type]]
        (is (str/includes? svg (kind-color k)) (name k))))
    (testing "an edge whose endpoint is not on the canvas is dropped"
      (is (= 2 (count (re-seq #"<line " svg))))
      (is (not (str/includes? svg "dangling"))))))

(deftest layout-places-what-nobody-dragged
  (let [ns' [{:id "a" :label "a"} {:id "b" :label "b"} {:id "c" :label "c"}]
        es  [{:source "a" :target "b"} {:source "b" :target "c"}]]
    (testing "with no stored position the DAG lays out left to right"
      (let [by-id (into {} (map (juxt :id identity)) (gl/place ns' es {}))]
        (is (< (:x (by-id "a")) (:x (by-id "b")) (:x (by-id "c"))))))
    (testing "stored positions win, and the rest lands beside them"
      (let [placed (gl/place ns' es {"a" {:x 10 :y 20}})
            by-id  (into {} (map (juxt :id identity)) placed)]
        (is (= [10 20] [(:x (by-id "a")) (:y (by-id "a"))]))
        (is (every? #(> (:x (by-id %)) 10) ["b" "c"]))))))

(def ^:private schema
  {:object-types {"rid.ot.befund" {:object-type-name :befund :plural "Befunde"}
                  "rid.ot.mangel" {:object-type-name :mangel :plural "Mängel"}}
   :datasets     {"rid.ds.raw" {:dataset-name :acars_roh}}
   :link-types   {"rid.lt.bm" {:link-type-name :befund-mangel
                               :from-object-type "rid.ot.befund"
                               :to-object-type   "rid.ot.mangel"}}
   :sources      {"rid.src.acars" {:source-name :acars}
                  "rid.src.herstellerportal"     {:source-name :herstellerportal}}
   :transforms
   {"rid.t.acars" {:transform-name :acars-abholen
                  :input  {:kind :datasource :datasource :acars}
                  :output {:kind :dataset :dataset "rid.ds.raw"}}
    "rid.t.klassifizieren" {:transform-name :befunde-klassifizieren
                      :external-systems [:herstellerportal]
                      :inputs {:mail {:kind :dataset :dataset "rid.ds.raw"}}
                      :output {:kind :object-type :object-type "rid.ot.befund"}}}})

(def ^:private scope
  {:transforms #{:acars-abholen :befunde-klassifizieren}})

(deftest server-renders-the-saved-graph
  (let [{:keys [nodes edges]} (render/graph-data schema scope)
        ids (set (map :id nodes))]
    (testing "the four-type model: sources, datasets and object types are nodes"
      (is (= #{"datasource/acars" "dataset/acars_roh" "befund"} ids)))
    (testing "a transform is an EDGE, carrying its own name"
      (is (= #{"acars-abholen" "befunde-klassifizieren"} (set (map :transform edges)))))
    (testing "an LLM grant is not a data source — it draws no source→type edge"
      (is (not (contains? ids "datasource/herstellerportal")))
      (is (empty? (filter #(= "datasource/herstellerportal" (:source %)) edges))))
    (testing "a data source reaches DATASET outputs only"
      (is (= #{["datasource/acars" "dataset/acars_roh"]
               ["dataset/acars_roh" "befund"]}
             (set (map (juxt :source :target) edges)))))))

(deftest a-removed-node-stays-removed-in-the-served-drawing
  (testing "a node the graph HIDES is not drawn — and its edges go with it.
            Removal cannot be a scope edit: `acars_roh` is drawn because
            two in-scope transforms name it, so un-scoping would have left it
            on the picture while the canvas showed it gone"
    (let [{:keys [nodes edges]} (render/graph-data schema scope ["dataset/acars_roh"])
          ids (set (map :id nodes))]
      (is (= #{"datasource/acars" "befund"} ids))
      (is (empty? edges) "every edge ran through the removed node")))
  (testing "the SVG and the DOT come off that same drawing"
    (let [spec {:schema schema :scope scope :hidden ["befund"]}]
      (is (not (str/includes? (render/graph-svg spec) "befund")))
      (is (not (str/includes? (render/graph-dot spec) "befund")))
      (is (str/includes? (render/graph-svg spec) "acars_roh")
          "hiding one node must not take the rest of the graph with it"))))

(deftest saved-layout-positions-the-drawing
  (let [svg (render/graph-svg
             {:schema schema :scope scope :title "Befundfluss"
              :positions [{:node-kind :datasource :node-key "acars" :x 0 :y 0}
                          {:node-kind :dataset :node-key "acars_roh" :x 400 :y 0}
                          {:node-kind :object-type :node-key "befund" :x 800 :y 0}]})]
    (is (str/includes? svg "Befundfluss"))
    (is (str/includes? svg "acars"))
    (is (str/includes? svg "befunde-klassifizieren"))
    (is (= "1076" (attr svg "width")) "three placed nodes plus padding, nothing cropped")
    (is (= 2 (count (re-seq #"<line " svg))))))

(deftest a-layout-row-addresses_the-node-the-canvas-drew
  (is (= "dataset/acars_roh" (lp/node-id :dataset "acars_roh")))
  (is (= "befund" (lp/node-id :object-type "befund"))
      "object-type ids are bare names — the layout rows the canvas wrote use them")
  (is (= {"datasource/acars" {:x 1.0 :y 2.0}}
         (render/positions [{:node-kind "datasource" :node-key "acars" :x 1 :y 2}]))))

(deftest dot-is-the-same-drawing-as-graphviz-source
  (let [dot (gdot/graph->dot {:nodes nodes :edges edges :title "Befundfluss"})]
    (testing "one node per line, labelled and coloured by kind"
      (is (str/includes? dot "\"dataset/acars_roh\" ["))
      (is (str/includes? dot "label=\"acars_roh\""))
      (is (str/includes? dot (str "color=\"" (kind-color :datasource) "\"")))
      (is (str/includes? dot "width=2.5 height=0.667")
          "sizes in inches, rounded — a DOT file is read and diffed by people"))
    (testing "the saved positions ride along PINNED, so `neato -n` reproduces the SVG"
      (is (str/includes? dot "pos=\"-440,208!\"")))
    (testing "edges carry the transform that made them"
      (is
       (str/includes? dot
                      "\"datasource/acars\" -> \"dataset/acars_roh\" [label=\"acars-abholen\"]")))
    (testing "an edge whose endpoint is not on the canvas is dropped here too"
      (is (not (str/includes? dot "dataset/gone"))))
    (is (str/starts-with? dot "digraph \"Befundfluss\" {"))
    (is (str/ends-with? dot "}\n"))))

(deftest server-draws-dot-from-the-same-projection
  (let [spec {:schema schema :scope scope :title "Befundfluss"
              :positions [{:node-kind :object-type :node-key "befund" :x 800 :y 0}]}
        dot  (render/graph-dot spec)
        svg  (render/graph-svg spec)]
    (doseq [n ["datasource/acars" "dataset/acars_roh" "befund"]]
      (is (str/includes? dot n)))
    (is (= (count (re-seq #" -> " dot)) (count (re-seq #"<line " svg)))
        "same edge count as the SVG — one projection, two serialisers")))
