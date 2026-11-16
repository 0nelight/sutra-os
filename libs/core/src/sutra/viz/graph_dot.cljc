(ns sutra.viz.graph-dot
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.viz.graph-layout :as gl]
            [sutra.viz.palette :as palette]))

(defn- esc [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")))

(defn- pos-attr
  [n]
  (when (and (some? (:x n)) (some? (:y n)))
    (let [[w h] (gl/box n)
          cx (+ (or (gl/->num (:x n)) 0) (/ w 2))
          cy (+ (or (gl/->num (:y n)) 0) (/ h 2))]
      (str "pos=\"" (gl/round (/ cx 0.75)) "," (gl/round (- (/ cy 0.75))) "!\""))))

(defn- node-line [n]
  (let [accent (palette/accent-of n)
        kind   (:kind n)
        [w h]  (gl/box n)
        attrs  (cond-> [(str "label=\"" (esc (or (:label n) (:id n))) "\"")
                        (str "color=\"" (esc accent) "\"")
                        (str "width=" (gl/fmt (gl/round (/ w 72.0) 3)))
                        (str "height=" (gl/fmt (gl/round (/ h 72.0) 3)))]
                 kind (conj (str "kind=\"" (esc (name kind)) "\""))
                 (pos-attr n) (conj (pos-attr n)))]
    (str "  \"" (esc (:id n)) "\" [" (str/join " " attrs) "];")))

(defn- edge-line [e]
  (let [lbl (:label e)]
    (str "  \"" (esc (:source e)) "\" -> \"" (esc (:target e)) "\""
         (when (seq (str lbl)) (str " [label=\"" (esc lbl) "\"]"))
         ";")))

(defn graph->dot
  [{:keys [nodes edges title]}]
  (let [by-id (coll/index-by :id nodes)
        edges (filterv (fn [e] (and (by-id (:source e)) (by-id (:target e)))) edges)]
    (str "digraph " (if (seq (str title)) (str "\"" (esc title) "\"") "lineage") " {\n"
         "  rankdir=LR;\n"
         "  node [shape=box style=\"rounded,filled\" fillcolor=\"#ffffff\" "
         "penwidth=2 fontname=\"sans-serif\" fontsize=11];\n"
         "  edge [color=\"#8f99a8\" fontname=\"sans-serif\" fontsize=9 "
         "fontcolor=\"#64748b\"];\n"
         (when (seq (str title)) (str "  label=\"" (esc title) "\";\n  labelloc=t;\n"))
         (str/join "\n" (map node-line nodes))
         (when (seq nodes) "\n")
         (str/join "\n" (map edge-line edges))
         (when (seq edges) "\n")
         "}\n")))
