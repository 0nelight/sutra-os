(ns sutra.dot.layout
  (:require [clojure.java.io :as io]
            [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.viz.graph-layout :as gl]))

(set! *warn-on-reflection* true)

(def ^:private seconds 20)

(def ^:private px-per-inch
  72.0)

(defn- median
  [xs fallback]
  (if (seq xs) (nth (vec (sort xs)) (quot (count xs) 2)) fallback))

(defn- gaps
  [nodes richtung]
  (let [w (median (map (comp first gl/box) nodes) gl/node-width)
        h (median (map (comp second gl/box) nodes) gl/node-height)
        [within across] (if (= :oben-unten richtung)
                          [(/ w gl/node-width) (/ h gl/node-height)]
                          [(/ h gl/node-height) (/ w gl/node-width)])]
    [(/ (* gl/row-gap within) px-per-inch)
     (/ (* gl/col-gap across) px-per-inch)]))

(def ^:private kraeftemodell
  #{"neato" "fdp"})

(defn- request
  [engine nodes edges opts]
  (let [at   (into {} (map-indexed (fn [i n] [(:id n) i])) nodes)
        ids  (fn [id] (some->> (at id) (str "n")))
        rank (if (= :oben-unten (:richtung opts)) "TB" "LR")
        [nodesep ranksep] (gaps nodes (:richtung opts))]
    (str "digraph g {\n"
         "  graph [rankdir=" rank
         (if (kraeftemodell engine)
           " overlap=prism sep=\"+16\""
           (str " nodesep=" (gl/fmt (gl/round nodesep 3))
                " ranksep=" (gl/fmt (gl/round ranksep 3))))
         "];\n"
         "  node [shape=box fixedsize=true label=\"\"];\n"
         (str/join (for [n nodes
                         :let [[w h] (gl/box n)]]
                     (str "  " (ids (:id n))
                          " [width=" (gl/fmt (gl/round (/ w px-per-inch) 4))
                          " height=" (gl/fmt (gl/round (/ h px-per-inch) 4)) "];\n")))
         (str/join (for [[s t] (sort (for [{:keys [source target]} edges
                                           :when (and (at source) (at target))]
                                       [(at source) (at target)]))]
                     (str "  n" s " -> n" t ";\n")))
         "}\n")))

(defn- run
  [engine dot]
  (let [p   (try (.start (ProcessBuilder. [engine "-Tplain"]))
                 (catch java.io.IOException e
                   (refusal/refuse! :refusal.dot/graphviz-missing {:engine engine}
                                    {:why (.getMessage e)})))
        out (future (slurp (.getInputStream p)))
        err (future (slurp (.getErrorStream p)))]
    (with-open [w (io/writer (.getOutputStream p))] (.write w dot))
    (if-not (.waitFor p seconds java.util.concurrent.TimeUnit/SECONDS)
      (do (.destroyForcibly p)
          (refusal/refuse! :refusal.dot/graphviz-timeout {:engine engine} {:seconds seconds}))
      (let [answer @out]
        (if (zero? (.exitValue p))
          answer
          (refusal/refuse! :refusal.dot/graphviz-refused {:engine engine}
                           {:reason (first (str/split-lines (str/trim (str @err))))}))))))

(defn coordinates
  [plain]
  (let [lines  (mapv #(str/split (str/trim %) #"\s+") (str/split-lines plain))
        header (or (first (filter #(= "graph" (first %)) lines))
                   (refusal/refuse! :refusal.dot/graphviz-no-header
                                    {:answer (subs plain 0 (min 200 (count plain)))}))
        scale  (or (gl/->num (second header)) 1.0)
        height (or (gl/->num (nth header 3 nil)) 0)]
    (into {}
          (for [[kind nm x y w h] lines
                :when (and (= "node" kind) (gl/->num x) (gl/->num y))
                :let  [cx (* scale (gl/->num x) px-per-inch)
                       cy (* scale (- height (gl/->num y)) px-per-inch)]]
            [nm [(- cx (/ (* scale (or (gl/->num w) 0) px-per-inch) 2))
                 (- cy (/ (* scale (or (gl/->num h) 0) px-per-inch) 2))]]))))

(defn- linksbuendig
  [placed richtung gap]
  (let [tb?   (= :oben-unten richtung)
        near  (if tb? :y :x)
        size  (fn [n] (if tb? (second (gl/box n)) (first (gl/box n))))
        layer (fn [n] (gl/round (+ (or (gl/->num (near n)) 0) (/ (size n) 2))))
        order (vec (sort (distinct (map layer placed))))
        depth (into {} (for [[l ns'] (group-by layer placed)]
                         [l (apply max (map size ns'))]))
        start (first (reduce (fn [[m at] l] [(assoc m l at) (+ at (depth l) gap)])
                             [{} (apply min (map #(or (gl/->num (near %)) 0) placed))]
                             order))]
    (mapv (fn [n] (assoc n near (get start (layer n) (or (gl/->num (near n)) 0)))) placed)))

(defn- place-with
  [engine]
  (fn [nodes edges opts]
    (let [nodes (gl/sorted-nodes nodes)
          found (coordinates (run engine (request engine nodes edges opts)))
          laid  (mapv (fn [i n]
                        (let [[x y] (get found (str "n" i) [0 0])]
                          (assoc n :x x :y y)))
                      (range)
                      nodes)]
      (if (kraeftemodell engine)
        laid
        (linksbuendig laid (:richtung opts)
                      (* px-per-inch (second (gaps nodes (:richtung opts)))))))))

(def ^:private engines
  {:organisch ["neato" "Organisch"
               "Netze ohne Richtung — Netzwerk-Segmente, Nachbarschaften. Kräftemodell."]
   :kreis     ["circo" "Kreis"
               "Ringe: alles auf einem Kreis, Zyklen als solche sichtbar."]
   :radial    ["twopi" "Radial"
               "Ein Mittelpunkt, Ringe nach Abstand — Bäume, die aus einem Knoten wachsen."]
   :dot       ["dot" "dot (graphviz)"
               "Graphviz' eigene Hierarchie, als Gegenprobe zu :hierarchie der Plattform."]
   :fdp       ["fdp" "fdp (graphviz)"
               "Kräftemodell für große, lose Netze; verträgt mehr Knoten als :organisch."]})

(def module
  {:name        :viz-dot
   :contributes {:viz/layouts
                 (into {} (for [[k [bin label doc]] engines]
                            [k {:label label :doc doc :layout (place-with bin)}]))}})
