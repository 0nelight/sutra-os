(ns sutra.viz.graph-svg
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.viz.graph-layout :as gl]
            [sutra.viz.palette :as palette]
            [sutra.kit.html :refer [esc]]))

(def default-pad 48)

(defn- truncate [s max-px char-px]
  (let [s         (str s)
        max-chars (max 3 (int (/ (max 0 (or (gl/->num max-px) 0)) char-px)))]
    (if (> (count s) max-chars)
      (str (subs s 0 (dec max-chars)) "…")
      s)))

(defn- with-box [n]
  (let [[w h] (gl/box n)
        x (or (gl/->num (:x n)) 0)
        y (or (gl/->num (:y n)) 0)]
    (assoc n :w w :h h :x x :y y :cx (+ x (/ w 2)) :cy (+ y (/ h 2)))))

(defn- icon-of [n]
  (let [i (:icon n)] (if (contains? n :icon) i (palette/kind-icon (:kind n)))))

(defn- border-point [n tx ty]
  (let [{:keys [cx cy w h]} n
        dx (- tx cx) dy (- ty cy)]
    (if (and (zero? dx) (zero? dy))
      [cx cy]
      (let [sx (if (zero? dx) ##Inf (/ (/ w 2) (Math/abs (double dx))))
            sy (if (zero? dy) ##Inf (/ (/ h 2) (Math/abs (double dy))))
            t  (min sx sy)]
        [(+ cx (* dx t)) (+ cy (* dy t))]))))

(def ^:private pair-offset
  7)

(defn- shifted
  [[x1 y1 x2 y2] d]
  (let [dx (- x2 x1) dy (- y2 y1)
        l  (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (if (zero? l)
      [x1 y1 x2 y2]
      (let [nx (* d (/ (- dy) l)) ny (* d (/ dx l))]
        [(+ x1 nx) (+ y1 ny) (+ x2 nx) (+ y2 ny)]))))

(defn- edge->svg [tx ty by-id pairs e]
  (let [s (by-id (:source e)) t (by-id (:target e))]
    (when (and s t (not= (:id s) (:id t)))
      (let [[px1 py1] (border-point s (:cx t) (:cy t))
            [px2 py2] (border-point t (:cx s) (:cy s))
            paired? (contains? pairs [(:target e) (:source e)])
            [x1 y1 x2 y2] (cond-> [px1 py1 px2 py2] paired? (shifted (- pair-offset)))
            [lx ly] (if paired?
                      (let [[a b c d] (shifted [x1 y1 x2 y2] (* -1.6 pair-offset))]
                        [(/ (+ a c) 2) (+ (/ (+ b d) 2) 4)])
                      [(/ (+ x1 x2) 2) (- (/ (+ y1 y2) 2) 4)])
            lbl     (:label e)]
        (str "<g><line x1='" (gl/fmt (tx x1)) "' y1='" (gl/fmt (ty y1))
             "' x2='" (gl/fmt (tx x2)) "' y2='" (gl/fmt (ty y2))
             "' stroke='#8f99a8' stroke-width='1.5' marker-end='url(#a)'/>"
             (when (seq (str lbl))
               (str "<text x='" (gl/fmt (tx lx)) "' y='" (gl/fmt (ty ly))
                    "' text-anchor='middle' font-family='sans-serif' font-size='11' "
                    "fill='#64748b' stroke='#fff' stroke-width='3' paint-order='stroke'>"
                    (esc (truncate lbl 170 6)) "</text>"))
             "</g>")))))

(defn- classed->svg
  [tx ty n]
  (let [{:keys [w h]} n
        x      (tx (:x n))
        y      (ty (:y n))
        accent (palette/accent-of n)]
    (str "<g><rect x='" (gl/fmt x) "' y='" (gl/fmt y)
         "' width='" (gl/fmt w) "' height='" (gl/fmt h)
         "' rx='3' fill='" (esc (:fill n)) "' stroke='" (esc accent) "' stroke-width='1'/>"
         (let [lines (gl/label-lines (str (or (:label n) (:id n)))
                                     (or (gl/->num (:umbruch n)) gl/label-wrap-px))
               top   (+ y (/ (- h (* gl/label-line-px (count lines))) 2) 12)]
           (str "<text x='" (gl/fmt (+ x (/ w 2))) "' y='" (gl/fmt top)
                "' text-anchor='middle' font-family='sans-serif' font-size='13' fill='#1c2127'>"
                (str/join (map-indexed (fn [i l]
                                         (str "<tspan x='" (gl/fmt (+ x (/ w 2)))
                                              (when (pos? i)
                                                (str "' dy='" (gl/fmt gl/label-line-px)))
                                              "'>" (esc l) "</tspan>"))
                                       lines))
                "</text>"))
         "</g>")))

(defn- node->svg [tx ty n]
  (if (:klasse n)
    (classed->svg tx ty n)
    (let [{:keys [w h]} n
          x      (tx (:x n))
          y      (ty (:y n))
          accent (palette/accent-of n)
          icon   (icon-of n)
          text-x (+ x (if (seq (str icon)) 30 14))
          base-y (+ y (/ h 2) 4)]
      (str "<g><rect x='" (gl/fmt x) "' y='" (gl/fmt y) "' width='" (gl/fmt w) "' height='"
           (gl/fmt h)
           "' rx='10' fill='" (esc accent) "'/>"
           "<rect x='" (gl/fmt (+ x 7)) "' y='" (gl/fmt (+ y 2))
           "' width='" (gl/fmt (max 1 (- w 9))) "' height='" (gl/fmt (max 1 (- h 4)))
           "' rx='8' fill='#fff'/>"
           (when (seq (str icon))
             (str "<text x='" (gl/fmt (+ x 16)) "' y='" (gl/fmt base-y)
                  "' font-family='sans-serif' font-size='13' fill='" (esc accent) "'>"
                  (esc icon) "</text>"))
           "<text x='" (gl/fmt text-x) "' y='" (gl/fmt base-y)
           "' font-family='sans-serif' font-size='13' fill='#1c2127'>"
           (esc (truncate (or (:label n) (:id n)) (- w (- text-x x) 12) 7))
           "</text></g>"))))

(defn- title->svg [title w]
  (when (seq (str title))
    (str "<text x='" (gl/fmt (/ w 2)) "' y='28' text-anchor='middle' "
         "font-family='sans-serif' font-size='15' fill='#404854'>"
         (esc title) "</text>")))

(defn- fitted
  [fit w h]
  (let [fw (or (gl/->num (first fit)) 0)
        fh (or (gl/->num (second fit)) 0)]
    (if (and (pos? fw) (pos? fh) (pos? w) (pos? h))
      (let [s (min (/ fw w) (/ fh h))]
        [(* w s) (* h s)])
      [w h])))

(defn graph->svg
  [{:keys [nodes edges title pad fit]}]
  (let [pad   (or pad default-pad)
        nodes (mapv with-box nodes)
        by-id (coll/index-by :id nodes)
        edges (filterv (fn [e] (and (by-id (:source e)) (by-id (:target e)))) edges)
        pairs (into #{} (map (juxt :source :target)) edges)
        minx  (if (seq nodes) (apply min (map :x nodes)) 0)
        miny  (if (seq nodes) (apply min (map :y nodes)) 0)
        maxx  (if (seq nodes) (apply max (map #(+ (:x %) (:w %)) nodes)) 1)
        maxy  (if (seq nodes) (apply max (map #(+ (:y %) (:h %)) nodes)) 1)
        w     (+ (max 1 (- maxx minx)) (* 2 pad))
        h     (+ (max 1 (- maxy miny)) (* 2 pad))
        tx    (fn [x] (+ (- x minx) pad))
        ty    (fn [y] (+ (- y miny) pad))
        [ew eh] (fitted fit w h)]
    (str "<svg xmlns='http://www.w3.org/2000/svg' width='" (gl/fmt (gl/round ew 2))
         "' height='" (gl/fmt (gl/round eh 2))
         "' viewBox='0 0 " (gl/fmt w) " " (gl/fmt h)
         "' preserveAspectRatio='xMidYMid meet' style='display:block'>"
         "<defs><marker id='a' markerWidth='8' markerHeight='8' refX='7' refY='3' "
         "orient='auto' markerUnits='strokeWidth'>"
         "<path d='M0,0 L7,3 L0,6 Z' fill='#8f99a8'/></marker></defs>"
         "<rect width='100%' height='100%' fill='#fff'/>"
         (title->svg title w)
         (str/join (keep #(edge->svg tx ty by-id pairs %) edges))
         (str/join (map #(node->svg tx ty %) nodes))
         "</svg>")))
