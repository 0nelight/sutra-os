(ns sutra.capps.polygon
  (:require [clojure.string :as str]))

(def min-points 3)

(def ^:private eps 1e-9)

(defn- cross-z [[ax ay] [bx by] [cx cy]]
  (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))

(defn- sgn [v]
  (cond (> v eps) 1 (< v (- eps)) -1 :else 0))

(defn- segments-cross? [a b c d]
  (let [d1 (sgn (cross-z c d a))
        d2 (sgn (cross-z c d b))
        d3 (sgn (cross-z a b c))
        d4 (sgn (cross-z a b d))]
    (and (not= 0 d1) (not= 0 d2) (not= 0 d3) (not= 0 d4)
         (not= d1 d2) (not= d3 d4))))

(defn- open-edges [points]
  (mapv vector points (rest points)))

(defn- ring-edges [{:keys [points closed?]}]
  (cond-> (open-edges points)
    (and closed? (<= min-points (count points)))
    (conj [(peek points) (first points)])))

(defn- crosses? [points a b]
  (boolean (some (fn [[c d]] (segments-cross? a b c d)) (open-edges points))))

(defn- dist2 [[ax ay] [bx by]]
  (let [dx (- ax bx) dy (- ay by)] (+ (* dx dx) (* dy dy))))

(defn- near? [a b tol]
  (<= (dist2 a b) (* tol tol)))

(defn- point [p]
  [(double (nth p 0)) (double (nth p 1))])

(defn state
  ([] {:points [] :closed? false :ignored nil})
  ([points closed?]
   {:points (mapv point points) :closed? (boolean closed?) :ignored nil}))

(defn tap
  [{:keys [points closed?] :as poly} p {:keys [tolerance] :or {tolerance 0}}]
  (let [p (point p)
        n (count points)]
    (cond
      closed?
      (assoc poly :ignored :geschlossen)

      (zero? n)
      (assoc poly :points [p] :ignored nil)

      (and (<= min-points n) (near? p (first points) tolerance))
      (if (crosses? points (peek points) (first points))
        (assoc poly :ignored :kreuzt)
        (assoc poly :closed? true :ignored nil))

      (some #(near? p % tolerance) points)
      (assoc poly :ignored :zu-nah)

      (crosses? points (peek points) p)
      (assoc poly :ignored :kreuzt)

      :else
      (assoc poly :points (conj points p) :ignored nil))))

(defn undo [{:keys [points closed?] :as poly}]
  (cond
    closed?      (assoc poly :closed? false :ignored nil)
    (seq points) (assoc poly :points (pop points) :ignored nil)
    :else        (assoc poly :ignored :leer)))

(defn clear [poly]
  (assoc poly :points [] :closed? false :ignored nil))

(defn move-point [{:keys [points] :as poly} i p]
  (if (and (nat-int? i) (< i (count points)))
    (assoc poly :points (assoc points i (point p)) :ignored nil)
    poly))

(defn edge-midpoints
  [{:keys [points closed?]}]
  (let [n (count points)]
    (when (and closed? (<= min-points n))
      (mapv (fn [i]
              (let [[ax ay] (nth points i)
                    [bx by] (nth points (mod (inc i) n))]
                [(inc i) [(/ (+ ax bx) 2.0) (/ (+ ay by) 2.0)]]))
            (range n)))))

(defn insert-point
  [{:keys [points] :as poly} i p]
  (if (and (nat-int? i) (<= i (count points)))
    (assoc poly :points (vec (concat (subvec points 0 i) [(point p)] (subvec points i)))
           :ignored nil)
    poly))

(defn drop-point
  [{:keys [points] :as poly} i]
  (if (and (nat-int? i) (< i (count points)) (< min-points (count points)))
    (assoc poly :points (vec (concat (subvec points 0 i) (subvec points (inc i))))
           :ignored nil)
    (assoc poly :ignored :zu-wenig)))

(defn centroid [points]
  (when (seq points)
    (let [n (count points)]
      [(/ (reduce + (map first points)) n)
       (/ (reduce + (map second points)) n)])))

(defn translate [poly dx dy]
  (update poly :points (fn [ps] (mapv (fn [[x y]] [(+ x dx) (+ y dy)]) ps))))

(defn rotate [poly radians]
  (if-let [[cx cy] (centroid (:points poly))]
    (let [co (Math/cos radians) si (Math/sin radians)]
      (update poly :points
              (fn [ps]
                (mapv (fn [[x y]]
                        (let [dx (- x cx) dy (- y cy)]
                          [(+ cx (- (* dx co) (* dy si)))
                           (+ cy (+ (* dx si) (* dy co)))]))
                      ps))))
    poly))

(defn simple? [poly]
  (let [es (vec (ring-edges poly))
        n  (count es)]
    (not (boolean (some (fn [[i j]]
                          (let [[a b] (nth es i)
                                [c d] (nth es j)]
                            (segments-cross? a b c d)))
                        (for [i (range n) j (range (inc i) n)] [i j]))))))

(defn ring? [{:keys [points closed?]}]
  (and closed? (<= min-points (count points))))

(defn vertex-at [{:keys [points]} p tolerance]
  (let [p (point p)]
    (first (keep-indexed (fn [i v] (when (near? p v tolerance) i)) points))))

(defn inside? [{:keys [points] :as poly} p]
  (when (ring? poly)
    (let [[px py] (point p)
          n       (count points)]
      (loop [i 0 j (dec n) in? false]
        (if (= i n)
          in?
          (let [[xi yi] (nth points i)
                [xj yj] (nth points j)
                hit?    (and (not= (> yi py) (> yj py))
                             (< px (+ xi (/ (* (- xj xi) (- py yi))
                                            (if (zero? (- yj yi)) eps (- yj yi))))))]
            (recur (inc i) i (if hit? (not in?) in?))))))))

(defn- seg-dist2 [p [a b]]
  (let [[px py] p [ax ay] a [bx by] b
        dx (- bx ax) dy (- by ay)
        l2 (+ (* dx dx) (* dy dy))
        t  (if (zero? l2) 0 (max 0 (min 1 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) l2))))]
    (dist2 p [(+ ax (* t dx)) (+ ay (* t dy))])))

(defn distance
  [poly p]
  (let [p (point p)]
    (if (inside? poly p)
      0
      (Math/sqrt (reduce min ##Inf (map #(seg-dist2 p %) (ring-edges poly)))))))

(defn- n->s [v]
  (let [r (/ (Math/round (* 100.0 (double v))) 100.0)
        i (long r)]
    (if (== r i) (str i) (str r))))

(defn ->str [{:keys [points]} as]
  (when (seq points)
    (if (= :geojson (keyword as))
      (let [ring (cond-> (vec points)
                   (not= (first points) (peek points)) (conj (first points)))]
        (str "{\"type\":\"Polygon\",\"coordinates\":[["
             (str/join "," (map (fn [[x y]] (str "[" (n->s x) "," (n->s y) "]")) ring))
             "]]}"))
      (str/join " " (map (fn [[x y]] (str (n->s x) "," (n->s y))) points)))))

(defn rings->str
  [rings]
  (let [ring (fn [ps]
               (let [v (cond-> (vec ps) (not= (first ps) (peek ps)) (conj (first ps)))]
                 (str "[" (str/join "," (map (fn [[x y]] (str "[" (n->s x) "," (n->s y) "]")) v))
                      "]")))
        ok   (filter #(<= min-points (count %)) (map vec rings))]
    (str "{\"type\":\"MultiPolygon\",\"coordinates\":["
         (str/join "," (map (fn [ps] (str "[" (ring ps) "]")) ok))
         "]}")))

(defn- ->num [s]
  #?(:clj  (Double/parseDouble s)
     :cljs (js/parseFloat s)))

(defn- drop-repeat [ps]
  (let [v (vec ps)]
    (cond-> v (and (< 1 (count v)) (= (first v) (peek v))) pop)))

(def ^:private num-char-re #"[0-9eE.+-]")

(defn- num-char?
  [c]
  (boolean (re-matches num-char-re (str c))))

(defn- coords-text
  [s]
  (when-let [i (str/index-of s "coordinates")]
    (when-let [o (str/index-of s "[" i)]
      (loop [j o d 0]
        (cond
          (>= j (count s))    (subs s o)
          (= \[ (nth s j))    (recur (inc j) (inc d))
          (= \] (nth s j))    (if (= 1 d) (subs s o (inc j)) (recur (inc j) (dec d)))
          :else               (recur (inc j) d))))))

(defn- geojson-rings
  [s]
  (if-let [t (coords-text s)]
    (loop [i 0 stack [] num nil rings []]
      (if (>= i (count t))
        rings
        (let [c    (nth t i)
              top  (dec (count stack))
              flush (fn [poly]
                      (if (and num (seq poly)) (update-in poly [top :nums] conj (->num num)) poly))]
          (cond
            (= \[ c) (recur (inc i) (conj stack {:nums [] :pairs []}) nil rings)
            (= \, c) (recur (inc i) (flush stack) nil rings)
            (= \] c) (let [poly   (flush stack)
                          frame (peek poly)
                          up    (pop poly)]
                      (cond
                        (and (= 2 (count (:nums frame))) (empty? (:pairs frame)))
                        (recur (inc i)
                               (if (seq up)
                                 (update-in up [(dec (count up)) :pairs] conj (vec (:nums frame)))
                                 up)
                               nil rings)

                        (seq (:pairs frame))
                        (recur (inc i) up nil (conj rings (:pairs frame)))

                        :else (recur (inc i) up nil rings)))
            (num-char? c) (recur (inc i) stack (str num c) rings)
            :else         (recur (inc i) stack num rings)))))
    []))

(defn- ->points [ps]
  (drop-repeat (map point (remove nil? ps))))

(defn parse-rings
  [v]
  (let [rings (cond
                (map? v)        [(:points v)]
                (sequential? v) [(filter #(and (sequential? %) (= 2 (count %))) v)]
                (nil? v)        []
                :else
                (let [s (str v)]
                  (cond
                    (str/includes? s "coordinates") (geojson-rings s)
                    (str/includes? s "Polygon")     []
                    :else [(map (fn [[_ x y]] [(->num x) (->num y)])
                                (re-seq #"(-?[\d.]+)\s*,\s*(-?[\d.]+)" s))])))]
    (into [] (comp (map ->points)
                   (map (fn [ps] (state ps (<= min-points (count ps))))))
          rings)))

(defn parse [v]
  (or (first (parse-rings v)) (state)))

(def ^:private ident [1.0 0.0 0.0 1.0 0.0 0.0])

(defn- mat* [[a b c d e f] [a2 b2 c2 d2 e2 f2]]
  [(+ (* a a2) (* c b2))
   (+ (* b a2) (* d b2))
   (+ (* a c2) (* c d2))
   (+ (* b c2) (* d d2))
   (+ (* a e2) (* c f2) e)
   (+ (* b e2) (* d f2) f)])

(defn- at-mat [[a b c d e f] [x y]]
  [(+ (* a x) (* c y) e) (+ (* b x) (* d y) f)])

(def ^:private num-src "-?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?")
(def ^:private num-re (re-pattern num-src))
(def ^:private path-token-re (re-pattern (str "[MmLlHhVvCcSsQqTtAaZz]|" num-src)))

(defn- nums [s]
  (mapv ->num (re-seq num-re (str s))))

(defn- deg->rad [d] (* d (/ Math/PI 180.0)))

(defn- rot-mat [a]
  (let [r (deg->rad a) co (Math/cos r) si (Math/sin r)]
    [co si (- si) co 0.0 0.0]))

(defn- ->mat [s]
  (reduce
   (fn [m [_ op args]]
     (let [v (nums args)
           g (fn [i d] (nth v i d))]
       (mat* m
             (case op
               "translate" [1.0 0.0 0.0 1.0 (g 0 0.0) (g 1 0.0)]
               "scale"     (let [sx (g 0 1.0)] [sx 0.0 0.0 (g 1 sx) 0.0 0.0])
               "rotate"    (if (< 1 (count v))
                             (-> [1.0 0.0 0.0 1.0 (g 1 0.0) (g 2 0.0)]
                                 (mat* (rot-mat (g 0 0.0)))
                                 (mat* [1.0 0.0 0.0 1.0 (- (g 1 0.0)) (- (g 2 0.0))]))
                             (rot-mat (g 0 0.0)))
               "matrix"    (if (= 6 (count v)) v ident)
               ident))))
   ident
   (re-seq #"([A-Za-z]+)\s*\(([^)]*)\)" (str s))))

(def ^:private attr-re #"([\w:.-]+)\s*=\s*(?:\"([^\"]*)\"|'([^']*)')")

(defn- attrs [s]
  (into {} (map (fn [[_ k dq sq]] [k (or dq sq)])) (re-seq attr-re (str s))))

(def ^:private cmd-arity
  {"M" 2 "L" 2 "T" 2 "H" 1 "V" 1 "C" 6 "S" 4 "Q" 4 "A" 7 "Z" 0})

(defn- path-cmds [d]
  (loop [ts  (re-seq path-token-re (str d))
         out []]
    (if-let [t (first ts)]
      (if (re-matches #"[A-Za-z]" t)
        (let [[as more] (split-with #(not (re-matches #"[A-Za-z]" %)) (rest ts))]
          (recur more (conj out [t (mapv ->num as)])))
        (recur (rest ts) out))
      out)))

(def ^:private two-thirds (/ 2.0 3.0))

(defn- bezier3 [[x0 y0] [x1 y1] [x2 y2] [x3 y3]]
  (for [t [0.25 0.5 0.75 1.0]]
    (let [u (- 1.0 t)
          a (* u u u) b (* 3 u u t) c (* 3 u t t) d (* t t t)]
      [(+ (* a x0) (* b x1) (* c x2) (* d x3))
       (+ (* a y0) (* b y1) (* c y2) (* d y3))])))

(defn- chunks [n v]
  (if (zero? n) [[]] (partition n n nil v)))

(defn- path->rings
  [d]
  (let [close (fn [rings ring] (cond-> rings (<= min-points (count ring)) (conj ring)))]
    (loop [cs (path-cmds d) cur [0.0 0.0] origin [0.0 0.0] ring [] rings [] ctrl nil]
      (if-let [[c args] (first cs)]
        (let [rel? (= c (str/lower-case c))
              C    (str/upper-case c)
              n    (cmd-arity C 2)]
          (if (= "Z" C)
            (recur (rest cs) origin origin [] (close rings ring) nil)
            (let [[cur' origin' ring' rings' ctrl']
                  (reduce
                   (fn [[cur origin ring rings _] [i as]]
                     (let [rp (fn [[x y]] (if rel? [(+ (first cur) x) (+ (second cur) y)] [x y]))
                           as (vec as)
                           K  (if (and (= "M" C) (pos? i)) "L" C)]
                       (case K
                         "M" (let [p (rp [(as 0) (as 1)])] [p p [p] (close rings ring) nil])
                         "L" (let [p (rp [(as 0) (as 1)])] [p origin (conj ring p) rings nil])
                         "H" (let [p [(if rel? (+ (first cur) (as 0)) (as 0)) (second cur)]]
                               [p origin (conj ring p) rings nil])
                         "V" (let [p [(first cur) (if rel? (+ (second cur) (as 0)) (as 0))]]
                               [p origin (conj ring p) rings nil])
                         "C" (let [c1 (rp [(as 0) (as 1)]) c2 (rp [(as 2) (as 3)])
                                   p  (rp [(as 4) (as 5)])]
                               [p origin (into ring (bezier3 cur c1 c2 p)) rings c2])
                         "S" (let [c1 (if ctrl
                                        [(- (* 2 (first cur)) (first ctrl))
                                         (- (* 2 (second cur)) (second ctrl))]
                                        cur)
                                   c2 (rp [(as 0) (as 1)]) p (rp [(as 2) (as 3)])]
                               [p origin (into ring (bezier3 cur c1 c2 p)) rings c2])
                         "Q" (let [q (rp [(as 0) (as 1)]) p (rp [(as 2) (as 3)])
                                   c1 [(+ (first cur) (* two-thirds (- (first q) (first cur))))
                                       (+ (second cur) (* two-thirds (- (second q) (second cur))))]
                                   c2 [(+ (first p) (* two-thirds (- (first q) (first p))))
                                       (+ (second p) (* two-thirds (- (second q) (second p))))]]
                               [p origin (into ring (bezier3 cur c1 c2 p)) rings q])
                         "T" (let [p (rp [(as 0) (as 1)])] [p origin (conj ring p) rings nil])
                         "A" (let [p (rp [(as 5) (as 6)])] [p origin (conj ring p) rings nil])
                         [cur origin ring rings nil])))
                   [cur origin ring rings ctrl]
                   (map-indexed vector (filter #(= n (count %)) (chunks n args))))]
              (recur (rest cs) cur' origin' ring' rings' ctrl'))))
        (close rings ring)))))

(defn- shape->rings
  [tag a]
  (case tag
    ("polygon" "polyline") (let [v (nums (get a "points"))]
                             (when (<= 6 (count v))
                               [(mapv vec (partition 2 v))]))
    "rect" (let [x (or (first (nums (get a "x"))) 0.0)
                 y (or (first (nums (get a "y"))) 0.0)
                 w (first (nums (get a "width")))
                 h (first (nums (get a "height")))]
             (when (and w h (pos? w) (pos? h))
               [[[x y] [(+ x w) y] [(+ x w) (+ y h)] [x (+ y h)]]]))
    "path" (seq (path->rings (get a "d")))
    nil))

(defn- label-of [a]
  (some (fn [k] (let [v (get a k)] (when-not (str/blank? v) v)))
        ["inkscape:label" "sutra:label" "data-label" "id"]))

(defn- extent-of
  [a]
  (let [vb (nums (get a "viewBox"))
        wh (fn [k] (first (nums (get a k))))]
    (if (= 4 (count vb))
      {:x (nth vb 0) :y (nth vb 1) :w (nth vb 2) :h (nth vb 3)}
      {:x 0.0 :y 0.0 :w (or (wh "width") 1000.0) :h (or (wh "height") 1000.0)})))

(def ^:private hidden-tags #{"defs" "clipPath" "mask" "marker" "symbol" "pattern"})

(defn svg->plan
  [svg]
  (let [s     (str svg)
        root  (attrs (second (re-find #"(?s)<\s*svg\b([^>]*)>" s)))
        {:keys [x y w h]} (extent-of root)
        base  [1.0 0.0 0.0 1.0 (- x) (- y)]]
    {:extent {:w w :h h}
     :rings
     (:rings
      (reduce
       (fn [{:keys [stack skip] :as acc} [_ closing? tag body self?]]
         (let [top (peek stack)]
           (cond
             (hidden-tags tag)
             (cond-> acc
               (str/blank? self?)
               (update :skip (if (seq closing?) (fn [n] (max 0 (dec n))) inc)))

             (pos? skip) acc

             (and (= "g" tag) (seq closing?)) (cond-> acc (< 1 (count stack)) (update :stack pop))
             (and (= "g" tag) (str/blank? self?))
             (update acc :stack conj (mat* top (->mat (get (attrs body) "transform"))))

             (seq closing?) acc

             :else
             (let [a  (attrs body)
                   m  (mat* top (->mat (get a "transform")))
                   rs (shape->rings tag a)]
               (cond-> acc
                 (seq rs)
                 (update :rings into
                         (map (fn [ps] {:label (label-of a)
                                        :points (mapv #(at-mat m (point %)) ps)}))
                         rs))))))
       {:stack [base] :rings [] :skip 0}
       (re-seq #"<\s*(/?)\s*([A-Za-z][\w:.-]*)([^>]*?)(/?)\s*>" s)))}))

(defn- esc [s]
  (-> (str s)
      (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")))

(defn plan->svg
  [{:keys [extent rings title]}]
  (let [w (or (:w extent) 1000.0)
        h (or (:h extent) 1000.0)]
    (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
         "<svg xmlns=\"http://www.w3.org/2000/svg\""
         " xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\""
         " width=\"" (n->s w) "\" height=\"" (n->s h) "\""
         " viewBox=\"0 0 " (n->s w) " " (n->s h) "\">\n"
         "<title>" (esc (or title "Grundriss")) "</title>\n"
         "<g inkscape:groupmode=\"layer\" inkscape:label=\"" (esc (or title "Grundriss")) "\">\n"
         (str/join
          "\n"
          (keep (fn [{:keys [label points]}]
                  (when (<= min-points (count points))
                    (str "<polygon inkscape:label=\"" (esc (or label "")) "\""
                         " points=\"" (->str {:points points} :points) "\""
                         " fill=\"none\" stroke=\"#334155\" stroke-width=\""
                         (n->s (/ (max w h) 400.0)) "\"/>")))
                rings))
         "\n</g>\n</svg>\n")))

(defn plan->json
  [{:keys [extent rings]}]
  (str "{\"extent\":{\"w\":" (n->s (or (:w extent) 0)) ",\"h\":" (n->s (or (:h extent) 0)) "},"
       "\"rings\":["
       (str/join ","
                 (keep (fn [{:keys [label points]}]
                         (when (<= min-points (count points))
                           (str "{\"label\":\""
                                (-> (str (or label "")) (str/replace "\\" "")
                                    (str/replace "\"" "'"))
                                "\",\"points\":\"" (->str {:points points} :points) "\"}")))
                       rings))
       "]}"))

(defn- label-key [v]
  (let [s (some-> v str str/trim str/lower-case)]
    (when-not (str/blank? s) s)))

(defn match-rings
  [rings shapes]
  (let [haeufigkeit (frequencies (keep (comp label-key :label) shapes))
        eindeutig   (into {} (keep (fn [s]
                                     (let [n (label-key (:label s))]
                                       (when (and n (:key s) (= 1 (get haeufigkeit n)))
                                         [n s])))
                                   shapes))
        bekannt     (into #{} (keep (comp label-key :label)) shapes)]
    (reduce (fn [acc r]
              (let [n (label-key (:label r))]
                (cond
                  (get eindeutig n)
                  (update acc :matched conj
                          [(:key (get eindeutig n)) (:points (state (:points r) true))])

                  (and n (< 1 (get haeufigkeit n 0)))
                  (update acc :ambiguous inc)

                  (contains? bekannt n) acc

                  :else (update acc :unmatched conj r))))
            {:matched [] :ambiguous 0 :unmatched []}
            rings)))

(defn stack-layout
  [plans]
  (when (seq plans)
    (let [plans (sort-by (juxt #(or (:order %) ##Inf) #(or (:label %) "")) plans)
          gap   (* 0.1 (apply max (map :h plans)))
          ys    (reductions + gap (map #(+ (:h %) gap) plans))
          ps    (mapv #(assoc %1 :y %2) plans ys)
          {:keys [y h]} (peek ps)]
      {:frames ps
       :extent {:w (apply max (map :w ps)) :h (+ y h)}})))
