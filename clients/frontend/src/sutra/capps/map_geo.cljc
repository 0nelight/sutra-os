(ns sutra.capps.map-geo
  (:require [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])))

(def ^:private earth-radius-m 6371008.8)

(defn- rad [d] (* d (/ Math/PI 180)))

(defn distance-m
  [[lon1 lat1] [lon2 lat2]]
  (let [dl (rad (- lat2 lat1))
        dn (rad (- lon2 lon1))
        a  (+ (Math/pow (Math/sin (/ dl 2)) 2)
              (* (Math/cos (rad lat1)) (Math/cos (rad lat2))
                 (Math/pow (Math/sin (/ dn 2)) 2)))]
    (* 2 earth-radius-m (Math/atan2 (Math/sqrt a) (Math/sqrt (- 1 a))))))

(defn line-length-m
  [coords]
  (reduce + 0 (map distance-m coords (rest coords))))

(defn segment-lengths-m
  [coords]
  (mapv distance-m coords (rest coords)))

(defn- closed
  [ring]
  (if (= (first ring) (peek (vec ring))) (vec ring) (conj (vec ring) (first ring))))

(defn perimeter-m [ring] (line-length-m (closed ring)))

(defn area-m2
  [ring]
  (let [r (closed ring)
        s (reduce + 0 (map (fn [[lon1 lat1] [lon2 lat2]]
                             (* (rad (- lon2 lon1))
                                (+ 2 (Math/sin (rad lat1)) (Math/sin (rad lat2)))))
                           r (rest r)))]
    (Math/abs (/ (* s earth-radius-m earth-radius-m) 2))))

(defn format-length
  [m]
  (if (< m 1000) (str (Math/round (double m)) " m") (str (/ (Math/round (/ m 10.0)) 100.0) " km")))

(defn format-area
  [m2]
  (if (< m2 1000000)
    (str (Math/round (double m2)) " m²")
    (str (/ (Math/round (/ m2 10000.0)) 100.0) " km²")))

(defn parse-json
  [s]
  (when-not (str/blank? (str s))
    (try
      #?(:cljs (js->clj (js/JSON.parse s) :keywordize-keys true)
         :clj  (json/read-str s :key-fn keyword))
      (catch #?(:cljs :default :clj Exception) _ nil))))

(defn write-json
  [x]
  #?(:cljs (js/JSON.stringify (clj->js x))
     :clj  (json/write-str x)))

(defn- geometry-of
  [g]
  (case (:type g)
    "FeatureCollection"  (mapcat geometry-of (:features g))
    "Feature"            (geometry-of (:geometry g))
    "GeometryCollection" (mapcat geometry-of (:geometries g))
    nil                  nil
    [g]))

(defn geometries
  [x]
  (vec (geometry-of (if (string? x) (parse-json x) x))))

(defn output
  [shapes kind]
  (write-json
   (if (= :geometry-collection kind)
     {:type "GeometryCollection" :geometries (vec shapes)}
     {:type "FeatureCollection"
      :features (mapv (fn [g] {:type "Feature" :geometry g :properties {}}) shapes)})))

(defn parse-point
  [x]
  (cond
    (map? x)    (cond (and (:lat x) (or (:lon x) (:lng x))) [(or (:lon x) (:lng x)) (:lat x)]
                      (= "Point" (:type x))                 (vec (:coordinates x)))
    (string? x) (if-let [[_ lat lon] (re-find #"^\s*(-?[\d.]+)\s*,\s*(-?[\d.]+)\s*$" x)]
                  [(parse-double lon) (parse-double lat)]
                  (some-> (first (geometries x)) parse-point))))

(defn parse-xy
  [x]
  (cond
    (map? x)    (cond (and (some? (:x x)) (some? (:y x))) [(:x x) (:y x)]
                      (= "Point" (:type x))               (vec (:coordinates x)))
    (string? x) (if-let [[_ a b] (re-find #"^\s*(-?[\d.]+)\s*,\s*(-?[\d.]+)\s*$" x)]
                  [(parse-double a) (parse-double b)]
                  (let [g (first (geometries x))]
                    (when (= "Point" (:type g)) (vec (:coordinates g)))))))

(defn point-in-ring?
  [[x y] ring]
  (let [r (closed ring)]
    (odd? (count (filter (fn [[[x1 y1] [x2 y2]]]
                           (and (not= (> y1 y) (> y2 y))
                                (< x (+ x1 (/ (* (- x2 x1) (- y y1)) (- y2 y1))))))
                         (map vector r (rest r)))))))

(defn- rings-of
  [{:keys [type coordinates]}]
  (case type
    "Polygon"      [(first coordinates)]
    "MultiPolygon" (mapv first coordinates)
    []))

(defn- vertices
  [{:keys [type coordinates]}]
  (case type
    "Point"           [coordinates]
    "MultiPoint"      coordinates
    "LineString"      coordinates
    "MultiLineString" (apply concat coordinates)
    "Polygon"         (apply concat coordinates)
    "MultiPolygon"    (apply concat (apply concat coordinates))
    []))

(defn point-in-shape?
  [pt shape]
  (boolean (some #(point-in-ring? pt %) (rings-of shape))))

(defn intersects?
  [geom shape]
  (or (some #(point-in-shape? % shape) (vertices geom))
      (some #(point-in-shape? % geom) (vertices shape))))

(defn bbox
  [coords]
  (when (seq coords)
    (let [xs (map first coords) ys (map second coords)]
      [(apply min xs) (apply min ys) (apply max xs) (apply max ys)])))

(defn inside-bbox?
  [[w s e n] [x y]]
  (and (<= w x e) (<= s y n)))

(defn bbox->polygon
  [[w s e n]]
  {:type "Polygon" :coordinates [[[w s] [e s] [e n] [w n] [w s]]]})

(defn- crossing-lat
  [[x1 y1] [x2 y2] edge]
  (let [x2* (if (neg? x2) (+ x2 360) x2)
        x1* (if (neg? x1) (+ x1 360) x1)]
    (+ y1 (* (- y2 y1) (/ (- edge x1*) (- x2* x1*))))))

(defn split-at-antimeridian
  [coords]
  (reduce
   (fn [parts [a b]]
     (if (> (Math/abs (double (- (first b) (first a)))) 180)
       (let [lat  (crossing-lat a b 180)
             sign (if (pos? (first a)) 180 -180)]
         (-> (vec (butlast parts))
             (conj (conj (peek parts) [sign lat]))
             (conj [[(- sign) lat] b])))
       (conj (vec (butlast parts)) (conj (peek parts) b))))
   [[(first coords)]]
   (map vector coords (rest coords))))

(defn split-shape
  [{:keys [type coordinates] :as shape}]
  (case type
    "LineString" (let [parts (split-at-antimeridian coordinates)]
                   (if (next parts) {:type "MultiLineString" :coordinates parts} shape))
    "Polygon"    (let [parts (split-at-antimeridian (first coordinates))]
                   (if (next parts)
                     {:type "MultiPolygon"
                      :coordinates (mapv (fn [p] [(conj (vec p) (first p))]) parts)}
                     shape))
    shape))

(def ^:private m-per-deg 111319.49079327357)

(defn plan->lnglat
  [[x y]]
  [(/ x m-per-deg) (- (/ y m-per-deg))])

(defn lnglat->plan [[lon lat]] [(* lon m-per-deg) (- (* lat m-per-deg))])

(defn map-coords
  [f {:keys [type] :as g}]
  (let [ring (fn [r] (mapv f r))]
    (case type
      "Point"                         (update g :coordinates f)
      ("MultiPoint" "LineString")     (update g :coordinates ring)
      ("MultiLineString" "Polygon")   (update g :coordinates #(mapv ring %))
      "MultiPolygon"                  (update g :coordinates #(mapv (fn [p] (mapv ring p)) %))
      "GeometryCollection"            (update g :geometries #(mapv (partial map-coords f) %))
      g)))

(defn- euclid [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1) dy (- y2 y1)] (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn- shoelace [ring]
  (let [r (closed ring)]
    (/ (reduce + 0 (map (fn [[x1 y1] [x2 y2]] (- (* x1 y2) (* x2 y1))) r (rest r))) 2)))

(defn distance [crs a b] (if (= :plan crs) (euclid a b) (distance-m a b)))

(defn line-length [crs coords] (reduce + 0 (map #(distance crs %1 %2) coords (rest coords))))

(defn segment-lengths [crs coords] (mapv #(distance crs %1 %2) coords (rest coords)))

(defn perimeter [crs ring] (line-length crs (closed ring)))

(defn area [crs ring] (if (= :plan crs) (Math/abs (double (shoelace ring))) (area-m2 ring)))

(defn- round2 [v] (/ (Math/round (* 100.0 (double v))) 100.0))

(defn format-plan-length [v unit] (str (round2 v) " " (or unit "m")))

(defn format-plan-area [v unit] (str (round2 v) " " (or unit "m") "²"))

(defn open-ring
  [ring]
  (let [r (vec ring)] (if (and (< 1 (count r)) (= (first r) (peek r))) (pop r) r)))

(defn centroid
  [coords]
  (when (seq coords)
    (let [n (count coords)]
      [(/ (reduce + (map first coords)) n) (/ (reduce + (map second coords)) n)])))

(defn- local-frame
  [crs [cx cy]]
  (if (= :plan crs)
    [identity identity]
    (let [k (* m-per-deg (Math/cos (rad cy)))]
      [(fn [[lon lat]] [(* (- lon cx) k) (* (- lat cy) m-per-deg)])
       (fn [[x y]] [(+ cx (/ x k)) (+ cy (/ y m-per-deg))])])))

(defn- shape-coords [g] (vertices g))

(defn translate
  [g [dx dy]]
  (map-coords (fn [[x y]] [(+ x dx) (+ y dy)]) g))

(defn rotate
  [crs g radians]
  (if-let [c (centroid (shape-coords g))]
    (let [[to from] (local-frame crs c)
          co (Math/cos radians) si (Math/sin radians)]
      (map-coords (fn [p] (let [[x y] (to p)]
                            (from [(- (* x co) (* y si)) (+ (* x si) (* y co))])))
                  g))
    g))

(defn- line-cross
  [[ax ay] [bx by] [cx cy] [dx dy]]
  (let [r1x (- bx ax) r1y (- by ay) r2x (- dx cx) r2y (- dy cy)
        den (- (* r1x r2y) (* r1y r2x))]
    (when (> (Math/abs (double den)) 1e-12)
      (let [t (/ (- (* (- cx ax) r2y) (* (- cy ay) r2x)) den)]
        [(+ ax (* t r1x)) (+ ay (* t r1y))]))))

(defn buffer-ring
  [crs ring d]
  (let [open (open-ring ring)]
    (if (< (count open) 3)
      ring
      (let [[to from] (local-frame crs (centroid open))
            pts  (mapv to open)
            n    (count pts)
            sign (if (pos? (shoelace pts)) 1 -1)
            edge (fn [i]
                   (let [[ax ay :as a] (nth pts i)
                         [bx by :as b] (nth pts (mod (inc i) n))
                         len (max 1e-12 (euclid a b))
                         nx  (* sign (/ (- by ay) len))
                         ny  (* sign (/ (- ax bx) len))
                         off (fn [[x y]] [(+ x (* d nx)) (+ y (* d ny))])]
                     [(off a) (off b)]))
            es   (mapv edge (range n))
            out  (mapv (fn [i]
                         (let [[a b] (nth es (mod (dec i) n))
                               [c e] (nth es i)]
                           (or (line-cross a b c e) c)))
                       (range n))]
        (closed (mapv from out))))))

(defn buffer
  [crs {:keys [type] :as g} d]
  (case type
    "Polygon"      (update g :coordinates #(into [(buffer-ring crs (first %) d)] (rest %)))
    "MultiPolygon" (update g :coordinates
                           #(mapv (fn [p] (into [(buffer-ring crs (first p) d)] (rest p))) %))
    g))

(defn circle
  [crs c p]
  (let [[to from] (local-frame crs c)
        r (euclid (to c) (to p))]
    {:type "Polygon"
     :coordinates [(closed (mapv (fn [i]
                                   (let [a (* 2 Math/PI (/ i 64))]
                                     (from [(* r (Math/cos a)) (* r (Math/sin a))])))
                                 (range 64)))]}))

(defn- cross-z [[ax ay] [bx by] [cx cy]]
  (- (* (- bx ax) (- cy ay)) (* (- by ay) (- cx ax))))

(defn- sgn [v] (cond (> v 1e-12) 1 (< v -1e-12) -1 :else 0))

(defn segments-cross?
  [a b c d]
  (let [d1 (sgn (cross-z c d a)) d2 (sgn (cross-z c d b))
        d3 (sgn (cross-z a b c)) d4 (sgn (cross-z a b d))]
    (and (not= 0 d1) (not= 0 d2) (not= 0 d3) (not= 0 d4) (not= d1 d2) (not= d3 d4))))

(defn simple-ring?
  [ring]
  (let [r  (closed (open-ring ring))
        es (vec (map vector r (rest r)))
        n  (count es)]
    (not-any? (fn [[i j]] (let [[a b] (nth es i) [c e] (nth es j)] (segments-cross? a b c e)))
              (for [i (range n) j (range (inc i) n)] [i j]))))

(defn clusters
  [points zoom cell-px]
  (let [deg (/ (* 360.0 cell-px) (* 256.0 (Math/pow 2 (max 0 (or zoom 1)))))
        key (fn [{[x y] :at}] [(Math/floor (/ x deg)) (Math/floor (/ y deg))])]
    (mapv (fn [[_ ps]]
            (let [n (count ps)]
              {:at    [(/ (reduce + (map (comp first :at) ps)) n)
                       (/ (reduce + (map (comp second :at) ps)) n)]
               :count n
               :rows  (mapv :row ps)}))
          (group-by key points))))
