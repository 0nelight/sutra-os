(ns sutra.ref.geo
  (:require [clojure.string :as str]))

(def ops #{:within-distance :within-bounding-box})

(def earth-radius-m 6371008.8)

(def unit-m
  {:m 1.0 :km 1000.0 :mi 1609.344 :nmi 1852.0 :ft 0.3048 :yd 0.9144})

(defn- num* [x]
  (cond (number? x) (double x)
        (string? x) (let [s (str/trim x)]
                      (when (re-matches #"[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?" s)
                        #?(:clj (Double/parseDouble s) :cljs (js/parseFloat s))))
        :else nil))

(defn point
  [p]
  (let [[a b] (cond (string? p)     (str/split p #",")
                    (map? p)        [(or (:lat p) (get p "lat")) (or (:lon p) (get p "lon"))]
                    (sequential? p) p)
        lat (num* a) lon (num* b)]
    (when (and lat lon (or (not (string? p)) (= 2 (count (str/split p #",")))))
      [lat lon])))

(defn field? [f]
  (or (keyword? f) (and (vector? f) (= 2 (count f)) (every? keyword? f))))

(defn field-attrs [f] (if (vector? f) f [f]))

(defn- unit-kw [u] (cond (keyword? u) u (string? u) (keyword (str/lower-case u)) :else u))

(defn- param? [x] (and (vector? x) (= :param (first x))))

(defn- point-problem [k p]
  (if-let [[lat lon] (point p)]
    (cond (not (<= -90.0 lat 90.0))   (str k " latitude " lat " is outside -90..90")
          (not (<= -180.0 lon 180.0)) (str k " longitude " lon " is outside -180..180"))
    (str k " must be a point — [lat lon], {:lat :lon} or \"lat,lon\" — got " (pr-str p))))

(defn explain
  [op field v]
  (cond
    (not (field? field))
    (str op " filters a geopoint attribute or a [lat-attr lon-attr] pair, got " (pr-str field))

    (param? v) nil

    (not (map? v))
    (str op " takes a map, got " (pr-str v))

    (= op :within-distance)
    (let [d (num* (:distance v)) u (unit-kw (:unit v :km))]
      (or (when-not (param? (:center v)) (point-problem :center (:center v)))
          (when-not (or (param? (:distance v)) (and d (>= d 0.0)))
            (str ":distance must be a number ≥ 0, got " (pr-str (:distance v))))
          (when-not (unit-m u) (str ":unit " (pr-str (:unit v)) " is not one of "
                                    (str/join " " (sort (keys unit-m)))))))

    (= op :within-bounding-box)
    (or (when-not (param? (:top-left v)) (point-problem :top-left (:top-left v)))
        (when-not (param? (:bottom-right v)) (point-problem :bottom-right (:bottom-right v)))
        (when-not (some param? [(:top-left v) (:bottom-right v)])
          (let [[n _] (point (:top-left v)) [s _] (point (:bottom-right v))]
            (when (< n s)
              (str ":top-left latitude " n " is south of :bottom-right latitude " s)))))))

(defn normalize
  [op v]
  (case op
    :within-distance     {:center (point (:center v))
                          :meters (* (num* (:distance v)) (unit-m (unit-kw (:unit v :km))))}
    :within-bounding-box {:top-left (point (:top-left v)) :bottom-right (point (:bottom-right v))}))

(defn- rad [x] (* x (/ #?(:clj Math/PI :cljs js/Math.PI) 180.0)))

(defn meters
  [[lat1 lon1] [lat2 lon2]]
  (let [h (+ (Math/pow (Math/sin (/ (rad (- lat2 lat1)) 2)) 2)
             (* (Math/cos (rad lat1)) (Math/cos (rad lat2))
                (Math/pow (Math/sin (/ (rad (- lon2 lon1)) 2)) 2)))]
    (* 2 earth-radius-m (Math/asin (Math/sqrt (min 1.0 h))))))

(defn in-box? [[lat lon] [[n w] [s e]]]
  (and (<= s lat n)
       (if (<= w e) (<= w lon e) (or (>= lon w) (<= lon e)))))

(defn object-point
  [get-value field]
  (if (vector? field)
    (let [[la lo] field] (point [(num* (get-value la)) (num* (get-value lo))]))
    (some-> (get-value field) point)))

(defn match?
  [op p v]
  (boolean
   (when p
     (let [n (normalize op v)]
       (case op
         :within-distance     (<= (meters p (:center n)) (:meters n))
         :within-bounding-box (in-box? p [(:top-left n) (:bottom-right n)]))))))
