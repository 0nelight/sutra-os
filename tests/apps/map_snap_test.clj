(ns ^:unit apps.map-snap-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.map-spec :as ms]))

(def ^:private plan {:crs :plan :tol 0.3 :snap-dirs []})

(defn- run [opts events]
  (reduce #(ms/step %1 %2 opts) {} events))

(deftest an-edge-near-an-axis-locks-onto-it
  (is (= [[0 0] [4.01 0.0]]
         (:draft (run plan [[:tool :polygon] [:click [0 0]] [:click [4.0 0.25]]])))))

(deftest a-typed-edge-turns-square-against-the-last
  (let [st (run plan [[:tool :polygon] [:click [0 0]] [:edge 4 :right] [:edge 3 :left-square]])]
    (is (= [[0 0] [4.0 0.0] [4.0 3.0]] (:draft st)))
    (testing "the next edge keeps parallel to the first"
      (is (= [0.09 3.0] (peek (:draft (ms/step st [:click [0.1 3.2]] plan))))))))

(deftest an-edge-lines-up-with-a-wall-on-the-plan
  (let [wall [[0 0] [3 1]]
        st   (run (assoc plan :snap-dirs [wall]) [[:tool :line] [:click [5 5]] [:click [8.1 5.9]]])]
    (is (= [8.06 6.02] (peek (:draft st))))))

(deftest without-snapping-a-tap-lands-where-it-is
  (is (= [4 0.25]
         (peek (:draft (run (dissoc plan :snap-dirs)
                            [[:tool :line] [:click [0 0]] [:click [4 0.25]]]))))))

(deftest a-tap-on-the-first-point-closes-the-outline
  (let [st (run plan [[:tool :polygon] [:click [0 0]] [:edge 4 :right] [:edge 3 :left-square]
                      [:edge 4 :left-square] [:click [0.05 0.05]]])]
    (is (= [[[0 0] [4.0 0.0] [4.0 3.0] [0.0 3.0] [0 0]]]
           (:coordinates (first (:shapes st)))))))

(deftest the-first-point-lands-on-the-frame
  (let [frame [[[0 0] [10 0]] [[10 0] [10 8]] [[10 8] [0 8]] [[0 8] [0 0]]]
        opts  (assoc plan :start-segs frame)]
    (is (= [0.0 3.0] (first (:draft (run opts [[:tool :polygon] [:click [0.2 3]]])))))
    (is (= [10.0 0.0] (first (:draft (run opts [[:tool :polygon] [:click [9.8 0.1]]])))))
    (testing "off the frame it stays where it is"
      (is (= [5 4] (first (:draft (run opts [[:tool :polygon] [:click [5 4]]]))))))))

(defn- poly [& pts] (str "{\"type\":\"Polygon\",\"coordinates\":[["
                         (apply str (interpose "," (map (fn [[x y]] (str "[" x "," y "]"))
                                                       (concat pts [(first pts)]))))
                         "]]}"))

(deftest a-wall-through-a-room-leaves-it-its-larger-part
  (let [shape {:geometry :shape :shape :umriss}
        cfg   {:crs :plan
               :layers [{:role :outline :geometries [shape]
                         :rows [{:id "o" :values {:umriss (poly [0 0] [10 0] [10 6] [0 6])}}]}
                        {:role :rooms :geometries [shape]
                         :rows [{:id "r1" :values {:umriss (poly [0 0] [6 0] [6 6] [0 6])}}]}]}
        areas (ms/plan-areas cfg [{:type "LineString" :coordinates [[4 0] [4 6]]}])
        [cut] (ms/room-cuts areas)]
    (is (= [1 "r1"] (:room cut)))
    (is (= 2 (:parts cut)))
    (is (= #{[0.0 0.0] [4.0 0.0] [4.0 6.0] [0.0 6.0]}
           (set (map (fn [[x y]] [(double x) (double y)]) (:ring cut)))))))
