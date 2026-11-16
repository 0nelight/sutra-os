(ns ^:unit apps.map-point-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.map-geo :as geo]
            [sutra.capps.map-spec :as ms]))

(deftest a-plan-point-is-x-first
  (is (= [12.5 3.0] (geo/parse-xy "12.5,3")))
  (is (= [12.5 3.0] (geo/parse-xy " 12.5 , 3 ")))
  (is (= [1 2] (geo/parse-xy "{\"type\":\"Point\",\"coordinates\":[1,2]}")))
  (is (= [4 5] (geo/parse-xy {:x 4 :y 5})))
  (is (nil? (geo/parse-xy "")))
  (is (nil? (geo/parse-xy nil)))
  (testing "a polygon is no point"
    (is (nil? (geo/parse-xy "{\"type\":\"Polygon\",\"coordinates\":[[[0,0],[1,0],[1,1],[0,0]]]}"))))
  (testing "a geopoint stays lat,lon"
    (is (= [3.0 12.5] (geo/parse-point "12.5,3")))))

(deftest a-point-geometry-reads-xy-from-the-row
  (is (= [7.0 2.0] (ms/point-of {:geometry :point :xy :position} {:position "7,2"})))
  (is (= [7.0 2.0] (ms/point-of {:geometry :point :xy :position}
                                {:values {:position "7,2"}})))
  (is (nil? (ms/point-of {:geometry :point :xy :position} {:position nil}))))

(defn- run [opts events] (reduce #(ms/step %1 %2 opts) {} events))

(deftest a-drawn-point-goes-to-an-action-as-x-comma-y
  (let [st (run {:crs :plan} [[:tool :point] [:click [12.25 3.5]]])
        p  (first (:shapes st))]
    (is (= {:type "Point" :coordinates [12.25 3.5]} p))
    (is (= "12.25,3.5" (ms/shape-text p :points)))
    (testing "under :geometry it stays GeoJSON, which a plan point reads as well"
      (is (= [12.25 3.5] (geo/parse-xy (ms/shape-text p :geometry)))))
    (testing "a polygon under :points is unchanged"
      (is (= "0,0 2,0 2,1"
             (ms/shape-text {:type "Polygon" :coordinates [[[0 0] [2 0] [2 1] [0 0]]]}
                            :points))))))

(deftest a-point-object-moves-by-its-handle
  (let [k  [0 "pc-1"]
        st (run {:crs :plan :tol 0.5}
                [[:edit-objects {k {:type "Point" :coordinates [1.0 1.0]}}]
                 [:press [1.1 1.0]] [:drag [4.0 2.0]] [:release]])]
    (is (= {k {:type "Point" :coordinates [4.0 2.0]}} (ms/changed-edits st)))
    (is (= "4,2" (ms/shape-text (get-in st [:editing k]) :points)))))

(deftest a-point-under-edit-is-drawn-by-the-edit-not-the-layer
  (let [layer {::ms/i 0 :rows [{:id "pc-1" :position "1,1"} {:id "pc-2" :position "5,5"}]
               :geometries [{:geometry :point :xy :position}]}
        feats (fn [editing]
                (-> (ms/layer-parts :plan layer {:editing editing})
                    :sources (get "sutra-l0-g0") :data :features count))]
    (is (= 2 (feats {})))
    (is (= 1 (feats {[0 "pc-1"] {:type "Point" :coordinates [1 1]}})))))
