(ns ^:unit apps.plan-faces-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.plan-faces :as pf]))

(def ^:private l-shape
  [[0 0] [10 0] [10 4] [6 4] [6 8] [0 8] [0 0]])

(defn- areas [r] (sort (map #(Math/round (* 100 (:area %))) (:faces r))))

(deftest an-outline-alone-is-one-free-face
  (let [r (pf/faces l-shape {} [])]
    (is (= [6400] (areas r)))
    (is (nil? (:room (first (:faces r)))))
    (is (empty? (:problems r)))))

(deftest a-wall-from-wall-to-wall-splits-the-floor
  (testing "a wall across the foot of the L"
    (is (= [1600 4800] (areas (pf/faces l-shape {} [[[6 0] [6 4]]]))))))

(deftest a-wall-that-ends-in-the-open-bounds-nothing
  (is (= [6400] (areas (pf/faces l-shape {} [[[6 0] [6 2]]])))))

(deftest a-room-on-the-outline-takes-its-face
  (let [r (pf/faces l-shape {:bad [[6 0] [10 0] [10 4] [6 4] [6 0]]} [])]
    (is (= [1600 4800] (areas r)))
    (is (= #{:bad nil} (set (map :room (:faces r)))))
    (is (empty? (:problems r)))))

(deftest a-wall-through-a-room-is-named
  (let [r (pf/faces l-shape {:bad [[6 0] [10 0] [10 4] [6 4] [6 0]]} [[[8 0] [8 4]]])]
    (is (= [[:cut :bad]] (:problems r)))))

(deftest a-room-outside-the-outline-is-named
  (is (= [[:outside :aussen]]
         (:problems (pf/faces l-shape {:aussen [[20 20] [22 20] [22 22] [20 20]]} [])))))

(deftest a-tap-near-a-wall-lands-on-it
  (let [segs (pf/ring-edges [l-shape])]
    (is (= [6.0 6.0] (pf/onto-wall segs [6.2 6.0] 0.3)))
    (is (= [6.0 4.0] (pf/onto-wall segs [6.1 3.9] 0.3)) "a corner wins over the wall")
    (is (nil? (pf/onto-wall segs [3 3] 0.3)))))

(deftest a-wall-runs-across-to-the-next
  (let [segs (pf/ring-edges [l-shape])]
    (is (= [6.0 4.0] (pf/across segs [6.0 0.0] (/ Math/PI 2))))
    (is (= [0.0 2.0] (pf/across segs [10.0 2.0] Math/PI)))))
