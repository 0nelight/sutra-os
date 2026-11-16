(ns sutra-ui.polygon-geojson-smoke-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [sutra.capps.polygon :as poly]))

(def ^:private multipolygon
  (str "{\"type\":\"MultiPolygon\",\"coordinates\":"
       "[[[[6.5,5],[14.25,5],[14.25,5.5],[6.5,5.5],[6.5,5]]],"
       "[[[20.5,7],[21.25,7],[21.25,9.5],[20.5,9.5],[20.5,7]]]]}"))

(deftest geojson-liest-im-browser-echte-zahlen
  (let [ringe (poly/parse-rings multipolygon)
        punkte (mapcat :points ringe)]
    (is (= 2 (count ringe)) "ein Ring je Wand")
    (is (every? poly/ring? ringe))
    (testing "jede Koordinate ist eine Zahl — NICHT NaN"
      (is (every? (fn [[x y]] (and (number? x) (number? y)
                                   (not (js/isNaN x)) (not (js/isNaN y))))
                  punkte)))
    (testing "und es sind die richtigen Zahlen"
      (is (= [[6.5 5] [14.25 5] [14.25 5.5] [6.5 5.5]] (:points (first ringe)))))))

(deftest ein-punkt-format-bleibt-lesbar
  (let [poly (poly/parse "36.00,7.75 41.75,7.75 41.75,9.00 36.00,9.00")]
    (is (poly/ring? poly))
    (is (= [[36 7.75] [41.75 7.75] [41.75 9] [36 9]] (:points poly)))))

(deftest was-die-leinwand-schreibt-ist-lesbar
  (let [s (poly/rings->str [[[0 0] [10 0] [10 1] [0 1]]])]
    (is (= 1 (count (poly/parse-rings s))))
    (is (every? (fn [[x y]] (and (not (js/isNaN x)) (not (js/isNaN y))))
                (:points (first (poly/parse-rings s)))))))
