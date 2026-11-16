(ns ^:unit apps.digit-leading-sort-key-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.format :as fmt]
            [sutra.capps.table :as tbl]))

(deftest a-string-is-a-number-only-when-it-is-wholly-one
  (testing "the two-dot key is not a number at all"
    (is (nil? (fmt/num "00000.00001.00003")))
    (is (nil? (fmt/num "0001.0003.0002")))
    (is (nil? (fmt/num "12,5 kg")) "a unit suffix is not a number either"))

  (testing "what IS wholly a number still reads as one"
    (is (= 0.0   (fmt/num "00000")))
    (is (= 1.0E-5 (fmt/num "00000.00001")))
    (is (= 12.5  (fmt/num "12,5")) "the German decimal comma")
    (is (= 42.0  (fmt/num " 42 ")) "surrounding blanks")
    (is (= 314.0 (fmt/num "3.14e2")))
    (is (nil?    (fmt/num "abc")))))

(deftest a-path-key-sorts-parent-before-child
  (let [rows   (mapv (fn [p] {:values {:gliederung p}})
                     ["0001.0003.0002" "0001.0001" "0001.0003.0001" "0001.0002" "0001"])
        sorted (mapv (comp :gliederung :values)
                     (tbl/sort-limit-rows rows [[:gliederung :asc]] nil))]
    (is (= ["0001" "0001.0001" "0001.0002" "0001.0003.0001" "0001.0003.0002"] sorted)
        "Arbeitskarten-Gliederung: der Stringvergleich ordnet den Baum, ohne führendes `/`")))
