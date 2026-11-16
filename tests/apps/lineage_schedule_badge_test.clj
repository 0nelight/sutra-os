(ns ^:unit apps.lineage-schedule-badge-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lineage.graph :as lineage]))

(defn- count-for [schedules k]
  ((lineage/schedule-counter schedules) k))

(deftest a-target-is-a-comma-separated-list-of-bare-names
  (let [scheds [{:id "a" :target "befund-ereignis,mangel,acars-abholen"}
                {:id "b" :target "mangel"}
                {:id "c" :target "teileverbrauch_roh,arbeitsauftrag_roh"}]]
    (testing "each named target counts its schedule"
      (is (= 2 (count-for scheds "object-type/mangel")) "named by BOTH a and b")
      (is (= 1 (count-for scheds "transform/acars-abholen")))
      (is (= 1 (count-for scheds "dataset/teileverbrauch_roh"))))
    (testing "the KIND half of the node key is not part of the match"
      (is (= 1 (count-for scheds "dataset/acars-abholen"))))
    (testing "a node no schedule names counts zero"
      (is (= 0 (count-for scheds "object-type/luftfahrzeug"))))
    (testing "a whole-key match is NOT what a target is"
      (is (= 0 (count-for scheds "object-type/befund-ereignis,mangel,acars-abholen"))))))

(deftest an-untargeted-schedule-counts-for-nothing
  (let [scheds [{:id "old" :target nil :paused true}
                {:id "blank" :target "   "}
                {:id "real" :target "mangel"}]]
    (is (= 1 (count-for scheds "object-type/mangel")))
    (is (= 0 (count-for scheds "object-type/anything-else")))))

(deftest whitespace-around-a-target-is-not-part-of-its-name
  (let [scheds [{:id "a" :target "mangel , befund-ereignis ,"}]]
    (is (= 1 (count-for scheds "object-type/mangel")))
    (is (= 1 (count-for scheds "object-type/befund-ereignis")))
    (is (= 0 (count-for scheds "object-type/")) "the trailing empty segment is not a node")))

(deftest the-count-is-over-schedules-not-over-targets
  (let [scheds [{:id "a" :target "mangel,mangel"}]]
    (is (= 1 (count-for scheds "object-type/mangel")))))
