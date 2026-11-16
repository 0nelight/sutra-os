(ns ^:unit platform.egress-policies-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.sources.egress-policies :as ep]))

(def ^:private aktiv {:id "a:1" :state :active})
(def ^:private wartet {:id "b:2" :state :pending})
(def ^:private widerrufen {:id "c:3" :state :revoked})
(def ^:private pausiert {:id "d:4" :state :active :paused true})

(deftest ohne-policy-wird-nichts-beurteilt
  (is (nil? (ep/entry-state [] "a:1")))
  (is (= ["a:1" "x:9"] (ep/effective [] ["a:1" "x:9"]))
      "adopting policies must change nothing before the first one exists"))

(deftest nur-eine-aktive-policy-oeffnet
  (let [ps [aktiv wartet widerrufen pausiert]]
    (is (= :active (ep/entry-state ps "a:1")))
    (is (= :pending (ep/entry-state ps "b:2")))
    (is (= :revoked (ep/entry-state ps "c:3")))
    (is (= :none (ep/entry-state ps "x:9")) "what no policy covers is blocked")
    (testing "paused stays in the list: the host's pause reports it from there"
      (is (= :paused (ep/entry-state ps "d:4")))
      (is (= ["a:1" "d:4"] (ep/effective ps ["a:1" "b:2" "c:3" "d:4" "x:9"]))))))

(deftest zustand-aus-dem-speicher-darf-ein-string-sein
  (is (= :active (ep/state-of {:state "active"})))
  (is (= :paused (ep/state-of {:state "active" :paused true})))
  (is (= :revoked (ep/state-of {:state :revoked :paused true}))
      "a revoked policy is revoked, whatever the host says"))

(deftest eintrag-wird-getrimmt
  (is (= :active (ep/entry-state [aktiv] " a:1 "))))
