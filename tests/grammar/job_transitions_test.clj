(ns ^:unit grammar.job-transitions-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.ledger :as ledger]))

(def ^:private facts {:job-rid "job-1" :error "e" :at "2026-10-11T00:00:00Z"})

(defn- moves? [to from]
  (some? (ledger/job-after to facts {:status (name from) :job-rid "job-1"})))

(deftest every-edge-moves-and-every-other-pair-stays
  (doseq [[to {:keys [from on-conflict]}] ledger/job-transitions
          :when (not= :keep on-conflict)
          s ledger/job-states]
    (testing (str s " → " to)
      (is (= (contains? from s) (moves? to s))))))

(deftest a-moved-job-carries-its-new-state
  (is (= "RUNNING"
         (:status (ledger/job-after :RUNNING (assoc facts :attempt 2) {:status "RUN_PENDING"}))))
  (is (= 2
         (:attempt (ledger/job-after :RUNNING (assoc facts :attempt 2)
                                     {:status "RUNNING" :attempt 1})))))

(deftest final-states-are-never-left
  (doseq [s [:ABORTED :DID_NOT_RUN] to (keys ledger/job-transitions)]
    (is (not (moves? to s)) (str s " → " to)))
  (testing "legacy names map onto the final states"
    (is (nil? (ledger/job-after :RUNNING facts {:status "SKIPPED"})))))

(deftest an-abort-pending-job-is-not-reopened
  (is (not (moves? :RUNNING :ABORT_PENDING)))
  (testing "but a compute that finished anyway is recorded"
    (is (moves? :COMPLETED :ABORT_PENDING))))

(deftest waiting-only-inserts
  (is (nil? (ledger/job-after :WAITING facts {:status "RUNNING"})))
  (is (= "WAITING" (:status (ledger/job-after :WAITING (assoc facts :run-id "r") nil))))
  (testing "without :run-id an absent job is not created"
    (is (nil? (ledger/job-after :RUNNING facts nil)))))

(deftest sweeps-name-known-states
  (doseq [[to {:keys [from]}] ledger/job-sweeps]
    (is (contains? ledger/job-states to))
    (is (every? ledger/job-states from))))
