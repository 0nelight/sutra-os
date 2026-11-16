(ns ^:integration ontology.automation-executions-store
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.automation.executions :as x]
            [sutra.store.access.scope :as ctx]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(deftest an-execution-is-run-batch-by-batch-under-a-claim
  (let [b "main" id (str (random-uuid)) a (str (random-uuid)) s (ctx/store)]
    (x/create! s b {:id id :automation-id a :pending ["o1" "o2" "o3"] :batch-size 2
                    :parallelism 4})
    (testing "it is running, and its next batch is the first batch-size ids"
      (is (= [id] (mapv :id (x/running s b))))
      (let [c (x/claim! s b id)]
        (is (= ["o1" "o2"] (x/next-batch c)))
        (testing "while one worker holds it, nobody else claims it — a draining conductor's
                  batch never runs beside a new leader's"
          (is (nil? (x/claim! s b id))))
        (testing "a stale claim cannot strike the batch off"
          (is (nil? (x/advance! s b id "not-mine" 2))))
        (testing "after the batch: struck off, claim released"
          (let [r (x/advance! s b id (:claim-id c) 2)]
            (is (= ["o3"] (:pending r)))
            (is (= 1 (:batches r)))
            (is (= "running" (:status r)))
            (is (nil? (:claim-id r)))))))
    (testing "the last batch ends it"
      (let [c (x/claim! s b id)]
        (is (= ["o3"] (x/next-batch c)))
        (is (= "done" (:status (x/advance! s b id (:claim-id c) 1))))
        (is (empty? (x/running s b)))
        (is (nil? (x/claim! s b id)))))
    (testing "a cancelled execution claims no further batch"
      (let [id2 (str (random-uuid))]
        (x/create! s b {:id id2 :automation-id a :pending ["p1"] :batch-size 1})
        (is (= "cancelled" (:status (x/cancel! s b id2))))
        (is (nil? (x/claim! s b id2)))))))

(deftest a-cancel-holds-while-the-running-batch-ends
  (let [b "main" id (str (random-uuid)) s (ctx/store)]
    (x/create! s b {:id id :automation-id (str (random-uuid)) :pending ["o1" "o2" "o3"]
                    :batch-size 1})
    (let [c (x/claim! s b id)]
      (is (= "cancelled" (:status (x/cancel! s b id))))
      (testing "the batch that ran is struck off and its claim released, but the execution
                stays cancelled: no further batch is claimed"
        (let [r (x/advance! s b id (:claim-id c) 1)]
          (is (= ["o2" "o3"] (:pending r)))
          (is (nil? (:claim-id r)))
          (is (= "cancelled" (:status r))))
        (is (nil? (x/claim! s b id)))))))
