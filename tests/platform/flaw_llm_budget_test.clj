(ns ^:unit platform.flaw-llm-budget-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.llm.budget :as budget]
            [support.mem :as mem])
  (:import (java.util.concurrent CyclicBarrier TimeUnit)))

(deftest over-budget
  (with-redefs [budget/budget-eur (constantly 1.0)]
    (let [store  (mem/store)
          ids [(str (random-uuid)) (str (random-uuid))]
          bar (CyclicBarrier. 2)
          rs (mapv deref (mapv (fn [id] (future (.await bar 5 TimeUnit/SECONDS)
                                                  (budget/reserve! store id 0.6 60000)))
                                 ids))]
      (is (= 1 (count (filter true? rs))) "one of the two fits, not both")
      (testing "a released reservation frees its share"
        (run! #(budget/release! store %) ids)
        (let [id (str (random-uuid))]
          (is (true? (budget/reserve! store id 0.9 60000)))
          (budget/release! store id)))
      (testing "an expired reservation no longer counts"
        (is (true? (budget/reserve! store (str (random-uuid)) 0.9 1)))
        (Thread/sleep 5)
        (let [id (str (random-uuid))]
          (is (true? (budget/reserve! store id 0.9 60000)))
          (budget/release! store id))))))
