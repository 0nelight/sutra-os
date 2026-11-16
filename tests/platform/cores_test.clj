(ns ^:unit platform.cores-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.compute.cores :as cores]))

(deftest cpu-lists-read-as-linux-writes-them
  (is (= [0 1 2 3 8 10 11] (cores/parse-cpu-list "0-3,8,10-11")))
  (is (= [5] (cores/parse-cpu-list " 5 ")))
  (is (= "0,1,4" (cores/format-cpu-list [0 1 4]))))

(deftest a-request-is-granted-within-the-queue
  (let [q (cores/queue [0 1 2 3])]
    (is (= cores/default-cores (cores/grant q nil)) "nothing requested is 2")
    (is (= 1 (cores/grant q 0)))
    (is (= 4 (cores/grant q 16)) "never more than the worker owns")))

(deftest runs-hold-disjoint-cores-and-give-them-back
  (let [q (cores/queue [0 1 2 3])
        a (promise)
        b (promise)]
    (cores/with-cores* q 2
      (fn [x]
        (deliver a x)
        (cores/with-cores* q 2 (fn [y] (deliver b y)))))
    (is (= 2 (count @a) (count @b)))
    (is (empty? (filter (set @a) @b)) "two runs never share a core")
    (is (= #{0 1 2 3} @(:free q)) "every core is back")))

(deftest a-run-waits-while-its-cores-are-taken
  (let [q       (cores/queue [0 1])
        release (promise)
        holder  (future (cores/with-cores* q 2 (fn [_] @release :first)))
        _       (Thread/sleep 50)
        waiter  (future (cores/with-cores* q 1 (fn [ids] ids)))]
    (testing "the second run cannot start while the first holds every core"
      (is (= ::waiting (deref waiter 100 ::waiting))))
    (deliver release true)
    (is (= :first @holder))
    (is (= 1 (count (deref waiter 1000 nil))) "it runs once the cores are free")))

(deftest a-failing-run-returns-its-cores
  (let [q (cores/queue [0 1])]
    (is (thrown? clojure.lang.ExceptionInfo
                 (cores/with-cores* q 2 (fn [_] (throw (ex-info "boom" {}))))))
    (is (= #{0 1} @(:free q)))))
