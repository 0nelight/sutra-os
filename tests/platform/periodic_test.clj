(ns platform.periodic-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.foundation.periodic :as periodic]))

(defn- settle [] (Thread/sleep 60))

(deftest the-beat-ends-with-the-work
  (testing "also when the work throws"
    (let [n (atom 0)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                            (periodic/renewing "t-beat" 5 #(swap! n inc)
                                               #(do (Thread/sleep 40)
                                                    (throw (ex-info "boom" {}))))))
      (is (pos? @n) "it beat while the work ran")
      (let [seen @n]
        (settle)
        (is (= seen @n) "and not after"))))
  (testing "the work's value comes back"
    (is (= :v (periodic/renewing "t-beat" 5 (constantly true) (constantly :v))))))

(deftest the-beat-stops-when-the-renewal-answers-nil
  (let [n (atom 0)]
    (periodic/renewing "t-beat" 5 #(when (< (swap! n inc) 3) true)
                       #(Thread/sleep 80))
    (is (= 3 @n))))

(deftest a-throwing-renewal-is-tried-again
  (let [n (atom 0)
        stop (periodic/beating "t-beat" 5 #(if (= 1 (swap! n inc))
                                             (throw (ex-info "blip" {}))
                                             true))]
    (Thread/sleep 50)
    (stop)
    (is (< 2 @n))))

(deftest a-slot-runs-pokes-and-drains
  (let [s (periodic/slot)
        n (atom 0)]
    (periodic/start! s "t-slot" 60000 60000 #(swap! n inc))
    (testing "a running slot does not start twice"
      (is (nil? (periodic/start! s "t-slot" 0 1 #(swap! n + 100)))))
    (testing "poke! runs it now"
      (is (true? (periodic/poke! s)))
      (settle)
      (is (= 1 @n)))
    (testing "drain! lets it end and starts nothing more"
      (is (true? (periodic/drain! [s] 1000)))
      (is (nil? (periodic/poke! s))))
    (periodic/stop! s)
    (is (nil? @s))))
