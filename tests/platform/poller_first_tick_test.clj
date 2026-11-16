(ns ^:unit platform.poller-first-tick-test
  (:require [sutra.store.access.scope :as ctx]
            [clojure.test :refer [deftest is testing]]
            [sutra.schedules.poller :as poller])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- ticks-within
  [state interval-ms initial wait-ms]
  (let [fired (CountDownLatch. 1)
        tick  (fn [_st] (.countDown fired))]
    (with-redefs [ctx/system-store (constantly {})]
     (try
      (if initial
        (poller/start! state "test" interval-ms tick initial)
        (poller/start! state "test" interval-ms tick))
      (.await fired wait-ms TimeUnit/MILLISECONDS)
      (finally (poller/stop! state))))))

(deftest without-an-initial-delay-the-first-tick-comes-after-first-tick-ms
  (testing "the short arity no longer waits a whole interval: a schedule armed
            hourly gets its first pass after `first-tick-ms`, so a restart
            before the interval elapses does not lose the run"
    (let [state (atom {})]
      (with-redefs [poller/first-tick-ms (constantly 50)]
        (is (true? (ticks-within state 3600000 nil 2000))
            "armed hourly, default delay, and the first pass runs right after start")))))

(deftest an-initial-delay-makes-a-restart-trigger-a-pass
  (testing "what retention now does: first tick shortly after boot, so the sweep
            survives an instance that is deployed several times a day"
    (let [state (atom {})]
      (is (true? (ticks-within state 3600000 50 2000))
          "armed hourly, but the first pass runs right after start"))))

(deftest the-initial-delay-never-exceeds-the-interval
  (testing "a poller that already ticks faster than a minute is not SLOWED by
            the rule that makes a restart trigger a pass"
    (is (= 30000 (poller/first-tick-ms 30000)) "a 30s poller keeps its 30s first tick")
    (is (= 60000 (poller/first-tick-ms 3600000)) "an hourly one gets a minute, not an hour")
    (is (= 60000 (poller/first-tick-ms 60000)) "exactly a minute is its own first tick")))
