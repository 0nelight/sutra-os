(ns ^:unit platform.profile-scope-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.foundation.profile :as prof]
            [sutra.foundation.profile.scope :as scope]))

(def ^:private db-wait
  [["sun.nio.ch.Net.poll" nil]
   ["sutra.store.core/answer" 10]
   ["sutra.store.core/answer" 12]
   ["sutra.builds.plan.freshness/asker" 7]
   ["clojure.core/apply" 1]])

(defn- frame [rows f]
  (some #(when (= f (:frame %)) %) rows))

(deftest time-off-the-cpu-is-counted-as-waiting
  (let [r (scope/report (-> scope/empty-tally
                            (scope/absorb db-wait 100.0 5.0)
                            (scope/absorb [["sutra.builds.plan.dag/run" 3]] 100.0 100.0)))]
    (is (= 200 (:thread-ms r)))
    (is (= 105 (:cpu-ms r)))
    (is (= 95 (:waiting-ms r)))
    (is (= 50.0 (:pct (frame (:self r) "sun.nio.ch.Net.poll"))))))

(deftest a-frame-counts-once-per-sample-and-only-ours-are-inclusive
  (let [{:keys [inclusive]} (scope/report (scope/absorb scope/empty-tally db-wait 50.0 0.0))]
    (is (= 50 (:ms (frame inclusive "sutra.store.core/answer"))))
    (is (some? (frame inclusive "sutra.builds.plan.freshness/asker")))
    (is (nil? (frame inclusive "clojure.core/apply")))
    (is (nil? (frame inclusive "sun.nio.ch.Net.poll")))))

(defn- napping [ms]
  (Thread/sleep (long ms))
  :done)

(deftest the-work-and-the-pools-it-fans-out-to-are-sampled
  (let [reports (atom [])
        pool    (java.util.concurrent.Executors/newFixedThreadPool 2)
        answer  (scope/run-scoped
                 {:label "test scope" :on-report #(swap! reports conj %)}
                 (fn []
                   (let [f (.submit pool ^java.util.concurrent.Callable
                                    (bound-fn* (scope/joined #(napping 300))))]
                     (napping 300)
                     @f)))]
    (.shutdown pool)
    (is (= :done answer))
    (let [{:keys [samples thread-ms cpu-ms elapsed-ms]} (last @reports)]
      (testing "both threads, so the threads' time exceeds what elapsed"
        (is (pos? samples))
        (is (> thread-ms elapsed-ms)))
      (is (< cpu-ms thread-ms) "a nap is time off the CPU"))))

(deftest a-run-that-throws-still-reports
  (let [reports (atom [])]
    (is (thrown? IllegalStateException
                 (scope/run-scoped {:label "test throw" :on-report #(swap! reports conj %)}
                                   (fn [] (napping 120) (throw (IllegalStateException. "no"))))))
    (is (= 1 (count @reports)))))

(deftest a-report-that-cannot-be-delivered-costs-only-the-report
  (is (= :done (scope/run-scoped {:label "test report" :on-report (fn [_] (throw (Exception. "x")))}
                                 #(napping 10)))))

(deftest a-fanned-out-future-answers-to-the-request-that-started-it
  (is (= "span-1" (prof/with-span "span-1" @(prof/span-future (prof/current-span)))))
  (is (nil? @(prof/span-future (prof/current-span)))))

(deftest a-parked-virtual-thread-spends-no-cpu
  (let [cpu (fn [parked?]
              (:cpu (first (prof/as-deltas {} 10 [{:tid 1 :cpu-total -1 :stack [["f" 1]]
                                                   :parked? parked?}]))))]
    (is (= 10000000 (cpu false)))
    (is (zero? (cpu true)))))

(defn- marker [nm start end span]
  {:name nm :start start :end end :data {:span span}})

(deftest paired-spans-bound-the-browsers-clock
  (testing "the browser's request must enclose the server's"
    (is (= 100.0 (prof/clock-shift [(marker "api/transit" 0.0 20.0 "a")
                                    (marker "Request" 105.0 115.0 "a")]))))
  (testing "bounds that disagree leave the recording as measured"
    (is (nil? (prof/clock-shift [(marker "api/transit" 0.0 20.0 "a")
                                 (marker "Request" 105.0 115.0 "a")
                                 (marker "api/transit" 0.0 20.0 "b")
                                 (marker "Request" 5.0 15.0 "b")]))))
  (testing "no pair, no claim"
    (is (nil? (prof/clock-shift [(marker "Request" 5.0 15.0 "a")])))))
