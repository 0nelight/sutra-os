(ns ^:unit platform.health-projection-lag-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.health.checks :as health]))

(def ^:private lag #'health/projection-lag)

(defn- tips [m]
  (into {} (for [[b [rev ms]] m]
             [b {:last {:rev rev :at (java.util.Date. (long ms))}}])))

(def ^:private now 1000000000000)
(def ^:private hour 3600000)

(deftest an-anchor-level-with-its-branch-is-not-lagging
  (is (empty? (lag [{:repo-rid "r" :branch "main" :tip-seq 42}]
                   (tips {"main" [42 now]})
                   now))))

(deftest a-backlog-is-counted-and-timed
  (let [[l :as all] (lag [{:repo-rid "r" :branch "main" :tip-seq 25}]
                         (tips {"main" [342 (- now (* 3 hour))]})
                         now)]
    (is (= 1 (count all)))
    (is (= "r/main" (:pair l)))
    (is (= 317 (:behind l)) "the count is the schema commits the projection never wrote")
    (is (= (* 3 hour) (:waited-ms l)) "and how long it has stood that way")))

(deftest a-branch-never-projected-is-not-reported-as-drift
  (testing "an anchorless branch is the projector's own first-run case, not a backlog"
    (is (empty? (lag [{:repo-rid "r" :branch "main" :tip-seq nil}]
                     (tips {"main" [7 now]})
                     now)))))

(deftest a-machine-branch-is-never-projected-so-never-behind
  (is (empty? (lag [{:repo-rid "r" :branch "staging/u/main" :tip-seq 1}]
                   (tips {"staging/u/main" [99 (- now (* 9 hour))]})
                   now))))

(deftest a-fresh-backlog-stays-quiet-and-an-old-one-does-not
  (testing "time, not count: a projection is behind for a moment after every write, and
            that is ordinary. Only a backlog nobody is clearing is a finding — and it
            clears itself, because the next run measures again."
    (let [frisch (health/projection-lag-verdict [{:pair "r/main" :behind 400 :waited-ms 5000}])
          alt    (health/projection-lag-verdict [{:pair "r/main" :behind 1 :waited-ms (* 4 hour)}])]
      (is (= "passed" (:status frisch)) "400 behind for five seconds is a due run, not a fault")
      (is (= "400" (:value frisch)) "and the count is reported either way")
      (is (= "failed" (:status alt)) "one behind for four hours is nobody clearing it")
      (is (re-find #"r/main" (:detail alt)))
      (is (re-find #"RETRACTS" (:detail alt)) "and it says what a push from that tree would do")))
  (testing "nothing lagging is a pass that says so"
    (is (= "passed" (:status (health/projection-lag-verdict []))))
    (is (= "0" (:value (health/projection-lag-verdict []))))))
