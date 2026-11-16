(ns ^:unit platform.linter-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.linter.core :as core]
            [sutra.linter.rules :as rules]))

(def ^:private now 1000000000000)
(def ^:private day (* 24 60 60 1000))

(defn- facts [m]
  (merge {:now now :known #{} :transforms [] :schedules [] :runs [] :branch-schedules []} m))

(defn- found [rule m]
  (filter #(= rule (:rule %)) (:found (rules/run (facts m)))))

(defn- sched [m]
  (merge {:name "s" :builds ["a"] :targets ["a"] :watched [] :retry-count 3
          :abort-on-failure true}
         m))

(defn- xf [m]
  (merge {:transform "a" :status "succeeded" :inputs [] :outputs ["dataset/a"]
          :readers ["application/x"] :triggers [{:schedule "s"}]}
         m))

(deftest a-schedule-always-fails-only-when-no-run-succeeded
  (let [run (fn [st] {:status st :targets {:origin "schedule s"}})
        m   {:schedules [(sched {})] :transforms [(xf {})] :known #{"a"}}]
    (is (seq (found :schedule-always-fails (assoc m :runs [(run :failed)]))))
    (is (empty? (found :schedule-always-fails (assoc m :runs [(run :failed) (run :succeeded)]))))
    (is (empty? (found :schedule-always-fails m)) "no run in the window says nothing")
    (is (empty? (found :schedule-always-fails
                       (assoc m :runs [{:status :failed :targets {:origin "schedule s2"}}])))
        "another schedule's run")))

(deftest a-paused-schedule-is-not-judged
  (is (empty? (filter #(= "schedule/s" (:resource %))
                      (:found (rules/run (facts {:schedules [(sched {:paused true
                                                                     :retry-count 0})]})))))))

(deftest schedule-shape-rules
  (let [known #{"a" "b"}]
    (is (seq (found :schedule-builds-unknown-target
                    {:known known :schedules [(sched {:targets ["weg"]})]})))
    (is (seq (found :schedule-no-built-datasets
                    {:known known :schedules [(sched {:builds []})]})))
    (is (seq (found :schedule-retries-disabled {:schedules [(sched {:retry-count 0})]})))
    (is (seq (found :schedule-does-not-abort-on-failure
                    {:schedules [(sched {:abort-on-failure false})]})))
    (is (= 2 (count (found :overlapping-schedules
                           {:schedules [(sched {}) (sched {:name "t"})]}))))
    (is (seq (found :schedule-triggers-itself
                    {:schedules [(sched {:watched ["a"]})] :transforms [(xf {})]})))
    (is (seq (found :schedule-trigger-is-not-input
                    {:schedules [(sched {:watched ["z"]})] :transforms [(xf {})]})))
    (is (empty? (found :schedule-trigger-is-not-input
                       {:schedules [(sched {:watched ["i"]})]
                        :transforms [(xf {:inputs ["dataset/i"]})]})))))

(deftest an-input-with-logic-but-no-schedule-is-named
  (let [up (xf {:transform "up" :outputs ["dataset/i"] :triggers []})
        a  (xf {:inputs ["dataset/i"]})]
    (is (re-find #"up" (:detail (first (found :schedule-inputs-unscheduled
                                              {:schedules [(sched {})]
                                               :transforms [up a]})))))
    (is (empty? (found :schedule-inputs-unscheduled
                       {:schedules [(sched {}) (sched {:name "t" :builds ["up"]})]
                        :transforms [up a]})))))

(deftest what-nobody-reads-is-potentially-unused
  (let [m #(hash-map :schedules [(sched {})] :transforms [(xf %)])]
    (is (seq (found :schedule-potentially-unused (m {:readers []}))))
    (is (seq (found :schedule-potentially-unused (m {:readers ["transform/a"]})))
        "its own transforms are no use")
    (is (empty? (found :schedule-potentially-unused (m {}))))
    (is (empty? (found :schedule-potentially-unused
                       (m {:readers [] :read-via-link-ends {"l" ["x"]}}))))))

(deftest a-force-build-is-unnecessary-without-an-external-source
  (let [m #(hash-map :schedules [(sched {:full true})] :transforms [(xf %)])]
    (is (seq (found :schedule-unnecessary-force-builds (m {}))))
    (is (empty? (found :schedule-unnecessary-force-builds (m {:external-systems ["x"]}))))))

(deftest transforms-are-judged-without-a-schedule
  (testing "failing: last run failed, no success for 30 days"
    (let [m #(hash-map :transforms [(xf %)])]
      (is (seq (found :transform-always-fails (m {:last-failure {:at-ms now}}))))
      (is (seq (found :transform-always-fails
                      (m {:last-failure {:at-ms now} :last-success {:at-ms (- now (* 40 day))}}))))
      (is (empty? (found :transform-always-fails
                         (m {:last-failure {:at-ms now} :last-success {:at-ms (- now day)}}))))
      (is (empty? (found :transform-always-fails
                         (m {:last-failure {:at-ms (- now (* 40 day))}
                             :last-success {:at-ms now}}))))))
  (testing "never built, and no schedule covers it"
    (is (seq (found :transform-never-built {:transforms [(xf {:status "never" :triggers []})]})))
    (is (empty? (found :transform-never-built {:transforms [(xf {:status "never"})]})))
    (is (empty? (found :transform-never-built
                       {:transforms [(xf {:status "never" :triggers []
                                          :last-failure {:at-ms now}})]}))))
  (testing "built with older logic"
    (is (seq (found :dataset-code-out-of-date {:transforms [(xf {:freshness "logic"})]})))))

(deftest a-rule-that-throws-costs-only-its-own-findings
  (let [{:keys [found failed]} (rules/run (facts {:schedules [(sched {:retry-count "x"})]}))]
    (is (= [:schedule-retries-disabled] (map :rule failed)))
    (is (seq found))))

(deftest a-sweep-keeps-what-it-still-finds-and-resolves-the-rest
  (let [t0   (java.util.Date. 1000)
        t1   (java.util.Date. 2000)
        hit  (fn [res] {:rule :r :resource res :mode :pipeline-resilience :detail "d"})
        kept {:rule "r" :resource "a" :first-seen t0 :snoozed-until t1 :snooze-reason "w"
              :assignee "x" :resolved-at nil}
        gone {:rule "r" :resource "b" :first-seen t0 :resolved-at nil}
        old  {:rule "r" :resource "c" :first-seen t0 :assignee "x" :resolved-at t0}
        rows (into {} (map (juxt :resource identity))
                   (core/merged [kept gone old] [(hit "a") (hit "c") (hit "n")] t1))]
    (testing "still found: first seen, snooze and assignee stay"
      (is (= t0 (:first-seen (rows "a"))))
      (is (= "w" (:snooze-reason (rows "a"))))
      (is (= "x" (:assignee (rows "a")))))
    (is (= t1 (:resolved-at (rows "b"))) "no longer found: resolved")
    (testing "found again after being resolved: it starts over"
      (is (= t1 (:first-seen (rows "c"))))
      (is (nil? (:assignee (rows "c"))))
      (is (nil? (:resolved-at (rows "c")))))
    (is (= t1 (:first-seen (rows "n"))) "new")
    (is (not (contains? (set (map :resource (core/merged [old] [] t1))) "c"))
        "what is already resolved is not written again")))

(deftest the-view-shows-state-and-hides-the-snoozed
  (let [now  (java.util.Date. 5000)
        rows [{:rule "r" :resource "a" :mode "pipeline-resilience" :project "p"}
              {:rule "r" :resource "b" :mode "resource-savings"
               :snoozed-until (java.util.Date. 9000)}
              {:rule "q" :resource "c" :mode "resource-savings"
               :snoozed-until (java.util.Date. 1000)}
              {:rule "r" :resource "d" :mode "x" :resolved-at now}]
        res  #(mapv :resource (:recommendations (core/view rows % now)))]
    (is (= ["a" "c"] (res {})))
    (is (= "snooze-expired"
           (:state (second (:recommendations (core/view rows {} now))))))
    (is (= ["a" "c" "b"] (res {:snoozed true})))
    (is (= ["a"] (res {:project "p"})))
    (is (= ["c"] (res {:mode "resource-savings"})))
    (is (= 1 (:resolved (core/view rows {} now))))))

(deftest sweep-facts-are-shaped-without-reading
  (let [f (core/sweep-facts
           {:now 1 :lineage {:nodes {"dataset/d" {}}
                             :transforms [{:transform "a" :last-failure {:at "t"}}]}
            :schedules [{:name :s}] :closure-of (constantly ["a"])
            :targets-of (constantly [:d]) :watched-of (constantly nil)
            :at-ms (constantly 7)})]
    (is (= #{"d"} (:known f)))
    (is (= 7 (get-in f [:transforms 0 :last-failure :at-ms])))
    (is (nil? (get-in f [:transforms 0 :last-success])))
    (is (= {:name "s" :builds ["a"] :targets ["d"] :watched []} (first (:schedules f))))))
