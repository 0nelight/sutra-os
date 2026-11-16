(ns ^:integration ontology.logic-events-store
  (:require [sutra.store.control :as ctl]
            [sutra.store.access.scope :as ctx]
            [sutra.automation.logic-events :as logic-events]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.world :as world]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(deftest an-event-is-claimed-once-and-settled
  (let [branch "main" a (str (random-uuid)) fp (logic-events/fingerprint :version {:d "d1"} {:x 1})
        claim #(logic-events/claim! (ctx/store) branch a "d1" fp (merge {:stale-ms 600000} %))]
    (let [c1 (claim {})]
      (is (string? (:event-id c1)))
      (is (= 0 (:attempts c1)))
      (testing "while it runs, nobody else claims it"
        (is (nil? (claim {})))
        (is (nil? (claim {:manual? true}))))
      (testing "a transient failure cools down; a manual run may take it at once"
        (is (= {:attempts 1 :retry-in-ms 60000}
               (logic-events/failed! (ctx/store) branch a "d1" fp
                                     {:attempts 0 :error "429" :transient? true})))
        (is (nil? (claim {})))
        (let [c2 (claim {:manual? true})]
          (is (= (:event-id c1) (:event-id c2)) "the same event, tried again")
          (is (= 1 (:attempts c2)))))
      (testing "due: the cool-down has passed"
        (logic-events/failed! (ctx/store) branch a "d1" fp
                              {:attempts 1 :error "503" :transient? true})
        (ctl/update! (ctx/store) :logic-events
                     {:branch "main" :automation-id a :object-id "d1" :fingerprint fp}
                     {:next-at (java.util.Date. (- (System/currentTimeMillis) 1000))})
        (is (some? (claim {}))))
      (testing "a deterministic failure is not due by itself"
        (logic-events/failed! (ctx/store) branch a "d1" fp
                              {:attempts 2 :error "422" :transient? false})
        (is (nil? (claim {})))
        (is (some? (claim {:manual? true}))))
      (testing "done is done — for a manual run too"
        (logic-events/done! (ctx/store) branch a "d1" fp {:proposed 1})
        (is (nil? (claim {})))
        (is (nil? (claim {:manual? true})))
        (is (= {"running" 0 "done" 1 "failed" 0} (logic-events/counts-for (ctx/store) branch a)))
        (is (= {:proposed 1}
               (:outcome (first (logic-events/list-for (ctx/store) branch a :done))))))
      (testing "…except for a manual execution over an object set: it runs a done object again"
        (let [since #(java.util.Date. (+ (System/currentTimeMillis) %))
              c3    (claim {:manual? true :rerun-since (since 1000)})]
          (is (= (:event-id c1) (:event-id c3)))
          (is (= 0 (:attempts c3)))
          (is (nil? (claim {:manual? true :rerun-since (since 1000)}))
              "running, so nobody else takes it")
          (logic-events/done! (ctx/store) branch a "d1" fp {:proposed 2} (:claim-id c3))
          (is (nil? (claim {:manual? true :rerun-since (since -60000)}))
              "done since the execution began: its own work, a repeated batch skips it"))))
    (testing "a new version is a new event; a stale running claim is taken over"
      (let [fp2 (logic-events/fingerprint :version {:d "d1"} {:x 2})]
        (is (some? (logic-events/claim! (ctx/store) branch a "d1" fp2 {})))
        (is (some? (logic-events/claim! (ctx/store) branch a "d1" fp2 {:stale-ms 0})))))
    (testing "the fallback's result stays on the event"
      (logic-events/fallback! (ctx/store) branch a "d1" fp {:action :x :status "applied"})
      (is (= {:action :x :status "applied"}
             (:fallback (first (get (logic-events/state-of (ctx/store) branch a ["d1"]) "d1"))))))))

(defn- commit-under!
  [held]
  (world/commit! (ctx/store "main") {:actor {:principal "test"} :records [held] :at-head? true})
  :committed)

(deftest a-superseded-worker-can-neither-commit-nor-settle
  (let [branch "main" a (str (random-uuid)) fp (logic-events/fingerprint :version {:d "d2"} {:x 1})
        old (logic-events/claim! (ctx/store) branch a "d2" fp {})
        held-old (logic-events/held (ctx/store) branch a "d2" fp (:claim-id old))
        new (logic-events/claim! (ctx/store) branch a "d2" fp {:stale-ms 0})
        under (fn [claim]
                (commit-under! (logic-events/held (ctx/store) branch a "d2" fp (:claim-id claim))))]
    (is (= (:event-id old) (:event-id new)) "a takeover keeps the event")
    (is (not= (:claim-id old) (:claim-id new)) "but mints a new claim")
    (testing "only the current claim commits"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"superseded" (under old)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"changed since it was read"
                            (commit-under! held-old))
          "a worker that read its claim before the takeover is refused at commit")
      (is (= :committed (under new))))
    (testing "the event settles once, and only by its current claim"
      (is (nil? (logic-events/failed! (ctx/store) branch a "d2" fp
                                      {:error "late" :claim-id (:claim-id old)})))
      (is (some? (logic-events/failed! (ctx/store) branch a "d2" fp
                                       {:error "timeout" :transient? true
                                        :claim-id (:claim-id new)})))
      (is (nil? (logic-events/failed! (ctx/store) branch a "d2" fp
                                      {:error "worker catch" :claim-id (:claim-id new)}))
          "the timeout settled it; the worker's own catch finds nothing to settle — no second fallback")
      (is (false? (logic-events/done! (ctx/store) branch a "d2" fp {} (:claim-id new))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"superseded" (under new))
          "a failed event is no longer running: its worker cannot commit either"))))
