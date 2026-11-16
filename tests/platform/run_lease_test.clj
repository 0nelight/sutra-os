(ns ^:integration platform.run-lease-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.run.rows :as run-rows]
            [sutra.store.access.scope :as ctx]
            [sutra.builds.run.monitor :as bm]
            [sutra.store.storage.node :as node]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- leased-run! [owner lease-ms]
  (let [id (str (random-uuid))]
    (run-rows/start-run! (ctx/store "main") "main"
                     {:id id :kind :import :inputs-hash "" :owner owner :lease-ms lease-ms})
    id))

(defn- status [id] (:status (run-rows/fetch-run-anywhere (ctx/store "main") id)))

(defn- stale-ids [] (set (map :id (run-rows/stale-running-runs (ctx/store "main") 0 0))))

(deftest a-live-lease-of-another-node-is-never-reaped
  (let [other (leased-run! "other-node" 60000)]
    (testing "age and uptime no longer decide: a leased run is not stale while its lease holds"
      (is (not (contains? (stale-ids) other))))
    (testing "reaping it anyway is refused in the UPDATE itself"
      (is (zero? (run-rows/reap-expired-run! (ctx/store "main") "main" other "test"))))
    (testing "this node sees it as a running build"
      (is (bm/any-build-active? (ctx/store "main")))
      (is (some #(and (= other (:run-id %)) (:remote? %)) (bm/active-builds (ctx/store "main")))))
    (testing "and cannot renew a lease it does not hold"
      (is (zero? (run-rows/renew-run-leases! (ctx/store "main") (node/id) [other] 60000))))
    (is (= "running" (status other)))))

(deftest an-expired-lease-is-reaped
  (let [dead (leased-run! "dead-node" -1000)]
    (is (contains? (stale-ids) dead))
    (is (not-any? #(= dead (:run-id %)) (bm/active-builds (ctx/store "main"))))
    (is (= 1 (run-rows/reap-expired-run! (ctx/store "main") "main" dead "test")))
    (is (= "failed" (status dead)))))

(deftest the-owner-keeps-its-lease-alive
  (let [mine (leased-run! (node/id) -1000)]
    (is (= 1 (run-rows/renew-run-leases! (ctx/store "main") (node/id) [mine] 60000)))
    (is (not (contains? (stale-ids) mine)))
    (run-rows/reap-run! (ctx/store "main") "main" mine "cleanup")))
