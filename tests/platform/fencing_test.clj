(ns ^:integration platform.fencing-test
  (:require [sutra.store.access.scope :as ctx]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.coord.api :as coord]
            [sutra.builds.run.leader :as leader]
            [sutra.builds.run.lock :as lock]
            [sutra.store.world :as world]
            [sutra.builds.run.lane :as lane]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- commit! [store]
  (world/commit! store
                 {:actor {:principal "test"} :message "fencing-test" :at-head? true})
  :committed)

(defn- fenced-commit! [fences] (commit! (lane/fenced (ctx/store "main") fences)))

(deftest a-tick-commits-only-under-the-current-lease-epoch
  (leader/start!)
  (try
    (is (= :committed (leader/as-leader fenced-commit!)) "the holder commits")
    (testing "another node took the lease over (epoch moved on): the old holder's tick cannot commit"
      (let [e (coord/revoke! (ctx/coord) "leader")]
        (world/raise-fence! (ctx/store "main") "leader" e))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"moved past epoch"
                            (leader/as-leader fenced-commit!))))
    (finally (leader/stop!))))

(deftest a-build-commits-only-while-it-holds-its-claim
  (let [run-id (random-uuid)]
    (lock/with-claim (ctx/store) {:run-id run-id :branch "main" :reads [] :writes []}
      (fn [store]
        (is (= :committed (commit! store)) "admitted: the build commits")
        (testing "force-fail or the lease sweep took the claim: the still-running thread cannot commit"
          (lock/revoke! (ctx/store) run-id)
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"moved past epoch"
                                (commit! store))))))))
