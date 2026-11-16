(ns ^:integration ontology.build-request-queue-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.run.queue :as q]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- row [s id] (ctl/get s :build-requests {:run-id id}))

(defn- lapse! [s id]
  (ctl/swap! s :build-requests {:run-id id}
             #(assoc % :claimed-until (java.util.Date. (- (ctl/millis s) 1000)))))

(deftest a-request-is-taken-once-and-settled-by-its-claim
  (let [s  (ctx/store)
        id (q/submit! s "main" {:origin "test"})]
    (is (= "queued" (:status (row s id))))
    (let [taken (some #(when (= id (:run-id %)) %)
                      (repeatedly 5 #(q/take! s "node-a")))]
      (testing "a worker takes it under a claim"
        (is (= "running" (:status taken)))
        (is (= "node-a" (:owner taken)))
        (is (string? (:claim-id taken))))
      (testing "nobody takes or cancels a running request"
        (is (not (q/cancel-queued! s id)))
        (is (not= id (:run-id (q/take! s "node-b")))))
      (testing "another claim cannot settle it"
        (is (nil? (q/finish! s id "not-mine" (q/done {:counts {}}))))
        (is (= "running" (:status (row s id)))))
      (testing "its own claim settles it, and the claim is released"
        (let [r (q/finish! s id (:claim-id taken) (q/done {:counts {:a 1}}))]
          (is (= "done" (:status r)))
          (is (= {:counts {:a 1}} (:result r)))
          (is (nil? (:claim-id r))))))))

(deftest a-draining-worker-hands-a-request-back
  (let [s  (ctx/store)
        id (q/submit! s "main" {})
        c  (:claim-id (some #(when (= id (:run-id %)) %) (repeatedly 5 #(q/take! s "w"))))
        r  (q/finish! s id c (q/failed (ex-info "drained" {:handed-back true})))]
    (is (= "queued" (:status r)))
    (is (nil? (:claim-id r)))))

(deftest a-queued-request-cancels-and-a-lapsed-one-fails
  (let [s   (ctx/store)
        id1 (q/submit! s "main" {})
        id2 (q/submit! s "main" {})]
    (testing "a queued request cancels"
      (is (true? (q/cancel-queued! s id1)))
      (is (= "cancelled" (:status (row s id1))))
      (is (not (q/cancel-queued! s id1))))
    (testing "tidy! fails a running request whose claim lapsed, and only that"
      (let [c (:claim-id (some #(when (= id2 (:run-id %)) %) (repeatedly 5 #(q/take! s "w"))))]
        (q/tidy! s)
        (is (= "running" (:status (row s id2))))
        (lapse! s id2)
        (q/tidy! s)
        (let [r (row s id2)]
          (is (= "failed" (:status r)))
          (is (nil? (:claim-id r)))
          (is (re-find #"stopped beating" (:error r))))
        (is (nil? (q/finish! s id2 c (q/done {}))))))))
