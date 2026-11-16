(ns ^:unit platform.build-abort-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.effects :as effects]
            [sutra.builds.run.monitor :as bm]
            [sutra.builds.run.lock :as lock]
            [sutra.schedules.dispatch :as dispatch]
            [sutra.foundation.rpc :as rpc]
            [sutra.serve.server :as server]))

(deftest a-token-keeps-the-first-reason-it-was-cancelled-for
  (let [t (effects/work-token)]
    (is (nil? (effects/reason t)))
    (effects/cancel! t :shutdown)
    (effects/cancel! t :user)
    (is (= :shutdown (effects/reason t)))
    (is (= :deadline (effects/reason (effects/work-token (- (System/currentTimeMillis) 1)))))
    (is (= :cancelled (effects/reason (doto (effects/work-token) effects/cancel!))))))

(deftest a-cancellation-is-found-through-its-causes
  (let [refused (try (effects/check! (doto (effects/work-token) (effects/cancel! :shutdown)))
                  (catch Exception e e))]
    (is (= :shutdown (effects/cancellation refused)))
    (is (= :shutdown
           (effects/cancellation (ex-info "transform failed" {} (ex-info "wrapped" {} refused)))))
    (is (= :user (effects/cancellation (ex-info "x" {:cancelled true :reason :user}))))
    (is (= :cancelled (effects/cancellation (ex-info "x" {:cancelled true}))))
    (is (nil? (effects/cancellation (ex-info "a real failure" {:status 500}))))))

(deftest cancel-all-reaches-each-run-and-its-calls-to-the-outside
  (let [id (str (random-uuid))
        pa (bm/register! id 2)]
    (try
      (is (= [id] (filterv #{id} (bm/cancel-all! :shutdown))))
      (is (= :shutdown (effects/reason (bm/work pa))) "its work token is cancelled with the reason")
      (let [e (try (bm/check-cancelled! pa {:transform "t"}) (catch Exception e e))]
        (is (= {:cancelled true :reason :shutdown :transform "t"} (ex-data e))))
      (is (not-any? :work (bm/active-builds nil))
          "no token leaks into what the endpoint serialises")
      (finally (bm/unregister! id)))))

(deftest a-stop-interrupts-a-scheduled-build-which-is-then-due-again
  (is (= :interrupted (dispatch/outcome (ex-info "x" {:cancelled true :reason :shutdown}))))
  (is (= :failed (dispatch/outcome (ex-info "x" {:cancelled true :reason :user})))
      "a cancel by a person ends the slot like a failure")
  (is (= :failed (dispatch/outcome (ex-info "boom" {}))))
  (is (= {:stamp? false :advance? false} (dispatch/settlement :interrupted)))
  (is (= {:stamp? true :advance? true} (dispatch/settlement :failed)))
  (is (= {:stamp? false :advance? true} (dispatch/settlement :fired))))

(deftest an-orphaned-claim-blocks-what-conflicts-with-it
  (let [orphan {:run-id "a" :status "orphaned" :writes #{:d1}}
        reader {:run-id "b" :status "queued" :reads #{:d1}}
        other  {:run-id "c" :status "queued" :reads #{:d9} :writes #{:d8}}]
    (is (= {:state :queued :blockers ["a"] :running 0} (lock/admission [orphan reader] "b" 2)))
    (is (:admit? (lock/admission [orphan other] "c" 2)) "one that does not conflict is admitted")))

(deftest a-stopping-process-waits-only-while-something-runs
  (is (= :done (server/drain-step {:ticks? false :requests? false :left-ms 5000})))
  (is (= :wait (server/drain-step {:ticks? true :requests? false :left-ms 5000})))
  (is (= :out-of-time (server/drain-step {:ticks? false :requests? true :left-ms 0})))
  (is (= ["run 12345678 · t · 1/3 transforms done · last moved 4s ago · cancelled (shutdown)"]
         (server/drain-lines [{:run-id "12345678-abcd" :current "t" :done [{}] :total 3
                               :idle-ms 4200 :cancel? :shutdown}
                              {:run-id "elsewhere" :remote? true}]))))

(deftest a-cancel-ends-the-wait-for-a-worker
  (let [work (doto (effects/work-token) (effects/cancel! :shutdown))
        e    (try (#'rpc/answer-of (promise) work) (catch Exception e e))]
    (is (= :work-cancelled (:refusal (ex-data e))))
    (is (= :shutdown (:reason (ex-data e)))))
  (testing "an answer that is there is returned"
    (is (= {:status 200} (#'rpc/answer-of (doto (promise) (deliver {:status 200})) nil)))))
