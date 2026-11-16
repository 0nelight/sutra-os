(ns ^:unit grammar.flaw-process-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing]]
            [sutra.sources.egress :as egress]
            [sutra.foundation.fn-trace :as fn-trace]
            [sutra.kit.pubsub :as pubsub]
            [sutra.authoring.mutation :as mutation]
            [sutra.serve.stream :as stream]
            [sutra.store.seat :as seat]))

(deftest late-delivery
  (testing "a sink unsubscribed while a publish runs is not called by it"
    (let [reg    (pubsub/registry)
          got    (atom [])
          unsub2 (atom nil)]
      (pubsub/subscribe! reg :t (fn [m] (swap! got conj [:a m]) (@unsub2)))
      (reset! unsub2 (pubsub/subscribe! reg :t (fn [m] (swap! got conj [:b m]))))
      (pubsub/publish! reg :t 1)
      (is (= 1 (count @got)) (pr-str @got))
      (is (= 1 (pubsub/count reg :t))))))

(deftest silent-drop
  (testing "a channel that opens after the hub stopped is closed at once, not kept silently"
    (let [register! @#'stream/register!
          unsubbed  (atom 0)]
      (try
        (stream/close-all!)
        (register! (Object.) #(swap! unsubbed inc))
        (is (= 1 @unsubbed))
        (finally (@#'stream/open-hub!))))))

(deftest lost-writeback
  (let [end! @#'egress/handle-write-end]
    (testing "committing a session that is gone is refused, never reported as ended"
      (is (thrown? clojure.lang.ExceptionInfo (end! {:tx "no-such-session" :commit true}))))
    (testing "rolling back a session that is gone is no error"
      (is (= {:ended "gone" :gone true} (end! {:tx "gone" :commit false}))))))

(deftest a-stale-dead-man-does-not-disarm-the-next-arm
  (testing "orphan-wrap, lost-deadman"
    (let [wrapped @#'fn-trace/wrapped]
      (try
        (fn-trace/arm! {:prefixes ["sutra.kit.pubsub"] :ttl-seconds 1})
        (fn-trace/arm! {:prefixes ["sutra.kit.pubsub"] :ttl-seconds 60})
        (Thread/sleep 1500)
        (is (pos? (count @wrapped)) "the second arm outlives the first arm's dead-man")
        (finally (fn-trace/disarm!)))
      (is (zero? (count @wrapped))))))

(deftest degraded-overwrite
  (testing "a branch booted degraded installs nothing over its empty stand-in"
    (with-redefs [seat/schema-load-error (constantly {:branch "main" :message "kaputt"})]
      (let [e (try (mutation/install! {:branch "main"} {} {}) nil
                   (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= 503 (anomaly/status e)))
        (is (= :degraded (:error e)))))))
