(ns ^:unit platform.scenario-replay-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.scenarios.apply :as scenario]))

(deftest opaque-entries-refuse-by-default
  (testing "a definition that cannot be fully reproduced must not half-replay"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"cannot reproduce"
         (scenario/replay-actions! nil nil {:replayable [] :opaque [{:seq 1 :action nil}]})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"cannot reproduce"
         (scenario/replay-actions! nil nil
                                   {:replayable [] :opaque [{:seq 7 :action "function"}]})))))

(deftest the-refusal-names-what-it-could-not-reproduce
  (testing "the opaque rows ride on the ex-data — a caller has to be able to SAY
            which history it is, not just that replay declined"
    (let [op [{:seq 3 :action "function"} {:seq 9 :action nil}]]
      (is (= op (:opaque (try (scenario/replay-actions! nil nil {:replayable [] :opaque op})
                              (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))

(deftest the-action-cap-is-enforced
  (testing "a scenario is a bounded overlay, not a second dataset"
    (let [over (mapv (fn [i] {:seq i :action :noop :args {}})
                     (range (inc scenario/max-scenario-actions)))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"over the"
           (scenario/replay-actions! nil nil {:replayable over :opaque []}))))
    (testing "and the cap is checked BEFORE anything is applied"
      (let [over (mapv (fn [i] {:seq i :action :noop :args {}})
                       (range (inc scenario/max-scenario-actions)))]
        (is (re-find #"over the"
                     (try (scenario/replay-actions! nil nil {:replayable over :opaque []}) ""
                          (catch clojure.lang.ExceptionInfo e (.getMessage e)))))))))

(deftest force-accepts-a-different-result
  (testing ":force? is the explicit 'replay the declared actions only'"
    (is (= [] (scenario/replay-actions! nil nil {:replayable [] :opaque [{:seq 1 :action nil}]}
                                          {:force? true})))))
