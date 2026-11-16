(ns ^:unit platform.scenario-writeback-defer-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.foundation.side-effect :as fx]
            [sutra.catalog.branch-policy :as policy]))

(def ^:private scenario "scenario/ri.app.1/abc.TmFtZQ")

(deftest a-scenario-defers-its-external-calls
  (testing "so the push is held, not refused, and
            the ontology edit beside it lands"
    (is (= :defer (fx/side-effect-verdict scenario #{} :external-calls)))))

(deftest a-plain-branch-still-fails-loudly
  (testing "only a scenario has an apply step; on a development branch a held
            push would never happen at all, so the refusal stays"
    (is (= :fail (fx/side-effect-verdict "feature-x" #{} :external-calls)))))

(deftest main-and-the-opt-in-are-untouched
  (is (= :allow (fx/side-effect-verdict "main" #{} :external-calls)))
  (is (= :allow (fx/side-effect-verdict nil #{} :external-calls)))
  (is (= :allow (fx/side-effect-verdict scenario #{:external-calls} :external-calls))
      ":on-branch {:external-calls true} still reaches the source from a fork"))

(deftest webhooks-and-notifications-keep-suppressing
  (is (= :suppress (fx/side-effect-verdict scenario #{} :webhooks)))
  (is (= :suppress (fx/side-effect-verdict scenario #{} :notifications))))

(deftest a-deferred-call-is-recorded-for-the-caller
  (testing "silence would leave the reader thinking the push happened"
    (let [fx (fx/side-effect {:branch scenario :allowed #{}} :external-calls
                                 {:table "WO_STAMM"})]
      (is (false? (fx/admit! fx)) "held, not refused")
      (is (= {:deferred-side-effects
              [{:kind :external-calls :verdict :defer :branch scenario
                :detail {:table "WO_STAMM"}}]}
             (policy/held-back [fx]))
          "deferred is its own word — a suppressed effect never happens"))))

(deftest nothing-is-deferred-on-the-base
  (let [fx (fx/side-effect {:branch "main" :allowed #{}} :external-calls {})]
    (is (true? (fx/admit! fx)))
    (is (= {} (policy/held-back [fx])))))
