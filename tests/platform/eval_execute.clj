(ns ^:unit platform.eval-execute
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.evals.suites :as evals]))

(def ^:private score-actuals #'evals/score-actuals)

(def ^:private abstain-suite
  {:function :classify
   :cases [{:input {:stub-kind "rechnung"}   :expected {:kind "rechnung"}}
           {:input {}                        :expected {:abstain true}}
           {:input {:stub-kind "bestellung"} :expected {:abstain true}}]
   :evaluators [{:name :kind_exact :kind :exact-match :field :kind}
                {:name :drops      :kind :abstain}]})

(deftest abstain-evaluator-scores-over-classification
  (let [scored (score-actuals abstain-suite [{:kind "rechnung"} nil {:kind "bestellung"}]
                              nil)]
    (testing "exact-match grades ONLY the labeled-kind case — abstain cases are out of scope, not misses"
      (is (== 1.0 (:value (:kind_exact scored))))
      (is (= 1 (:n (:detail (:kind_exact scored))))))
    (testing "abstain: 1 of 2 drop-cases correctly dropped; the leak is named"
      (is (== 0.5 (:value (:drops scored))))
      (is (= {:abstained 1 :n 2 :leaked [2]} (:detail (:drops scored)))))))

(deftest abstain-evaluator-is-vacuously-one-without-drop-cases
  (is (== 1.0 (:value (#'evals/score-evaluator {:kind :abstain} [{:kind "x"}] [{:kind "x"}])))))
