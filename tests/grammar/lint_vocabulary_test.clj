(ns ^:unit grammar.lint-vocabulary-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.parser.lint :as lint]))

(def ^:private schema
  {:actions {:a {:rules [{:op :teleport}]
                 :params {:x {:type :string}
                          :y {:type :string :overrides [{:when [:get :z] :required true}]}}}}})

(defn- rules-of [vocab]
  (into #{} (map :rule) (lint/run-lint schema vocab)))

(deftest lint-judges-with-the-vocabulary-it-is-handed
  (testing "without a vocabulary, the checks that need one are skipped"
    (is (not-any? #{:action-rule-op-unknown :action-override-misleading} (rules-of {}))))
  (testing "an op the vocabulary does not know is refused"
    (is (contains? (rules-of {:rule-ops #{:create}}) :action-rule-op-unknown)))
  (testing "an override reading an undeclared parameter misleads"
    (is (contains? (rules-of {:field-refs (fn [_] [:z])}) :action-override-misleading))))

(deftest an-override-may-read-a-hidden-parameter-the-host-passes
  (let [schema {:actions
                {:a {:rules [{:op :create}]
                     :form {:sections [{:params [:neu]} {:params [:lesung]}]}
                     :params {:lesung {:type :beleglesung :exposure :hidden}
                              :neu {:type :boolean :default false
                                    :overrides [{:when [:!= [:get :lesung :name] nil]
                                                 :default true}]}}}}}]
    (is (not-any? #{:action-override-misleading}
                  (map :rule (lint/run-lint schema {:field-refs (fn [_] [:lesung])}))))))
