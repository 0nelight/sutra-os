(ns ^:unit grammar.bad-attrs-diagnosis-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.actions.batch :as batch]))

(def ^:private SCHEMA
  {:object-types
   {:ri.o.1 {:object-type-name :einbau-nachweis
             :attribute-list [{:attribute-name :nachweis-id :attribute-type :string}
                              {:attribute-name :name    :attribute-type :string}
                              {:attribute-name :eingebaut-am    :attribute-type :date}
                              {:attribute-name :bemerkung    :attribute-type :string}]
             :plural "Einbau nachweise"}}
   :value-types {}})

(def ^:private good {:nachweis-id "ESN-7781-2001" :name "Triebwerk 1" :eingebaut-am "2001-01-01"})

(deftest the-publish-plan-carries-the-named-diagnosis
  (let [{:keys [row-fn]} (batch/compile-plan
                          SCHEMA {:kind :objects :verb :create :type :ri.o.1} nil nil)
        boom (try (row-fn {:id (random-uuid) :values (assoc good :quell-pdf "x")})
                  (catch clojure.lang.ExceptionInfo e e))]
    (testing "it still throws"
      (is (instance? clojure.lang.ExceptionInfo boom)))
    (testing "the message names the key"
      (is (re-find #":quell-pdf" (ex-message boom))))
    (testing "and the ex-data carries it structurally, for a caller that reports"
      (is (= [:quell-pdf] (:undeclared (ex-data boom))))
      (is (= :ri.o.1 (:type (ex-data boom)))))
    (testing "a clean row still passes"
      (is (= good (:values (row-fn {:id (random-uuid) :values good})))))))

(deftest the-publish-plan-carries-the-named-value-diagnosis
  (let [{:keys [row-fn]} (batch/compile-plan
                          SCHEMA {:kind :objects :verb :create :type :ri.o.1} nil nil)
        boom (try (row-fn {:id (random-uuid)
                           :values (assoc good :bemerkung {:id 1 :values {}})})
                  (catch clojure.lang.ExceptionInfo e e))]
    (is (instance? clojure.lang.ExceptionInfo boom))
    (testing "the message names the attribute rather than only the type"
      (is (re-find #":bemerkung\b" (ex-message boom))))
    (testing "and the ex-data carries the findings for a caller that reports"
      (is (= [:bemerkung] (map :attribute (:bad-values (ex-data boom)))))
      (is (= :ri.o.1 (:type (ex-data boom)))))))
