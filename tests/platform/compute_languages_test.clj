(ns ^:unit platform.compute-languages-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.compute.runner :as runner]
            [sutra.compute.sql :as sql]
            [sutra.types.languages :as langs]
            [support.registry :as registry]))

(def ^:private code-languages (set (filter langs/code-language? langs/compute-language-names)))
(def ^:private query-languages (set (filter langs/query-language? langs/compute-language-names)))

(deftest every-language-is-code-or-query
  (registry/covers "code + query" langs/compute-language-names
                   (into code-languages query-languages)))

(deftest every-code-language-has-a-whole-runner
  (registry/covers "runner/runners" code-languages runner/runners)
  (registry/complete runner/runners [:exec :prepare-inputs :introspect]))

(deftest every-query-language-runs-through-its-dialect
  (testing "an action refuses a language without a runner: a query language must stay refused"
    (is (not-any? runner/runners query-languages)))
  (doseq [language query-languages]
    (testing language
      (is (contains? @#'sql/dialects (langs/dialect language)))
      (testing "a dialect says how it is sealed read-only, also when it cannot be"
        (is (contains? (@#'sql/dialects (langs/dialect language)) :seal-sql)))
      (is (= :engine (langs/executor language))))))

(deftest only-a-query-language-runs-on-an-engine
  (doseq [language code-languages]
    (testing language
      (is (not= :engine (langs/executor language))))))
