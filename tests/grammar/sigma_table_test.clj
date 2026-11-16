(ns ^:unit grammar.sigma-table-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.ref.sigma :as sigma]
            [sutra.catalog.kernel :as kernel]))

(defn- kind-of [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:ref/kind (ex-data e)))))

(deftest a-stray-key-is-refused-where-sigma-is-built
  (is (= :sigma-unknown (kind-of #(sigma/check {:persited (fn [] {})})))
      "a misspelt hook is not silently left out")
  (is (= :sigma-unknown (kind-of #(sigma/check {:lint "gate-violations"})))
      "a function key holds a function")
  (is (map? (sigma/check (:sigma kernel/install-cx))) "the platform's install Σ is all declared"))

(deftest a-key-is-read-by-its-declaration
  (testing "a key no table names fails at the read"
    (is (= :sigma-unknown (kind-of #(sigma/call {} :persited)))))
  (testing "a left-out hook takes its default"
    (is (= [] (sigma/call {} :lint {})))
    (is (= :w1 (sigma/call {} :place :w :w1))))
  (testing "a required key left out fails"
    (is (= :sigma-missing (kind-of #(sigma/call {} :unpack {})))))
  (testing "an optional key is asked, not defaulted"
    (is (false? (sigma/provides? {} :structure-ok?)))
    (is (true? (sigma/provides? {:sigma {:structure-ok? (constantly true)}} :structure-ok?)))))
