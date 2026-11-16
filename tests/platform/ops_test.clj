(ns ^:unit platform.ops-test
  (:require [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [sutra.foundation.ops :as op]
            [sutra.kit.system :as sys]))

(deftest the-operations-hold-their-rules
  (sys/discover)
  (testing "rules 1, 2, 4, 12 over the table and every body the modules bind"
    (is (= [] (op/problems))))
  (testing "rule 13: every operation has a body once every module is loaded"
    (is (= [] (vec (op/unbound))))))

(deftest a-call-is-checked-against-its-contract
  (sys/discover)
  (testing "an edge checks :in always"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"in breaks its contract"
                          (op/query {} :lineage/drawing {:format :png :drawing {}}))))
  (testing "a non-query is not asked, an unknown name is refused"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not an operation"
                          (op/query {} :no/such {}))))
  (testing "a body asks only what its :uses names"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not in its :uses"
                          (op/ask {::op/caller :lineage/drawing} :branch/ended {})))))

(deftest the-entry-shape-refuses-a-command-without-domain
  (is (some? (#'op/load-table "sutra/ops.edn")))
  (is (not (m/validate op/entry-schema
                       {:service :x :kind :command :doc "d" :in :any :out :any}))))
