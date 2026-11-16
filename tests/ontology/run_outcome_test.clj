(ns ^:unit ontology.run-outcome-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.run]))

(def ^:private outcome #'sutra.builds.run/outcome)
(def ^:private completed-writes #'sutra.builds.run/completed-writes)

(deftest what-the-end-of-a-run-writes-is-decided-without-a-store
  (let [run    {:id #uuid "00000000-0000-0000-0000-000000000001"}
        writes {:dataset-writes [{:dataset-rid :d}] :consumed [{:consumer-rid :c}]}]
    (testing "a run that ends finishes its row and lands its writes as transactions"
      (is (= {:start?       false
              :finish       {:counts {:objects 2} :summary {:s 1} :snapshot-id "v2"}
              :transactions {:branch "main" :run-id (:id run) :snapshot-id "v2"
                             :writes [{:dataset-rid :d}] :consumed [{:consumer-rid :c}]
                             :at "t"}}
             (outcome "main" run {:counts {:objects 2} :summary {:s 1}} writes "v2" "t"))))
    (testing "an :in-commit? run whose body did not commit its row starts it now"
      (is (:start? (outcome "main" (assoc run :in-commit? true) {} {} "v2" "t"))))
    (testing "a failed run, or one its body committed, writes no end facts"
      (is (nil? (:finish (outcome "main" run {:failure (ex-info "x" {})} writes "v2" "t"))))
      (is (nil? (:finish (outcome "main" run {:run-committed? true} writes "v2" "t"))))
      (is (false? (:start? (outcome "main" (assoc run :in-commit? true)
                                    {:run-committed? true} {} "v2" "t")))))
    (testing "without dataset writes there is no transaction"
      (is (nil? (:transactions (outcome "main" run {} {:dataset-writes []} "v2" "t")))))))

(deftest a-failed-run-lands-only-what-its-completed-jobs-wrote
  (is (= {:dataset-writes [{:job-rid "ok"}] :consumed []}
         (completed-writes #{"dead"} #uuid "00000000-0000-0000-0000-000000000001"
                           {:dataset-writes [{:job-rid "ok"} {:job-rid "dead"}]}))))
