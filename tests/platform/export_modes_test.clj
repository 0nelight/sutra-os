(ns ^:unit platform.export-modes-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.run.exports :as exports]
            [sutra.builds.plan :as bplan]
            [sutra.sources.policy :as policy]
            [sutra.types.export :as export]
            [sutra.kit.refusal :as refusal]))

(def ^:private t0 (java.time.LocalDateTime/of 2026 10 25 9 30 5))

(deftest the-modes-are-standard-six
  (is (= #{:mirror :full :full-truncate :incremental :incremental-truncate :incremental-strict}
         (set (keys export/modes))))
  (testing "no upsert among them — a row-level change goes through a webhook"
    (is (empty? (filter #(re-find #"upsert" (name %)) (keys export/modes))))))

(deftest only-the-declared-modes-clear-the-destination
  (is (= #{:full-truncate :incremental-truncate} export/truncating-modes))
  (testing ":mirror clears only when the dataset was snapshot"
    (is (true?  (export/truncates? :mirror true)))
    (is (false? (export/truncates? :mirror false))))
  (testing "a non-truncating mode never clears, snapshot or not"
    (is (false? (export/truncates? :incremental true)))
    (is (false? (export/truncates? :full true)))))

(deftest an-incremental-mode-refuses-what-it-cannot-express
  (testing "\"does not support UPDATE and DELETE transactions\""
    (is (= "UPDATE" (export/refused-transaction :mirror ["APPEND" "UPDATE"] false)))
    (is (= "DELETE" (export/refused-transaction :incremental ["DELETE"] false)))
    (is (nil? (export/refused-transaction :incremental ["APPEND" "APPEND"] false))))
  (testing "a full export rewrites whatever it finds"
    (is (nil? (export/refused-transaction :full ["UPDATE" "DELETE" "SNAPSHOT"] false)))
    (is (nil? (export/refused-transaction :full-truncate ["DELETE"] false))))
  (testing ":incremental-strict fails on a SNAPSHOT — but not on the first run, which
            has nothing it could already have exported"
    (is (= "SNAPSHOT" (export/refused-transaction :incremental-strict ["SNAPSHOT"] false)))
    (is (nil? (export/refused-transaction :incremental-strict ["SNAPSHOT"] true)))))

(deftest the-first-run-of-an-incremental-export-writes-everything
  (let [p (exports/plan-export {:mode :mirror} {:first-run? true :types ["SNAPSHOT"]})]
    (is (= :all (:rows p)))
    (is (true? (:truncate? p))))
  (let [p (exports/plan-export {:mode :mirror} {:first-run? false :types ["APPEND"]})]
    (is (= :unexported (:rows p)))
    (is (false? (:truncate? p)))))

(deftest a-declaration-without-a-mode-mirrors
  (is (= :mirror (:mode (exports/plan-export {} {:first-run? true :types []}))))
  (is (= export/default-mode :mirror)))

(deftest an-unsupported-transaction-is-reported-not-written
  (let [p (exports/plan-export {:mode :incremental} {:first-run? false :types ["UPDATE"]})]
    (is (= "UPDATE" (:refused p)))
    (is (nil? (:rows p)) "a refused plan names no rows to write")))

(def ^:private source
  {:access :read-write
   :writeback {"acars_raw" {:insertable-columns [:id :zyklen] :truncatable? true}
               "locked"    {:insertable-columns [:id]}}})

(deftest an-export-writes-only-an-allowlisted-destination
  (is (thrown? clojure.lang.ExceptionInfo
               (policy/write-plan t0 source {:table "somewhere_else" :rows [{:id "1"}]}))))

(deftest an-export-writes-only-insertable-columns
  (is (= {:op :insert-rows :table "acars_raw" :columns ["id" "zyklen"]
          :rows [{"id" "1" "zyklen" 7}]}
         (policy/write-plan t0 source {:table "acars_raw" :rows [{:id "1" :zyklen 7}]})))
  (is (thrown? clojure.lang.ExceptionInfo
               (policy/write-plan t0 source {:table "acars_raw" :rows [{:id "1" :geheim "x"}]}))))

(deftest every-row-of-one-export-carries-the-same-columns
  (is (thrown? clojure.lang.ExceptionInfo
               (policy/write-plan t0 source {:table "acars_raw"
                                             :rows [{:id "1" :zyklen 7} {:id "2"}]}))))

(deftest clearing-the-destination-is-its-own-grant
  (is (= {:op :truncate :table "acars_raw"}
         (policy/write-plan t0 source {:table "acars_raw" :truncate true})))
  (testing "a table that may be inserted into may not therefore be cleared"
    (is (thrown? clojure.lang.ExceptionInfo
                 (policy/write-plan t0 source {:table "locked" :truncate true})))))

(deftest a-read-only-source-takes-no-export
  (is (thrown? clojure.lang.ExceptionInfo
               (policy/write-plan t0 (assoc source :access :read-only)
                                  {:table "acars_raw" :rows [{:id "1"}]}))))

(def ^:private schema-with-export
  {:datasets   {"ri.d.1" {:dataset-name :roh :columns []}}
   :exports    {"ri.ex.1" {:export-name :roh-nach-camo :source "ri.s.1" :dataset "ri.d.1"
                           :destination {:table "acars_raw"} :mode :mirror}}
   :transforms {"ri.t.1" {:transform-name :roh-nach-camo-export
                          :input  {:kind :dataset :dataset "ri.d.1"}
                          :output {:kind :export :export "ri.ex.1"}}}})

(deftest a-schedule-names-the-export-not-its-build-node
  (testing "its own name resolves — an export carries its own schedule"
    (is (= #{"ri.t.1"} (:transforms (bplan/resolve-build-targets schema-with-export
                                                                 [:roh-nach-camo])))))
  (testing "and so does the lifted node, which is what the DAG runs"
    (is (= #{"ri.t.1"} (:transforms (bplan/resolve-build-targets schema-with-export
                                                                 [:roh-nach-camo-export])))))
  (testing "naming the dataset is not naming the export — an export CONSUMES it, so it
            comes in through a :downstream scope, not through the dataset's producers"
    (is (empty? (:transforms (bplan/resolve-build-targets schema-with-export [:roh]))))))

(deftest an-export-is-its-own-job-scope
  (is (= #{"ri.t.1"} (get-in (#'bplan/job-scope schema-with-export ["ri.t.1"])
      [:node-scope :exports])))
  (testing "it produces no dataset, so :datasets never holds it"
    (is (empty? (get-in (#'bplan/job-scope schema-with-export ["ri.t.1"])
        [:node-scope :datasets])))))

(deftest nothing-unexported-is-nothing-new-unless-the-mode-rewrites
  (is (true? (:nothing-new? (exports/plan-export {:mode :mirror}
                                                 {:first-run? false :types []}))))
  (is (false? (:nothing-new? (exports/plan-export {:mode :mirror}
                                                  {:first-run? false :types ["APPEND"]}))))
  (is (false? (:nothing-new? (exports/plan-export {:mode :mirror}
                                                  {:first-run? true :types []})))))

(deftest a-declaration-that-cannot-run-says-why
  (is (re-find #"names a source"
               (apply refusal/message (exports/export-misconfiguration :e :s nil "t"))))
  (is (re-find #"no destination table"
               (apply refusal/message (exports/export-misconfiguration :e :s {} " "))))
  (is (nil? (exports/export-misconfiguration :e :s {} "t"))))
