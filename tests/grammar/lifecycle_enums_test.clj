(ns grammar.lifecycle-enums-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.automation.executions :as executions]
            [sutra.builds.run.queue :as queue]
            [sutra.kit.lifecycle :as lc]
            [sutra.store.control.tables :as cc]))

(defn- status-enum [t]
  (some (fn [[_ col vs]] (when (= :status col) (set vs)))
        (:enums (some #(when (= t (:name %)) %) cc/tables))))

(deftest a-table-names-only-its-states
  (is (thrown? clojure.lang.ExceptionInfo
               (lc/table {:name :x :states #{:a} :final #{}
                          :edges {:go {:from #{:a} :to :b}}})))
  (is (thrown? clojure.lang.ExceptionInfo
               (lc/table {:name :x :states #{:a :b} :final #{:b}
                          :edges {:back {:from #{:b} :to :a}}}))))

(deftest next-state-answers-where-decide-refuses
  (let [t (lc/table {:name :x :states #{:a :b} :final #{:b}
                     :edges {:go {:from #{:a} :to :b
                                  :guard (fn [r _] (when (:held r) "held"))}}})]
    (is (= :b (lc/next-state t :go :a {} nil)))
    (is (nil? (lc/next-state t :go :b {} nil)) "a state the edge does not leave from")
    (is (nil? (lc/next-state t :go nil nil nil)) "no record")
    (is (nil? (lc/next-state t :go :a {:held true} nil)) "a guard refuses")
    (is (nil? (lc/next-state t :nope :a {} nil)) "no such event")))

(deftest status-constraints-name-the-lifecycle-states
  (doseq [[t table] {:build-requests        queue/request-table
                     :automation-executions executions/execution-table}]
    (testing (name t)
      (is (some? (status-enum t)))
      (is (= (lc/state-names table) (status-enum t))))))
