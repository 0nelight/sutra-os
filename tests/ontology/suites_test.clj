(ns ^:integration ontology.suites-test
  (:require [clojure.test :refer [deftest use-fixtures]]
            [support.pod :as pod]
            [ontology.scheduler :as scheduler]
            [ontology.writeback-changelog :as writeback-changelog]
            [ontology.writeback-create :as writeback-create]
            [ontology.writeback-target-declared :as writeback-target-declared]
            [sutra.sources.writeback-dataset]))

(use-fixtures :once pod/with-mem-stores-only)

(deftest scheduler-suite (scheduler/run-tests))
(deftest writeback-create-suite (writeback-create/run-tests))
(deftest writeback-changelog-suite (writeback-changelog/run-tests))
(deftest writeback-target-declared-suite (writeback-target-declared/run-tests))
