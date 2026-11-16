(ns ^:unit platform.kernel-suites-test
  (:require [clojure.test :refer [deftest]]
            [platform.schema-views :as schema-views]
            [platform.kernel-instance :as kernel-instance]
            [platform.kernel-trace :as kernel-trace]))

(deftest schema-views-suite (schema-views/run-tests))
(deftest kernel-instance-suite (kernel-instance/run-tests))
(deftest kernel-trace-suite (kernel-trace/run-tests))
