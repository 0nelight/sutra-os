(ns ^:integration platform.connector-suites-test
  (:require [clojure.test :refer [deftest]]
            [platform.connectors :as connectors]
            [platform.sandbox-egress :as sandbox-egress]))

(deftest connectors-suite (connectors/run-tests))
(deftest sandbox-egress-suite (sandbox-egress/run-tests))
