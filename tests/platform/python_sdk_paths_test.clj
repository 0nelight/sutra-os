(ns ^:unit platform.python-sdk-paths-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.serve.sdk-paths :as sdk-paths]))

(deftest the-python-endpoint-table-is-the-servers
  (is (= (sdk-paths/fresh) (sdk-paths/committed))
      (str sdk-paths/file " is stale — run `clojure -M:dev -m sutra.serve.sdk-paths`")))
