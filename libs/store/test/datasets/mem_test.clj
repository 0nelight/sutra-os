(ns datasets.mem-test
  (:require [clojure.test :refer [deftest]]
            [sutra.datasets.conformance :as datasets-conf]
            [sutra.datasets.mem :as mem]))

(deftest mem-datasets-conform
  (datasets-conf/check-all mem/datasets))
