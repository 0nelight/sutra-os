(ns coord.mem-test
  (:require [clojure.test :refer [deftest]]
            [sutra.coord.conformance :as coord-conf]
            [sutra.coord.mem :as mem]))

(deftest mem-coordinator-conforms
  (coord-conf/check-all mem/coordinator))
