(ns blobs.mem-test
  (:require [sutra.blobs.conformance :as blobs-conf]
            [sutra.blobs.mem :as blobs-mem]
            [clojure.test :refer [deftest]]))

(deftest mem-blobs-conform
  (blobs-conf/check-all blobs-mem/blobs))
