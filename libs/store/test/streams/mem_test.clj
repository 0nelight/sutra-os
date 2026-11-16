(ns streams.mem-test
  (:require [clojure.test :refer [deftest]]
            [sutra.streams.conformance :as streams-conf]
            [sutra.streams.mem :as mem]))

(deftest mem-streams-conform
  (streams-conf/check-all mem/streams))
