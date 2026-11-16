(ns ^:api platform.decision-record-build-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.compute.sandbox :as sandbox]
            [sutra.foundation.decision-trace :as dtrace]
            [sutra.kit.decisions :as decisions]
            [support.build-scenario :as bs]
            [support.pod :as pod]))

(use-fixtures :once pod/with-all-stores-pod)

(deftest a-recorded-build-replays-from-its-text
  (if-not (sandbox/available?)
    (println "  (decision-record-build SKIPPED — python sandbox unavailable)")
    (try
      (dtrace/arm! {:kinds [:build/plan :build/merge]})
      (bs/install-chain!)
      (bs/run-and-assert-convergence!)
      (let [text (dtrace/records-text)
            rs   (decisions/read-text text)
            by   (group-by :kind rs)]
        (testing "the build kept its plan and its merges"
          (is (seq (:build/plan by)) (pr-str (decisions/status)))
          (is (seq (:build/merge by)) (pr-str (decisions/status))))
        (testing "each, read back from text, is decided again to the same answer"
          (doseq [r rs]
            (is (:same? (decisions/replay r)) (str (:kind r) " at " (:at r))))))
      (finally (dtrace/disarm!)))))
