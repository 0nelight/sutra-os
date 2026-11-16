(ns sutra.ref.run-tests
  (:require [clojure.test :as t]
            [sutra.ref.conformance-test]
            [sutra.ref.objectset-test]
            [sutra.ref.recorder-model-test]))

(defn -main [& _]
  (let [{:keys [fail
                error]} (t/run-tests 'sutra.ref.conformance-test 'sutra.ref.objectset-test
                                     'sutra.ref.recorder-model-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
