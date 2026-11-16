(ns sutra.service.run-tests
  (:require [clojure.test :as t]
            [sutra.service.marking-gate-test]
            [sutra.service.merge-test]
            [sutra.service.rights-test]
            [sutra.service.access-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'sutra.service.merge-test 'sutra.service.rights-test
                                          'sutra.service.marking-gate-test
                                          'sutra.service.access-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
