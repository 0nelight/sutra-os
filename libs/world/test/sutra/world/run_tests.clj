(ns sutra.world.run-tests
  (:require [clojure.test :as t]
            [sutra.world.delta-test]
            [sutra.world.gate-space-test]
            [sutra.world.install-test]
            [sutra.world.law-test]
            [sutra.world.near-test]
            [sutra.world.register-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'sutra.world.delta-test 'sutra.world.gate-space-test
                                          'sutra.world.law-test 'sutra.world.near-test
                                          'sutra.world.register-test 'sutra.world.install-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
