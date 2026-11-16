(ns sutra.lib.run-tests
  (:require [clojure.test :as t]
            [sutra.lib.mro-test]
            [sutra.lib.stored-link-test]
            [sutra.lib.fence-test]
            [sutra.lib.object-set-test]
            [sutra.lib.pushdown-test]
            [sutra.lib.derived-filter-test]
            [sutra.lib.reinstall-test]
            [sutra.lib.read-test]
            [sutra.lib.boundary-test]
            [sutra.lib.lowering-test]))

(defn -main [& _]
  (let [{:keys [fail
                error]} (t/run-tests 'sutra.lib.mro-test 'sutra.lib.stored-link-test
                                     'sutra.lib.fence-test
                                             'sutra.lib.object-set-test
                                             'sutra.lib.pushdown-test
                                             'sutra.lib.derived-filter-test
                                             'sutra.lib.reinstall-test
                                             'sutra.lib.read-test
                                             'sutra.lib.boundary-test
                                             'sutra.lib.lowering-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
