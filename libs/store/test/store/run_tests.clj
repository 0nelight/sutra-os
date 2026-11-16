(ns store.run-tests
  (:require [blobs.mem-test]
            [clojure.test :as t]
            [coord.mem-test]
            [datasets.landing-test]
            [datasets.mem-test]
            [store.granular-test]
            [store.edit-history-test]
            [store.machine-test]
            [store.mem-test]
            [store.pushdown-test]
            [store.records-test]
            [streams.mem-test]
            [sutra.store.core.engine]))

(defn -main [& _]
  (alter-var-root #'sutra.store.core.engine/*verify* (constantly true))
  (alter-var-root #'sutra.store.core.engine/*one-handle-per-lane* (constantly true))
  (let [{:keys [fail
                error]} (t/run-tests 'store.mem-test 'store.pushdown-test 'store.machine-test
                                     'store.records-test 'store.granular-test
                                     'store.edit-history-test
                                          'blobs.mem-test 'coord.mem-test 'datasets.mem-test 'datasets.landing-test
                                          'streams.mem-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
