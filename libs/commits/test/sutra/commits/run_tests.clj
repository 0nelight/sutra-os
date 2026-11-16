(ns sutra.commits.run-tests
  (:require [clojure.test :as t]
            [sutra.commits.loader-test]
            [sutra.commits.branches-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'sutra.commits.loader-test
                                             'sutra.commits.branches-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
