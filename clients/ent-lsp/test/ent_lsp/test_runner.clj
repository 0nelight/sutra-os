(ns ent-lsp.test-runner
  (:require [clojure.test :as t]
            [ent-lsp.spec-test]
            [ent-lsp.analysis-test]
            [ent-lsp.features-test]
            [ent-lsp.server-test]
            [ent-lsp.diff-test]
            [ent-lsp.checks-test]
            [ent-lsp.cursor-test]
            [ent-lsp.grammar-test]
            [ent-lsp.projekttext-test]
            [ent-lsp.online-test]
            [ent-lsp.python-test]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'ent-lsp.spec-test 'ent-lsp.analysis-test
                                          'ent-lsp.features-test 'ent-lsp.server-test
                                          'ent-lsp.diff-test 'ent-lsp.checks-test
                                          'ent-lsp.cursor-test 'ent-lsp.grammar-test
                                          'ent-lsp.projekttext-test 'ent-lsp.online-test
                                          'ent-lsp.python-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
