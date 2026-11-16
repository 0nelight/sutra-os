(ns ^:unit grammar.gate-on-crash-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.authoring.install.plan :as install-plan]
            [sutra.parser.resolve :as res]
            [sutra.branching.push]))

(deftest a-crashing-reference-check-fails-the-push-gate
  (with-redefs [res/explain             (fn [& _] (throw (ex-info "boom" {})))
                install-plan/live-ref-symbols (fn [_] {})]
    (let [{:keys [status
                  detail]}
            (@#'sutra.branching.push/lint-gate {} (delay {}) {:schema {:objects {}}})]
      (is (= :failed status))
      (is (re-find #"could not run: boom" detail)))))
