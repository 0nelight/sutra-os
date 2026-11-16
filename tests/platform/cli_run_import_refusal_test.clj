(ns ^:unit platform.cli-run-import-refusal-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.cli.verbs.imports]))

(deftest a-refused-build-raises-instead-of-reporting-green
  (let [settle #'sutra.cli.verbs.imports/settle-import!
        body   {:error "build scope ausgangsrechnung selects no build node — …"
                :data  {:empty-build-scope ["ausgangsrechnung"]}}
        e      (try (with-out-str
                      (settle {} {:resp {:status 400 :body body}} nil [:ausgangsrechnung]))
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
    (is (some? e) "a 400 is a failed line, not a printed-and-forgotten one")
    (is (= 400 (:status (ex-data e))))
    (is (= body (:body (ex-data e))) "the server's finding travels whole")))
