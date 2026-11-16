(ns ^:unit ontology.transform-run-flags-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.compute.external :as ext]))

(def ^:private flags {:refuse-cap? true :incremental-run? true})

(deftest the-remote-job-carries-the-run-flags
  (let [job (atom nil)]
    (with-redefs-fn {#'ext/post-edn (fn [_ payload _ & _] (reset! job payload) {})
                     #'ext/read-result-refs! identity}
      #(#'ext/run-transform-remote! "http://w" (assoc flags :transform-name :t :code "") {}))
    (is (= flags (select-keys @job (keys flags))))))

(deftest the-worker-hands-them-to-the-sandbox
  (let [compute (atom nil)]
    (with-redefs-fn {#'ext/run-transform-local! (fn [c _] (reset! compute c) {})
                     #'ext/spill-result identity}
      #(#'ext/run-transform-from-content! (assoc flags :transform-name :t :code "" :inputs {})))
    (is (= flags (select-keys @compute (keys flags))))
    (is (= {"SUTRA_REFUSE_CAP" "1" "SUTRA_INCREMENTAL_RUN" "1"}
           (select-keys (#'ext/sandbox-env @compute {} false)
                        ["SUTRA_REFUSE_CAP" "SUTRA_INCREMENTAL_RUN"])))))
