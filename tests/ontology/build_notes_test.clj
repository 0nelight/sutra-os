(ns ^:unit ontology.build-notes-test
  (:require [sutra.builds.ledger.jobs :as jl]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.builds.plan.dag :as dag]
            [clojure.test :refer [deftest is testing]]
            [sutra.builds.ledger.facts :as facts]))

(def ^:private note! #'inputs/note!)

(def ^:private run-id #uuid "00000000-0000-0000-0000-000000000002")

(defn- with-job [body]
  (let [ledger (atom [])]
    (with-redefs [jl/ledger! (fn [_ effects] (swap! ledger into effects))]
      (assoc (#'dag/with-job! {:schema {} :run-id run-id} (facts/empty-facts false) :t 1 body)
             :ledger @ledger))))

(deftest a-job-hands-back-its-notes
  (let [{:keys [r noted ledger]} (with-job (fn [notes]
                                             (note! {:notes notes} :rows-in :t 4)
                                             (note! {:notes notes} :job-log :t "ran")
                                             {:facts []}))]
    (is (= {:facts []} r))
    (is (= {:t 4} (:rows-in noted)))
    (is (= [4 "ran"] ((juxt :rows-in :job-log) (:facts (last ledger))))
        "the ledger's finished job carries what the job noted")))

(deftest a-failed-job-still-hands-back-its-notes
  (let [{:keys [e noted ledger]} (with-job (fn [notes]
                                             (note! {:notes notes} :job-log :t "select 1")
                                             (throw (ex-info "boom" {}))))]
    (is (= "boom" (ex-message e)))
    (is (= {:t "select 1"} (:job-log noted)) "the failure report reads the log the job wrote")
    (is (= [:RUNNING] (map :state ledger))
        "only a finished job is closed here; run-node! records the failure")))

(deftest a-preview-has-no-sink
  (is (nil? (note! {} :rows-in :t 1))))

(defn- code-node [t] {:kind :object :spec {:from {:compute {:transform-name t :language :python}}}})

(deftest a-prewarmed-compute-notes-once
  (let [computed (atom 0)
        env      {:row-cache (atom {}) :ctx {}}
        build-state       {:noted (facts/empty-facts false) :acc {}}]
    (with-redefs [inputs/resolve-code-compute  (fn [ctx c] (note! ctx :rows-in :t 5) {:compute c})
                  inputs/run-resolved-compute! (fn [_ {:keys [compute]}]
                                                   (swap! computed inc)
                                                   (when (= :bad (:transform-name compute))
                                                     (throw (ex-info "fails" {})))
                                                   {:rows [{:a 1}]})]
      (let [build-state (#'dag/prewarm-level! env [(code-node :t) (code-node :bad)] build-state)
            build-state (#'dag/prewarm-level! env [(code-node :t) (code-node :bad)] build-state)]
        (is (= {:t 5} (:rows-in (:noted build-state)))
            "a cached compute is neither run nor noted again; a failed one notes nothing")
        (is (= 3 @computed) ":t once; :bad in each level, as it is never cached")
        (testing "a node reading the cache notes nothing"
          (let [notes (atom [])]
            (is (= [{:a 1}] (#'inputs/read-source-rows
                             (assoc env :notes notes)
                             {:compute {:transform-name :t :language :python}} nil)))
            (is (empty? @notes))))))))
