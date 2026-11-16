(ns ^:unit ontology.job-attempt-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.plan.dag]
            [sutra.builds.ledger.facts :as facts]))

(def ^:private attempt-end #'sutra.builds.plan.dag/attempt-end)
(def ^:private verdict #'sutra.builds.plan.dag/verdict)
(def ^:private failure-of #'sutra.builds.plan.dag/failure-of)

(def ^:private run-id #uuid "00000000-0000-0000-0000-000000000001")

(deftest how-an-attempt-ends-is-decided-without-a-store
  (let [noted  (facts/empty-facts false)
        out    {:commit "v2" :r {:facts [{:dataset-rid :d1 :written 3}
                                         {:dataset-rid :d2 :written 4}]}}
        cursor {:consumer-rid :t :input-dataset-rid :in :last-snapshot-id "v1"}
        notes  [[:consumed-cursors nil cursor] [:rows-in :t 7]]]
    (testing "a succeeded job finishes, sums its rows and advances its cursors"
      (let [{:keys [ledger retract-unless-done retract]}
            (attempt-end {} "main" :t :jr "v0" out notes noted "at")
            [finish advance] ledger]
        (is (= :COMPLETED (:state finish)))
        (is (= 7 (get-in finish [:facts :rows-out])))
        (is (= "v2" (get-in finish [:facts :snapshot-id])))
        (is (= [cursor] (:consumed advance)))
        (is (= [:d1 :d2] retract-unless-done))
        (is (nil? retract))))
    (testing "without versions nothing is advanced or rolled back"
      (let [{:keys [ledger retract-unless-done]}
            (attempt-end {} "main" :t :jr nil out notes noted "at")]
        (is (= 1 (count ledger)))
        (is (empty? retract-unless-done))))
    (testing "a failed attempt rolls back what its transform declares, and finishes nothing"
      (let [schema {:transforms {:t {:outputs [{:kind :dataset :dataset :d1}]}}}
            end    (attempt-end schema "main" :t :jr "v0" {:e (ex-info "x" {})} [] noted "at")]
        (is (nil? (:ledger end)))
        (is (contains? end :retract))))))

(deftest what-follows-an-attempt
  (let [transient (ex-info "x" {:status 503})
        hard      (ex-info "x" {})
        violation (ex-info "x" {:violations [{}]})]
    (is (= :done (verdict :object 1 3 nil)))
    (is (= :retry (verdict :object 1 3 transient)))
    (is (= :failure (verdict :object 3 3 transient)) "the last attempt is not retried")
    (is (= :failure (verdict :object 1 3 hard)))
    (is (= :restriction (verdict :object 1 3 violation)))
    (is (= :failure (verdict :producer 1 3 violation)))
    (is (= :rethrow (verdict :object 1 3 (Error. "jvm"))))))

(deftest a-failure-dooms-its-dependents-as-a-value
  (let [graph {:rdeps {:a #{:b} :b #{}}}
        node  {:t-kw :a :kind :producer :key :a}
        state {:failures [] :doomed #{}}
        e     (ex-info "boom" {})]
    (let [{:keys [state ledger line]} (failure-of {} graph run-id false state node :a e "at")]
      (is (= #{:b} (:doomed state)))
      (is (= [:b] (vec (:aborted (first (:failures state))))))
      (is (= 1 (count ledger)) "only the dependents are aborted")
      (is (re-find #"isolated" line)))
    (is (= 2 (count (:ledger (failure-of {} graph run-id true state node :a e "at"))))
        "under abort-on-failure every other job is aborted too")))
