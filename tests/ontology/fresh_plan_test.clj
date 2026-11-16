(ns ^:unit ontology.fresh-plan-test
  (:require [sutra.builds.plan.freshness :as fresh]
            [sutra.builds.plan :as plan]
            [clojure.test :refer [deftest is testing]]))

(def ^:private graph {:rdeps {:a #{:b} :b #{} :c #{}}})

(def ^:private nodes
  [{:kind :producer :key :a :t-kw :a}
   {:kind :producer :key :b :t-kw :b}
   {:kind :producer :key :c :t-kw :c}])

(def ^:private last-run
  {":a" {:fingerprint "fa" :run_id "r1"} ":b" {:fingerprint "fb" :run_id "r1"}
   ":c" {:fingerprint "fc" :run_id "r1"}})

(defn- standing
  [fps]
  (update-vals fps #(if (= ::volatile %) {:volatile? true} {:fingerprint %})))

(defn- plan [fps] (fresh/fresh-plan graph nodes (standing fps) last-run))

(deftest what-runs-is-decided-from-fingerprints
  (testing "nothing changed: nothing runs, every job is skipped as fresh since its run"
    (is (= {:runs #{} :deferred #{} :skipped {:a "r1" :b "r1" :c "r1"}}
           (plan {:a "fa" :b "fb" :c "fc"}))))
  (testing "a changed job runs, and so does what reads it"
    (is (= {:runs #{:a :b} :deferred #{} :skipped {:c "r1"}}
           (plan {:a "fa2" :b "fb" :c "fc"}))))
  (testing "a job whose fingerprint cannot be read runs"
    (is (= #{:a :b} (:runs (plan {:a nil :b "fb" :c "fc"}))))))

(deftest a-volatile-job-runs-but-does-not-stale-its-readers
  (testing "a volatile job always runs; a dataset producer it feeds waits for its level"
    (is (= {:runs #{:a} :deferred #{:b} :skipped {:c "r1"}}
           (plan {:a ::volatile :b "fb" :c "fc"}))))
  (testing "a reader stale on its own runs at once"
    (is (= {:runs #{:a :b} :deferred #{} :skipped {:c "r1"}}
           (plan {:a ::volatile :b "fb2" :c "fc"}))))
  (testing "a reader a type rule ties to other jobs is not deferred"
    (let [nodes (assoc nodes 1 {:kind :object :key :ot :t-kw :b})]
      (is (= #{:a :b}
             (:runs (fresh/fresh-plan graph nodes (standing {:a ::volatile :b "fb" :c "fc"})
                                      last-run)))))))

(deftest a-deferred-job-is-settled-by-its-inputs-once-its-feeder-ran
  (testing "the feeder committed nothing: the reader's fingerprint is the recorded one, it is skipped"
    (is (= {:runs #{} :skipped {:b "r1"}}
           (fresh/settle #{:b} (standing {:b "fb"}) last-run))))
  (testing "the feeder committed a new version: the reader's fingerprint moved, it runs"
    (is (= {:runs #{:b} :skipped {}}
           (fresh/settle #{:b} (standing {:b "fb2"}) last-run)))))

(deftest only-a-clean-completed-job-gets-a-fingerprint
  (let [schema {:transforms {:a {} :b {} :c {} :d {}}}
        job    (fn [t status] {:job_rid (str "j-" (name t)) :transform_rid (str t) :status status})
        done   "COMPLETED"]
    (is (= [["j-a" :a]]
           (fresh/stampable-jobs schema
                                 [(job :a done) (job :b done) (job :c done) (job :d "FAILED")]
                                 []
                                 {:incomplete [:b] :pending-observed {:c 2 :a 0}})))))

(deftest what-the-run-wrote-is-signed-at-its-end-the-rest-as-the-job-saw-it
  (let [schema {:datasets {:mine {} :other {}}}
        t      {:inputs {:m {:kind :dataset :dataset :mine}
                         :o {:kind :dataset :dataset :other}}}
        keyof  #(plan/input-key schema (get-in t [:inputs %]))]
    (is (= {:m "end-m" :o "start-o"}
           (fresh/signatures-as-read schema t #{(keyof :m)}
                                     {:m "start-m" :o "start-o"}
                                     {:m "end-m" :o "end-o"})))))
