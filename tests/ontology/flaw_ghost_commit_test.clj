(ns ^:api ontology.flaw-ghost-commit-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.ledger.jobs :as jl]
            [sutra.builds.ledger :as ledger]
            [sutra.store.control :as ctl]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- out-keys []
  (set (map (comp :k :values)
            (:rows (:body (api/get! :dataset-preview {:id (dataset-rid :gc_out)}))))))

(def ^:private schema
  {:schema {:transforms {:gc-out {:project :ghost :language :clojure
                                  :code
                                    "(fn [{:keys [gc_seed]}] (map #(select-keys % [:k]) gc_seed))"
                                  :inputs {:gc_seed {:kind :dataset :dataset :gc_seed}}
                                  :output {:kind :dataset :name :gc_out :dataset :gc_out
                                           :primary-key :k}}}}})

(deftest ghost-commit
  (is (ok? (api/mutate :create-project {:name :ghost} {:stage? true})))
  (is (ok? (api/mutate :create-dataset {:name :gc_seed :project :ghost :columns [(col :k :string)]}
                       {:stage? true})))
  (is (ok? (api/mutate :create-dataset {:name :gc_out :project :ghost :columns [(col :k :string)]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :dataset-write {:name "gc_seed"}
                      {:mode :snapshot :rows [{:values {:k "a"}}] :message "seed"})))
  (is (ok? (api/post! :import-run)))
  (is (= #{"a"} (out-keys)))
  (is (ok? (api/post! :dataset-write {:name "gc_seed"}
                      {:mode :snapshot :rows [{:values {:k "a"}} {:values {:k "b"}}]
                       :message "seed 2"})))
  (testing "the job is aborted after it wrote its output, before it completes"
    (let [orig   jl/ledger!
          run-of (atom nil)]
      (with-redefs [jl/ledger! (fn [store effects]
                                 (doseq [{:keys [op state facts]} effects
                                         :when (and (= :job/transition op) (= :RUNNING state))]
                                   (reset! run-of (:run-id facts)))
                                 (doseq [{:keys [op state facts]} effects
                                         :when
                                           (and (= :job/transition op) (= :COMPLETED state)
                                                @run-of)]
                                   (orig store
                                         [(ledger/abort-jobs
                                           @run-of [(:job-rid facts)] (ctl/stamp store)
                                           "eingeschleust: Abbruch")]))
                                 (orig store effects))]
        (api/post! :import-run))))
  (is (= #{"a"} (out-keys)) "the aborted attempt's output is retracted"))
