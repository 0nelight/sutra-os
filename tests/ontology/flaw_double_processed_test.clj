(ns ^:api ontology.flaw-double-processed-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.ledger.jobs :as jl]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- out-ks []
  (sort (map (comp :k :values)
             (:rows (:body (api/get! :dataset-preview {:id (dataset-rid :dp_out)}))))))

(def ^:private schema
  {:schema {:transforms {:dp-out {:project :dblproc :language :clojure
                                  :code
                                    "(fn [{:keys [dp_seed]}] (map #(select-keys % [:k]) dp_seed))"
                                  :inputs {:dp_seed {:kind :dataset :dataset :dp_seed}}
                                  :incremental true
                                  :output {:kind :dataset :name :dp_out :dataset :dp_out}}}}})

(deftest double-processed
  (is (ok? (api/mutate :create-project {:name :dblproc} {:stage? true})))
  (is (ok? (api/mutate :create-dataset
                       {:name :dp_seed :project :dblproc :columns [(col :k :string)]}
                       {:stage? true})))
  (is (ok? (api/mutate :create-dataset {:name :dp_out :project :dblproc :columns [(col :k :string)]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :install schema)))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :dataset-write {:name "dp_seed"}
                      {:mode :append :rows [{:values {:k "a"}}] :message "a"})))
  (is (ok? (api/post! :import-run)))
  (is (= ["a"] (out-ks)))
  (is (ok? (api/post! :dataset-write {:name "dp_seed"}
                      {:mode :append :rows [{:values {:k "b"}}] :message "b"})))
  (testing "the build writes b, then crashes before its job completes"
    (let [orig jl/ledger!]
      (with-redefs [jl/ledger! (fn [store effects]
                                 (when (some #(and (= :job/transition (:op %))
                                                   (= :COMPLETED (:state %))) effects)
                                   (throw (ex-info "eingeschleust: Absturz vor dem Abschluss" {})))
                                 (orig store effects))]
        (api/post! :import-run))))
  (testing "the next build processes b once"
    (is (ok? (api/post! :import-run)))
    (is (= ["a" "b"] (out-ks)))))
