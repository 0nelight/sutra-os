(ns ^:api ontology.flaw-torn-reset-test
  (:require [support.wire :refer [ok?]]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.ledger :as ledger]
            [sutra.store.control :as ctl]
            [sutra.store.access.scope :as ctx]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- col [n t] {:attribute-name n :attribute-type t})

(defn- dataset-rid [nm]
  (->> (:datasets (:schema (:body (api/get! :snapshot))))
       (some (fn [[rid d]] (when (= nm (:dataset-name d)) rid)))))

(defn- schema [sv]
  {:schema
   {:transforms
    {:tr-out (cond-> {:project :torn :language :sqlite-sql :query "SELECT k FROM tr_seed"
                      :inputs {:tr_seed {:kind :dataset :dataset :tr_seed}}
                      :incremental true
                      :output {:kind :dataset :name :tr_out :dataset :tr_out :primary-key :k}}
               sv (assoc :semantic-version sv))}}})

(deftest torn-reset
  (is (ok? (api/mutate :create-project {:name :torn} {:stage? true})))
  (is (ok? (api/mutate :create-dataset {:name :tr_seed :project :torn :columns [(col :k :string)]}
                       {:stage? true})))
  (is (ok? (api/mutate :create-dataset {:name :tr_out :project :torn :columns [(col :k :string)]}
                       {:stage? true})))
  (is (ok? (api/post! :staging)))
  (is (ok? (api/post! :install (schema nil))))
  (is (ok? (api/post! :staging)))
  (let [out    (str (dataset-rid :tr_out))
        seen   (atom nil)
        orig   ledger/reset-consumer!]
    (with-redefs [ledger/reset-consumer! (fn [store & args]
                                           (reset! seen
                                                   (ctl/list (ctx/store) :build-claims
                                                             {:status "running"}))
                                           (apply orig store args))]
      (is (ok? (api/post! :install (schema 2))))
      (is (ok? (api/post! :staging)) "the bump commits"))
    (testing "the reset ran while a build claim on the output was held"
      (is (some #(contains? (set (map str (:writes %))) out) @seen) (pr-str @seen)))))
