(ns ^:api ontology.staged-row-kinds-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :each pod/with-mem-pod)

(def ^:private value-type
  {:schema {:value-types {:iban-nr {:type :string :description "IBAN"}}}})

(defn- staged [] (get-in (api/get! :staging) [:body :staged]))

(deftest a-staged-value-type-is-a-value-type
  (api/delete! :staging)
  (is (ok? (api/post! :install value-type)))
  (testing "the staging list names the row's kind"
    (let [row (first (staged))]
      (is (= :value-type (keyword (:kind row))) (pr-str row))
      (is (= :create (keyword (:verb row))))))
  (testing "a second create of the same name is refused at the door"
    (let [r (api/post! :install value-type)]
      (is (= 400 (:status r)) (pr-str (:body r)))
      (is (some #(= :already-staged (keyword (:rule %))) (get-in r [:body :data :problems])))))
  (api/delete! :staging))
