(ns ^:api platform.flaw-trash-restore-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [support.pod :as pod]
            [support.wire :refer [ok?]]
            [sutra.wire.api :as api]))

(use-fixtures :once pod/with-mem-pod-auth pod/as-admin)

(defn- install! [schema]
  (let [r (api/post! :install {:schema schema})
        c (when (ok? r) (api/post! :staging))]
    (when-not (and (ok? r) (ok? c)) (api/delete! :staging))
    (and (ok? r) (ok? c))))

(def ^:private welt
  {:projects {:trash {}}
   :objects {:wiederkehr {:project :trash :primary-key :nr :attributes [[:nr :string]]
                          :match-keys [:nr]
                          :plural "Wiederkehren"}}
   :actions {:wk-anlegen {:params {:nr {:name "Nr" :type :string :required true}}
                          :rules [{:op :create :object-type :wiederkehr :values {:nr [:get :nr]}}]
                          :edits [{:object-type :wiederkehr}]}}})

(defn- nummern []
  (set (map #(get-in % [:values :nr]) (:rows (:body (api/list-objects :wiederkehr))))))

(defn- typ []
  (some #(when (= "wiederkehr" (name (:object-type-name %))) %)
        (vals (:object-types (:body (api/get! :schema))))))

(deftest trash-restore-object-type
  (is (install! welt))
  (is (ok? (api/mutate :wk-anlegen {:nr "1"} {:publish? true})))
  (is (= #{"1"} (nummern)))
  (is (ok? (api/delete-action-type :wk-anlegen)))
  (is (ok? (api/delete-object-type :wiederkehr)))
  (is (nil? (typ)))
  (let [r (api/post! :trash-restore {:kind "object-type" :name "wiederkehr"})]
    (is (ok? r) (pr-str r))
    (testing "the type is back unprotected, with its rows"
      (is (= "experimental" (some-> (typ) :status name)))
      (is (= #{"1"} (nummern))))))
