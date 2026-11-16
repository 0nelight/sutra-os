(ns ^:api ontology.shared-property-value-type-test
  (:require [sutra.catalog.grants :as grants]
            [support.system :as system]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.seat]
            [sutra.access.principals :as pr]
            [support.pod :as pod]
            [support.wire :refer [commit! ok? stage!]]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]))

(use-fixtures :each pod/with-mem-pod-auth)

(def ^:private gut     "AT61 1904 3002 3457 3201")
(def ^:private schlecht "AT00 1904 3002 3457 3201")

(def ^:private ontology
  {:schema
   {:projects          {:konten {}}
    :value-types       {:iban-nr {:type :string :description "IBAN, Pruefziffer validiert"
                                  :constraints {:format :iban}}}
    :shared-properties {:iban {:project :konten :value-type :iban-nr :name "IBAN"}}
    :objects           {:konto {:project :konten :title :konto-id
                                :primary-key :konto-id :attributes [[:konto-id :string] :iban]
                                :plural "Konten"}}}})

(defn- anlegen [konto]
  {:name :konto-anlegen
   :params {:nr {:name "Nr" :type :string :required true} :iban {:name "Iban" :type :string}}
   :rules [{:op :create :object-type konto :id [:get :arg0 :nr]
            :values {:konto-id [:get :arg0 :nr] :iban [:get :arg0 :iban]}}]
   :edits [{:object-type konto}]})

(def ^:private aendern
  {:name :iban-aendern
   :params {:konto {:name "Konto" :type :konto :required true} :iban {:name "Iban" :type :string}}
   :rules [{:op :update :target [:get :konto] :values {:iban [:get :arg0 :iban]}}]})

(defn- sp-named [schema nm]
  (some (fn [[_ v]] (when (= nm (:shared-property-name v)) v)) (:shared-properties schema)))

(deftest a-shared-propertys-value-type-holds-on-its-object-types
  (tx/with-token (-> (api/post! :login
                                {:username grants/admin-username
                                 :password pr/default-admin-password})
                     :body :token)
    (let [r (api/post! :install ontology)]
      (is (ok? r) (pr-str (:body r))))
    (is (ok? (commit!)) "value type, shared property, object type")

    (testing "the canonical schema carries the value type and the reference"
      (let [schema (:body (api/get! :schema))]
        (is (some #(= :iban-nr (:value-type-name %)) (vals (:value-types schema))))
        (is (= {:type :string :value-type :iban-nr}
               (select-keys (sp-named schema :iban) [:type :value-type])))))

    (let [konto (lookup/rid-of (system/seated-schema) :object-types :konto)]
      (is (ok? (stage! :create-action (anlegen konto))))
      (is (ok? (stage! :create-action aendern)))
      (is (ok? (commit!)) "actions"))

    (testing "create: an invalid IBAN is refused, a valid one accepted"
      (let [{:keys [status body]} (api/mutate :konto-anlegen {:nr "k1" :iban schlecht})]
        (is (not= 200 status))
        (is (re-find #"iban" (pr-str body)) (pr-str body)))
      (let [{:keys [status body]} (api/mutate :konto-anlegen {:nr "k1" :iban gut})]
        (is (= 200 status) (pr-str body))))

    (testing "edit: the same rule holds on modify"
      (let [{:keys [status body]} (api/mutate :iban-aendern {:konto "k1" :iban schlecht})]
        (is (not= 200 status))
        (is (re-find #"not a valid iban" (pr-str body)) (pr-str body)))
      (is (= 200 (:status (api/mutate :iban-aendern {:konto "k1" :iban gut})))))

    (testing "the staging mutation takes {:name :type :value-type} and judges it"
      (is (ok? (stage! :create-shared-property
                       {:name :gegen-iban :project :konten :type :string :value-type :iban-nr})))
      (is (ok? (commit!)))
      (is (= :iban-nr (:value-type (sp-named (:body (api/get! :schema)) :gegen-iban))))
      (let [r (stage! :create-shared-property
                      {:name :falsch :project :konten :type :integer :value-type :iban-nr})]
        (is (not (ok? r)) "a base mismatch is refused at the door")
        (is (re-find #"is not the base :string" (pr-str (:body r))) (pr-str (:body r))))
      (let [r (stage! :create-shared-property
                      {:name :falsch2 :project :konten :type :string :value-type :gibts-nicht})]
        (is (not (ok? r)) "an unknown value type is refused at the door")
        (is (re-find #"names no declared value type" (pr-str (:body r))) (pr-str (:body r)))))))
