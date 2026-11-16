(ns ^:api ontology.materialization-install-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(defn- ok? [r] (< (:status r) 300))

(defn- dataset-columns [ds]
  (->> (vals (:datasets (:body (api/get! :schema))))
       (some #(when (= ds (keyword (:dataset-name %))) %))
       :columns
       (mapv (juxt (comp keyword :attribute-name) (comp keyword :attribute-type)))))

(defn- transform [nm]
  (some #(when (= nm (keyword (:transform-name %))) %)
        (vals (:transforms (:body (api/get! :schema))))))

(def ^:private schema
  {:schema {:projects          {:matp {}}
            :value-types       {:iban-nr {:type :string :constraints {:format :iban}}}
            :shared-properties {:notiz {:project :matp :type :string :name "Notiz"}
                                :iban  {:project :matp :value-type :iban-nr :name "IBAN"}}
            :objects           {:konto {:project :matp
                                        :primary-key :konto-id
                                        :attributes [[:konto-id :string] :notiz :iban]
                                        :plural "Konten"}}
            :datasets          {:konto_mat {:project :matp :kind :materialization
                                            :object-type :konto}}}})

(deftest a-materialization-over-shared-properties-installs
  (testing "one install, one producer — no second create of <ds>-materialize"
    (is (ok? (api/post! :install schema)))
    (is (ok? (api/post! :staging))))
  (testing "a bare shared-property reference resolves to its column type"
    (is (= [[:konto-id :string] [:notiz :string] [:iban :string]]
           (dataset-columns :konto_mat))))
  (testing "the lifted producer carries its dataset's project"
    (let [t (transform :konto_mat-materialize)]
      (is (some? t))
      (is (some? (:project t)))))
  (testing "re-installing the same file changes nothing"
    (let [r (api/post! :install schema)]
      (is (ok? r))
      (is (zero? (get-in r [:body :created] 0))))))
