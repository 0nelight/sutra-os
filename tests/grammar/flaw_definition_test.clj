(ns ^:unit grammar.flaw-definition-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing]]
            [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [sutra.catalog.kernel :as kernel]
            [support.in-project :refer [in-project]]))

(defn- world [status]
  (ri/derived {:sigma clock/system}
              (in-project :t
                          {:object-types {:werft (cond-> {:object-type-name :werft
                                                          :attribute-list
                                                            [{:attribute-name :nr
                                                              :attribute-type :string}]}
                                                   status (assoc :status status))}})))

(defn- refused [steps W]
  (try (kernel/eval-steps steps W {} {} {:installer {}}) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest a-platform-action-deletes-no-protected-definition
  (doseq [status [:active :endorsed]]
    (testing (str "a " status " type leaves only through :deprecated")
      (let [e (refused [{:op :delete-object-type :name :werft}] (world status))]
        (is (= 409 (anomaly/status e)) (pr-str e)))))
  (testing "a deprecated or experimental one goes"
    (is (nil? (refused [{:op :delete-object-type :name :werft}] (world :deprecated))))
    (is (nil? (refused [{:op :delete-object-type :name :werft}] (world nil))))))

(deftest create-on-a-living-name-is-refused
  (let [e (refused [{:op :create-object-type :name :werft
                     :attributes [[:nr :string] [:ort :string]]
                     :plural "Werften"}]
                   (world nil))]
    (is (= 409 (anomaly/status e)) (pr-str e)))
  (testing "update is the way to change it"
    (is (nil? (refused [{:op :update-object-type :name :werft
                         :attributes [[:nr :string] [:ort :string]]}]
                       (world nil))))))
