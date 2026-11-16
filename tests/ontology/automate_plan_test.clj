(ns ^:unit ontology.automate-plan-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.automation.rules]))

(def ^:private plan @#'sutra.automation.rules/effect-plan)
(def ^:private when->pred @#'sutra.automation.rules/when->pred)

(def ^:private firma "ri.sutra.main.object-type.0000-firma")

(def ^:private schema
  {:object-types {firma {:id firma :object-type-name :firma
                         :attribute-list [{:attribute-name :domains
                                           :attribute-type [:array :string]}]
                         :plural "Firmen"}}
   :actions {:mail-zuordnen {:rules [{:op :update :target [:get :mail]
                                      :values {:firma [:get :firma]}}]}}})

(def ^:private effect
  {:action :mail-zuordnen
   :args   {:mail  {:self :id}
            :firma {:resolve {:type :firma
                              :match {:against {:self :von} :array-attr :domains}
                              :create-if-missing {:action :firma-anlegen
                                                  :id {:template "f-{{id}}"}
                                                  :values
                                                    {:domains [{:reg-domain {:self :von}}]}}}}}})

(def ^:private env
  {:schema schema
   :rows   {firma [{:id "f-alt" :values {:domains ["luftteile.example"]}}]}})

(deftest an-action-effect-is-decided-before-anything-runs
  (let [[a b c] (plan env "auto" effect [{:id "m1" :values {:von "alma@luftteile.example"}}
                                          {:id "m2" :values {:von "x@neu.example"}}
                                          {:id "m3" :values {:von "y@mail.neu.example"}}])]
    (testing "a known domain resolves and creates nothing"
      (is (= {:mail "m1" :firma "f-alt"} (:args a)))
      (is (empty? (:creates a))))
    (testing "an unknown one plans the create and takes the id it will have"
      (is (= "f-m2" (get-in b [:args :firma])))
      (is (= [{:type firma :action :firma-anlegen
               :args {:id "f-m2" :values {:domains ["neu.example"]}}}]
             (:creates b))))
    (testing "a later object sees what an earlier one plans to create"
      (is (= "f-m2" (get-in c [:args :firma])))
      (is (empty? (:creates c))))))

(deftest the-when-sugar-is-E
  (is (= [:= [:get :status] "offen"] (when->pred {:eq {:attr :status :value :offen}}))
      "an enum value is a literal, the attribute is read with :get")
  (is (= [:includes-any [:get :labels] "versand"]
         (when->pred {:contains-any {:attr :labels :values ["versand"]}}))
      "contains-any is membership in the attribute's list"))
