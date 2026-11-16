(ns ^:unit apps.module-inputs-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.capps.variables :as vars]))

(deftest an-input-is-part-of-the-interface-and-typed
  (testing "one object, one set"
    (is (nil? (vars/interface-problem :auftrag {:type :string :external-id :auftrag
                                                :object-type :auftrag :default ""})))
    (is (nil? (vars/interface-problem :auftraege {:type :array :external-id :auftraege
                                                  :object-type :auftrag :default []}))))
  (testing "no external id — nothing outside could hand the object in"
    (is (re-find #":external-id"
                 (vars/interface-problem :a {:type :string :object-type :auftrag}))))
  (testing "a number cannot carry an object"
    (is (re-find #":type :string"
                 (vars/interface-problem :a {:type :number :external-id :a
                                             :object-type :auftrag})))))

(deftest object-inputs-name-what-the-module-takes
  (is (= [{:external-id :auftrag :object-type :auftrag :set? false}
          {:external-id :posten :object-type :posten :set? true}]
         (vars/object-inputs {:auftrag {:type :string :external-id :auftrag
                                        :object-type :auftrag}
                              :filter  {:type :string :external-id :filter}
                              :posten  {:type :array :external-id :posten
                                        :object-type :posten}}))))
