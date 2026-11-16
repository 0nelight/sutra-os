(ns ^:unit ontology.automate-manual-execution-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.automation.rules :as rules]))

(def ^:private objs
  [{:id "a" :values {:gelesen-art "mahnung" :lesungen 2}}
   {:id "b" :values {:gelesen-art "rechnung" :lesungen 1}}
   {:id "c" :values {:gelesen-art "mahnung" :lesungen 0}}])

(defn- ids [spec] (mapv :id (rules/object-set objs spec)))

(deftest a-manual-execution-names-its-objects
  (testing "by id, whatever the rule's :when says about them"
    (is (= ["a" "b"] (ids {:ids ["a" "b"]}))))
  (testing "by a predicate in the :when grammar, vector or map sugar"
    (is (= ["a" "c"] (ids {:where [:= [:get :gelesen-art] "mahnung"]})))
    (is (= ["b"] (ids {:where {:eq {:attr :gelesen-art :value "rechnung"}}}))))
  (testing "both: the ids the predicate holds for"
    (is (= ["a"] (ids {:ids ["a" "b"] :where [:= [:get :gelesen-art] "mahnung"]}))))
  (testing "an id that is no object of the type is no object of the set"
    (is (= [] (ids {:ids ["zz"]})))))
