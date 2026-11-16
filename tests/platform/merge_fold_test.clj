(ns ^:unit platform.merge-fold-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.types.shape :as tshape]
            [sutra.types.tree :as tree]))

(def ^:private base {:object-types {:a {:title "A" :checks nil :resolve-entity false}}})

(def ^:private mine {:object-types {:a {:title "A" :checks [] :resolve-entity nil}}})

(def ^:private theirs {:object-types {:a {:title "A2"}}})

(deftest an-empty-folded-difference-is-neither-a-conflict-nor-a-change
  (testing "without the entity rules nil, [] and an absent field are three values"
    (is (seq (:conflicts (tree/merge3 base mine theirs {:token pr-str})))))
  (testing "with them they are one: theirs' title lands, nothing conflicts"
    (let [{:keys [body conflicts]}
          (tree/merge3 base mine theirs {:token pr-str :fold tshape/fold})]
      (is (empty? conflicts))
      (is (= "A2" (get-in body [:object-types :a :title]))))))

(deftest a-fold-follows-the-compare-rule-of-the-field
  (is (nil? (tshape/fold :object-types :checks [])) ":empty-folded")
  (is (nil? (tshape/fold :object-types :resolve-entity false)) ":bool")
  (is (true? (tshape/fold :object-types :resolve-entity "yes")) ":bool")
  (is (= [] (tshape/fold :actions :nothing-declared []))
      "a field without a rule keeps its value"))
