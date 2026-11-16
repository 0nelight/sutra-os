(ns ^:unit ontology.outputs-of-precedence-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.catalog.outputs :as outputs]))

(deftest output-entry-wins-over-transform-level
  (testing "the output's own :primary-key beats a stale top-level one"
    (let [t {:transform-name :shape-wartungs-satz :language :pg-sql
             :primary-key :name
             :output {:kind :object-type :object-type :wartungs-satz
                      :primary-key :satz-id}}
          [out] (outputs/outputs-of t)]
      (is (= :satz-id (:primary-key out)))
      (is (= :shape-wartungs-satz (:name out)) "still named by its transform")))

  (testing "…and the top-level value still FILLS IN when the output is silent"
    (let [t {:transform-name :thing-rows
             :primary-key :pk :column-map {:a :b}
             :output {:kind :object-type :object-type :thing}}
          [out] (outputs/outputs-of t)]
      (is (= :pk (:primary-key out)))
      (is (= {:a :b} (:column-map out)))))

  (testing "every backing field follows the same rule, not just the key"
    (let [t {:transform-name :link-mro
             :from-key-column :old-from :to-key-column :old-to
             :fk-attribute :old-fk :column-map {:stale :map}
             :output {:kind :link-type :link-type :l
                      :from-key-column :new-from :fk-attribute :new-fk}}
          [out] (outputs/outputs-of t)]
      (is (= :new-from (:from-key-column out)) "declared on the output ⇒ wins")
      (is (= :new-fk   (:fk-attribute out))    "declared on the output ⇒ wins")
      (is (= :old-to   (:to-key-column out))   "not declared ⇒ filled in")
      (is (= {:stale :map} (:column-map out))  "not declared ⇒ filled in")))

  (testing "an explicit :outputs list is carried verbatim — no lifting at all"
    (let [t {:transform-name :multi :primary-key :top
             :outputs [{:kind :object-type :object-type :a :primary-key :a-id}
                       {:kind :object-type :object-type :b}]}
          outs (outputs/outputs-of t)]
      (is (= 2 (count outs)))
      (is (= :a-id (:primary-key (first outs))))
      (is (nil? (:primary-key (second outs)))
          "the top-level key does NOT leak into a multi-output entry"))))
