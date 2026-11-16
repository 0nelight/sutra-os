(ns ^:unit ontology.transform-shape-superset-test
  (:require [clojure.test :refer [deftest testing is]]
            [sutra.parser.spec :as spec]))

(deftest output-shapes-judge-the-knobs-a-side-takes
  (let [t (fn [input output] {:transform-name :t :input input :output output})
        obj {:kind :object-type :object-type :o}
        ds  {:kind :dataset :dataset :d}]
    (testing "a knob outside the shape is refused — an fk-attribute on an object-type output"
      (is (not (spec/transform-valid?
                (t ds {:kind :object-type :object-type :o :primary-key :id :fk-attribute :x})))))
    (testing "a source link takes EITHER a to-key-column OR the shared-property match, never both"
      (is (not (spec/transform-valid?
                (t ds {:kind :link-type :link-type :l :from-key-column :a :to-key-column :b
                       :match-column :doc})))))
    (testing "the match knobs are left alone elsewhere —
    sutra.catalog.problems/link-type-problem names
              the mistake on the endpoint-fk link, and an object-type output never read them"
      (is (spec/transform-valid?
           (t obj {:kind :link-type :link-type :l :fk-attribute :b-id :match-column :doc})))
      (is (spec/transform-valid?
           (t ds {:kind :object-type :object-type :o :primary-key :id :match-column :doc}))))))
