(ns ^:unit grammar.ent-materialization-test
  (:require [sutra.catalog.shape :as shape]
            [clojure.test :refer [deftest testing is]]
            [sutra.parser.spec :as spec]
            [sutra.authoring.install :as install]))

(deftest the-plan-mints-the-materialize-node-and-the-lift-does-not
  (let [schema {:projects {:p {}}
                :objects  {:stundenbuchung {:attributes [[:buchung-id :string]]
                                            :plural "Stundenbuchungen"}}
                :datasets {:zeitbuchung_mat {:project :p :kind :materialization
                                             :object-type :stundenbuchung}}}
        plan   (install/plan-schema schema {} {})
        mats   (filter #(= :zeitbuchung_mat-materialize (:entity %)) plan)]
    (testing "exactly one producer, planned where a reader of the plan sees it"
      (is (= [[:dataset-source-transforms :create-transform]]
             (map (juxt :phase :action) mats))))
    (testing "the suffix says which way the node runs; ontology in, dataset out"
      (is (= {:kind :object-type :object-type :stundenbuchung} (:input (:args (first mats)))))
      (is (= {:kind :dataset :dataset :zeitbuchung_mat} (:output (:args (first mats))))))
    (testing "no :primary-key — the row id is the OBJECT's id"
      (is (nil? (:primary-key (:args (first mats))))))
    (testing "the staged-world lift no longer mints a second one"
      (let [lifted (shape/migrate-external-source-to-transforms
                    {:datasets {:zeitbuchung_mat {:dataset-name :zeitbuchung_mat
                                                  :kind :materialization
                                                  :object-type :stundenbuchung}}})]
        (is (empty? (:transforms lifted)))
        (is (= :stundenbuchung (get-in lifted [:datasets :zeitbuchung_mat :object-type])))))))

(deftest an-object-type-input-may-feed-a-dataset-and-nothing-else
  (testing "the materialization shape is valid"
    (is (spec/transform-valid?
         {:transform-name :zeitbuchung_mat-materialize
          :input  {:kind :object-type :object-type :stundenbuchung}
          :output {:kind :dataset :dataset :zeitbuchung_mat}})))
  (testing "a declared :primary-key is refused — the object's id already is one"
    (is (not (spec/transform-valid?
              {:transform-name :bad
               :input  {:kind :object-type :object-type :stundenbuchung}
               :output {:kind :dataset :dataset :zeitbuchung_mat}
               :primary-key :buchung-id}))))
  (testing "an OBJECT-TYPE output from an object-type input stays refused"
    (is (not (spec/transform-valid?
              {:transform-name :bad2
               :input  {:kind :object-type :object-type :stundenbuchung}
               :output {:kind :object-type :object-type :arbeitszeit}
               :primary-key :az-id})))))
