(ns ^:unit ontology.reprocessing-warnings-test
  (:require [sutra.builds.ledger.report :as report]
            [sutra.builds.import.nodes :as import-nodes]
            [clojure.test :refer [deftest is testing]]))

(def ^:private dropped-output-cols #'import-nodes/dropped-output-cols)
(def ^:private reprocessing-warnings #'report/reprocessing-warnings)

(def ^:private schema
  {:object-types
   {:rechnungsposition
    {:attribute-list [{:attribute-name :position-id}
                      {:attribute-name :rechnungs-id}
                      {:attribute-name :beschreibung}]
     :plural "Rechnungspositionen"}
    :rechnung {:attribute-list [{:attribute-name :rechnungs-id}] :plural "Rechnungen"}}
   :link-types
   {:position-rechnung {:from-object-type :rechnungsposition :to-object-type :rechnung}
    :position-ueber    {:from-object-type :rechnungsposition :to-object-type :rechnungsposition}
    :rechnung-bezahlt  {:from-object-type :zahlung :to-object-type :rechnungsposition}}
   :transforms
   {:link-position-rechnung
    {:transform-name :link-position-rechnung
     :input  {:kind :object-type :object-type :rechnungsposition}
     :output {:kind :link-type :link-type :position-rechnung :fk-attribute :rechnungs-id}}
    :link-position-ueber
    {:transform-name :link-position-ueber
     :input  {:kind :object-type :object-type :rechnungsposition}
     :output {:kind :link-type :link-type :position-ueber :fk-attribute :parent-position-id}}
    :link-rechnung-bezahlt
    {:transform-name :link-rechnung-bezahlt
     :input  {:kind :object-type :object-type :zahlung}
     :output {:kind :link-type :link-type :rechnung-bezahlt :fk-attribute :zahlungs-ref}}}})

(deftest dropped-output-cols-flags-only-truly-unconsumed
  (testing "a scalar attr, a link FK, and the is_deleted control column are NOT dropped;
            only the emitted-but-undeclared cursor is"
    (is (= #{:quell_pdf}
           (dropped-output-cols
            schema :rechnungsposition nil
            {:position_id "x∥1" :rechnungs_id "x" :beschreibung "y"
             :parent_position_id "x" :quell_pdf "sha" :is_deleted false}))))
  (testing "a well-formed row (every emitted key maps to an attr / FK / control) → nothing dropped"
    (is (empty?
         (dropped-output-cols
          schema :rechnungsposition nil
          {:position_id "x∥1" :rechnungs_id "x" :beschreibung "y"
           :parent_position_id "x" :is_deleted false}))))
  (testing "an FK that is NOT a declared attribute is still consumed — by its link.
            The check read `(:from lt)`, which a stored link type does not carry, so
            this case was never reached and :parent_position_id was flagged"
    (is (empty?
         (dropped-output-cols
          schema :rechnungsposition nil
          {:position_id "x" :beschreibung "y" :parent_position_id "p"}))))
  (testing "an FK held by ANOTHER type's rows is not this type's column"
    (is (= #{:zahlungs_ref}
           (dropped-output-cols
            schema :rechnungsposition nil
            {:position_id "x" :zahlungs_ref "z"}))))
  (testing "kebab-spelled emitted keys fold to the same canonical col as the attr"
    (is (empty?
         (dropped-output-cols
          schema :rechnungsposition nil
          {:position-id "x" :rechnungs-id "x" :beschreibung "y"})))))

(deftest reprocessing-warnings-surfaces-dropped-columns
  (testing "a dropped output column becomes a :dropped-output-column warning naming the transform + column"
    (let [ws (reprocessing-warnings
              (assoc schema :transforms {})
              {:dropped-cols {:parse-positions #{:quell_pdf}}
               :bootstrapped #{} :incomplete #{}})
          w  (first ws)]
      (is (= 1 (count ws)))
      (is (= :dropped-output-column (:kind w)))
      (is (= "parse-positions" (:transform w)))
      (is (= ["quell_pdf"] (:columns w)))))
  (testing "a clean build yields no warnings"
    (is (empty? (reprocessing-warnings
                 (assoc schema :transforms {})
                 {:dropped-cols {} :bootstrapped #{} :incomplete #{}})))))
