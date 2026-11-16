(ns ^:unit grammar.interface-link-target-test
  (:require [sutra.ref.install :as ri]
            [clojure.test :refer [deftest is testing]]))

(defn- schema
  ([to] (schema to {}))
  ([to {:keys [card link required] :or {card :many-to-one required true}}]
   {:interfaces   {:stelle    {:property-list []}
                   :nachweis {:property-list []
                               :link-constraints
                                 [(cond-> {:link-name :verantwortlich :to-interface :stelle
                                           :cardinality :one}
                                    required (assoc :required true))]}}
    :object-types {:camo    {:implements [:stelle] :plural "CAMOs"}
                   :hangar       {}
                   :arc {:implements [:nachweis] :plural "Arcs"}}
    :link-types   {:l1 (or link
                           {:link-type-name :arc-verantwortlich :from-object-type :arc
                            :to-object-type to :cardinality card
                            :satisfies {:interface :nachweis :constraint :verantwortlich}})}}))

(deftest ziel-interface-wird-durch-einen-implementierer-erfuellt
  (testing "die CAMO implementiert die verantwortliche Stelle"
    (is (empty? ((ri/stated-gates :link-constraints-met?) (schema :camo)))))
  (testing "ein Ziel, das das Interface NICHT implementiert, erfuellt nichts"
    (is (= [:unsatisfied-constraint]
           (map :reason ((ri/stated-gates :link-constraints-met?) (schema :hangar)))))))

(deftest einen-oder-mehrere-vom-implementierer-aus
  (testing "ONE erfuellt ONE"
    (is (empty? ((ri/stated-gates :link-constraints-met?) (schema :camo {:card :one-to-one})))))
  (testing "MANY erfuellt ONE nicht"
    (is (= [:unsatisfied-constraint]
           (map :reason
                ((ri/stated-gates :link-constraints-met?) (schema :camo {:card :one-to-many}))))))
  (testing "die Rueckseite eines Links zaehlt vom Implementierer aus"
    (is (empty? ((ri/stated-gates :link-constraints-met?)
                 (schema :camo
                         {:link {:link-type-name :verantwortete-arcs :from-object-type :camo
                                   :to-object-type :arc :cardinality :one-to-many
                                   :satisfies
                                     {:interface :nachweis :constraint :verantwortlich}}}))))))

(deftest nur-ein-pflicht-constraint-bindet
  (is (empty? ((ri/stated-gates :link-constraints-met?) (schema :hangar {:required false})))))
