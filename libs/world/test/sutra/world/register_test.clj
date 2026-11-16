(ns sutra.world.register-test
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [sutra.world.gate :as gate]
            [sutra.world.gate.near :as near]
            [sutra.world.index :as ix]
            [sutra.world.install :as k]
            [sutra.world.schema :as schema]
            [sutra.ref.install :as ri]))

(def ^:private all-rows
  (gate/rows-with (into {:media? any?} (map (fn [s] [(:check s) (constantly nil)]))
                        gate/near-specs)))

(deftest rows-conform
  (is (every? #(m/validate schema/IndexRow %) ix/rows))
  (is (every? #(m/validate schema/ViewRow %) k/views))
  (is (m/validate schema/ViewRow (near/near-view gate/near-specs near/fact-rows)))
  (is (every? #(m/validate schema/GateRow %) all-rows)))

(deftest the-gates-are-the-references
  (let [rule-of (into {} (map (juxt :id :rule)) ri/gates)
        ours    (into {} (map (juxt #(:gate % (:id %)) :rule))
                      all-rows)]
    (is (every? (set (keys ours)) (keys ri/stated-gates)) "every gate §8.5 states")
    (is (every? (fn [[id rule]] (= rule (rule-of id))) ours)
        "each one a gate of libs/kernel/data/gates.edn, under its rule")))

(deftest a-global-gate-says-why
  (is (every? :why (filter #(= :global (:scope %)) all-rows))))
