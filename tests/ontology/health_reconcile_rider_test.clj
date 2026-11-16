(ns ^:unit ontology.health-reconcile-rider-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.health.checks :as health]
            [sutra.types.health-metric :as hm]))

(def ^:private rider
  {:name "zeitbuchung-stunden-gegen-quelle" :target :zeitbuchung :kind :reconcile
   :measure :sum :column :stunden
   :against {:on :dataset :target :zv_zeiten_raw :column :h}
   :tolerance 0.01 :severity :critical})

(deftest the-rider-carries-both-sides-and-the-tolerance
  (let [{:keys [kind node-key expects]} (health/rider->check rider)]
    (is (= "reconcile" kind))
    (is (= "zeitbuchung" node-key))
    (is (= {:severity :critical :op :<= :value 0.01 :column :stunden :measure :sum
            :against {:on :dataset :target :zv_zeiten_raw :column :h}}
           expects)))
  (testing "no tolerance means exact agreement"
    (is (= 0 (:value (:expects (health/rider->check (dissoc rider :tolerance))))))))

(deftest a-sum-folds-numbers-and-numeric-text
  (is (= 4.5 (hm/of-rows :sum [{} {} {}] [1 "3.5" nil] "stunden" nil))))
