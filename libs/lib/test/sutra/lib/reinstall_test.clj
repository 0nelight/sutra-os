(ns sutra.lib.reinstall-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lib :as lib]
            [sutra.lib.mro-test :as mro]
            [sutra.lib.store :as store]
            [sutra.lib.test-store :as ts]))

(def without-report
  (-> mro/ent
      (update-in [:schema :actions] dissoc :report-defect)
      (assoc-in [:schema :objects :mechanic]
                {:name "Mechanic" :plural "Mechanics" :match-keys [:licence] :title :licence
                 :attributes [[:licence :string {:name "Licence" :required true}]]})))

(deftest reinstall-deletes-creates-and-keeps
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W1    (store/install! db (lib/lower mro/ent) {:clock clock})
        W2    (store/install! db (lib/lower without-report) {:clock clock})]
    (testing "a dropped action is gone, the other kept"
      (is (= #{:report-defect :defer-defect} (set (map :action-name (vals (:actions W1))))))
      (is (= #{:defer-defect} (set (map :action-name (vals (:actions W2))))))
      (is (:error (store/submit! db W2 :report-defect
                                 {:aircraft "D-AINA" :summary "x" :severity "minor"}
                                 {:clock clock}))))
    (testing "a new object type is created; the others keep their RIDs"
      (is (nil? (lib/type-rid W1 :mechanic)))
      (is (some? (lib/type-rid W2 :mechanic)))
      (is (= (lib/type-rid W1 :aircraft) (lib/type-rid W2 :aircraft)))
      (is (= (lib/type-rid W1 :defect) (lib/type-rid W2 :defect))))
    (testing "a dropped object type is deleted"
      (let [W3 (store/install! db
                               (lib/lower
                                (update-in without-report [:schema :objects] dissoc :mechanic))
                               {:clock clock})]
        (is (nil? (lib/type-rid W3 :mechanic)))
        (is (= (lib/type-rid W1 :aircraft) (lib/type-rid W3 :aircraft)))))))
