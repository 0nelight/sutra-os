(ns ^:unit ontology.sql-read-window-test
  (:require [sutra.builds.plan.inputs :as inputs]
            [clojure.test :refer [deftest is]]
            [sutra.catalog.outputs :as outputs]
            [sutra.catalog.transforms :as transforms]
            [sutra.catalog.lookup :as lookup]
            [sutra.builds.ledger :as ledger]
            [sutra.store.access.datasets :as dsets]))

(def ^:private compute
  {:transform-name :t :incremental true :inputs {:in {:kind :dataset :dataset :d}}})

(defn- sql-window [low high & [only-added?]]
  (let [notes (atom [])]
    (with-redefs [transforms/added-read?             (constantly true)
                  lookup/rid-of                  (constantly "ri.t")
                  outputs/input-dataset-rid       (constantly "ri.d")
                  ledger/read-boundary        (constantly {:low low :high high})
                  dsets/below-horizon?        (constantly false)
                  dsets/empty-at?             (constantly false)
                  dsets/only-added?           (constantly (not= false only-added?))]
      {:added (#'inputs/sql-added-map {:store {:branch "main"} :schema {} :notes notes} compute)
       :booted (some #(= :bootstrapped (first %)) @notes)})))

(deftest a-versioned-mark-reads-the-delta
  (is (= {:added {:in {:low "3" :high 7}} :booted nil} (sql-window "3" 7))))

(deftest a-missing-mark-reads-the-whole-input
  (is (= {:added {} :booted true} (sql-window nil 7))))

(deftest a-mark-that-is-no-version-reads-the-whole-input
  (is (= {:added {} :booted true} (sql-window "ri.txn.legacy" 7))))

(deftest an-input-that-overwrote-or-removed-rows-reads-whole
  (is (= {:added {} :booted true} (sql-window "3" 7 false))))

(deftest retention-past-the-cursor-ends-the-increment
  (with-redefs [dsets/below-horizon? (constantly true)]
    (let [notes (atom [])]
      (with-redefs [transforms/added-read?       (constantly true)
                    lookup/rid-of                (constantly "ri.t")
                    outputs/input-dataset-rid    (constantly "ri.d")
                    ledger/read-boundary         (constantly {:low "3" :high 7})]
        (is (= {} (#'inputs/sql-added-map {:store {:branch "main"} :schema {} :notes notes}
                                          compute)))
        (is (some #(= :snapshot-run (first %)) @notes))))))

(deftest allow-retention-reads-on-from-what-retention-left
  (let [notes (atom [])]
    (with-redefs [transforms/added-read?       (constantly true)
                  lookup/rid-of                (constantly "ri.t")
                  outputs/input-dataset-rid    (constantly "ri.d")
                  ledger/read-boundary         (constantly {:low "3" :high 7})
                  dsets/below-horizon?         (constantly true)
                  dsets/retained-floor         (constantly 5)
                  dsets/empty-at?              (constantly false)
                  dsets/only-added?            (constantly true)]
      (is (= {:in {:low 5 :high 7}}
             (#'inputs/sql-added-map {:store {:branch "main"} :schema {} :notes notes}
                                     (assoc compute :incremental {:allow-retention true}))))
      (is (not-any? #(#{:snapshot-run :bootstrapped} (first %)) @notes)))))
