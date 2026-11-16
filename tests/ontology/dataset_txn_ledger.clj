(ns ^:integration ontology.dataset-txn-ledger
  (:require [clojure.test :refer [deftest use-fixtures]]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.types.naming :as naming]
            [sutra.builds.ledger :as ledger]
            [support.helpers :refer [check section]]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- uuid [] (str (java.util.UUID/randomUUID)))

(deftest classify-types-a-bootstrap-run-as-snapshot
  (section "classify: :type reflects the run mode, not just the static flag")
  (check "non-incremental full-refresh ⇒ SNAPSHOT"
         (= :SNAPSHOT (:type (ledger/classify {:incremental? false :written 3}))))
  (check "incremental steady-state append ⇒ APPEND"
         (= :APPEND (:type (ledger/classify {:incremental? true :written 1}))))
  (check "incremental WITH tombstones ⇒ DELETE"
         (= :DELETE (:type (ledger/classify {:incremental? true :written 1 :deleted 2}))))
  (check "incremental but BOOTSTRAPPED this build ⇒ SNAPSHOT (the ripple signal)"
         (= :SNAPSHOT (:type (ledger/classify {:incremental? true :bootstrap? true :written 5}))))
  (check "bootstrap SNAPSHOT still wins even with tombstones present"
         (= :SNAPSHOT (:type (ledger/classify {:incremental? true :bootstrap? true
                                                :written 5 :deleted 1})))))

(deftest snapshot-transaction-ripples-downstream
  (let [branch (str "ripple-" (subs (uuid) 0 8))
        D1 "ri.sutra.main.dataset.d1000000-0000-0000-0000-000000000001"
        D2 "ri.sutra.main.dataset.d2000000-0000-0000-0000-000000000002"
        D3 "ri.sutra.main.dataset.d3000000-0000-0000-0000-000000000003"
        C1 "ri.sutra.main.transform.c1000000-0000-0000-0000-0000000000c1"
        C2 "ri.sutra.main.transform.c2000000-0000-0000-0000-0000000000c2"
        C3 "ri.sutra.main.transform.c3000000-0000-0000-0000-0000000000c3"
        col [{:attribute-name :label :attribute-type :string}]
        store     #(ctx/store branch)
        low #(:low (ledger/read-boundary (store) %1 %2 branch))
        clock (atom 0)
        commit! (fn [writes consumed]
                  (ledger/commit-transactions!
                   (store) {:branch branch :run-id (uuid) :snapshot-id (uuid)
                         :writes writes :consumed consumed
                         :at (format "2026-01-01T00:00:%02dZ" (swap! clock inc))}))]
    (try
      (store/fork! branch)
      (dsets/ensure-tables! (store) {(naming/dataset-table D2) col (naming/dataset-table D3) col})
      (doseq [[c d] [[C1 D1] [C2 D2] [C3 D3]]]
        (ledger/advance-pointer! (store) {:consumer-rid c :input-dataset-rid d :branch branch
                                        :last-txn-rid nil :last-snapshot-id "seed"
                                        :updated-at "2026-01-01T00:00:00Z"}))

      (section "steady state: an APPEND output does NOT reset its consumers")
      (commit! [{:dataset-rid D2 :job-rid "jA" :incremental? true :bootstrap? false :written 1}]
               [{:consumer-rid C1 :input-dataset-rid D1 :last-snapshot-id "seed"}])
      (check "D2 APPEND ⇒ C2's pointer into D2 survives" (some? (low C2 D2)))

      (section "build N: C1 bootstrapped ⇒ its output D2 is SNAPSHOT, C2's cursor stands (delta says it)")
      (commit! [{:dataset-rid D2 :job-rid "jB" :incremental? true :bootstrap? true :written 5}]
               [{:consumer-rid C1 :input-dataset-rid D1 :last-snapshot-id "seed"}])
      (check "D2's newest transaction is SNAPSHOT"
             (= "SNAPSHOT" (:type (first (ledger/list-transactions (store) D2 branch 1)))))
      (check "C2's pointer into D2 stands" (some? (low C2 D2)))
      (check "C1's pointer into D1 is re-advanced (C1 consumed D1 this build)" (some? (low C1 D1)))
      (check "C3's pointer into D3 is untouched" (some? (low C3 D3)))

      (section "build N+1: C2 now bootstraps ⇒ D3 SNAPSHOT, C3's cursor stands")
      (commit! [{:dataset-rid D3 :job-rid "jC" :incremental? true :bootstrap? true :written 5}]
               [{:consumer-rid C2 :input-dataset-rid D2 :last-snapshot-id "seed"}])
      (check "C3's pointer into D3 stands" (some? (low C3 D3)))
      (check "C2's pointer into D2 is re-advanced (C2 consumed D2 this build)" (some? (low C2 D2)))

      (finally (try (store/retire! branch) (catch Exception _ nil))))))
