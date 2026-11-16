(ns sutra.lib.pushdown-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.lib :as lib]
            [sutra.lib.object-set-test :as os-test]
            [sutra.lib.store :as store]
            [sutra.lib.test-store :as ts]
            [sutra.store.api :as api]))

(defn- recorded-plans
  [db f]
  (with-open [s (api/snapshot db {:branch "main"})]
    (let [asked (volatile! [])
          ask   (fn [q] (when (= :select (first q)) (vswap! asked conj (second q)))
                  (api/read s store/system q))]
      (f ask)
      (mapv (fn [ir] [ir (api/read s store/system [:select/explain ir])]) @asked))))

(deftest the-library-asks-plans-the-engine-claims-whole
  (let [clock (ts/fixed-clock)
        db    (ts/fresh clock)
        W     (store/install! db (lib/lower os-test/ent) {:clock clock})
        ac    (lib/type-rid W :aircraft)]
    (store/put! db ac [{:id "D-AINA" :values {:registration "D-AINA"}}])
    (store/submit! db W :report-defect {:aircraft "D-AINA" :summary "Cabin light" :severity "minor"}
                   {:clock clock})
    (let [asked
          (recorded-plans db (fn [ask]
                               (lib/select W ask
                                           {:op :order :order-by :registration
                                            :in {:op :scan :root ac}})
                               (lib/query W ask :open-defects-of {:reg "D-AINA"})))
          plans (map first asked)]
      (testing "a read is one plan: reading aircraft asks for the rows and their derived columns at once; the set one more"
        (is (= 2 (count plans)))
        (is (some #(= :with (:op %)) (tree-seq map? (comp #(if (map? %) [%] %) :in) (first plans)))
            "the aircraft plan carries the column of their open defects"))
      (when ts/pg?
        (testing "Postgres claims every one of them whole: each is a single SQL statement"
          (doseq [[ir e] asked]
            (is (:pushdown? e) (pr-str (:op ir)))
            (is (= [[]] (:claims e)) "the claim is the whole plan, from its root")
            (is (string? (get-in e [:engine :sql])))))))))
