(ns ^:integration ontology.sql-transform-inputs
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.types.naming :as naming]
            [sutra.builds.import.materialize :as mat]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [support.pod :as pod]))

(def ^:private store-inputs #'mat/store-inputs)

(use-fixtures :once pod/with-stores-only)

(def ^:private orders :sqlx-orders)
(defn- tbl [] (naming/dataset-table orders))
(defn- row [id vals] {:id id :values vals})

(defn- run
  ([q] (run q nil))
  ([q added]
   (dsets/query-tables (ctx/system-store)
                       (store-inputs (partial dsets/added-ids (ctx/system-store))
                                      {"orders" (tbl)} added)
                       q {})))

(defn- current [] (dsets/current-version (ctx/system-store)))

(deftest an-input-resolves-to-its-catalog-table
  (dsets/write-rows! (ctx/system-store) orders :snapshot [(row "o1" {:sku "A" :qty 2})])
  (is (= {"orders" {:table (tbl)}}
         (store-inputs (partial dsets/added-ids (ctx/system-store)) {"orders" (tbl)} nil)))
  (testing "a table the store does not hold is refused by the dataset port"
    (is (= "nope"
           (try (dsets/query-tables (ctx/system-store)
                                    (store-inputs (partial dsets/added-ids (ctx/system-store))
                                                  {"nope" "d_not_in_the_store"} nil)
                                 "SELECT 1 FROM nope" {})
                nil
                (catch clojure.lang.ExceptionInfo e (:datasets/missing-input (ex-data e))))))))

(deftest the-authors-own-query-runs-unchanged-against-store-data
  (dsets/write-rows! (ctx/system-store) orders :snapshot
                  [(row "o1" {:sku "A" :qty 2})
                   (row "o2" {:sku "B" :qty 5})
                   (row "o3" {:sku "A" :qty 7})])
  (is (= [{:sku "A" :total 9} {:sku "B" :total 5}]
         (mapv #(update % :total long)
               (run "SELECT sku, sum(qty) AS total FROM orders GROUP BY sku ORDER BY sku")))))

(deftest an-added-view-narrows-to-the-delta-and-includes-modifications
  (dsets/write-rows! (ctx/system-store) orders :snapshot
                  [(row "o1" {:sku "A" :qty 2}) (row "o2" {:sku "B" :qty 5})])
  (let [low (current)]
    (dsets/write-rows! (ctx/system-store) orders :append
                    [(row "o1" {:sku "A" :qty 200})
                     (row "o4" {:sku "C" :qty 1})])
    (let [added {:orders {:low low :high (current)}}]
      (is (= #{"200" "1"} (set (map (comp str :qty) (run "SELECT qty FROM orders" added))))
          "the modified row and the new one, and nothing untouched")
      (testing "the same query without a boundary still sees the whole table"
        (is (= 3 (count (run "SELECT qty FROM orders"))))))))

(deftest an-empty-delta-is-an-empty-view-not-a-full-one
  (dsets/write-rows! (ctx/system-store) orders :snapshot [(row "o1" {:sku "A" :qty 2})])
  (let [at (current)]
    (is (= [] (run "SELECT qty FROM orders" {:orders {:low at :high at}})))))

(deftest a-bounded-input-does-not-disagree-with-the-python-read
  (dsets/write-rows! (ctx/system-store) orders :snapshot [(row "o1" {:sku "A" :qty 1})])
  (let [low (current)]
    (dsets/write-rows! (ctx/system-store) orders :append [(row "o1" {:sku "A" :qty 9})
                                        (row "o2" {:sku "Z" :qty 4})])
    (let [high (current)
          sql  (set (map (juxt (comp str :sku) (comp str :qty))
                         (run "SELECT sku, qty FROM orders" {:orders {:low low :high high}})))
          py   (set (map (juxt (comp str :sku) (comp str :qty))
                         (dsets/added-values (ctx/system-store) (tbl) low high nil)))]
      (is (= sql py) "one delta, two languages"))))

(deftest a-run-leaves-no-temp-view-on-the-pooled-connection
  (dsets/write-rows! (ctx/system-store) orders :snapshot [(row "o1" {:sku "A" :qty 2})])
  (run "SELECT sku FROM orders")
  (is (zero? (:n (jdbc/execute-one! (tsys/ds)
                                    ["SELECT count(*) AS n FROM pg_class c
                                      JOIN pg_namespace n ON n.oid = c.relnamespace
                                      WHERE n.nspname LIKE 'pg_temp_%' AND c.relkind = 'v'"]
                                    {:builder-fn rs/as-unqualified-maps})))
      "a view left behind blocks every later DROP of the table it reads"))
