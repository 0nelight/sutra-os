(ns ^:integration platform.sql-runner-store-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [support.pod :as pod]
            [support.system :as tsys]
            [sutra.builds.import.materialize :as mat]
            [sutra.builds.plan.preview :as transform-preview]
            [sutra.compute.sql :as sql]
            [sutra.datasets.api :as datasets]
            [sutra.datasets.mem :as mem]
            [sutra.store.access.datasets :as dsets]
            [sutra.authoring.install.findings.sql :as sql-findings]
            [sutra.store.pg.datasets :as pg-datasets]
            [sutra.store.read-only :as read-only]
            [sutra.types.naming :as naming]))

(use-fixtures :once pod/with-stores-only)

(def ^:private table "d_runner_orders")

(def ^:private rows
  [{:internal_uuid "a" :kunde "x" :n 1}
   {:internal_uuid "b" :kunde "y" :n 2}
   {:internal_uuid "c" :kunde "y" :n 4}])

(defn- store-over [driver]
  (datasets/write! driver {:branch "main" :dataset table}
                   {:kind :snapshot :pk "internal_uuid" :rows rows})
  {:datasets driver :branch "main"})

(defn- pg-store []
  (let [driver (pg-datasets/->PgDatasets (tsys/ds) (tsys/ds))]
    (datasets/drop-dataset! driver {:branch "main" :dataset table})
    (store-over driver)))

(defn- mem-store [] (store-over (mem/datasets)))

(defn- pg-url []
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (let [meta (.getMetaData c)]
      (str (.getURL meta) "?user=" (.getUserName meta) "&password="
           (or (not-empty (System/getenv "SUTRA_TEST_PG_PASSWORD")) "sutra")))))

(def ^:private total
  "SELECT kunde, sum(n) AS s FROM o GROUP BY kunde ORDER BY kunde")

(defn- sums [store language]
  (->> (mat/run-query! store language {"o" {:table table}} total)
       (mapv (fn [{:keys [kunde s]}] [kunde (long s)]))))

(def ^:private expected [["x" 1] ["y" 6]])

(deftest one-transform-answers-alike-whatever-runs-it-and-whatever-stores-it
  (testing "a Pg store answers Postgres SQL in place, without a runner"
    (with-redefs-fn {#'dsets/runner-inputs (fn [& _] (throw (ex-info "inputs were copied" {})))}
      #(is (= expected (sums (pg-store) :sql) (sums (pg-store) :pg-sql)))))
  (testing "a Pg store runs SQLite"
    (is (= expected (sums (pg-store) :sqlite-sql))))
  (testing "a store without an engine runs SQLite"
    (is (= expected (sums (mem-store) :sqlite-sql))))
  (testing "a store without an engine runs Postgres SQL on an external Postgres"
    (let [url (pg-url)]
      (with-redefs-fn {#'sql/runner-url (constantly url)}
        #(is (= expected (sums (mem-store) :pg-sql)))))))

(deftest a-store-without-the-dialect-refuses-without-a-runner
  (with-redefs-fn {#'sql/runner-url (constantly nil)}
    #(is (thrown? Exception (sums (mem-store) :pg-sql)))))

(deftest an-incremental-window-reaches-the-runner
  (is (= [{:n 4}] (mat/run-query! (pg-store) :sqlite-sql {"o" {:table table :ids ["c"]}}
                                  "SELECT n FROM o")))
  (is (= [] (mat/run-query! (pg-store) :sqlite-sql {"o" {:table table :ids []}}
                            "SELECT n FROM o"))))

(def ^:private rid "ri.sutra.main.dataset.00000000-0000-0000-0000-00000000c0de")

(defn- schema-over []
  {:datasets {rid {:id rid :dataset-name :bestellung}}})

(defn- spec [language query]
  {:transform-name :summe :language language :query query
   :inputs {:o {:kind :dataset :dataset rid}}
   :output {:kind :dataset :dataset "ziel"}})

(defn- store-with-rid []
  (let [driver (pg-datasets/->PgDatasets (tsys/ds) (tsys/ds))
        addr   {:branch "main" :dataset (naming/dataset-table rid)}]
    (datasets/drop-dataset! driver addr)
    (datasets/write! driver addr {:kind :snapshot :pk "internal_uuid" :rows rows})
    {:datasets driver :branch "main"}))

(deftest a-sqlite-transform-goes-the-way-of-a-postgres-one
  (let [store (store-with-rid)
        schema (schema-over)]
    (testing "a preview gives the same rows"
      (let [q     "SELECT kunde, sum(n) AS s FROM o GROUP BY kunde ORDER BY kunde"
            shown (fn [language]
                    (mapv (fn [{:keys [kunde s]}] [kunde (long s)])
                          (:rows (transform-preview/preview-spec! store schema (spec language q)
                                                                  :summe))))]
        (is (= expected (shown :sql) (shown :sqlite-sql)))))
    (testing "the install gate judges each dialect by its own engine"
      (is (= :ok (:state (mat/explain-transform store schema
                                                (spec :sqlite-sql "SELECT ifnull(n, 0) FROM o")))))
      (is (= :refused (:state (mat/explain-transform store schema
                                                     (spec :sql "SELECT ifnull(n, 0) FROM o")))))
      (is (= :refused (:state (mat/explain-transform store schema
                                                     (spec :sqlite-sql "SELECT nope FROM o"))))))
    (testing "without an engine the gate refuses and names the language"
      (with-redefs-fn {#'sql/runner-url (constantly nil)}
        #(let [{:keys [state detail]} (mat/explain-transform store schema
                                                             (spec :mssql-sql "SELECT 1 AS a"))]
           (is (= :refused state))
           (is (re-find #"mssql-sql" detail)))))))

(deftest the-sql-check-only-reads
  (let [store  (store-with-rid)
        schema (schema-over)
        specs  [[:a (spec :sql "SELECT kunde, sum(n) AS s FROM o GROUP BY kunde")]
                [:b (spec :sql "SELECT nope FROM o")]
                [:c (spec :sqlite-sql "SELECT ifnull(n, 0) FROM o")]
                [:d (spec :sqlite-sql "SELECT nope FROM o")]]
        states (fn [st]
                 (mapv (juxt :transform :state)
                       (sql-findings/sql-verdicts st {:sql-explain mat/explain-transform}
                                                  schema specs)))]
    (is (= [[:a :ok] [:b :refused] [:c :ok] [:d :refused]] (states store)))
    (is (= (states store) (states (read-only/of store)))
        "the same verdicts through a store that refuses every write")))

(deftest t-sql-runs-on-a-real-server
  (if-let [url (not-empty (System/getenv "SUTRA_TEST_MSSQL_URL"))]
    (with-redefs-fn {#'sql/runner-url (constantly url)}
      #(let [store (mem-store)]
         (is (= expected (sums store :mssql-sql)))
         (testing "a query that has its own WITH"
           (is (= [{:s 6}]
                  (mat/run-query! store :mssql-sql {"o" {:table table}}
                                  (str "WITH y AS (SELECT n FROM o WHERE kunde = 'y') "
                                       "SELECT sum(n) AS s FROM y")))))
         (testing "T-SQL is judged by T-SQL"
           (is (= [{:k "y"}]
                  (mat/run-query! store :mssql-sql {"o" {:table table}}
                                  "SELECT TOP 1 kunde AS k FROM o ORDER BY n DESC"))))
         (testing "a write is refused"
           (is (thrown? Exception
                        (mat/run-query! store :mssql-sql {"o" {:table table}} "DROP TABLE o"))))))
    (is true "no server given")))

(deftest a-missing-input-is-named
  (is (= "o"
         (try (mat/run-query! (mem-store) :sqlite-sql {"o" {:table "d_never"}} "SELECT 1")
              (catch clojure.lang.ExceptionInfo e (:datasets/missing-input (ex-data e)))))))

(deftest a-shape-plans-with-its-declared-columns
  (is (= []
         (mat/run-query! (mem-store) :sqlite-sql
                         {"o" {:columns [{:attribute-name :n :attribute-type :long}]}}
                         "SELECT n FROM o"))))
