(ns ^:unit platform.sql-runner-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.compute.sql :as sql]
            [sutra.foundation.settings :as settings]
            [sutra.parser.ent :as ent]
            [sutra.parser.spec :as spec]
            [sutra.types.coltype :as ctype]
            [sutra.types.languages :as langs]))

(def ^:private orders
  {:columns [{:name "id" :type (ctype/col-type :varchar)}
             {:name "n" :type (ctype/col-type :bigint)}
             {:name "price" :type (ctype/col-type :decimal)}
             {:name "paid" :type (ctype/col-type :boolean)}]
   :rows    [{:id "a" :n 1 :price 1.5M :paid true}
             {:id "b" :n 2 :price 2.5M :paid false}
             {:id "c" :n nil :price nil :paid nil}]})

(defn- run [query inputs]
  (sql/run-query! :sqlite-sql {:query query :inputs inputs}))

(deftest the-registry-names-each-sql-dialect
  (is (= :postgres (langs/dialect :sql) (langs/dialect :pg-sql)))
  (is (= :sqlite (langs/dialect :sqlite-sql)))
  (is (= :mssql (langs/dialect :mssql-sql)))
  (is (every? langs/query-language? [:sql :pg-sql :sqlite-sql :mssql-sql]))
  (is (not-any? langs/query-language? [:python :clojure nil])))

(deftest sqlite-runs-a-query-over-its-inputs
  (testing "rows come back under the keys a stored query gives"
    (is (= [{:id "a" :n 1} {:id "b" :n 2}]
           (run "SELECT id, n FROM orders WHERE n IS NOT NULL ORDER BY id" {"orders" orders}))))
  (testing "a marked identifier is quoted for the dialect"
    (is (= [{:n 1}] (run "SELECT `n` FROM `orders` WHERE id = 'a'" {"orders" orders}))))
  (testing "values are stored as the dialect reads them"
    (is (= [{:s 4.0 :p 1}]
           (run "SELECT sum(price) AS s, sum(paid) AS p FROM orders" {"orders" orders}))))
  (testing "an alias joins another"
    (is (= [{:n 2}]
           (run "SELECT o.n FROM orders o JOIN wanted w ON w.id = o.id"
                {"orders" orders
                 "wanted" {:columns [{:name "id" :type nil}] :rows [{:id "b"}]}}))))
  (testing "an empty input has its columns"
    (is (= [] (run "SELECT id FROM orders" {"orders" (assoc orders :rows [])})))))

(deftest a-query-says-what-is-wrong-with-it
  (is (thrown? java.sql.SQLException (run "SELECT missing FROM orders" {"orders" orders}))))

(deftest sqlite-cannot-reach-past-its-memory
  (testing "no write"
    (is (thrown? java.sql.SQLException (run "DROP TABLE orders" {"orders" orders}))))
  (testing "no file"
    (let [f (java.io.File/createTempFile "sutra-attach" ".db")]
      (.delete f)
      (is (thrown? java.sql.SQLException
                   (run (str "ATTACH DATABASE '" (.getPath f) "' AS x") {"orders" orders})))
      (is (not (.exists f)))))
  (testing "no endless query"
    (with-redefs [settings/value (constantly 1)]
      (is (thrown? java.sql.SQLException
                   (run (str "WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM c) "
                             "SELECT count(*) FROM c")
                        {"orders" orders}))))))

(deftest external-dialects-stage-under-their-own-rules
  (let [cols (:columns orders)]
    (is (= (str "CREATE TEMP TABLE \"o\" (\"id\" character varying, \"n\" bigint, "
                "\"price\" numeric, \"paid\" boolean)")
           (sql/create-sql :postgres "o" cols)))
    (is (= (str "CREATE TABLE [#o] ([id] nvarchar(max), [n] bigint, "
                "[price] decimal(38,10), [paid] bit)")
           (sql/create-sql :mssql "o" cols)))
    (is (= "CREATE TABLE \"o\" (\"id\", \"n\", \"price\", \"paid\")"
           (sql/create-sql :sqlite "o" cols)))
    (is (str/starts-with? (sql/insert-sql :mssql "o" (take 2 cols))
                          "INSERT INTO [#o] ([id], [n])"))))

(deftest t-sql-reaches-a-temp-table-by-its-alias
  (is (= "WITH [o] AS (SELECT * FROM [#o]) SELECT [n] FROM o"
         (sql/query-sql :mssql ["o"] "SELECT `n` FROM o")))
  (is (= "WITH [o] AS (SELECT * FROM [#o]), x AS (SELECT n FROM o) SELECT n FROM x"
         (sql/query-sql :mssql ["o"] "with x AS (SELECT n FROM o) SELECT n FROM x")))
  (is (= "SELECT \"n\" FROM o" (sql/query-sql :postgres ["o"] "SELECT `n` FROM o"))))

(deftest the-parser-takes-each-dialect-as-a-sql-transform
  (doseq [lang [:sql :pg-sql :sqlite-sql :mssql-sql]]
    (is (= :compute/sql (spec/transform-kind {:language lang})))
    (is (empty? (spec/shape-problems {:transform-name :t :language lang :query "SELECT 1"
                                      :output {:kind :dataset :dataset :d}
                                      :inputs {:a {:kind :dataset :dataset :e}}})))))

(defn- pipeline [member]
  {:name :p
   :transforms {:t (merge {:output {:kind :dataset :dataset :d}
                           :inputs {:a {:kind :dataset :dataset :e}}}
                          member)}})

(deftest a-pipeline-query-names-its-dialect
  (let [parsed #(ent/parse-entp (pipeline %) :inline)]
    (is (= :sqlite-sql
           (get-in (parsed {:language :sqlite-sql :query "SELECT 1"})
                   [:schema :transforms :t :language])))
    (is (thrown? Exception (parsed {:query "SELECT 1"})) "no language")
    (is (thrown? Exception (parsed {:language :python :query "SELECT 1"})) "no SQL dialect")))

(deftest an-external-dialect-needs-a-runner
  (is (true? (sql/runnable? :sqlite-sql)))
  (with-redefs-fn {#'sql/runner-url (constantly nil)}
    #(do (is (false? (sql/runnable? :mssql-sql)))
         (is (thrown? Exception (sql/run-query! :mssql-sql {:query "SELECT 1" :inputs {}}))))))
