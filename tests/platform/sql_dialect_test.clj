(ns ^:unit platform.sql-dialect-test
  (:require [clojure.test :refer [deftest testing is are]]
            [sutra.sources.dialect :as dialect]))

(def ^:private quote-ident @#'dialect/quote-ident)

(deftest quoting-per-vendor
  (are [type expected] (= expected (quote-ident type "a]b`c\"d"))
    :mysql    "`a]b``c\"d`"
    :mssql    "[a]]b`c\"d]"
    :postgres "\"a]b`c\"\"d\""
    :sqlite   "\"a]b`c\"\"d\""
    :other    "\"a]b`c\"\"d\""))

(deftest mssql-output-writes-into-a-table-variable
  (testing "a bare OUTPUT is refused by SQL Server on any table with a trigger"
    (let [sql (dialect/insert-sql :mssql {:table "WO_STAMM" :returning "WO_ID"}
                            "[A], [B]" "?, ?" :output)]
      (is (re-find #"OUTPUT INSERTED\.\[WO_ID\] INTO @sutra_out \(id\)" sql))
      (is (not (re-find #"OUTPUT INSERTED\.\[WO_ID\] AS id" sql))
          "the old shape, the one SQL Server refuses")
      (is (re-find #"DECLARE @sutra_out TABLE \(id sql_variant\)" sql))
      (is (re-find #"SELECT id FROM @sutra_out" sql))
      (is (re-find #"SET NOCOUNT ON" sql))
      (is (re-find #"SET NOCOUNT OFF" sql)
          "left as found — a connection must not keep NOCOUNT on")))
  (testing "the other dialects are untouched"
    (is (= (str "INSERT INTO \"T\" (a) VALUES (?) RETURNING \"ID\" AS id")
           (dialect/insert-sql :postgres {:table "T" :returning "ID"} "a" "?" :returning)))
    (is (= "INSERT INTO `T` (a) VALUES (?)"
           (dialect/insert-sql :mysql {:table "T" :returning "ID"} "a" "?" :generated-keys))))
  (testing "no returning column means no read-back at all"
    (is (= "INSERT INTO [T] (a) VALUES (?)"
           (dialect/insert-sql :mssql {:table "T" :returning nil} "a" "?" :output)))))
