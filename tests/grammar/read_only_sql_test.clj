(ns ^:unit grammar.read-only-sql-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.read-only-sql :as ro]))

(defn- verdict
  ([sql] (verdict sql nil))
  ([sql dialect]
   (try (ro/assert-read-only-sql! sql dialect) :ok
        (catch clojure.lang.ExceptionInfo e (:refusal/code (ex-data e))))))

(deftest read-only-statements-pass
  (doseq [sql ["SELECT * FROM t" "SELECT 1;" "select 1 ; -- done\n"
               "WITH x AS (SELECT 1) SELECT * FROM x" "(SELECT 1) UNION (SELECT 2)"
               "VALUES (1), (2)" "TABLE t" "EXPLAIN SELECT 1"
               "SELECT replace(a, 'x', 'y'), insert(a, 1, 2, 'z'), truncate(n, 2) FROM t"
               "SELECT 'please drop; delete' AS msg" "SELECT \"delete\" FROM t"
               "SELECT a /* drop */ FROM t -- delete\n" "SELECT update_count FROM t"]]
    (is (= :ok (verdict sql)) sql)))

(deftest postgres-reads-its-own-quotes
  (is (= :ok (verdict "SELECT a FROM t WHERE b ~ '\\d+$' AND c = $$x$$ AND d = $1" :postgres)))
  (is (= :ok (verdict "SELECT $tag$ it's ; DROP $tag$" :postgres)))
  (is (= :ok (verdict "SELECT a$b$ FROM t" :postgres)) "a $ inside a word opens no quote"))

(deftest a-second-statement-is-refused
  (doseq [sql ["SELECT 1; DROP TABLE t" "SELECT 1; SELECT 2" "SELECT 1;;SELECT 2"
               "SELECT $$;$$ ; DROP TABLE t"]]
    (is (= :refusal.foreign/sql-multiple (verdict sql)) sql)))

(deftest quotes-one-dialect-reads-differently-hide-nothing
  (testing "a T-SQL bracket identifier holding a quote"
    (is (not= :ok (verdict "SELECT [x'] FROM t; DROP TABLE t --']")))
    (is (not= :ok (verdict "SELECT [x'] FROM t DROP TABLE t --']"))))
  (testing "a backslash: an escape in MySQL and E'', a plain char in a standard string"
    (is (not= :ok (verdict "SELECT '\\'; DELETE FROM t; --'")))
    (is (= :ok (verdict "SELECT E'\\'; DELETE FROM t; --'" :postgres)) "one E'' literal")
    (is (not= :ok (verdict "SELECT E'\\\\'; DELETE FROM t" :postgres))))
  (testing "a dollar quote pg sees and others do not"
    (is (not= :ok (verdict "SELECT $x$ ' $x$; DROP TABLE t; --'"))))
  (testing "nested comments in pg and T-SQL, flat in MySQL"
    (is (not= :ok (verdict "SELECT 1 /* /* */ DROP TABLE t */"))))
  (testing "MySQL: # comments, -- needs a space, /*! runs"
    (is (not= :ok (verdict "SELECT 1 #'\nDROP TABLE t")))
    (is (not= :ok (verdict "SELECT 1 --'\n; DROP TABLE t; --'")))
    (is (= :refusal.foreign/sql-ambiguous (verdict "SELECT 1 /*! ; DROP TABLE t */"))))
  (testing "an Oracle q-quote"
    (is (not= :ok (verdict "SELECT q'[ ' ]' FROM t; DROP TABLE t; --'"))))
  (testing "a pg unicode identifier spells any name"
    (is (= :refusal.foreign/sql-ambiguous
           (verdict "SELECT U&\"\\0073et_config\"('role', 'x', true)" :postgres)))))

(deftest what-writes-or-escapes-is-refused
  (doseq [sql ["WITH x AS (DELETE FROM t RETURNING *) SELECT * FROM x"
               "WITH x AS (INSERT INTO t VALUES (1) RETURNING *) SELECT * FROM x"
               "WITH x AS (UPDATE t SET a = 1 RETURNING *) SELECT * FROM x"
               "SELECT * INTO t2 FROM t" "SELECT * FROM t FOR UPDATE"
               "SELECT * FROM t FOR NO KEY UPDATE" "SELECT * FROM t FOR SHARE"
               "SELECT * FROM t LOCK IN SHARE MODE" "SELECT 1 DROP TABLE t"
               "SELECT 1 EXEC sp_who" "EXPLAIN ANALYZE DELETE FROM t"
               "EXPLAIN (ANALYZE) SELECT 1" "SELECT set_config('role', 'postgres', true)"
               "SELECT \"set_config\"('session_authorization', 'postgres', true)"
               "SELECT pg_read_file('/etc/passwd')" "SELECT * FROM dblink('x', 'y')"
               "SELECT query_to_xml('delete from t', true, true, '')"
               "SELECT nextval('s')" "SELECT load_extension('x')"]]
    (is (= :refusal.foreign/sql-forbidden-verb (verdict sql)) sql)))

(deftest only-a-reading-statement-may-lead
  (doseq [sql ["COMMIT" "ROLLBACK" "SET ROLE postgres" "RESET ROLE" "DO $$ BEGIN END $$"
               "COPY t TO '/tmp/x'" "DELETE FROM t" "EXEC('drop table t')" "SHOW ALL"
               "START TRANSACTION READ WRITE"]]
    (is (= :refusal.foreign/sql-not-select (verdict sql)) sql)))

(deftest broken-text-is-refused
  (is (= :refusal.foreign/sql-empty (verdict "  ")))
  (is (= :refusal.foreign/sql-empty (verdict "; -- nothing")))
  (doseq [sql ["SELECT 'open" "SELECT \"open" "SELECT 1 /* open"]]
    (is (= :refusal.foreign/sql-unterminated (verdict sql :postgres)) sql)))
