(ns ^:unit platform.sql-ident-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.store.pg.dataset-exec :as pgexec]
            [sutra.types.aggregate :as agg]
            [sutra.types.sql-ident :as sqli]))

(deftest the-store-renders-the-marker-away
  (testing "no marker survives to the driver"
    (let [sql (str "SELECT t." (sqli/quote-ident "a") " FROM " (sqli/quote-ident "x"))]
      (is (not (str/includes? (pgexec/render-sql sql) sqli/mark-str)))
      (is (= "SELECT t.\"a\" FROM \"x\"" (pgexec/render-sql sql)))))
  (testing "a doubled marker is one literal marker in the name"
    (is (= "\"we`ird\"" (pgexec/render-sql (sqli/quote-ident "we`ird")))))
  (testing "an existing double quote in a name is escaped, not passed through"
    (is (= "\"we\"\"ird\"" (pgexec/render-sql (sqli/quote-ident "we\"ird")))))
  (testing "an unterminated marker is refused rather than silently mangled"
    (is (thrown? clojure.lang.ExceptionInfo
                 (pgexec/render-sql (str "SELECT " sqli/mark-str "oops"))))))

(deftest the-compiler-emits-for-the-bound-store
  (testing "the bucket compiles in postgres, with no rewrite after"
    (is (str/includes? (agg/group-sql :postgres {:kind :bucket :unit :month}
                                      (str "t." (sqli/quote-ident "datum")))
                       "to_char")))
  (testing "rendering only quotes — it never rewrites an expression"
    (let [pg (agg/group-sql :postgres {:kind :bucket :unit :month}
                            (str "t." (sqli/quote-ident "datum")))]
      (is (= "to_char(t.\"datum\", 'YYYY-MM')" (pgexec/render-sql pg))))))
