(ns ^:unit platform.stream-cdc-sync-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.builds.run.stream-index :as stream-index]
            [sutra.parser.ent :as ent]
            [sutra.sources.dialect :as dialect]
            [malli.core :as m]))

(def ^:private lsn1 "0x00000000000000000001")
(def ^:private lsn2 "0x00000000000000000002")
(def ^:private lsn3 "0x00000000000000000003")

(deftest was-ein-changelog-sync-als-naechstes-tut
  (let [schritt #(stream-index/cdc-step "T" %1 {:min_lsn %2 :max_lsn %3})]
    (testing "zuerst der Stand der Tabelle, dann die Aenderungen"
      (is (= {:step :snapshot :to lsn2} (schritt nil lsn1 lsn2)))
      (is (= {:step :changes :from lsn1 :to lsn3} (schritt lsn1 lsn1 lsn3))))
    (testing "ohne Aenderung vor dem Einschalten steht max hinter min: der Cursor nimmt min"
      (is (= {:step :snapshot :to lsn2} (schritt nil lsn2 lsn1))))
    (testing "nichts Neues liest nichts"
      (is (= {:step :idle} (schritt lsn2 lsn1 lsn2))))
    (testing "ein Cursor hinter der Aufbewahrung ist ein Halt, kein stilles Loch"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"retention" (schritt lsn1 lsn2 lsn3))))
    (testing "eine Tabelle ohne CDC sagt, was an der Quelle fehlt"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"sp_cdc_enable_table"
                            (schritt nil "0x00000000000000000000" lsn1)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"sp_cdc_enable_db"
                            (schritt nil nil nil))))))

(deftest die-statements-tragen-die-loeschspalte
  (let [[sql from to] (dialect/cdc-changes-stmt "dbo_T" [:id :gone] :gone lsn1 lsn2)]
    (is (= [lsn1 lsn2] [from to]))
    (is (re-find #"CASE WHEN \[__\$operation\] = 1 THEN 1 ELSE 0 END AS \[gone\]" sql))
    (is (re-find #"\[id\] AS \[id\]" sql))
    (is (not (re-find #"\[gone\] AS" sql))))
  (is (re-find #"0 AS \[gone\] FROM \[dbo\]\.\[T\]"
               (first (dialect/cdc-snapshot-stmt {:table "T"} [:id :gone] :gone))))
  (is (= "dbo_T" (dialect/cdc-instance {:table "T"})))
  (is (= "x" (dialect/cdc-instance {:table "T" :capture-instance "x"})))
  (testing "ein Name mit SQL darin kommt nicht ins Statement"
    (is (thrown? Exception (dialect/cdc-instance {:table "T'; DROP TABLE x;--"})))))

(deftest ein-sync-liest-entweder-cdc-oder-eine-abfrage
  (let [ok? #(m/validate @#'ent/Stream {:sync (assoc % :source :s)})]
    (is (ok? {:cdc {:table "T"}}))
    (is (ok? {:query "SELECT 1" :cursor :c}))
    (is (not (ok? {:query "SELECT 1"})))
    (is (not (ok? {:cdc {:table "T"} :query "SELECT 1" :cursor :c})))))
