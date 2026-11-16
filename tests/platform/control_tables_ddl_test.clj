(ns platform.control-tables-ddl-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.store.control.tables :as cc]
            [sutra.store.pg.control-tables :as ct]))

(def ^:private decl
  {:name :runs :table "control_runs" :key [:id]
   :cols [[:id "VARCHAR(36) NOT NULL"] [:branch "VARCHAR(255) NOT NULL"] [:status "TEXT"]]
   :indexes [["control_runs_branch_idx" "branch" nil]]
   :moved-cols [[:old-status :status]]
   :dropped-cols [:legacy]})

(defn- stmts [catalog] (map second (ct/pending-ddl [decl] catalog)))

(def ^:private laid-out
  {:tables      #{"control_runs"}
   :columns     {["control_runs" "id"] {} ["control_runs" "branch"] {} ["control_runs" "status"] {}}
   :index-names #{"control_runs_branch_idx"}})

(deftest a-laid-out-table-takes-no-table-lock
  (let [ss (stmts laid-out)]
    (is (not-any? #(str/starts-with? % "ALTER TABLE") ss))
    (is (not-any? #(str/starts-with? % "CREATE INDEX") ss))
    (is (every? #(str/starts-with? % "DO $$") ss) "only the self-checking unique blocks remain")))

(deftest only-what-is-missing
  (testing "a missing column is added, and only it"
    (let [ss (stmts (update laid-out :columns dissoc ["control_runs" "status"]))]
      (is (some #{"ALTER TABLE control_runs ADD COLUMN IF NOT EXISTS \"status\" TEXT"} ss))
      (is (not-any? #(str/includes? % "\"branch\" VARCHAR") ss))))
  (testing "a missing index is created"
    (is (some #(str/starts-with? % "CREATE INDEX IF NOT EXISTS control_runs_branch_idx")
              (stmts (assoc laid-out :index-names #{})))))
  (testing "a moved or dropped column is handled only while it is there"
    (let [ss (stmts (update laid-out :columns assoc ["control_runs" "old_status"] {}
                            ["control_runs" "legacy"] {}))]
      (is (some #(str/includes? % "DROP COLUMN \"old_status\"") ss))
      (is (some #{"ALTER TABLE control_runs DROP COLUMN IF EXISTS \"legacy\""} ss)))))

(deftest a-new-table-gets-its-whole-ddl
  (is (some #(str/starts-with? % "CREATE TABLE IF NOT EXISTS control_runs")
            (stmts {:tables #{} :columns {} :index-names #{}}))))

(deftest the-real-declarations-are-quiet-on-their-own-catalog
  (let [catalog {:tables      (into #{} (map #(str/replace (:table %) "\"" "")) cc/tables)
                 :columns     (into {} (for [{:keys [table cols]} cc/tables [c] cols]
                                         [[(str/replace table "\"" "")
                                           (str/replace (name c) "-" "_")] {}]))
                 :index-names (into #{}
                                    (for [{:keys [indexes]} cc/tables [nm] indexes]
                                      (str/lower-case nm)))}]
    (is (empty? (remove #(str/starts-with? (second %) "DO $$")
                        (ct/pending-ddl cc/tables catalog))))))
