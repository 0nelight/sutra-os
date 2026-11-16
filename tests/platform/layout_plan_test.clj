(ns ^:unit platform.layout-plan-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [sutra.store.pg.layout :as layout]))

(def ^:private t (layout/object-table :x))

(def ^:private main-cols
  [{:name "internal_uuid" :type "character varying" :not-null? true}
   {:name "n" :type "integer" :not-null? false}
   {:name "_markings" :type "character varying" :not-null? false}
   {:name "sys_from" :type "bigint" :not-null? false}
   {:name "sys_to" :type "bigint" :not-null? false}])

(def ^:private K
  {:tables  {"main" #{t (str t "__history")}
             "bo_b1" #{t (str t "__history") "_detached" (str t "__tomb")}
             "br_b1" #{t}}
   :columns {["main" t] main-cols ["main" (str t "__history")] main-cols
             ["bo_b1" t] main-cols ["bo_b1" (str t "__history")] main-cols}
   :owned   {"main" {t "sutra:store" "d_gone" "sutra:store" "d_landed" "sutra:datasets"}
             "bo_b1" {t "sutra:store"}}
   :key-idx {}
   :views   {t #{"b1"}}})

(def ^:private sig {:types {:x {:attrs [{:name :n :type :string}]}}})

(defn- sql-of [ops] (keep (fn [[k s]] (when (#{:exec :update} k) s)) ops))
(defn- index-of [ops pred] (first (keep-indexed (fn [i o] (when (pred o) i)) ops)))

(deftest a-retype-is-checked-then-altered-around-the-views-over-the-table
  (let [ops    (layout/layout-plan K "b1" 7 {:old {} :new sig :steps []})
        assert (index-of ops #(= :assert-castable (first %)))
        drop-v (index-of ops #(and (= :exec (first %)) (str/starts-with? (second %) "DROP VIEW")))
        alter  (index-of ops
                         #(and (= :exec (first %))
                               (str/starts-with? (second %) "ALTER TABLE \"bo_b1\"")))
        view   (index-of ops
                         #(and (= :exec (first %)) (str/starts-with? (second %) "CREATE VIEW")))]
    (is (= ["bo_b1" t "n"] (subvec (nth ops assert) 1 4))
        "the values of n are checked before anything changes")
    (is (= [(str "\"bo_b1\".\"" t "\"") (str "\"bo_b1\".\"" t "__history\"")]
           (last (nth ops assert)))
        "in the live table and its history")
    (is (< assert drop-v alter view)
        "the view over the table is dropped, the column retyped, the view rebuilt")
    (is (= 2 (count (filter #(str/starts-with? % "ALTER TABLE") (sql-of ops))))
        "table and history alike")))

(deftest drop-unclaimed-drops-only-the-stores-tables-the-signature-does-not-hold
  (let [ops (layout/layout-plan K "main" 7 {:old sig :new sig :steps [[:drop-unclaimed]]})
        sql (sql-of ops)]
    (is (some #(str/includes? % "DROP TABLE IF EXISTS \"main\".\"d_gone\"") sql))
    (is (not-any? #(str/includes? % "d_landed") sql)
        "a dataset's table is not the store's to drop")
    (is (not-any? #(str/includes? % (str "\"main\".\"" t "\" CASCADE")) sql))))

(deftest a-fork-of-main-only-makes-its-schemas
  (is (= [[:exec "CREATE SCHEMA IF NOT EXISTS \"br_b2\""]
          [:exec "CREATE SCHEMA IF NOT EXISTS \"bo_b2\""]]
         (layout/fork-plan K "main" "b2" sig 7))))

(deftest an-orphan-tomb-history-goes-before-the-tomb-is-made
  (let [K   (assoc-in K [:tables "bo_b2"] #{(str t "__tomb__history")})
        sql (vec (sql-of (layout/own-plan K "b2" t)))
        drp (index-of sql
                      #(= %
                          (str "DROP TABLE IF EXISTS \"bo_b2\".\"" t "__tomb__history\" CASCADE")))
        mk  (index-of sql #(str/starts-with? % (str "CREATE TABLE \"bo_b2\".\"" t "__tomb\"")))]
    (is (some? drp) "the history left without its tomb is dropped")
    (is (< drp mk) "before the tomb and its history are made")
    (is (not-any? #(str/includes? % "__tomb__history\" CASCADE")
                  (sql-of (layout/own-plan (assoc-in K [:tables "bo_b2"] #{}) "b2" t)))
        "without an orphan nothing is dropped")))
