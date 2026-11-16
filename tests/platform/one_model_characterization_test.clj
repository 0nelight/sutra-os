(ns ^:integration platform.one-model-characterization-test
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.datasets.api :as datasets]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.datasets :as pg-datasets]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.rows :as rows]
            [sutra.store.pg.sql :as sql]
            [support.pod :as pod])
  (:import (java.sql Connection)))

(use-fixtures :once pod/with-dataset-store)
(use-fixtures :each (fn [f] (pod/reset-dataset-store!) (f)))

(def ^:private pk "internal_uuid")

(def ^:private T :cmp)
(def ^:private sig {:types {T {:attrs [{:name :v :type :string}]}}})

(defn- obj-tx!
  [f]
  (with-open [^Connection c (jdbc/get-connection (tsys/ds))]
    (.setAutoCommit c false)
    (let [rev (parse-long
               (:rev
                (sql/one c
                         "SELECT set_config('sutra.v', nextval('public.dataset_seq')::text, true) AS rev" [])))]
      (f c rev)
      (.commit c)
      rev)))

(defn- obj-read [ns as-of]
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (sort (for [{:keys [id values]} (rows/rows c {:ns ns :sig sig :as-of as-of :view c} T nil)]
            [id (:v values)]))))

(defn- obj-write! [ns kvs]
  (obj-tx! (fn [c rev] (rows/write-rows! c {:ns ns :rev rev :sig sig :view c} T
                                         (for [[k v] kvs] {:id k :values {:v v}})))))

(defn- obj-delete! [ns ids]
  (obj-tx! (fn [c rev] (rows/delete-rows! c {:ns ns :rev rev :view c} T ids))))

(defn- obj-fork! [from to]
  (obj-tx! (fn [c rev] (layout/fork! c from to sig rev))))

(defn- D [] (pg-datasets/->PgDatasets (tsys/ds) (tsys/ds)))
(def ^:private dataset "x_cmp")
(defn- addr [b] {:branch b :dataset dataset :pk pk})

(defn- ds-version [] (datasets/-version (D) nil))

(defn- ds-read [b at]
  (sort (for [r (datasets/-read (D) (addr b) (cond-> {} at (assoc :at at)))]
          [(get r (keyword pk)) (:v r)])))

(defn- ds-write! [b kvs]
  (datasets/-write! (D) (addr b) {:kind :update :pk pk :rows (for [[k v] kvs] {pk k "v" v})})
  (ds-version))

(defn- ds-delete! [b ids]
  (datasets/-write! (D) (addr b) {:kind :delete :pk pk :ids ids})
  (ds-version))

(defn- one-value [q params]
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (first (vals (jdbc/execute-one! c (into [q] params)
                                    {:builder-fn rs/as-unqualified-lower-maps})))))

(defn- table-exists? [s t]
  (some?
   (one-value
    "SELECT 1 AS x FROM information_schema.tables WHERE table_schema = ? AND table_name = ?"
    [s t])))

(defn- constraints-of [s t]
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (set (map :conname
              (jdbc/execute! c ["SELECT conname FROM pg_constraint WHERE conrelid = to_regclass(?)"
                                (str "\"" s "\".\"" t "\"")]
                             {:builder-fn rs/as-unqualified-lower-maps})))))

(defn- partitions-of [s t]
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (set (map :relname (jdbc/execute! c [(str "SELECT cl.relname FROM pg_inherits i"
                                              " JOIN pg_class cl ON cl.oid = i.inhrelid"
                                              " WHERE i.inhparent = to_regclass(?)")
                                         (str "\"" s "\".\"" (layout/history-of t) "\"")]
                                      {:builder-fn rs/as-unqualified-lower-maps})))))

(deftest the-same-sequence-read-back-on-both-sides
  (obj-tx! (fn [c rev] (layout/layout! c "main" rev {:old {} :new sig :steps []})))
  (let [o1 (obj-write! "main" [["a" "1"] ["b" "2"]])
        _  (obj-fork! "main" "b1")
        o2 (obj-write! "b1" [["a" "10"]])
        o3 (obj-delete! "b1" ["a" "b"])
        _  (obj-fork! "b1" "b2")
        o4 (obj-write! "main" [["c" "3"]])
        d1 (ds-write! "main" [["a" "1"] ["b" "2"]])
        d2 (ds-write! "b1" [["a" "10"]])
        d3 (ds-delete! "b1" ["a" "b"])
        d4 (ds-write! "main" [["c" "3"]])
        at (fn [r] {:rev r :root-rev r})]
    (testing "main, now and as of v1: the same on both sides"
      (is (= [["a" "1"] ["b" "2"] ["c" "3"]] (obj-read "main" nil) (ds-read "main" nil)))
      (is (= [["a" "1"] ["b" "2"]] (obj-read "main" (at o1)) (ds-read "main" d1))))
    (testing "branch b1 now: its deletes stick, main's later row shows"
      (is (= [["c" "3"]] (obj-read "b1" nil) (ds-read "b1" nil))))
    (testing "branch b1 as of v1 (before it wrote) and v3 (after its deletes)"
      (is (= [["a" "1"] ["b" "2"]] (obj-read "b1" (at o1)) (ds-read "b1" d1)))
      (is (= [] (obj-read "b1" (at o3)) (ds-read "b1" d3)))
      (is (= [["c" "3"]] (obj-read "b1" (at o4)) (ds-read "b1" d4))))
    (testing "branch b1 as of v2, between its write of a and its delete of a: its own a hides main's (row 9, store-gaps §2.7)"
      (is (= [["a" "10"] ["b" "2"]] (obj-read "b1" (at o2)) (ds-read "b1" d2))))
    (testing "grandchild b2 (forked from b1 after v3), now"
      (is (= [["c" "3"]] (obj-read "b2" nil)) "objects inherit b1's own rows and tombs")
      (testing "DIFFERS, intended today (row 13, sutra.datasets.api \"reads its root live\", open O1)"
        (is (= [["a" "1"] ["b" "2"] ["c" "3"]] (ds-read "b2" nil))
            "datasets on a grandchild read main live and do not see b1's deletes")))))

(deftest rows-from-before-versioning-as-of
  (let [t "x_legacy"]
    (with-open [c (jdbc/get-connection (tsys/ds))]
      (jdbc/execute! c [(str "CREATE TABLE main." t " (" pk " varchar NOT NULL, v text)")])
      (jdbc/execute! c [(str "INSERT INTO main." t " VALUES ('a', '1'), ('b', '2')")])
      (pgds/ensure-versioned! c "main" t))
    (is (= 2 (one-value (str "SELECT count(*) AS n FROM main." t " WHERE sys_from IS NULL") []))
        "ensure-versioned! leaves the rows it found without a version")
    (let [v (ds-version)]
      (with-open [c (jdbc/get-connection (tsys/ds))]
        (let [layout-rows (sort (map (juxt (keyword pk) :v)
                                     (jdbc/execute!
                                      c
                                      (let [{rel :sql
                                             ps :params} (layout/relation c "main" t {:rev v})]
                                      (into [(str "SELECT * FROM " rel " q")] ps))
                                      {:builder-fn rs/as-unqualified-lower-maps})))
              dataset-rows (sort (map (juxt (keyword pk) :v)
                                      (jdbc/execute!
                                       c (let [{rel :sql ps :params} (pgds/as-of c "main" t v)]
                                         (into [(str "SELECT * FROM " rel " q")] ps))
                                       {:builder-fn rs/as-unqualified-lower-maps})))]
          (testing "such rows existed always (row 8, O2: coalesce 0)"
            (is (= [["a" "1"] ["b" "2"]] layout-rows dataset-rows))
            (is (pgds/had-rows-at? c "main" t v))))))))

(deftest no-history-partition-carries-an-overlap-constraint
  (let [ovl? (fn [s t] (boolean (some (fn [p] (some #(re-find #"_ovl$" %) (constraints-of s p)))
                                      (partitions-of s t))))
        t1   "x_by_conductor"
        t2   "x_by_trigger"]
    (datasets/-write! (D) {:branch "main" :dataset t1 :pk pk}
                      {:kind :update :pk pk :rows [{pk "a" "v" "1"}]})
    (datasets/-write! (D) {:branch "main" :dataset t1 :pk pk}
                      {:kind :update :pk pk :rows [{pk "a" "v" "2"}]})
    (with-open [c (jdbc/get-connection (tsys/ds))]
      (pgds/create-table! c "main" t2
                          [{:name pk :type "VARCHAR" :not-null? true} {:name "v" :type "text"}])
      (jdbc/execute! c [(str "INSERT INTO main." t2 " VALUES ('a', '1')")])
      (jdbc/execute! c [(str "UPDATE main." t2 " SET v = '2'")]))
    (datasets/-write! (D) {:branch "main" :dataset t2 :pk pk}
                      {:kind :update :pk pk :rows [{pk "a" "v" "3"}]})
    (obj-tx! (fn [c rev] (layout/layout! c "main" rev {:old {} :new sig :steps []})))
    (obj-write! "main" [["a" "1"]])
    (obj-write! "main" [["a" "2"]])
    (is (seq (partitions-of "main" t1)))
    (is (seq (partitions-of "main" t2)))
    (is (seq (partitions-of "main" (layout/object-table T))))
    (testing "row 15, O3: whoever makes the partition, the conductor or the trigger, adds no constraint"
      (is (false? (ovl? "main" t1)))
      (is (false? (ovl? "main" t2)))
      (is (false? (ovl? "main" (layout/object-table T)))))
    (testing "version-integrity is the check"
      (with-open [c (jdbc/get-connection (tsys/ds))]
        (is (= {:overlap 0 :gap 0 :orphan 0} (pgds/version-integrity c "main" t1)))
        (is (= {:overlap 0 :gap 0 :orphan 0} (pgds/version-integrity c "main" t2)))))))

(deftest an-orphan-tomb-history-does-not-stop-a-re-fork
  (ds-write! "main" [["a" "1"] ["b" "2"]])
  (ds-delete! "b1" ["a"])
  (with-open [c (jdbc/get-connection (tsys/ds))]
    (doseq [x [dataset (layout/history-of dataset) (layout/tomb-of dataset)]]
      (jdbc/execute! c [(str "DROP TABLE bo_b1." x " CASCADE")])))
  (is (true? (table-exists? "bo_b1" (layout/history-of (layout/tomb-of dataset)))) "the orphan")
  (ds-write! "b1" [["c" "3"]])
  (is (= [["a" "1"] ["b" "2"] ["c" "3"]] (ds-read "b1" nil))
      "the re-fork starts without the old tombs")
  (ds-delete! "b1" ["b"])
  (is (= [["a" "1"] ["c" "3"]] (ds-read "b1" nil))))

(deftest drop-unclaimed-leaves-dataset-tables
  (let [ds-t  (layout/object-table :ds_owned)
        gone  :gone
        sig2  (assoc-in sig [:types gone] {:attrs [{:name :v :type :string}]})]
    (obj-tx! (fn [c rev] (layout/layout! c "main" rev {:old {} :new sig2 :steps []})))
    (datasets/-write! (D) {:branch "main" :dataset ds-t :pk pk}
                      {:kind :update :pk pk :rows [{pk "a" "v" "1"}]})
    (is (= "sutra:datasets"
           (one-value "SELECT obj_description(to_regclass(?), 'pg_class') AS d"
                      [(str "main." ds-t)])))
    (with-open [c (jdbc/get-connection (tsys/ds))]
      (is (not-any? #(= ds-t (:name %)) (:orphans (layout/inspect c "main" sig)))
          "a dataset table under the object prefix is no store orphan"))
    (obj-tx! (fn [c rev]
               (layout/layout! c "main" rev {:old sig2 :new sig :steps [[:drop-unclaimed]]})))
    (is (false? (table-exists? "main" (layout/object-table gone)))
        "the store's undeclared table goes")
    (is (true? (table-exists? "main" ds-t)) "the dataset table stays")
    (is (= [["a" "1"]]
           (sort (map (juxt (keyword pk) :v)
                      (datasets/-read (D) {:branch "main" :dataset ds-t :pk pk} {})))))))

(deftest restore-store-to-leaves-object-tables-to-the-store
  (obj-tx! (fn [c rev] (layout/layout! c "main" rev {:old {} :new sig :steps []})))
  (ds-write! "main" [["a" "1"]])
  (obj-write! "main" [["a" "1"]])
  (let [point (ds-version)
        r2    (obj-write! "main" [["a" "2"]])
        _     (ds-write! "main" [["a" "2"]])
        moved (with-open [c (jdbc/get-connection (tsys/ds))]
                (pgds/restore-store-to! c point))
        obj-t (str "main." (layout/object-table T))]
    (is (contains? moved (str "main." dataset)) "the dataset table is restored, as it should be")
    (is (= [["a" "1"]] (ds-read "main" nil)))
    (testing "row 19/K14: objects restore by a store commit :restore, so d_ tables are sutra.store.core's"
      (is (not (contains? moved obj-t)))
      (is (= [["a" "2"]] (obj-read "main" nil)) "the object row stays as the store wrote it")
      (is (= r2 (long (one-value (str "SELECT sys_from FROM " obj-t) [])))
          "stamped with the revision of the store commit that wrote it"))))
