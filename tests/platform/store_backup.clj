(ns ^:integration platform.store-backup
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.dataset-store :as dstore]
            [support.mem :as mem]
            [support.pod :as pod]
            [sutra.types.coltype :as ctype]))

(def ^:private ^:dynamic *store* nil)

(defn- with-store [f]
  (with-open [c (pgds/open (tsys/ds))]
    (binding [*store* (dstore/pg-store c)] (f))))

(use-fixtures :once pod/with-dataset-store with-store)

(defn- put! [rows]
  (dstore/merge-rows! *store* "main" "d_x" "internal_uuid" ["internal_uuid" "betrag"] rows {}))

(defn- live [] (set (map :internal_uuid (dstore/select-rows *store* "main" "d_x" {}))))

(defn- mark-dataset! [t]
  (with-open [stmt (.createStatement ^java.sql.Connection (:conn *store*))]
    (.execute stmt (str "COMMENT ON TABLE main." t " IS 'sutra:datasets'"))))

(defn- fresh-table! []
  (when (dstore/table-exists? *store* "main" "d_x") (dstore/drop-table! *store* "main" "d_x"))
  (dstore/create-table! *store* "main" "d_x"
                    [{:name "internal_uuid" :type (ctype/col-type :varchar) :not-null? true}
                     {:name "betrag" :type (ctype/col-type :bigint)}])
  (dstore/ensure-versioned! *store* "main" "d_x")
  (mark-dataset! "d_x"))

(deftest a-restore-point-is-a-number-and-the-way-back-is-a-write
  (fresh-table!)
  (put! [{"internal_uuid" "a" "betrag" 1} {"internal_uuid" "b" "betrag" 2}])
  (let [{:keys [snapshot] :as point} (dstore/backup! *store* "before")]
    (is (pos? snapshot) "the restore point carries the store's snapshot, not a copy")
    (is (= "before" (:label point)))

    (testing "the store moves on"
      (put! [{"internal_uuid" "c" "betrag" 3}])
      (dstore/delete-pks! *store* "main" "d_x" "internal_uuid" ["a"])
      (is (= #{"b" "c"} (live))))

    (testing "and the whole store goes back to the number, with nothing stopped"
      (dstore/restore-store-to! *store* snapshot)
      (is (= #{"a" "b"} (live))))

    (testing "the rollback is itself a version, so it is its own inverse"
      (let [after (dstore/backup! *store* "after-the-rollback")]
        (put! [{"internal_uuid" "d" "betrag" 4}])
        (is (= #{"a" "b" "d"} (live)))
        (dstore/restore-store-to! *store* (:snapshot after))
        (is (= #{"a" "b"} (live)) "back to the state the rollback itself produced")))

    (testing "the points are listed newest first"
      (is (= ["after-the-rollback" "before"] (mapv :label (dstore/backups *store*)))))))

(deftest a-rollback-moves-every-versioned-table-on-one-version
  (fresh-table!)
  (dstore/create-table! *store* "main" "d_y"
                    [{:name "internal_uuid" :type (ctype/col-type :varchar) :not-null? true}])
  (dstore/ensure-versioned! *store* "main" "d_y")
  (mark-dataset! "d_y")
  (put! [{"internal_uuid" "a" "betrag" 1}])
  (dstore/merge-rows! *store* "main" "d_y" "internal_uuid" ["internal_uuid"]
                  [{"internal_uuid" "y1"}] {})
  (let [snap (:snapshot (dstore/backup! *store* "two tables"))]
    (put! [{"internal_uuid" "b" "betrag" 2}])
    (dstore/merge-rows! *store* "main" "d_y" "internal_uuid" ["internal_uuid"]
                    [{"internal_uuid" "y2"}] {})
    (let [moved (dstore/restore-store-to! *store* snap)]
      (is (= 1 (dstore/count-rows *store* "main" "d_x")))
      (is (= 1 (dstore/count-rows *store* "main" "d_y")))
      (testing "the result names every relation it moved"
        (is (contains? moved "main.d_x"))
        (is (contains? moved "main.d_y")))))
  (testing "history relations are not themselves rolled back"
    (is (not-any? #(pgds/history-name? (second %)) (pgds/versioned-tables (:conn *store*))))))

(deftest a-store-without-history-says-so-rather-than-pretending
  (let [m (mem/mem-dataset)]
    (is (thrown? clojure.lang.ExceptionInfo (dstore/backup! m "x")))
    (is (thrown? clojure.lang.ExceptionInfo (dstore/backups m)))
    (is (thrown? clojure.lang.ExceptionInfo (dstore/restore-store-to! m 1)))
    (is (not (contains? (dstore/capabilities m) :backup))))
  (is (contains? (dstore/capabilities *store*) :backup)))
