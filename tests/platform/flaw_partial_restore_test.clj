(ns ^:integration platform.flaw-partial-restore-test
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.dataset-store :as dstore]
            [support.pod :as pod]
            [sutra.types.coltype :as ctype]))

(def ^:private ^:dynamic *store* nil)

(defn- with-pg [f]
  (pod/with-dataset-store
   #(with-open [c (pgds/open (tsys/ds))]
      (binding [*store* (dstore/pg-store c)] (f)))))

(use-fixtures :once with-pg)

(defn- mark-dataset! [t]
  (with-open [stmt (.createStatement ^java.sql.Connection (:conn *store*))]
    (.execute stmt (str "COMMENT ON TABLE main." t " IS 'sutra:datasets'"))))

(defn- dataset-table! [t]
  (when (dstore/table-exists? *store* "main" t) (dstore/drop-table! *store* "main" t))
  (dstore/create-table! *store* "main" t
                        [{:name "internal_uuid" :type (ctype/col-type :varchar) :not-null? true}])
  (dstore/ensure-versioned! *store* "main" t)
  (mark-dataset! t))

(defn- put! [t ids]
  (dstore/merge-rows! *store* "main" t "internal_uuid" ["internal_uuid"]
                      (mapv (fn [i] {"internal_uuid" i}) ids) {}))

(defn- live [t] (set (map :internal_uuid (dstore/select-rows *store* "main" t {}))))

(deftest a-restore-point-brings-back-a-table-dropped-since
  (dataset-table! "d_keep")
  (dataset-table! "d_gone")
  (put! "d_keep" ["k1"])
  (put! "d_gone" ["g1" "g2"])
  (let [snap (:snapshot (dstore/backup! *store* "both tables"))]
    (testing "the store moves on: one table is dropped, the other written"
      (dstore/drop-table! *store* "main" "d_gone")
      (put! "d_keep" ["k2"])
      (is (not (dstore/table-exists? *store* "main" "d_gone"))))
    (testing "restoring to the point"
      (dstore/restore-store-to! *store* snap)
      (is (= #{"k1"} (live "d_keep")) "the surviving table is back at the point")
      (is (dstore/table-exists? *store* "main" "d_gone")
          "the dropped table of the restore point exists again")
      (when (dstore/table-exists? *store* "main" "d_gone")
        (is (= #{"g1" "g2"} (live "d_gone")) "with the rows it had at the point")))))
