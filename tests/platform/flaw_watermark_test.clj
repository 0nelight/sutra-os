(ns ^:integration platform.flaw-watermark-test
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [sutra.store.pg.dataset :as pgds]
            [support.pod :as pod])
  (:import (java.sql Connection)))

(defn- with-pg [f]
  (pod/with-dataset-store #(with-open [_ (pgds/open (tsys/ds))] (f))))

(use-fixtures :once with-pg)

(defn- watermark [] (with-open [c (pgds/open (tsys/ds))] (long (pgds/current-snapshot c))))

(deftest skipped-version
  (with-open [^Connection slow (pgds/open (tsys/ds))]
    (.setAutoCommit slow false)
    (let [v1 (long (pgds/next-snapshot! slow))
          v2 (with-open [fast (pgds/open (tsys/ds))] (long (pgds/next-snapshot! fast)))]
      (is (< v1 v2) "the slow writer drew its version first")
      (testing "the fast writer committed; the slow one is still open"
        (is (< (watermark) v1) "the watermark stays below the open version"))
      (.commit slow)
      (testing "once the slow writer is through, the watermark moves past both"
        (is (<= v2 (watermark)))))))

(deftest a-failed-watermark-keeps-no-lock
  (with-open [^Connection c (pgds/open (tsys/ds))]
    (.setAutoCommit c false)
    (jdbc/execute! c ["ALTER SEQUENCE public.dataset_seq RENAME TO dataset_seq_weg"])
    (is (thrown? Exception (pgds/current-snapshot c)) "the watermark fails holding its lock")
    (.rollback c)
    (.setAutoCommit c true)
    (is (empty? (jdbc/execute!
                 c [(str "SELECT 1 FROM pg_locks WHERE pid = pg_backend_pid() "
                         "AND locktype = 'advisory' AND mode = 'ShareLock'")]))
        "a ROLLBACK keeps a session's lock, so the function must have let it go")
    (is (pos? (long (pgds/next-snapshot! c))) "the next version is not held up")))
