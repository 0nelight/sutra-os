(ns ^:integration platform.pg-records-clock-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [sutra.builds.ledger :as ledger]
            [sutra.store.storage.pg-store :as pg-store]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.store.control.tables :as cc]
            [sutra.store.pg.records :as pg-records]
            [sutra.store.storage.command :as cmd]
            [sutra.types.naming :as naming]
            [support.pod :as pod]
            [support.system :as tsys]))

(use-fixtures :once pod/with-stores-only)

(defn- do! [effects-of & args]
  (let [fx (apply effects-of args)]
    (cmd/execute! (ctx/store) (if (map? fx) [fx] fx))))

(def ^:private jobs-table (str "\"" (naming/dataset-table cc/ledger-jobs-rid) "\""))

(deftest the-storage-counts-every-write-to-the-ledger
  (let [b   (str "clock-" (random-uuid))
        t   "ri.sutra.main.transform.00000000-0000-0000-0000-0000000c10c0"
        jr  (ledger/job-rid "r1" t)
        clk #(ctl/written (ctx/store) :ledger-jobs)]
    (do! ledger/open-job {:job-rid jr :run-id "r1" :transform-rid t :branch b :attempt 1
                          :at "2026-11-03T10:00:00Z"})
    (let [c1  (clk)
          seen (ledger/last-jobs-per-transform (ctx/store) b)]
      (is (number? c1) "pg keeps the ledger in a table it counts")
      (is (= c1 (clk)) "asked again, the same")
      (is (= "RUNNING" (get-in seen [:any t :status])))
      (testing "a write through the store moves it"
        (do! ledger/finish-job {:job-rid jr :outputs [] :snapshot-id "c" :rows-out 3
                                :at "2026-11-03T10:05:00Z"})
        (let [c2 (clk)]
          (is (not= c1 c2))
          (is (= "COMPLETED" (get-in (ledger/last-jobs-per-transform (ctx/store) b)
                                     [:any t :status])))
          (testing "and so does one that never passes the store's commit"
            (with-open [c (jdbc/get-connection (tsys/ds))]
              (jdbc/execute! c [(str "UPDATE " jobs-table " SET rows_out = 7"
                                     " WHERE branch = ?") b]))
            (is (not= c2 (clk)) "the trigger counted the bare UPDATE")
            (is (= 7 (get-in (ledger/last-jobs-per-transform (ctx/store) b)
                             [:any t :rows-out]))
                "the kept answer is not the one from before the write")))))))

(deftest a-table-no-trigger-counts-is-refused-not-read
  (let [clk #(ctl/written (ctx/store) :ledger-jobs)]
    (is (number? (clk)))
    (with-open [c (jdbc/get-connection (tsys/ds))]
      (jdbc/execute! c [(str "DROP TRIGGER sutra_records_clock ON " jobs-table)]))
    (try
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not counted" (clk))
          "a clock nobody moves is refused, never answered")
      (finally
        (pg-records/count-writes! (tsys/ds) (select-keys pg-store/records
                                                         [:ctl/ledger-jobs]))))
    (is (number? (clk)) "laid out again, counted again")))
