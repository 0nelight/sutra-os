(ns ^:integration platform.pg-ledger
  (:require [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [sutra.store.control :as ctl]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.system-datasets :as sysds]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.builds.ledger :as ledger]
            [sutra.store.storage.command :as cmd]
            [sutra.store.world :as world]
            [support.pod :as pod]
            [support.store :as store]
            [sutra.store.control.tables :as cc]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private D  "ri.sutra.main.dataset.11111111-1111-1111-1111-111111111111")
(def ^:private D2 "ri.sutra.main.dataset.22222222-2222-2222-2222-222222222222")
(def ^:private T  "ri.sutra.main.transform.33333333-3333-3333-3333-333333333333")
(def ^:private T2 "ri.sutra.main.transform.44444444-4444-4444-4444-444444444444")

(defn- do!
  [effects-of & args]
  (let [fx (apply effects-of args)]
    (cmd/execute! (ctx/store) (if (map? fx) [fx] fx))))

(def ^:private colls
  {cc/ledger-txns-rid :ledger-txns cc/ledger-consumption-rid :ledger-consumption
   cc/ledger-jobs-rid :ledger-jobs})

(defn- clear!
  []
  (doseq [t (vals colls)] (ctl/delete! (ctx/store) t {})))

(defn- rows [rid] (ctl/list (ctx/store) (colls rid) {}))

(deftest a-transaction-chain-links-to-its-predecessor
  (clear!)
  (let [a (ledger/record-txn! (ctx/store) {:dataset-rid D :branch "main" :run-id "r1"
                                           :job-rid "j1" :snapshot-id "c1" :type :SNAPSHOT
                                           :status :COMMITTED :row-count 3 :row-delta 3
                                           :started-at "2026-08-01T10:00:00Z"
                                           :ended-at "2026-08-01T10:00:01Z"})
        b (ledger/record-txn! (ctx/store) {:dataset-rid D :branch "main" :run-id "r2"
                                           :job-rid "j2" :snapshot-id "c2" :type :APPEND
                                           :status :COMMITTED :row-count 5 :row-delta 2
                                           :started-at "2026-08-01T11:00:00Z"
                                           :ended-at "2026-08-01T11:00:01Z"})]
    (is (not= a b))
    (is (= b (:txn_rid (ledger/latest-txn (ctx/store) D "main"))))
    (let [[newest older] (ledger/list-transactions (ctx/store) D "main" 10)]
      (is (= b (:txn_rid newest)) "newest first")
      (is (= a (:prev_txn_rid newest)) "and chained to its predecessor")
      (is (= a (:txn_rid older)))
      (is (= "APPEND" (:type newest)) "a keyword type is stored as its name")
      (is (= 5 (:row_count newest)) "a :long column round-trips as a number"))))

(deftest transactions-are-per-branch-and-per-dataset
  (clear!)
  (ledger/record-txn! (ctx/store) {:dataset-rid D :branch "main" :snapshot-id "m"
                                   :type :APPEND :status :COMMITTED :started-at "t1"})
  (ledger/record-txn! (ctx/store) {:dataset-rid D :branch "feat" :snapshot-id "f"
                                   :type :APPEND :status :COMMITTED :started-at "t1"})
  (ledger/record-txn! (ctx/store) {:dataset-rid D2 :branch "main" :snapshot-id "o"
                                   :type :APPEND :status :COMMITTED :started-at "t1"})
  (is (= ["m"] (mapv :snapshot_id (ledger/list-transactions (ctx/store) D "main" 10))))
  (is (= ["f"] (mapv :snapshot_id (ledger/list-transactions (ctx/store) D "feat" 10))))
  (testing "and a chain does not pick up the other branch's tail"
    (is (nil? (:prev_txn_rid (first (ledger/list-transactions (ctx/store) D "feat" 10)))))))

(deftest the-limit-is-honoured
  (clear!)
  (dotimes [i 4]
    (ledger/record-txn! (ctx/store) {:dataset-rid D :branch "main" :snapshot-id (str i)
                                     :type :APPEND :status :COMMITTED
                                     :started-at (str "2026-08-01T0" i ":00:00Z")}))
  (is (= 4 (count (ledger/list-transactions (ctx/store) D "main" 10))))
  (is (= 2 (count (ledger/list-transactions (ctx/store) D "main" 2)))
      "LIMIT ? binds an integer, which MySQL and Postgres spell the same way"))

(deftest a-pointer-upserts-in-place
  (clear!)
  (ledger/advance-pointer! (ctx/store) {:consumer-rid T :input-dataset-rid D :branch "main"
                                        :last-txn-rid "x1" :last-snapshot-id "c1"
                                        :updated-at "t1"})
  (ledger/advance-pointer! (ctx/store) {:consumer-rid T :input-dataset-rid D :branch "main"
                                        :last-txn-rid "x2" :last-snapshot-id "c2"
                                        :updated-at "t2"})
  (is (= 1 (count (rows cc/ledger-consumption-rid))) "moved, not duplicated")
  (let [p (ledger/consumer-pointer (ctx/store) T D "main")]
    (is (= "x2" (:last_txn_rid p)))
    (is (= "c2" (:last_snapshot_id p)))))

(deftest two-branches-hold-their-own-pointer-for-the-same-edge
  (clear!)
  (ledger/advance-pointer! (ctx/store) {:consumer-rid T :input-dataset-rid D :branch "main"
                                        :last-txn-rid "on-main" :last-snapshot-id "cm"
                                        :updated-at "t1"})
  (ledger/advance-pointer! (ctx/store) {:consumer-rid T :input-dataset-rid D :branch "feat"
                                        :last-txn-rid "on-feat" :last-snapshot-id "cf"
                                        :updated-at "t1"})
  (is (= 2 (count (rows cc/ledger-consumption-rid))))
  (is (= "on-main" (:last_txn_rid (ledger/consumer-pointer (ctx/store) T D "main"))))
  (is (= "on-feat" (:last_txn_rid (ledger/consumer-pointer (ctx/store) T D "feat")))))

(deftest an-unread-edge-has-no-pointer
  (clear!)
  (is (nil? (ledger/consumer-pointer (ctx/store) T D "main"))))

(deftest resetting-a-consumer-clears-all-its-inputs-and-nobody-elses
  (clear!)
  (doseq [[t d] [[T D] [T D2] [T2 D]]]
    (ledger/advance-pointer! (ctx/store) {:consumer-rid t :input-dataset-rid d :branch "main"
                                          :last-txn-rid "x" :last-snapshot-id "c"
                                          :updated-at "t"}))
  (is (= 2 (ledger/reset-consumer! (ctx/store) T "main")))
  (is (empty? (ledger/consumption-of (ctx/store) T "main")))
  (is (= 1 (count (ledger/consumption-of (ctx/store) T2 "main")))))

(deftest a-job-flips-one-row-through-its-whole-lifecycle
  (clear!)
  (do! ledger/seed-jobs {:run-id "r1" :transform-rids [T T2] :branch "main"})
  (is (= #{"WAITING"} (set (map :status (ledger/jobs-of-run (ctx/store) "r1")))))
  (let [jr (ledger/job-rid "r1" T)]
    (do! ledger/open-job {:job-rid jr :run-id "r1" :transform-rid T :branch "main"
                          :attempt 1 :at "2026-08-01T10:00:00Z"})
    (is (= 2 (count (ledger/jobs-of-run (ctx/store) "r1"))) "opened in place, not appended")
    (do! ledger/finish-job {:job-rid jr :outputs [D] :snapshot-id "c1"
                            :rows-in 10 :rows-out 4 :code-sha "sha"
                            :at "2026-08-01T10:00:05Z"})
    (let [j (first (filter #(= jr (:job_rid %)) (ledger/jobs-of-run (ctx/store) "r1")))]
      (is (= "COMPLETED" (:status j)))
      (is (= 4 (:rows_out j)))
      (is (= "sha" (:code_sha j)))
      (is (= (pr-str [D]) (:outputs j)))))
  (testing "and the roster's untouched job is DID_NOT_RUN at build end, not the run one"
    (do! ledger/skip-unstarted-jobs "r1" "2026-08-01T10:01:00Z")
    (is (= #{"COMPLETED" "DID_NOT_RUN"}
           (set (map :status (ledger/jobs-of-run (ctx/store) "r1")))))))

(deftest re-seeding-never-regresses-a-job
  (clear!)
  (let [jr (ledger/job-rid "r2" T)]
    (do! ledger/seed-jobs {:run-id "r2" :transform-rids [T] :branch "main"})
    (do! ledger/open-job {:job-rid jr :run-id "r2" :transform-rid T :branch "main"
                          :attempt 1 :at "t1"})
    (do! ledger/finish-job {:job-rid jr :outputs [] :snapshot-id "c" :at "t2"})
    (do! ledger/seed-jobs {:run-id "r2" :transform-rids [T] :branch "main"})
    (is (= ["COMPLETED"] (mapv :status (ledger/jobs-of-run (ctx/store) "r2"))))
    (is (= 1 (count (ledger/jobs-of-run (ctx/store) "r2"))))))

(deftest a-retry-reopens-the-same-row-and-clears-the-error
  (clear!)
  (let [jr (ledger/job-rid "r3" T)]
    (do! ledger/open-job {:job-rid jr :run-id "r3" :transform-rid T :branch "main"
                          :attempt 1 :at "t1"})
    (do! ledger/fail-job {:job-rid jr :error "boom" :at "t2"})
    (is (= "boom" (:error (first (ledger/jobs-of-run (ctx/store) "r3")))))
    (do! ledger/open-job {:job-rid jr :run-id "r3" :transform-rid T :branch "main"
                          :attempt 2 :at "t3"})
    (let [j (first (ledger/jobs-of-run (ctx/store) "r3"))]
      (is (= "RUNNING" (:status j)))
      (is (= 2 (:attempt j)))
      (is (nil? (:error j)) "the previous attempt's error does not survive the retry"))
    (is (= 1 (count (ledger/jobs-of-run (ctx/store) "r3"))))))

(deftest a-failed-job-is-not-overwritten-by-a-later-completed
  (clear!)
  (let [jr (ledger/job-rid "r-term" T)]
    (do! ledger/open-job {:job-rid jr :run-id "r-term" :transform-rid T :branch "main"
                          :attempt 1 :at "t1"})
    (do! ledger/fail-job {:job-rid jr :error "boom" :at "t2"})
    (do! ledger/finish-job {:job-rid jr :run-id "r-term" :transform-rid T
                            :branch "main" :rows-out 3 :outputs [D] :snapshot-id "c" :at "t3"})
    (do! ledger/finish-job {:job-rid jr :outputs [D] :snapshot-id "c" :at "t4"})
    (let [j (first (ledger/jobs-of-run (ctx/store) "r-term"))]
      (is (= "FAILED" (:status j)))
      (is (= "boom" (:error j))))))

(deftest a-terminal-job-upserts-in-one-statement
  (clear!)
  (let [jr (ledger/job-rid "r4" T)]
    (do! ledger/finish-job {:job-rid jr :run-id "r4" :transform-rid T
                            :branch "main" :rows-in 0 :rows-out 0
                            :outputs [] :snapshot-id "c" :at "t1"})
    (do! ledger/finish-job {:job-rid jr :run-id "r4" :transform-rid T
                            :branch "main" :rows-in 7 :rows-out 3
                            :outputs [D] :snapshot-id "c2" :at "t2"})
    (is (= 1 (count (ledger/jobs-of-run (ctx/store) "r4"))))
    (let [j (first (ledger/jobs-of-run (ctx/store) "r4"))]
      (is (= "COMPLETED" (:status j)))
      (is (= 3 (:rows_out j)) "a rerun overwrites in place"))))

(deftest the-execution-log-survives-the-coarse-writer-and-dies-with-a-retry
  (clear!)
  (let [jr (ledger/job-rid "r-log" T)]
    (do! ledger/open-job {:job-rid jr :run-id "r-log" :transform-rid T :branch "main"
                          :attempt 1 :at "t1"})
    (do! ledger/finish-job {:job-rid jr :outputs [D] :snapshot-id "c1"
                            :rows-out 4 :job-log "skipped 40 rows: no PDF"
                            :at "t2"})
    (is (= "skipped 40 rows: no PDF" (:job_log (first (ledger/jobs-of-run (ctx/store) "r-log")))))
    (testing "the coarse terminal writer, which has no text, does not erase it"
      (do! ledger/finish-job {:job-rid jr :run-id "r-log" :transform-rid T
                              :branch "main" :rows-in 7 :rows-out 3
                              :outputs [D] :snapshot-id "c2" :at "t3"})
      (is (= "skipped 40 rows: no PDF"
             (:job_log (first (ledger/jobs-of-run (ctx/store) "r-log"))))))
    (testing "…and a FAILED that carries no log keeps it too — the log is what the
              body had done before it threw"
      (do! ledger/fail-job {:job-rid jr :error "boom" :at "t4"})
      (let [j (first (ledger/jobs-of-run (ctx/store) "r-log"))]
        (is (= "boom" (:error j)))
        (is (= "skipped 40 rows: no PDF" (:job_log j)))))
    (testing "a RETRY clears it, like the error — the row is the CURRENT attempt"
      (do! ledger/open-job {:job-rid jr :run-id "r-log" :transform-rid T :branch "main"
                            :attempt 2 :at "t5"})
      (is (nil? (:job_log (first (ledger/jobs-of-run (ctx/store) "r-log"))))))))

(deftest a-runaway-log-is-truncated-not-stored-whole
  (let [huge (apply str (repeat (* 3 ledger/max-job-log-chars) "x"))
        got  (ledger/clamp-log huge)]
    (is (< (count got) (+ ledger/max-job-log-chars 100)))
    (is (str/includes? got "log truncated"))
    (is (str/starts-with? got "xxx") "the HEAD is kept — a body says what it decided first"))
  (is (nil? (ledger/clamp-log "   ")) "blank is absent, not an empty log")
  (is (nil? (ledger/clamp-log nil))))

(deftest one-statement-answers-the-newest-job-of-every-transform
  (clear!)
  (doseq [[run t status at] [["r1" T  "SUCCEEDED" "2026-08-01T10:00:00Z"]
                             ["r2" T  "FAILED"    "2026-08-02T10:00:00Z"]
                             ["r1" T2 "SUCCEEDED" "2026-08-03T10:00:00Z"]]]
    (let [jr (ledger/job-rid run t)]
      (do! ledger/open-job {:job-rid jr :run-id run :transform-rid t :branch "main"
                            :attempt 1 :at at})
      (if (= "FAILED" status)
        (do! ledger/fail-job {:job-rid jr :error "boom" :at at})
        (do! ledger/finish-job {:job-rid jr :outputs [D] :snapshot-id "c"
                                :rows-out 9 :at at}))))
  (let [jr (ledger/job-rid "r9" T)]
    (do! ledger/open-job {:job-rid jr :run-id "r9" :transform-rid T :branch "feature"
                          :attempt 1 :at "2026-08-09T10:00:00Z"})
    (do! ledger/finish-job {:job-rid jr :outputs [] :snapshot-id "c"
                            :at "2026-08-09T10:00:00Z"}))
  (let [got (ledger/last-job-per-transform (ctx/store) "main")]
    (is (= 2 (count got)) "one row per transform, not one per run")
    (is (= "FAILED" (:status (get got T))) "the NEWEST job wins, not the first or the green one")
    (is (= "COMPLETED" (:status (get got T2))))
    (is (= "boom" (:error (get got T)))))
  (testing "the other branch answers with its own"
    (is (= "COMPLETED" (:status (get (ledger/last-job-per-transform (ctx/store) "feature") T)))))
  (testing "a branch that has never built answers empty, not with someone else's"
    (is (empty? (ledger/last-job-per-transform (ctx/store) "nobody")))))

(deftest the-last-fingerprint-is-the-newest-a-completed-job-carries
  (clear!)
  (doseq [[run status at fp] [["r1" "COMPLETED" "2026-08-01T10:00:00Z" "old"]
                              ["r2" "COMPLETED" "2026-08-02T10:00:00Z" "new"]
                              ["r3" "COMPLETED" "2026-08-03T10:00:00Z" nil]
                              ["r4" "FAILED"    "2026-08-04T10:00:00Z" nil]]]
    (let [jr (ledger/job-rid run T)]
      (do! ledger/open-job {:job-rid jr :run-id run :transform-rid T :branch "main"
                            :attempt 1 :at at})
      (if (= "FAILED" status)
        (do! ledger/fail-job {:job-rid jr :error "boom" :at at})
        (do! ledger/finish-job {:job-rid jr :outputs [D] :snapshot-id "c" :rows-out 9 :at at}))
      (when fp
        (cmd/execute! (ctx/store) (ledger/stamp-fingerprints run {jr fp})))))
  (let [got (get (ledger/last-fingerprints (ctx/store) "main") T)]
    (is (= "new" (:fingerprint got)) "a newer job without one does not hide it")
    (is (= "r2" (str (:run_id got)))))
  (testing "a branch that never built has none"
    (is (empty? (ledger/last-fingerprints (ctx/store) "nobody")))))

(deftest one-statement-answers-the-newest-transaction-of-every-dataset-with-its-age
  (clear!)
  (doseq [[ds n at] [[D  10 "2026-01-04T08:00:00Z"]
                     [D  25 "2026-05-04T08:00:00Z"]
                     [D  40 "2026-08-04T08:00:00Z"]
                     [D2  7 "2026-03-04T08:00:00Z"]]]
    (ledger/record-txn! (ctx/store) {:dataset-rid ds :branch "main" :run-id "r" :job-rid "j"
                                     :snapshot-id "c" :type :APPEND :status :COMMITTED
                                     :row-count n :row-delta 1
                                     :started-at at :ended-at at}))
  (let [got (ledger/last-txn-per-dataset (ctx/store) "main")]
    (is (= 2 (count got)))
    (is (= 40 (:row_count (get got D))) "the newest transaction's absolute count")
    (is (= "2026-01-04T08:00:00Z" (str (:created (get got D))))
        "…and the FIRST one's timestamp, which is the dataset's age")
    (is (= 3 (long (:txns (get got D)))))
    (is (= 7 (:row_count (get got D2))))
    (is (= 1 (long (:txns (get got D2)))))))

(defmacro ^:private without-the-data-store [& body]
  `(with-redefs [dsets/table-count     (constantly 42)
                 dsets/current-version (constantly 77)]
     ~@body))

(deftest a-build-records-one-transaction-per-write-and-advances-every-pointer
  (clear!)
  (without-the-data-store
   (ledger/commit-transactions!
    (ctx/store) {:branch "main" :run-id "r1" :schema-commit "s1"
                 :writes [{:dataset-rid D :job-rid "j1" :incremental? true :written 2}
                          {:dataset-rid D2 :job-rid "j1" :incremental? true :written 1}]
                 :consumed [{:consumer-rid T :input-dataset-rid D :last-snapshot-id "in1"}]
                 :at "2026-08-01T12:00:00Z"}))
  (is (= 2 (count (rows cc/ledger-txns-rid))))
  (is (= #{"APPEND"} (set (map :type (rows cc/ledger-txns-rid)))))
  (is (= 42 (:row_count (first (ledger/list-transactions (ctx/store) D "main" 1))))
      "the row count is read from the DATA store, before the ledger transaction")
  (let [p (ledger/consumer-pointer (ctx/store) T D "main")]
    (is (= "in1" (:last_snapshot_id p))
        "advanced to the exact input commit read, not to the merge commit")))

(deftest an-edge-with-no-cursor-advances-to-the-build-commit
  (clear!)
  (without-the-data-store
   (ledger/commit-transactions!
    (ctx/store) {:branch "main" :run-id "r1"
                 :writes [{:dataset-rid D :job-rid "j1" :incremental? true :written 1}]
                 :consumed [{:consumer-rid T :input-dataset-rid D}]
                 :at "t"}))
  (is (= "77" (:last_snapshot_id (ledger/consumer-pointer (ctx/store) T D "main")))
      "no cursor ⇒ this build's own version token, read off the bulk store"))

(deftest a-snapshot-leaves-its-consumers-cursors-standing
  (clear!)
  (ledger/advance-pointer! (ctx/store) {:consumer-rid T2 :input-dataset-rid D :branch "main"
                                        :last-txn-rid "old" :last-snapshot-id "old"
                                        :updated-at "t0"})
  (ledger/advance-pointer! (ctx/store) {:consumer-rid T2 :input-dataset-rid D :branch "feat"
                                        :last-txn-rid "old" :last-snapshot-id "old"
                                        :updated-at "t0"})
  (without-the-data-store
   (ledger/commit-transactions!
    (ctx/store) {:branch "main" :run-id "r1"
                 :writes [{:dataset-rid D :job-rid "j1" :incremental? false :written 9}]
                 :consumed [] :at "t"}))
  (is (= "SNAPSHOT" (:type (first (ledger/list-transactions (ctx/store) D "main" 1)))))
  (is (some? (ledger/consumer-pointer (ctx/store) T2 D "main"))
      "the delta from it says the input was replaced; a dropped one reads as a first run")
  (is (some? (ledger/consumer-pointer (ctx/store) T2 D "feat")) "untouched on the other branch"))

(deftest the-commit-is-one-transaction
  (clear!)
  (let [real @#'ledger/pointer-row
        n    (atom 0)]
    (with-redefs-fn {#'ledger/pointer-row
                     (fn [m]
                       (if (= 2 (swap! n inc))
                         (throw (ex-info "injected: the second pointer fails" {}))
                         (real m)))}
      (fn []
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo #"injected"
             (without-the-data-store
              (ledger/commit-transactions!
               (ctx/store) {:branch "main" :run-id "r1"
                            :writes [{:dataset-rid D :job-rid "j1" :incremental? true :written 1}]
                            :consumed [{:consumer-rid T :input-dataset-rid D :last-snapshot-id "i1"}
                                       {:consumer-rid T2 :input-dataset-rid D :last-snapshot-id "i2"}]
                            :at "t"})))))))
  (is (empty? (rows cc/ledger-txns-rid))
      "the transaction rows rolled back with the pointer that failed")
  (is (empty? (rows cc/ledger-consumption-rid))
      "…and so did the pointer that had already succeeded"))

(deftest dropping-a-branch-takes-its-ledger-rows-with-it
  (clear!)
  (let [doomed (store/fresh-branch! "doomed")]
    (doseq [b ["main" doomed]]
      (ledger/record-txn! (ctx/store) {:dataset-rid D :branch b :snapshot-id "c"
                                       :type :APPEND :status :COMMITTED :started-at "t"})
      (ledger/advance-pointer! (ctx/store) {:consumer-rid T :input-dataset-rid D :branch b
                                            :last-txn-rid "x" :last-snapshot-id "c"
                                            :updated-at "t"})
      (do! ledger/seed-jobs {:run-id (str "run-" b) :branch b :transform-rids [T]}))
    (store/retire! doomed)
    (is (empty? (ledger/list-transactions (ctx/store) D doomed 10)))
    (is (nil? (ledger/consumer-pointer (ctx/store) T D doomed)))
    (is (empty? (ledger/jobs-of-run (ctx/store) (str "run-" doomed))))
    (testing "and the trunk kept every one of its own"
      (is (= 1 (count (ledger/list-transactions (ctx/store) D "main" 10))))
      (is (some? (ledger/consumer-pointer (ctx/store) T D "main")))
      (is (= 1 (count (ledger/jobs-of-run (ctx/store) "run-main")))))))

(deftest the-catalog-tables-are-declared-control-resident
  (is (= #{:dataset_transactions :dataset_consumption :jobs :object_edits :link_edits}
         (into #{} (comp (filter #(= :control (:store (val %))))
                         (map (comp :dataset-name val)))
               sysds/system-datasets))))

(def ^:private EW
  (ri/derived {:sigma clock/system}
              {:object-types {:item {:object-type-name :item
                                     :attribute-list [{:attribute-name :name :attribute-type :string}
                                                      {:attribute-name :colour :attribute-type :string}
                                                      {:attribute-name :size :attribute-type :string}]
                                     :plural "Items"}}
               :link-types   {:rel {:link-type-name :rel :from-object-type :item :to-object-type :item
                                    :cardinality :many-to-many}}}))

(def ^:private OT (lookup/rid-of EW :object-types :item))
(def ^:private LT (lookup/rid-of EW :link-types :rel))

(defonce ^:private installed (atom false))

(defn- clear-overlay!
  []
  (when (compare-and-set! installed false true) (store/install! "main" EW))
  (world/forget-edits! (world/at (ctx/store) "main") [OT LT]))

(defn- edit! [branch pk kind values]
  (if (= :create kind)
    (store/insert-object! branch OT {:id pk :values values})
    (store/update-object! branch OT pk values)))

(defn- provenance [branch] (world/edited (ctx/store branch) OT))

(defn- uid [s] (str s "-" (subs (str (random-uuid)) 0 8)))

(deftest a-branch-inherits-the-trunks-edits-per-key
  (clear-overlay!)
  (let [a (uid "row-a") b (uid "row-b")]
    (edit! "main" a :create {:name "trunk-a"})
    (edit! "main" b :create {:name "trunk-b"})
    (let [feat (store/fresh-branch! "feat")]
      (is (= #{a b} (set (keys (provenance feat))))
          "a branch that has edited nothing still sees the trunk's edits"))))

(deftest a-branchs-own-edit-wins-without-overwriting-the-trunks
  (clear-overlay!)
  (let [a (uid "row-a")]
    (edit! "main" a :create {:name "trunk-value"})
    (let [feat  (store/fresh-branch! "feat")
          trunk (get (provenance "main") a)]
      (edit! feat a :update {:name "branch-value"})
      (is (= 1 (count (provenance feat))) "the reader resolves the two to one")
      (is (not= (:rev trunk) (:rev (get (provenance feat) a))) "the branch's own edit")
      (is (= trunk (get (provenance "main") a)) "and the trunk still reads its own, unchanged")
      (is (= "branch-value" (get-in (store/object feat OT a) [:values :name])))
      (is (= "trunk-value" (get-in (store/object "main" OT a) [:values :name]))))))

(deftest an-update-on-a-branch-merges-onto-the-inherited-overlay
  (clear-overlay!)
  (let [a (uid "row-a")]
    (edit! "main" a :create {:colour "red" :size "L"})
    (let [feat (store/fresh-branch! "feat")]
      (edit! feat a :update {:size "XL"})
      (is (= #{:colour :size} (:attrs (get (provenance feat) a)))
          "the trunk's edited column came along")
      (is (= {:colour "red" :size "XL"}
             (select-keys (:values (store/object feat OT a)) [:colour :size]))
          "and the branch's own value overrides it"))))

(deftest the-trunk-does-not-inherit-a-branchs-edits
  (clear-overlay!)
  (let [feat (store/fresh-branch! "feat")
        z    (uid "row-z")]
    (edit! feat z :create {:name "branch-only"})
    (is (empty? (provenance "main")))
    (is (= [z] (vec (keys (provenance feat)))))))

(defn- link-edit-ids
  [b kind]
  (into #{} (keep (fn [[id {:keys [deleted?]}]]
                    (when (= deleted? (= "disconnect" kind)) id)))
        (world/edited (ctx/store b) LT)))

(deftest a-branch-disconnect-beats-the-trunks-connect
  (clear-overlay!)
  (let [a (uid "row-a") b (uid "row-b") e (uid "edge")]
    (edit! "main" a :create {:name "a"})
    (edit! "main" b :create {:name "b"})
    (store/insert-link! "main" LT {:id e :from-id a :to-id b})
    (let [feat (store/fresh-branch! "feat")]
      (store/commit-ops! feat [(store/delete-link LT e)])
      (is (empty? (link-edit-ids feat "connect"))
          "the branch's disconnect removed the inherited connect")
      (is (= #{e} (link-edit-ids feat "disconnect")))
      (is (= #{e} (link-edit-ids "main" "connect"))
          "…and the trunk still connects it"))))

(deftest forgetting-edits-on-a-branch-forgets-them-there-only
  (clear-overlay!)
  (let [a (uid "row-a") b (uid "row-b")]
    (edit! "main" a :create {:name "t"})
    (let [feat (store/fresh-branch! "feat")]
      (edit! feat b :create {:name "b"})
      (is (= #{a b} (set (keys (provenance feat)))))
      (world/forget-edits! (ctx/store feat) [OT])
      (is (empty? (provenance feat))
          "the branch keeps the type itself from now on, without any edits")
      (is (= #{a} (set (keys (provenance "main")))) "the trunk keeps its own")
      (edit! "main" b :create {:name "later"})
      (is (empty? (provenance feat)) "and the trunk's later edits stay off the branch"))))

(deftest dropping-a-branch-takes-its-overlay-rows-too
  (clear-overlay!)
  (let [a      (uid "row-a")
        doomed (uid "doomed")]
    (edit! "main" a :create {:name "t"})
    (store/fork! doomed)
    (edit! doomed a :update {:name "d"})
    (store/retire! doomed)
    (store/fork! doomed)
    (is (= (get (provenance "main") a) (get (provenance doomed) a))
        "the reborn branch reads the trunk's edits, not the dead branch's")
    (is (= "t" (get-in (store/object doomed OT a) [:values :name])))))
