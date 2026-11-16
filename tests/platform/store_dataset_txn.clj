(ns ^:integration platform.store-dataset-txn
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [sutra.store.access.datasets :as dsets]
            [support.pod :as pod]
            [sutra.store.access.scope :as ctx]
            [sutra.types.naming :as naming]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.builds.import.phases :as phases]
            [sutra.builds.import.changelog :as changelog]
            [sutra.builds.import.reconcile :as reconcile]
            [sutra.catalog.backing :as backing]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.pg.sql :as sql]
            [sutra.foundation.sql-trace :as sqltrace])
  (:import [java.io StringReader]
           [org.postgresql PGConnection]))

(use-fixtures :once pod/with-dataset-store)
(use-fixtures :each (fn [f] (pod/reset-dataset-store!) (f)))

(defn- row [id vals] {:id id :values vals})

(defn- current []
  (with-open [c (pgds/open (tsys/ds))] (pgds/current-snapshot c)))

(defn- table-rows
  ([ds] (table-rows "main" ds))
  ([s ds]
   (with-open [c (pgds/open (tsys/ds))]
     (jdbc/execute! c [(str "SELECT * FROM " (sql/dq s) "."
                            (sql/dq (naming/dataset-table ds))
                            " ORDER BY " (sql/dq naming/pk-col))]
                    {:builder-fn rs/as-unqualified-lower-maps}))))

(defn- pks
  ([ds] (pks "main" ds))
  ([s ds] (mapv #(get % (keyword naming/pk-col)) (table-rows s ds))))

(deftest snapshot-replaces-the-view
  (let [d :txn-snapshot]
    (dsets/write-rows! (ctx/system-store) d :snapshot
                        [(row "a" {:v 1}) (row "b" {:v 2}) (row "c" {:v 3})])
    (is (= ["a" "b" "c"] (pks d)))

    (testing "a second SNAPSHOT is the producer's whole current set — vanished keys go"
      (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "d" {:v 4})])
      (is (= ["a" "d"] (pks d))))))

(deftest append-is-an-upsert-not-an-insert
  (let [d :txn-append]
    (dsets/write-rows! (ctx/system-store) d :append [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/write-rows! (ctx/system-store) d :append [(row "a" {:v 99}) (row "c" {:v 3})])
    (is (= ["a" "b" "c"] (pks d)) "a re-emitted key updates in place")
    (is (= [99] (map :v (filter #(= "a" (get % (keyword naming/pk-col))) (table-rows d))))
        "and carries the new value")))

(deftest append-does-not-sweep
  (testing "an incremental delta MERGES into the prior set; it is not the whole view"
    (let [d :txn-append-keeps]
      (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
      (dsets/write-rows! (ctx/system-store) d :append [(row "c" {:v 3})])
      (is (= ["a" "b" "c"] (pks d))))))

(deftest delete-removes-named-keys
  (let [d :txn-delete]
    (dsets/write-rows! (ctx/system-store) d :snapshot
                        [(row "a" {:v 1}) (row "b" {:v 2}) (row "c" {:v 3})])
    (dsets/delete-rows! (ctx/system-store) d ["b"])
    (is (= ["a" "c"] (pks d)))
    (testing "re-deleting an absent key is a no-op, not an error"
      (dsets/delete-rows! (ctx/system-store) d ["b"])
      (is (= ["a" "c"] (pks d))))))

(deftest duplicate-pks-in-one-batch-are-last-wins
  (let [d :txn-dupes]
    (dsets/write-rows! (ctx/system-store) d :snapshot
                        [(row "a" {:v 1}) (row "a" {:v 2}) (row "a" {:v 3})])
    (is (= ["a"] (pks d)))
    (is (= [3] (map :v (table-rows d))))))

(deftest a-column-that-is-nil-in-every-row-still-gets-a-column
  (let [d :txn-all-nil]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:name "A" :maybe nil})
                                                (row "b" {:name "B" :maybe nil})])
    (is (= ["a" "b"] (pks d)))
    (testing "and a later real value lands in it"
      (dsets/write-rows! (ctx/system-store) d :append [(row "a" {:maybe "now set"})])
      (is (= #{"now set" nil} (set (map :maybe (table-rows d))))))))

(deftest rewriting-identical-rows-moves-nothing-in-the-feed
  (let [d :txn-noop-rewrite]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (let [low (current)]
      (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
      (let [delta (with-open [c (pgds/open (tsys/ds))]
                    (pgds/table-delta c {:schema "main"
                                         :table (naming/dataset-table d)
                                         :pk-col naming/pk-col
                                         :low (long low) :high (long (current))}))]
        (when delta
          (is (empty? (:changed delta)) "an identical rewrite is not a change")
          (is (empty? (:removed delta)))
          (is (empty? (:added delta)))))))
  (testing "while a row that REALLY changed still shows up"
    (let [d :txn-real-change]
      (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
      (let [low (current)]
        (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 99}) (row "b" {:v 2})])
        (let [delta (with-open [c (pgds/open (tsys/ds))]
                      (pgds/table-delta c {:schema "main"
                                           :table (naming/dataset-table d)
                                           :pk-col naming/pk-col
                                           :low (long low) :high (long (current))}))]
          (is (= #{"a"} (:changed delta)) "exactly the row that moved, and only it")))))
  (testing "and a value going to or from NULL is a change — `IS DISTINCT FROM`, not `<>`"
    (let [d :txn-null-change]
      (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
      (let [low (current)]
        (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v nil})])
        (is (= [nil] (map :v (table-rows d))) "the NULL actually landed")
        (let [delta (with-open [c (pgds/open (tsys/ds))]
                      (pgds/table-delta c {:schema "main"
                                           :table (naming/dataset-table d)
                                           :pk-col naming/pk-col
                                           :low (long low) :high (long (current))}))]
          (is (= #{"a"} (:changed delta)) "value→NULL is reported"))))))

(deftest a-sparse-delta-row-does-not-null-its-other-columns
  (let [d :txn-sparse]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:name "A" :qty 1})])
    (dsets/write-rows! (ctx/system-store) d :append   [(row "a" {:qty 7})])
    (is (= [{:name "A" :qty 7}]
           (map #(select-keys % [:name :qty]) (table-rows d)))
        "qty updated, name preserved")))

(deftest edges-are-a-snapshot-and-idempotent
  (let [d :txn-edges
        e (fn [id f t] {:id id :from-id f :to-id t})]
    (dsets/write-edges! (ctx/system-store) d [(e "e1" "x" "y") (e "e2" "y" "z")])
    (is (= ["e1" "e2"] (pks d)))
    (testing "recomputing the same edge set changes nothing"
      (dsets/write-edges! (ctx/system-store) d [(e "e1" "x" "y") (e "e2" "y" "z")])
      (is (= ["e1" "e2"] (pks d))))
    (testing "an edge the producer no longer backs is dropped — a recompute IS a snapshot"
      (dsets/write-edges! (ctx/system-store) d [(e "e1" "x" "y")])
      (is (= ["e1"] (pks d))))))

(deftest the-changelog-is-derived-over-low-then-write
  (testing "capture low, write, derive — the ordering Phase 2 turns around"
    (let [d :cl-basic]
      (dsets/write-rows! (ctx/system-store) d :snapshot
                         [(row "a" {:v 1}) (row "b" {:v 2}) (row "c" {:v 3})])
      (let [low (current)]
        (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 22})])
        (is (= ["c"] (dsets/removed-pks (ctx/system-store) d low))
            "exactly the key the producer stopped backing")))))

(deftest a-boundary-captured-after-the-write-sees-nothing
  (let [d :cl-late-boundary]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (let [too-late (current)]
      (is (= [] (dsets/removed-pks (ctx/system-store) d too-late))
          "a boundary at or after the write can carry no news")
      (is (= ["a"] (pks d))
          "…while the write it missed really did remove a row — that is the hazard"))))

(deftest an-idempotent-refresh-writes-nothing-at-all
  (let [d :cl-idempotent
        rows [(row "a" {:v 1}) (row "b" {:v 2}) (row "c" {:v 3})]]
    (dsets/write-rows! (ctx/system-store) d :snapshot rows)
    (let [low (current)]
      (dsets/write-rows! (ctx/system-store) d :snapshot rows)
      (is (= low (current)) "the refresh took no snapshot — nothing was written")
      (is (= [] (dsets/removed-pks (ctx/system-store) d low))
          "an empty range is a definitive 'nothing removed', not an undecidable one")
      (is (= ["a" "b" "c"] (pks d)) "and the rows are all still there"))))

(deftest nil-and-empty-are-different-answers
  (let [d :cl-nil-vs-empty]
    (testing "no boundary at all ⇒ nil"
      (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
      (is (nil? (dsets/removed-pks (ctx/system-store) d nil))))
    (testing "a real boundary with no deletions ⇒ []"
      (let [low (current)]
        (dsets/write-rows! (ctx/system-store) d :append [(row "b" {:v 2})])
        (is (= [] (dsets/removed-pks (ctx/system-store) d low)))))))

(deftest a-table-the-lake-never-held-derives-nothing
  (testing "an unknown dataset answers nil rather than 'everything was removed'"
    (is (nil? (dsets/removed-pks (ctx/system-store) :cl-never-written 1)))))

(deftest an-added-read-includes-modified-rows
  (let [d   :added-read
        tbl (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (let [low (current)]
      (dsets/write-rows! (ctx/system-store) d :append [(row "a" {:v 111})
                                                (row "c" {:v 3})])
      (let [got (set (map :v (dsets/added-values (ctx/system-store) tbl low (current) nil)))]
        (is (= #{111 3} got) "both the modified row and the new one")))))

(deftest a-date-column-reads-back-as-the-store-types-it
  (let [d   :txn-dates
        tbl (naming/dataset-table d)
        day (java.sql.Date/valueOf "2024-06-01")
        ts  (java.sql.Timestamp/valueOf "2024-06-01 12:30:00")]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:day day :at ts :v 1})])
    (let [low (current)
          _   (dsets/write-rows! (ctx/system-store) d :append [(row "b" {:day day :at ts :v 2})])
          full-row  (:values (first (dsets/rows (ctx/system-store) d)))
          raw-row   (first (dsets/row-values (ctx/system-store) tbl nil))
          delta-row (first (dsets/added-values (ctx/system-store) tbl low (current) nil))]
      (doseq [[label r] [["rows" full-row] ["row-values" raw-row]
                         ["added-values" delta-row]]]
        (is (instance? java.sql.Date (or (:day r) (get r "day")))
            (str label " — DATE comes back as java.sql.Date"))
        (is (instance? java.sql.Timestamp (or (:at r) (get r "at")))
            (str label " — TIMESTAMP comes back as java.sql.Timestamp"))
        (is (string? (json/write-str r))
            (str label " — and the python-input export can serialize it"))))))

(deftest an-added-read-over-a-quiet-range-is-empty
  (let [d   :added-quiet
        tbl (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (let [at (current)]
      (is (= [] (dsets/added-values (ctx/system-store) tbl at at nil))))))

(deftest counts-and-pks-answer-from-the-lake
  (let [d :reads-basic]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (is (= 2 (dsets/table-count (ctx/system-store) d)))
    (is (= #{"a" "b"} (set (dsets/table-pks (ctx/system-store) d))))
    (is (= #{"a" "b"} (set (map :id (dsets/rows (ctx/system-store) d)))))))

(deftest a-preview-time-travels-by-snapshot-id
  (let [d :reads-as-of]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (let [before (current)]
      (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
      (is (= #{"a"} (set (map :id (dsets/rows (ctx/system-store) d))))          "current view")
      (is (= #{"a" "b"} (set (map :id (dsets/rows (ctx/system-store) d before))))
          "as of the earlier snapshot"))))

(deftest a-snapshot-id-cannot-address-a-lake-dataset
  (testing "it throws rather than silently answering with the current rows"
    (let [d :reads-bad-ref]
      (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
      (is (thrown? clojure.lang.ExceptionInfo
                   (dsets/rows (ctx/system-store) d "9fk2n1qh8v0k3j5m7p2r4t6y8u0i2o4a"))))))

(deftest current-version-advances-on-every-committed-write
  (let [d :ver]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (let [v1 (dsets/current-version (ctx/system-store))]
      (dsets/write-rows! (ctx/system-store) d :append [(row "b" {:v 2})])
      (is (> (long (dsets/current-version (ctx/system-store))) (long v1))))))

(def ^:private br "scenario-a")
(def ^:private br-ns (pgns/read-schema br))

(deftest a-branch-reads-the-trunk-until-it-writes
  (let [d :br-read-through]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (is (= #{"a" "b"} (set (map :id (dsets/rows (ctx/system-store br) d))))
        "the trunk's rows, through the fallback")
    (is (= 2 (dsets/table-count (ctx/system-store br) d)))
    (is (= #{"a" "b"} (set (dsets/table-pks (ctx/system-store br) d))))))

(deftest an-append-on-a-branch-keeps-the-trunks-rows
  (let [d :br-append]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "c" {:v 3})])
    (is (= ["a" "b" "c"] (pks br-ns d)) "the branch: the trunk's rows PLUS its own")
    (is (= ["a" "b"] (pks d))           "the trunk: untouched")))

(deftest a-table-the-branch-holds-is-not-forked-again
  (let [d     :br-memo
        table (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "c" {:v 3})])
    (is (= ["a" "b" "c"] (pks br-ns d)) "the branch: the trunk's rows PLUS its own")
    (is (= ["a" "b"] (pks d))           "the trunk: untouched")
    (with-open [c (pgds/open (tsys/ds))]
      (is (false? (pgds/fork-table! c br table)) "no copy over the branch's own table"))
    (is (= ["a" "b" "c"] (pks br-ns d)))))

(deftest a-branch-owns-only-what-it-wrote
  (let [d :br-owns-delta
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot (mapv #(row (str "r" %) {:v %}) (range 200)))
    (dsets/write-rows! (ctx/system-store br) d :append [(row "r7" {:v 999}) (row "neu" {:v 1})])
    (testing "the branch READS all 201 — its two, plus 199 inherited"
      (do (is (= 201 (dsets/table-count (ctx/system-store br) d)))
          (is (= 999 (->> (dsets/rows (ctx/system-store br) d)
                           (filter #(= "r7" (:id %))) first :values :v))
              "its own value wins over the inherited one")))
    (testing "…and OWNS two"
      (with-open [c (pgds/open (tsys/ds))]
        (is (= 2 (long (or (first (vals (jdbc/execute-one!
                                         c [(str "SELECT count(*) AS n FROM "
                                                 (sql/dq (pgns/own-schema br)) "."
                                                 (sql/dq t))])))
                           0)))
            "the branch's own table carries the delta, not a copy of the trunk")))
    (is (= 200 (count (table-rows "main" d))) "the trunk is untouched")))

(deftest a-branch-that-restates-what-it-read-does-not-fork
  (let [d :br-restate
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "a" {:v 1}) (row "b" {:v 2})])
    (is (= #{"a" "b"} (set (map :id (dsets/rows (ctx/system-store br) d))))
        "the branch reads both rows — through the trunk")
    (with-open [c (pgds/open (tsys/ds))]
      (is (not (pgds/table-exists? c br-ns t))
          "…and owns no copy of its own, because nothing it wrote differed"))))

(deftest a-branch-that-changes-ONE-row-still-forks
  (let [d :br-restate-one
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "a" {:v 1}) (row "b" {:v 99})])
    (with-open [c (pgds/open (tsys/ds))]
      (is (pgds/table-exists? c br-ns t) "the branch owns its copy"))
    (is (= ["a" "b"] (pks br-ns d)) "with BOTH rows, not just the changed one")
    (is (= 2 (count (table-rows "main" d))) "and the trunk is untouched")
    (is (= #{1 99} (set (map (comp :v :values) (dsets/rows (ctx/system-store br) d))))
        "the branch reads its own value")))

(deftest a-large-write-lands-what-it-was-given
  (let [d    :txn-large
        odd  "tab\there\nline\\slash\rend"
        rows (mapv (fn [i] (row (str "k" i) {:s (case (long (mod i 4)) 0 odd 1 "" 2 nil "x")
                                             :n i :f (+ i 0.5) :b (even? i)}))
                   (range 5000))
        copied (atom 0)
        text   pgds/copy-text]
    (with-redefs [pgds/copy-text (fn [cols rs] (swap! copied + (count rs)) (text cols rs))]
      (dsets/write-rows! (ctx/system-store) d :snapshot rows))
    (is (= 5000 @copied) "staged by COPY")
    (let [got (into {} (map (juxt :id :values)) (dsets/rows (ctx/system-store) d))]
      (is (= 5000 (count got)))
      (doseq [i [0 1 2 3 4999]]
        (is (= (dissoc (:values (nth rows i)) :s)
               (dissoc (get got (str "k" i)) :s)))
        (is (= (:s (:values (nth rows i))) (:s (get got (str "k" i)))))))
    (let [low (current)]
      (dsets/write-rows! (ctx/system-store) d :snapshot (assoc-in rows [7 :values :n] -1))
      (let [delta (with-open [c (pgds/open (tsys/ds))]
                    (pgds/table-delta c {:schema "main" :table (naming/dataset-table d)
                                         :pk-col naming/pk-col
                                         :low (long low) :high (long (current))}))]
        (is (= #{"k7"} (:changed delta)) "a value read back alike is no change")))))

(deftest a-large-write-of-dates-is-copied-and-reads-back
  (let [d      :txn-large-dates
        day    (java.time.LocalDate/of 2026 9 24)
        at     (java.time.LocalDateTime/of 2026 9 24 10 15)
        rows   (mapv #(row (str "k" %) {:d day :t at :n %}) (range 5000))
        landed (atom nil)
        store  (assoc (ctx/system-store) :on-landing (fn [_ l] (reset! landed l)))]
    (dsets/write-rows! store d :snapshot rows)
    (dsets/write-rows! store d :snapshot rows)
    (is (= ["copy"] (get-in @landed [:merge :modes])))
    (is (zero? (get-in @landed [:merge :changed])) "a value read back alike is no change")
    (let [{:keys [t] day-read :d} (:values (first (dsets/rows (ctx/system-store) d)))]
      (is (= "2026-09-24" (str day-read)))
      (is (= "2026-09-24T10:15" (subs (.replace (str t) " " "T") 0 16))))))

(deftest a-date-column-keeps-the-day-of-a-local-time-through-copy
  (let [d      :txn-date-from-time
        day    (java.time.LocalDate/of 2026 9 24)
        landed (atom nil)
        store  (assoc (ctx/system-store) :on-landing (fn [_ l] (reset! landed l)))
        rows   (fn [at] (mapv #(row (str "k" %) {:d at}) (range 5000)))]
    (dsets/write-rows! (ctx/system-store) d :append (rows day))
    (dsets/write-rows! store d :append (rows (.atTime day 10 15)))
    (is (= ["copy"] (get-in @landed [:probe :modes])))
    (is (:unchanged @landed) "the same day, whatever the time")
    (dsets/write-rows! store d :append (rows (.atTime (.plusDays day 1) 23 59)))
    (is (= 5000 (get-in @landed [:merge :changed])) "another day is a change")
    (is (= (str (.plusDays day 1))
            (str (:d (:values (first (dsets/rows (ctx/system-store) d)))))))))

(deftest a-landing-names-what-kept-it-from-copy
  (let [d      :txn-copy-refused
        landed (atom nil)
        store  (assoc (ctx/system-store) :on-landing (fn [_ l] (reset! landed l)))
        rows   #(mapv (fn [i] (row (str "k" i) {:x (% i)})) (range 5000))]
    (dsets/write-rows! (ctx/system-store) d :append (rows str))
    (dsets/write-rows! store d :append (rows identity))
    (is (= ["insert"] (get-in @landed [:probe :modes])))
    (is (= "x" (get-in @landed [:probe :copy-refused :column])))
    (is (= "java.lang.Long" (get-in @landed [:probe :copy-refused :class])))
    (is (:unchanged @landed) "a number an INSERT casts to the text it holds is no change")))

(deftest a-landing-says-where-its-time-went
  (let [d       :txn-landing
        rows    (mapv #(row (str "k" %) {:n %}) (range 5000))
        landed  (atom [])
        store   (assoc (ctx/system-store) :on-landing (fn [ds l] (swap! landed conj [ds l])))]
    (dsets/write-rows! store d :append rows)
    (dsets/write-rows! store d :append rows)
    (let [[[ds first-landing] [_ again]] @landed]
      (is (= d ds))
      (is (= ["copy"] (get-in first-landing [:merge :modes])))
      (is (= 5000 (get-in first-landing [:merge :changed])))
      (is (nat-int? (:total-ms first-landing)))
      (is (:unchanged again) "nothing differs, so nothing merged")
      (is (= ["copy"] (get-in again [:probe :modes])))
      (is (nat-int? (get-in again [:probe :encode-ms])))
      (is (nil? (:merge again))))))

(deftest a-copy-is-a-traced-statement
  (with-open [raw (pgds/open (tsys/ds))]
    (let [c (sqltrace/wrap-connection raw "test")]
      (jdbc/execute! c ["CREATE TEMP TABLE _copy_traced (x text)"])
      (sqltrace/arm! {:shapes? true})
      (try
        (let [mgr (.getCopyAPI ^PGConnection (.unwrap c PGConnection))]
          (is (= 2 (.copyIn mgr "COPY _copy_traced (x) FROM STDIN" (StringReader. "a\nb\n")))
              "the driver's answer comes through"))
        (let [ev (last (:events (sqltrace/since 0)))]
          (is (re-find #"^COPY _copy_traced" (:sql ev)))
          (is (= 2 (:update-count ev)))
          (is (not (:explainable ev))))
        (is (= [1] (->> (sqltrace/shapes-report {:limit 50}) :rows
                        (filter #(re-find #"^COPY" (:sql %))) (map :calls))))
        (finally (sqltrace/disarm!))))))

(deftest a-signature-moves-with-every-write-and-only-with-a-write
  (doseq [[where store] {"main" (ctx/system-store) "branch" (ctx/system-store br)}]
    (testing where
      (let [d    (keyword (str "txn-signature-" where))
            t    (naming/dataset-table d)
            sig  #(dsets/content-signature store t)
            base [(row "a" {:v 1}) (row "b" {:v 2}) (row "c" {:v 3})]]
        (dsets/write-rows! (ctx/system-store) d :snapshot base)
        (dsets/write-rows! store d :append [(row "a" {:v 9})])
        (let [s0 (sig)]
          (is (some? s0))
          (is (= s0 (sig)) "reading moves nothing")
          (dsets/write-rows! store d :append [(row "a" {:v 9})])
          (is (= s0 (sig)) "a write of what it holds moves nothing"))
        (doseq [[what write!]
                {"a new row"     #(dsets/write-rows! store d :append [(row "n" {:v 4})])
                 "a changed row" #(dsets/write-rows! store d :append [(row "a" {:v 10})])
                 "a removal"     #(dsets/delete-rows! store d ["b"])
                 "a snapshot"    #(dsets/write-rows! store d :snapshot [(row "a" {:v 10})])
                 "all of it"     #(dsets/truncate! store d)}]
          (let [before (sig)]
            (write!)
            (is (not= before (sig)) what)))))))

(deftest a-removal-is-known-from-publish-until-the-dataset-is-written
  (doseq [[where store] {"main" (ctx/system-store) "branch" (ctx/system-store br)}]
    (testing where
      (let [d     (keyword (str "txn-known-" where))
            ot    :thing
            rows  {ot [(row "a" {:v 1})]}
            _     (dsets/write-rows! (ctx/system-store) d :snapshot
                                     [(row "a" {:v 1}) (row "gone" {:v 2}) (row "later" {:v 3})])
            low   (current)
            _     (dsets/write-rows! store d :snapshot
                                     [(row "a" {:v 1}) (row "later" {:v 3})])]
        (with-redefs [backing/object-backing   (fn [_ _] {:dataset d})
                      changelog/unindexed-ids  (constantly #{})]
          (let [{:keys [changed removals]}
                (#'phases/changes-since store {} rows low false)
                known (get removals ot)]
            (is (contains? changed ot))
            (is (= ["gone"] (:removed known)))
            (is (= (set (dsets/removed-pks store d low)) (set (:removed known)))
                "what publish keeps is what reconcile would have asked")
            (is (= ["gone"] (reconcile/still-known-removed store d known)))
            (testing "a write after publish ends it, and asking again finds the new removal"
              (dsets/delete-rows! store d ["later"])
              (is (nil? (reconcile/still-known-removed store d known)))
              (is (= #{"gone" "later"} (set (dsets/removed-pks store d low)))))))))))

(deftest reconcile-takes-a-known-removal-and-asks-only-without-one
  (let [d      :txn-reconcile-known
        ot     :thing
        store  (ctx/system-store)
        asked  (atom 0)
        sweep  (fn [known]
                 (reset! asked 0)
                 (first (reconcile/reconcile-object-tables! store "main" {} {ot []} "1" nil
                                                            {ot known})))]
    (dsets/write-rows! store d :snapshot [(row "a" {:v 1})])
    (with-redefs [backing/object-backing          (fn [_ _] {:snapshot? true :dataset d})
                  changelog/edit-exempt-pks       (constantly #{})
                  dsets/removed-pks               (fn [& _] (swap! asked inc) [])]
      (let [signature (dsets/content-signature store (naming/dataset-table d))]
        (testing "the dataset is as publish saw it: nothing is asked"
          (is (= :changelog (:via (sweep {:signature signature :removed []}))))
          (is (zero? @asked)))
        (testing "it was written since: reconcile asks"
          (sweep {:signature "older" :removed []})
          (is (= 1 @asked)))
        (testing "publish knew nothing: reconcile asks"
          (sweep nil)
          (is (= 1 @asked)))))))

(deftest a-first-snapshot-on-a-branch-reports-only-what-differs
  (let [d :br-first-snapshot
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot
                       [(row "a" {:v 1}) (row "b" {:v 2}) (row "gone" {:v 3})])
    (let [low (current)]
      (dsets/write-rows! (ctx/system-store br) d :snapshot
                         [(row "a" {:v 1}) (row "b" {:v 99}) (row "new" {:v 4})])
      (let [delta (with-open [c (pgds/open (tsys/ds))]
                    (pgds/table-delta c {:schema br-ns :table t :pk-col naming/pk-col
                                         :low (long low) :high (long (current))}))]
        (is (= #{"b" "new"} (:changed delta)) "the identical row a stays out")
        (is (= #{"new"} (:added delta)))
        (is (= #{"gone"} (:removed delta)))))))

(deftest a-branch-that-adds-a-COLUMN-forks-even-with-identical-values
  (let [d :br-restate-widen
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "a" {:v 1 :extra "x"})])
    (with-open [c (pgds/open (tsys/ds))]
      (is (pgds/table-exists? c br-ns t)))
    (is (contains? (set (map name (keys (first (table-rows br-ns d))))) "extra"))
    (is (not (contains? (set (map name (keys (first (table-rows "main" d))))) "extra"))
        "the trunk did not grow the branch's column")))

(deftest a-snapshot-on-a-branch-does-not-copy-first
  (let [d :br-snapshot]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/write-rows! (ctx/system-store br) d :snapshot [(row "z" {:v 9})])
    (is (= ["z"] (pks br-ns d)))
    (is (= ["a" "b"] (pks d)))))

(deftest a-delete-on-a-branch-shadows-the-trunks-row
  (let [d :br-delete]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/delete-rows! (ctx/system-store br) d ["a"])
    (is (= ["b"] (pks br-ns d)))
    (is (= ["a" "b"] (pks d)))))

(deftest a-truncate-on-a-branch-empties-only-the-branch
  (let [d :br-truncate]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/truncate! (ctx/system-store br) d)
    (is (= [] (mapv :id (dsets/rows (ctx/system-store br) d))) "the branch reads empty")
    (is (= ["a" "b"] (pks d)) "the trunk keeps its rows")))

(deftest a-branch-widens-its-OWN-copy-of-a-dataset
  (let [d :br-widen]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "b" {:v 2 :extra "x"})])
    (is (contains? (set (map name (keys (first (table-rows br-ns d))))) "extra"))
    (is (not (contains? (set (map name (keys (first (table-rows d))))) "extra"))
        "the trunk's table did not grow the branch's column")))

(deftest a-branch-can-still-drop-and-rename-a-column-under-its-view
  (let [d :br-ddl-under-view
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "b" {:v 2})])
    (with-open [c (pgds/open (tsys/ds))]
      (let [own (pgns/own-schema br)]
        (pgds/add-column! c own t "stück_preis" "VARCHAR")
        (is (contains? (pgds/columns c br-ns t) "stück_preis")
            "die Sicht trägt die neue Spalte")

        (pgds/rename-column! c own t "stück_preis" "stueck_preis")
        (is (contains? (pgds/columns c br-ns t) "stueck_preis")
            "ein Rename bewegt die Sicht mit")
        (is (not (contains? (pgds/columns c br-ns t) "stück_preis")))

        (pgds/drop-column! c own t "stueck_preis")
        (is (not (contains? (pgds/columns c own t) "stueck_preis"))
            "und ein Drop scheitert nicht an der Sicht, die daran hängt")
        (is (not (contains? (pgds/columns c br-ns t) "stueck_preis")))))))

(deftest a-branch-reads-its-own-past-and-the-trunks-at-once
  (let [d :br-zeitreise]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (let [v0 (current)]
      (dsets/write-rows! (ctx/system-store br) d :append [(row "a" {:v 99}) (row "c" {:v 3})])
      (is (= #{"a" "b" "c"} (set (map :id (dsets/rows (ctx/system-store br) d)))) "jetzt: drei")
      (is (= 99 (->> (dsets/rows (ctx/system-store br) d)
                      (filter #(= "a" (:id %))) first :values :v)))
      (is (= #{"a" "b"} (set (map :id (dsets/rows (ctx/system-store br) d v0))))
          "damals: die zwei des Trunks, ohne die des Zweigs")
      (is (= 1 (->> (dsets/rows (ctx/system-store br) d v0)
                    (filter #(= "a" (:id %))) first :values :v))
          "und mit dem Wert, den der Trunk damals hielt"))))

(deftest a-deletion-on-a-branch-moves-the-branchs-fingerprint
  (let [d :br-sig-delete
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "c" {:v 3})])
    (let [own-row (dsets/content-signature (ctx/system-store br) t)]
      (dsets/delete-rows! (ctx/system-store br) d ["c"])
      (is (= #{"a" "b"} (set (map :id (dsets/rows (ctx/system-store br) d)))))
      (is (not= own-row (dsets/content-signature (ctx/system-store br) t))
          "eine eigene Zeile zu löschen bewegt den Fingerabdruck")

      (let [inherited (dsets/content-signature (ctx/system-store br) t)]
        (dsets/delete-rows! (ctx/system-store br) d ["a"])
        (is (= #{"b"} (set (map :id (dsets/rows (ctx/system-store br) d)))))
        (is (not= inherited (dsets/content-signature (ctx/system-store br) t))
            "und eine nur GEERBTE zu löschen ebenso — sie liegt im Grabstein")))
    (is (= ["a" "b"] (pks d)) "und der Trunk merkt von beidem nichts")))

(deftest dropping-a-branchs-own-table-takes-its-tombstones-with-it
  (let [d :br-drop-tomb
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/delete-rows! (ctx/system-store br) d ["a"])
    (is (= #{"b"} (set (map :id (dsets/rows (ctx/system-store br) d))))
         "der Zweig verdeckt die geerbte Zeile")
    (with-open [c (pgds/open (tsys/ds))]
      (pgds/drop-table! c (pgns/own-schema br) t))
    (is (= #{"a" "b"} (set (map :id (dsets/rows (ctx/system-store br) d))))
        "ohne eigene Tabelle erbt der Zweig wieder alles")
    (dsets/write-rows! (ctx/system-store br) d :append [(row "c" {:v 3})])
    (is (= #{"a" "b" "c"} (set (map :id (dsets/rows (ctx/system-store br) d))))
        "und der neue Fork bringt den Grabstein des alten nicht zurück")))

(deftest a-rollback-on-a-branch-restores-what-the-branch-READS
  (let [d :br-rollback
        _t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "c" {:v 3})])
    (let [v (current)]
      (dsets/delete-rows! (ctx/system-store br) d ["a"])
      (is (= #{"b" "c"} (set (map :id (dsets/rows (ctx/system-store br) d))))
          "die geerbte Zeile ist verdeckt")
      (let [r (dsets/rollback-to! (ctx/system-store br) d (str v))]
        (is (= 3 (:rows-after r)) "der Zweig LIEST wieder drei, nicht nur seine eigene")
        (is (= #{"a" "b" "c"} (set (map :id (dsets/rows (ctx/system-store br) d))))
            "der Grabstein ist zurückgenommen")))
    (is (= ["a" "b"] (pks d)) "und der Trunk stand nie zur Debatte")))

(deftest a-dataset-born-on-a-branch-stays-there
  (let [d :br-native]
    (dsets/write-rows! (ctx/system-store br) d :append [(row "a" {:v 1})])
    (is (= ["a"] (pks br-ns d)))
    (with-open [c (pgds/open (tsys/ds))]
      (is (not (pgds/table-exists? c "main" (naming/dataset-table d)))))))

(deftest dropping-a-branch-takes-its-namespace-and-leaves-the-trunk
  (let [d :br-drop]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (dsets/write-rows! (ctx/system-store br) d :append [(row "b" {:v 2})])
    (with-open [c (pgds/open (tsys/ds))]
      (is (pgds/table-exists? c br-ns (naming/dataset-table d)))
      (pgds/drop-branch! c br)
      (is (not (pgds/table-exists? c br-ns (naming/dataset-table d))))
      (is (pgds/table-exists? c "main" (naming/dataset-table d))))
    (is (= #{"a"} (set (map :id (dsets/rows (ctx/system-store br) d))))
        "a recreated branch of the same name inherits the TRUNK, not the dead fork")))

(deftest a-fork-carries-the-trunks-history-so-a-pre-fork-boundary-still-derives
  (let [d :br-boundary]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1}) (row "b" {:v 2})])
    (let [low (current)]
      (dsets/write-rows! (ctx/system-store br) d :append [(row "c" {:v 3})])
      (is (= [] (dsets/removed-pks (ctx/system-store br) d low))
          "an ANSWER, and the right one — the branch kept both inherited rows"))))

(deftest a-signature-is-per-NAMESPACE-not-per-table-name
  (let [d :br-sig
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (let [trunk-before (dsets/content-signature (ctx/system-store) t)]
      (is (some? trunk-before) "the trunk's table fingerprints")
      (is (= trunk-before (dsets/content-signature (ctx/system-store br) t))
          "before the branch writes, it reads the trunk's table and so its
           signature — the same fallback every other read takes")
      (dsets/write-rows! (ctx/system-store br) d :append [(row "b" {:v 2})])
      (is (= trunk-before (dsets/content-signature (ctx/system-store) t))
          "the trunk did not move: a branch's build must not make a trunk
           schedule due")
      (is (not= trunk-before (dsets/content-signature (ctx/system-store br) t))
          "the branch did: its own fork was written by a later snapshot"))))

(deftest a-signature-is-nil-for-what-cannot-be-fingerprinted
  (is (nil? (dsets/content-signature (ctx/system-store) "d_never_created")))
  (with-open [c (pgds/open (tsys/ds))]
    (pgds/create-table! c "main" "d_created_empty" [{:name "id" :type "VARCHAR"}])
    (is (nil? (pgds/content-signature c "main" "d_created_empty"))
        "a table that exists but was never WRITTEN has no mark — a creation
         names its table by name rather than by id, which is the same
         distinction `ever-written?` draws, and it is what a not-yet-built
         parent should read as"))
  (let [d :br-sig-empty]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (let [full (dsets/content-signature (ctx/system-store) (naming/dataset-table d))]
      (dsets/truncate! (ctx/system-store) d)
      (is (some? (dsets/content-signature (ctx/system-store) (naming/dataset-table d)))
          "emptying is a WRITE, and fingerprints as one")
      (is (not= [full] [(dsets/content-signature (ctx/system-store) (naming/dataset-table d))])
          "…and the vector the trigger actually compares still MOVES"))))

(deftest a-created-but-never-written-table-is-not-yet-the-lakes
  (let [t "d_installed_not_written"]
    (with-open [c (pgds/open (tsys/ds))]
      (pgds/create-table! c "main" t [{:name "id" :type "VARCHAR"}])
      (is (pgds/table-exists? c "main" t) "the table is there…")
      (is (not (pgds/ever-written? c "main" t))
          "…but nothing ever wrote it, so the lake does not yet OWN this dataset —
           which is what routes the read back to the store that does"))))

(deftest a-truncated-table-IS-the-lakes-even-though-it-is-empty
  (let [d :straddle-trunc
        t (naming/dataset-table d)]
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (dsets/truncate! (ctx/system-store) d)
    (with-open [c (pgds/open (tsys/ds))]
      (is (pgds/ever-written? c "main" t)
          "…and still the store's, because it was written once")))
  (is (= 0 (dsets/table-count (ctx/system-store) :straddle-trunc))
      "so the seam reads the store's emptiness, not a fallback"))

(deftest the-guard-flips-the-moment-something-is-written
  (let [d :straddle-flip
        t (naming/dataset-table d)]
    (with-open [c (pgds/open (tsys/ds))]
      (pgds/create-table! c "main" t [{:name naming/pk-col :type "VARCHAR"}])
      (is (not (pgds/ever-written? c "main" t))))
    (dsets/write-rows! (ctx/system-store) d :snapshot [(row "a" {:v 1})])
    (with-open [c (pgds/open (tsys/ds))]
      (is (pgds/ever-written? c "main" t)))
    (is (= 1 (dsets/table-count (ctx/system-store) d)))))

(deftest the-watermark-is-this-databases-own
  (testing "a version open in ANOTHER database on the same server does not hold this one's watermark back (pg_locks lists every database's advisory locks)"
    (with-open [c (pgds/open (tsys/ds))]
      (jdbc/execute! c ["SELECT setval('public.dataset_seq', 1000)"]))
    (let [mine  (current)
          other (pod/start-pg!)]
      (try
        (with-open [o (jdbc/get-connection (:url other)
                                           {:user (:user other) :password (:password other)})]
          (.setAutoCommit o false)
          (let [v (:v (jdbc/execute-one! o ["SELECT public.sutra_next_version() AS v"]
                                         {:builder-fn rs/as-unqualified-lower-maps}))]
            (is (< (long v) (long mine))
                "the other database's open version lies below this one's watermark")
            (is (= mine (current)) "and this database's watermark does not fall to it"))
          (.rollback o))
        (finally (pod/stop-pg! other))))))
