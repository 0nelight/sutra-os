(ns ^:unit platform.dataset-store-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.pg.dataset-store :as dstore]
            [support.mem :as mem]
            [sutra.types.coltype :as ctype]))

(defn- store-with-table []
  (let [s (mem/mem-dataset)]
    (dstore/create-table! s "main" "d_x"
                      [{:name "internal_uuid" :type (ctype/col-type :varchar) :not-null? true}
                       {:name "betrag" :type (ctype/col-type :decimal {:precision 38 :scale 2})}])
    s))

(deftest a-column-type-crosses-the-seam-as-a-value
  (let [s (store-with-table)]
    (is (= (ctype/col-type :decimal {:precision 38 :scale 2})
           (get (dstore/column-types s "main" "d_x") "betrag"))
        "what went in as a value comes back as the same value — no spelling in between")
    (dstore/add-column! s "main" "d_x" "menge" (ctype/col-type :bigint))
    (is (= #{"internal_uuid" "betrag" "menge"} (dstore/columns s "main" "d_x")))))

(deftest merge-is-per-column-and-retain-is-the-only-retraction
  (let [s (store-with-table)]
    (dstore/merge-rows! s "main" "d_x" "internal_uuid"
                    ["internal_uuid" "betrag"]
                    [{"internal_uuid" "a" "betrag" 1M}
                     {"internal_uuid" "b" "betrag" 2M}]
                    {})
    (is (= 2 (dstore/count-rows s "main" "d_x")))
    (testing "a sparse merge accumulates per column rather than nulling the row"
      (dstore/merge-rows! s "main" "d_x" "internal_uuid" ["internal_uuid" "menge"]
                      [{"internal_uuid" "a" "menge" 7}] {})
      (let [a (first (filter #(= "a" (get % "internal_uuid"))
                             (dstore/select-rows s "main" "d_x" {})))]
        (is (= 1M (get a "betrag")) "the column this merge did not name kept its value")
        (is (= 7 (get a "menge")))))
    (testing "retain-pks! is what retracts — a merge never does"
      (dstore/retain-pks! s "main" "d_x" "internal_uuid" ["a"])
      (is (= 1 (dstore/count-rows s "main" "d_x")))
      (is (= ["a"] (dstore/select-pks s "main" "d_x" "internal_uuid"))))))

(deftest a-branch-reads-the-trunk-until-it-forks
  (let [s (store-with-table)]
    (dstore/create-branch! s "b1")
    (dstore/merge-rows! s "main" "d_x" "internal_uuid" ["internal_uuid"]
                    [{"internal_uuid" "a"}] {})
    (is (= "main" (dstore/read-scope s "b1" "d_x"))
        "unforked, the branch resolves to the trunk")
    (is (true? (dstore/fork-table! s "b1" "d_x")))
    (is (= "b1" (dstore/read-scope s "b1" "d_x")))
    (is (false? (dstore/fork-table! s "b1" "d_x")) "forking twice is a no-op")
    (testing "a write to the fork leaves the trunk alone"
      (dstore/delete-pks! s (dstore/write-scope s "b1") "d_x" "internal_uuid" ["a"])
      (is (= 0 (dstore/count-rows s "b1" "d_x")))
      (is (= 1 (dstore/count-rows s "main" "d_x"))))))

(deftest a-store-says-what-it-cannot-do-instead-of-failing-late
  (let [s (mem/mem-dataset)]
    (is (false? (dstore/supports? s :sql)))
    (is (true? (dstore/supports? s :branches)))
    (testing "every method it cannot carry out names the missing capability"
      (doseq [[what f] {"as-of"       #(dstore/as-of s "main" "d_x" 1)
                        "restore-to!" #(dstore/restore-to! s "main" "d_x" 1)}]
        (let [e (try (f) nil (catch clojure.lang.ExceptionInfo e e))]
          (is (some? e) (str what " must refuse, not answer"))
          (is (= #{:snapshot-reserve :branches} (:capabilities (ex-data e)))
              (str what " names what the store CAN do")))))))

(deftest a-capability-is-claimed-only-where-the-method-answers
  (testing "MemDataset refuses as-of, so it must not claim :time-travel"
    (let [s (mem/mem-dataset)]
      (is (false? (dstore/supports? s :time-travel)))
      (is (thrown? clojure.lang.ExceptionInfo (dstore/as-of s "main" "d_x" 1)))
      (is (true? (dstore/supports? s :snapshot-reserve)))
      (is (int? (dstore/next-snapshot! s)) "claimed, so it answers")))
  (testing "MemDataset keeps no history, so it must not claim :backup"
    (let [s (mem/mem-dataset)]
      (is (false? (dstore/supports? s :backup)))
      (is (thrown? clojure.lang.ExceptionInfo (dstore/backup! s "x")))
      (is (thrown? clojure.lang.ExceptionInfo (dstore/backups s)))
      (is (thrown? clojure.lang.ExceptionInfo (dstore/restore-store-to! s 1)))))
  (testing "the store that holds the bulk datasets claims all of them"
    (is (= #{:sql :time-travel :snapshot-reserve :rollback :backup :branches
             :concurrent-writers :distinct-on}
           (dstore/capabilities (dstore/pg-store nil))))))

(deftest the-content-signature-moves-only-when-the-content-does
  (let [s (store-with-table)
        sig0 (dstore/content-signature s "main" "d_x")]
    (dstore/merge-rows! s "main" "d_x" "internal_uuid" ["internal_uuid"]
                    [{"internal_uuid" "a"}] {})
    (let [sig1 (dstore/content-signature s "main" "d_x")]
      (is (not= sig0 sig1) "a write moves it")
      (is (= sig1 (dstore/content-signature s "main" "d_x")) "a read does not"))))

(defn- trunk-with-two-rows []
  (let [s (store-with-table)]
    (dstore/merge-rows! s "main" "d_x" "internal_uuid" ["internal_uuid" "betrag"]
                    [{"internal_uuid" "a" "betrag" 1M}
                     {"internal_uuid" "b" "betrag" 2M}] {})
    (dstore/create-branch! s "b1")
    s))

(defn- refresh-branch!
  [s rows]
  (dstore/commit! s "b1" "d_x" {:type :snapshot :pk "internal_uuid" :fork? false :rows rows}))

(def ^:private trunk-rows
  [{"internal_uuid" "a" "betrag" 1M}
   {"internal_uuid" "b" "betrag" 2M}])

(deftest a-refresh-that-changes-nothing-leaves-the-branch-on-the-trunk
  (testing "a scenario fork reindexes its links on its first write and lands the
            edges it derived — the same edges the trunk already holds. Detaching
            and writing them back copied the whole dataset into the fork to
            express no change at all: 67 MB for one edited row."
    (let [s (trunk-with-two-rows)]
      (refresh-branch! s trunk-rows)
      (is (= "main" (dstore/read-scope s "b1" "d_x"))
          "the branch still reads the trunk — nothing was copied")
      (is (= 2 (dstore/count-rows s (dstore/read-scope s "b1" "d_x") "d_x"))
          "and it sees the contents it just landed"))))

(deftest a-refresh-that-drops-a-row-is-never-skipped
  (testing "the half `rows-differ?` cannot see: every row it carries is present and
            equal, and the row it no longer carries must still go"
    (let [s (trunk-with-two-rows)]
      (refresh-branch! s [{"internal_uuid" "a" "betrag" 1M}])
      (is (= "b1" (dstore/read-scope s "b1" "d_x")) "the branch took its own copy")
      (is (= ["a"] (dstore/select-pks s "b1" "d_x" "internal_uuid")))
      (is (= 2 (dstore/count-rows s "main" "d_x")) "and the trunk kept both"))))

(deftest a-refresh-that-alters-or-adds-a-row-is-never-skipped
  (let [changed (trunk-with-two-rows)
        added   (trunk-with-two-rows)]
    (refresh-branch! changed [{"internal_uuid" "a" "betrag" 1M}
                              {"internal_uuid" "b" "betrag" 99M}])
    (is (= "b1" (dstore/read-scope changed "b1" "d_x")))
    (is (= 99M (->> (dstore/select-rows changed "b1" "d_x" {})
                    (filter #(= "b" (get % "internal_uuid")))
                    first
                    (#(get % "betrag")))))
    (refresh-branch! added (conj trunk-rows {"internal_uuid" "c" "betrag" 3M}))
    (is (= "b1" (dstore/read-scope added "b1" "d_x")))
    (is (= 3 (dstore/count-rows added "b1" "d_x")))))

(deftest the-trunk-itself-still-refreshes
  (testing "the guard asks whether a branch may keep reading the trunk, so on the
            trunk it must not fire — there is nothing to inherit from"
    (let [s (trunk-with-two-rows)]
      (dstore/commit! s "main" "d_x" {:type :snapshot :pk "internal_uuid" :fork? false
                                  :rows [{"internal_uuid" "a" "betrag" 1M}]})
      (is (= ["a"] (dstore/select-pks s "main" "d_x" "internal_uuid"))))))
