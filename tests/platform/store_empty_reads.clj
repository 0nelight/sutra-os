(ns ^:integration platform.store-empty-reads
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.types.naming :as naming]
            [clojure.string :as str]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private cols
  [{:attribute-name :v :attribute-type :string}])

(defn- installed!
  [d]
  (dsets/ensure-tables! (ctx/system-store) {(naming/dataset-table d) cols})
  d)

(deftest an-unwritten-table-reads-empty
  (testing "installed but never written — the read that used to be refused"
    (let [d (installed! :store-empty-unwritten)]
      (is (= [] (dsets/rows (ctx/system-store) d)))))

  (testing "a name the lake does not hold at all is the same answer, because
            `store-table` resolving to nil IS the empty answer"
    (is (= [] (dsets/rows (ctx/system-store) :store-empty-never-created)))))

(deftest a-written-table-reads-its-rows
  (testing "the empty answer is about the LAKE's state, not a blanket one"
    (let [d :store-empty-written]
      (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "a" :values {:v "1"}}])
      (is (= ["a"] (mapv :id (dsets/rows (ctx/system-store) d))))))

  (testing "written-then-emptied is a real fact about the lake and stands"
    (let [d :store-empty-truncated]
      (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "a" :values {:v "1"}}])
      (dsets/write-rows! (ctx/system-store) d :snapshot [])
      (is (= [] (dsets/rows (ctx/system-store) d))))))

(deftest a-versioned-read-refuses-rather-than-answering-empty
  (testing "no table ⇒ no history, so `as-of` addresses nothing and raises"
    (let [e (try (dsets/rows (ctx/system-store) :store-empty-no-history 7) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e) "must throw")
      (is (= :as-of-unsupported (:store/refusal (ex-data e)))
          "a refusal of the as-of, not an empty answer")
      (is (str/includes? (ex-message e) (naming/dataset-table :store-empty-no-history))
          "the message names the dataset, because the failure it replaces was silent")))

  (testing "a non-snapshot token cannot address a table either — answering
            with the current rows would silently ignore the `as-of`"
    (let [d :store-empty-bad-token]
      (dsets/write-rows! (ctx/system-store) d :snapshot [{:id "a" :values {:v "1"}}])
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"not a snapshot id"
           (dsets/rows (ctx/system-store) d "qq3n8k9vhs1p2rj4"))))))
