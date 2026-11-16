(ns ^:integration platform.flaw-documents-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.documents.records :as docs]
            [sutra.store.access.scope :as ctx]
            [sutra.catalog.grants :as grants]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-mem-stores-only)

(def ^:private sys grants/system-authority)

(defn- store [b] (ctx/store b))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest every-save-is-a-version-fenced-on-what-was-read
  (store/with-fresh-branch [b "docs"]
    (let [d  (docs/save! (store b) sys {:name "Plan" :body "eins"})
          id (:id d)]
      (is (= 1 (:version d)))
      (is (= 1 (count (docs/versions (store b) sys id)))
          "unversioned-save: the first save is a version")
      (docs/save! (store b) sys {:id id :body "zwei" :expect-version 1 :label "zwei"})
      (is (= 2 (count (docs/versions (store b) sys id))) "a named save is a version of its own")
      (testing "doc-lost-update: a writer that read version 1 does not overwrite version 2"
        (is (= :conflict
               (:error (refusal #(docs/save! (store b) sys
                                             {:id id :body "alt" :expect-version 1})))))
        (is (= "zwei" (:body (docs/document (store b) sys id :read))))
        (is (= 2 (count (docs/versions (store b) sys id))) "and writes no version"))
      (testing "unmarked-restore: restore is one save, with its version"
        (let [first-seq (:seq (last (docs/versions (store b) sys id)))]
          (is (= "eins" (:body (docs/restore! (store b) sys id first-seq {:author "t"}))))
          (is (= 3 (count (docs/versions (store b) sys id))))))
      (testing "history-lost, orphan-version: delete takes document and versions together"
        (let [seq-no (:seq (first (docs/versions (store b) sys id)))]
          (docs/delete! (store b) sys id)
          (is (nil? (docs/document (store b) sys id :read)))
          (is (nil? (docs/version-body (store b) id seq-no)) "no version outlives its document")
          (is (nil? (docs/restore! (store b) sys id seq-no {:author "t"}))))))))

(defn- at-minute [store m]
  (assoc store :clock (constantly (java.util.Date. (long (* m 60 1000))))))

(deftest autosave-versions-keep-a-five-minute-pace
  (store/with-fresh-branch [b "docs-pace"]
    (let [save! (fn [m doc] (docs/save! (at-minute (store b) m) sys doc))
          d     (save! 0 {:name "Takt" :body "a" :author "anna"})
          id    (:id d)
          vs    #(docs/versions (store b) sys id)
          body  #(docs/version-body (store b) id (:seq (first (vs))))]
      (save! 1 {:id id :body "b" :author "anna" :expect-version 1})
      (is (= 1 (count (vs))) "an unnamed save inside five minutes refreshes the autosave")
      (is (= "b" (body)) "which carries the latest text: no state goes unversioned")
      (save! 2 {:id id :body "c" :author "bert" :expect-version 2})
      (is (= 2 (count (vs))) "another author starts a version of their own")
      (save! 3 {:id id :body "d" :author "bert" :expect-version 3 :label "Freigabe"})
      (save! 4 {:id id :body "e" :author "bert" :expect-version 4})
      (is (= 4 (count (vs))) "a named version is never refreshed")
      (save! 8 {:id id :body "f" :author "bert" :expect-version 5})
      (is (= 4 (count (vs))) "within five minutes of the autosave's start")
      (save! 10 {:id id :body "g" :author "bert" :expect-version 6})
      (is (= 5 (count (vs))) "five minutes after it began, the next autosave starts")
      (is (= "g" (body))))))
