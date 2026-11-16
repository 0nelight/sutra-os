(ns ^:unit platform.instance-config-key-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.storage.instance-config :as ic]))

(def ^:private scenario-branch
  "scenario/ri.sutra.main.application.f8df53d9-4e71-41e2-82fd-1cb399085e01/3ad6294a-5960-4fa6-b5d6-9d44f9393fd0.ZTJlLWRlZmVy")

(def ^:private conn-rid
  "ri.sutra.main.connection.7c1f0f52-9d3e-4a11-8f6b-2b0c2a5d91ee")

(deftest a-scenario-branch-still-yields-a-key-that-fits
  (testing "an action on a scenario marks its FK reindex; the mark's key carried
            the whole branch name and overran the column, so the action died with
            a bare 'value too long' and no scenario could take a write at all"
    (is (= 121 (count scenario-branch)) "the branch names our app scenarios carry")
    (is (> (count (str "fk-reindex-sig:" scenario-branch ":" conn-rid)) ic/max-key-length)
        "the old spelling did not fit")
    (is (<= (count (ic/cfg-key "fk-reindex-sig:" scenario-branch conn-rid))
            ic/max-key-length))))

(deftest a-key-that-fits-keeps-its-exact-old-spelling
  (testing "a changed key reads as 'never seen', and every branch would redo the
            work its old mark already recorded"
    (is (= "fk-reindex-sig:main:conn-1" (ic/cfg-key "fk-reindex-sig:" "main" "conn-1")))
    (is (= "physical-ddl-fp:main" (ic/cfg-key "physical-ddl-fp:" "main")))))

(deftest two-long-keys-stay-two-keys
  (testing "the digest replaces the tail, so two branches must not collapse onto
            one mark — that would skip a reindex someone else's branch did"
    (is (not= (ic/cfg-key "fk-reindex-sig:" scenario-branch conn-rid)
              (ic/cfg-key "fk-reindex-sig:" (str scenario-branch "x") conn-rid)))
    (is (= (ic/cfg-key "fk-reindex-sig:" scenario-branch conn-rid)
           (ic/cfg-key "fk-reindex-sig:" scenario-branch conn-rid))
        "and the same input keeps giving the same key")))

(deftest an-over-long-key-names-itself
  (testing "the column's own refusal arrives as a bare PSQLException — the write
            site cost a server-log expedition to find"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"over the 191"
         (ic/set-cfgs! nil {(apply str (repeat 200 "x")) "v"})))))
