(ns ^:unit platform.dataset-removal-predicate-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.store.pg.dataset :as pgds]))

(def ^:private predicate #'pgds/missing-from-stage)

(deftest rows-gone-from-the-stage-are-found-by-anti-join
  (testing "a full refresh asks which rows the stage no longer carries. As
            `NOT IN` over a nullable column Postgres cannot anti-join it and
            stays on a nested loop: on a scenario's own copy of a dataset that
            count ran for eight minutes without returning, and the action that
            triggered it never came back. Shape, not speed — a test table is far
            too small for the cliff to show in a timing."
    (let [p (predicate "\"bo_branch\".\"d_1\"" "\"internal_uuid\"" "_stage_7")]
      (is (re-find #"NOT EXISTS" p))
      (is (nil? (re-find #"NOT IN" p)))
      (is (re-find #"_s\.\"internal_uuid\" = \"bo_branch\"\.\"d_1\"\.\"internal_uuid\"" p)
          "correlated on the target by its qualified name — the predicate is
           dropped into SELECT, INSERT … SELECT and DELETE, all of which name
           the table that way and none of which alias it"))))
