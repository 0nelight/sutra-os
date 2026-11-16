(ns platform.pg-fork-detached-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [sutra.store.pg.layout :as layout]))

(deftest fork-of-branch-without-owned-tables-creates-its-detached-table
  (let [K    {:tables  {"bo_parent" #{"_detached"}}
              :columns {["bo_parent" "_detached"]
                        [{:name "table_name" :type "text" :not-null? true}
                         {:name "sys_from" :type "bigint"} {:name "sys_to" :type "bigint"}]}
              :owned   {}}
        ops  (pr-str (layout/fork-plan K "parent" "child" {} 7))
        made (str/index-of ops "CREATE TABLE IF NOT EXISTS \\\"bo_child\\\".\\\"_detached\\\"")
        used (str/index-of ops "INSERT INTO \\\"bo_child\\\".\\\"_detached\\\"")]
    (is (some? used))
    (is (some? made) "the child's _detached is created before rows are copied into it")
    (is (and made used (< made used)))))
