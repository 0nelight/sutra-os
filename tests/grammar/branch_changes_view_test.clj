(ns ^:unit grammar.branch-changes-view-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.kit.remote :as rd]
            [sutra.viz.lineage-branch :as lb]))

(def ^:private answer
  {:branch "b" :target "main" :proposal {:id "7"}
   :tasks [{:section "object-types" :name "flug"}]})

(deftest the-trunk-has-no-changes-to-show
  (is (= {:show :main} (lb/changes-view "main" rd/absent)))
  (is (= {:show :main} (lb/changes-view nil rd/loading))))

(deftest a-branch-waits-for-its-own-answer
  (is (= {:show :loading} (lb/changes-view "b" rd/loading)))
  (is (= {:show :loading} (lb/changes-view "c" answer)) "the answer is about another branch")
  (is (= {:show :failed :reason "HTTP 500"} (lb/changes-view "b" (rd/failed "HTTP 500")))))

(deftest the-answer-says-the-changes
  (let [v (lb/changes-view "b" answer)]
    (is (= :changes (:show v)))
    (is (= "main" (:target v)))
    (is (= [[:object-type :flug]] (lb/view-modified-refs v))))
  (is (= [] (lb/view-modified-refs (lb/changes-view "b" rd/loading)))))
