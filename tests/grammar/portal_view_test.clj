(ns ^:unit grammar.portal-view-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.portal :as portal]
            [sutra.kit.remote :as rd]))

(def ^:private a {:id 1 :name :a})
(def ^:private b {:id 2 :name :b :workspaces [:ws1]})

(deftest the-portal-waits-for-both-lists
  (is (= {:show :loading} (portal/view rd/loading rd/loading)))
  (is (= {:show :loading} (portal/view [a] rd/loading))
      "not the whole tray first and the shelves later")
  (is (= {:show :loading} (portal/view rd/loading [])) "not \"no apps granted\" on a cold start"))

(deftest a-failure-is-said
  (is (= {:show :failed :reason "HTTP 500"} (portal/view (rd/failed "HTTP 500") [])))
  (testing "a workspace list that failed is no shelf"
    (is (= :list (:show (portal/view [a] (rd/failed "x")))))))

(deftest what-the-lists-say
  (is (= :no-apps (:show (portal/view [] []))))
  (is (= :no-apps (:show (portal/view rd/absent rd/absent))))
  (is (= :all-assigned (:show (portal/view [b] [{:id :ws1}]))))
  (let [v (portal/view [a b] [{:id :ws1}])]
    (is (= :list (:show v)))
    (is (= [a] (:loose v)))
    (is (= [a b] (:apps v))))
  (is (= [a b] (:loose (portal/view [a b] []))) "without shelves every app is in the tray"))

(deftest a-workspace-page-renders-objects-only-with-its-tables
  (is (= {:show :loading} (portal/workspace-tables rd/loading)))
  (is (= {:show :failed :reason "HTTP 500"} (portal/workspace-tables (rd/failed "HTTP 500"))))
  (is (= {:show :failed :reason nil} (portal/workspace-tables rd/absent))
      "no descriptor is no tables, not an empty workspace")
  (is (= {:show :ready :descriptor {:id 1}} (portal/workspace-tables {:id 1})))
  (is (thrown? Exception (portal/workspace-tables nil))
      "nil is no state"))
