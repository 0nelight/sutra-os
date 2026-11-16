(ns ^:unit platform.access-requests-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.access.requests :as ar]
            [sutra.types.review :as review]))

(deftest a-request-names-what-it-lacks
  (is (nil? (ar/problems {:kind :project-role :project-rid "ri.p.l" :role "ontology-editor"
                          :principal "u"})))
  (is (seq (ar/problems {:kind :project-role :project-rid "ri.p.l" :principal "u"})) "no role")
  (is (seq (ar/problems {:kind :group-membership :principal "u"})) "no group")
  (is (re-find #"unknown request kind" (first (ar/problems {:kind :weltherrschaft})))))

(deftest the-owners-of-its-project-decide
  (let [[t] (ar/tasks {:id "r1" :kind :project-role :project-rid "ri.p.l" :role "ontology-editor"
                       :principal "u" :author "u"})]
    (is (= "ri.p.l" (:project t)))
    (is (= "r1" (:fingerprint t)) "a request does not change: a verdict on it never goes stale")
    (is (review/eligible? t {:who "o" :owner? true} {:author "u"}))
    (is (not (review/eligible? t {:who "x" :owner? false} {:author "u"})))
    (is (seq (review/unmet t [] {:author "u"})))
    (is (empty? (review/unmet t [{:who "o" :owner? true}] {:author "u"})))))

(deftest what-no-project-holds-is-the-admins
  (let [[t] (ar/tasks {:id "r2" :kind :group-membership :group-id "g" :group-project nil
                       :principal "u" :author "u"})]
    (is (not (review/eligible? t {:who "o" :owner? true} {:author "u"}))
        "an owner of something else is not enough")
    (is (review/eligible? t {:who "admin"} {:author "u"}))))
