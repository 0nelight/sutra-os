(ns ^:unit platform.proposal-review-eligibility-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.catalog.grants :as grants]
            [sutra.types.review :as review]
            [sutra.governance.merge :as bm]))

(def ^:private owner-rule [{:when [:breaking :foreign] :by [:owner] :count 1}])

(defn- tasks [author-owns]
  (for [t [{:task "object-types/ri.ot.1" :section "object-types" :project "ri.p.1" :kind :breaking}
           {:task "transforms/ri.t.1"    :section "transforms"   :project "ri.p.2" :kind :breaking}
           {:task "datasets/ri.d.1"      :section "datasets"     :project nil
            :kind :breaking}]]
    (assoc t :requires (review/requires owner-rule t {:author "a" :author-owns? author-owns}))))

(defn- eligible [ops ts]
  (let [owns (grants/owner-of {:project-operations ops})]
    (vec (for [t ts
               :when (review/eligible? t
                                       {:who "r"
                                        :owner?
                                          (or (nil? (:project t)) (owns (:project t)))}
                                       {:author "a"})]
           (:task t)))))

(deftest an-owner-may-judge-only-their-own-projects-breaking-tasks
  (is (= ["object-types/ri.ot.1" "datasets/ri.d.1"]
         (eligible {"ri.p.1" #{:read :create :update :delete :manage-permissions}}
                   (tasks (constantly false))))))

(deftest an-editor-owns-nothing
  (testing "`:manage-permissions` is the owner's alone — an editor changes a resource, they do not judge it"
    (is (= ["datasets/ri.d.1"]
           (eligible {"ri.p.1" #{:read :create :update}} (tasks (constantly false)))))))

(deftest a-resource-without-a-project-is-everyones
  (testing "nobody to ask means nobody waits: no rule applies to it"
    (is (empty? (:requires (last (tasks (constantly false))))))))

(deftest an-unrestricted-principal-owns-every-project
  (is (true? ((grants/owner-of {:project-operations grants/all}) "ri.p.any")))
  (is (= 3 (count (eligible grants/all (tasks (constantly false)))))))

(deftest what-the-author-owns-needs-no-judge
  (testing "the default rule asks only for a FOREIGN breaking change"
    (let [ts (tasks #{"ri.p.1"})]
      (is (empty? (:requires (first ts))) "the author owns ri.p.1")
      (is (seq (:requires (second ts)))))))

(deftest two-owners-cover-a-proposal-between-them
  (testing "per-task eligibility does not deadlock the merge: each task finds its own owner"
    (is (= ["object-types/ri.ot.1" "datasets/ri.d.1"]
           (eligible {"ri.p.1" #{:manage-permissions}} (tasks (constantly false)))))
    (is (= ["transforms/ri.t.1" "datasets/ri.d.1"]
           (eligible {"ri.p.2" #{:manage-permissions}} (tasks (constantly false)))))))

(deftest a-task-carries-the-project-and-kind-of-the-version-being-proposed
  (testing "read off the entity in the diff, not looked up in the live schema: an ADDED resource
  does
            not exist there yet and a REMOVED one no longer will"
    (let [rows (bm/preview-tasks
                {:diff [{:table "object-types/ri.ot.1" :section "object-types" :rid "ri.ot.1"
                         :status "added"
                         :project "ri.p.1" :name "widget" :fingerprint "f1" :kind :accretion}]})]
      (is (= [{:task "object-types/ri.ot.1" :section "object-types" :rid "ri.ot.1" :name "widget"
               :project "ri.p.1" :status "added" :kind :accretion :fingerprint "f1"}]
             rows)))))
