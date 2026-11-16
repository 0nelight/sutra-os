(ns ^:unit platform.protection-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.branch-policy :as policy]
            [sutra.catalog.grants :as grants]
            [sutra.types.authority :as authority]
            [sutra.types.review :as review]))

(def ^:private schutz {:resources #{"ri.ot.werkzeug"} :projects #{"ri.p.finanzen"}})

(defn- row [rid project] {:task (str "object-types/" rid) :rid rid :name rid :project project})

(deftest a-protected-resource-changes-on-main-only-through-a-proposal
  (testing "a resource protected by itself"
    (is (some? (policy/protection-block schutz "main" [(row "ri.ot.werkzeug" "ri.p.lager")]))))
  (testing "a resource whose project protects all it holds — also one new in it"
    (is (some? (policy/protection-block schutz "main" [(row "ri.ot.neu" "ri.p.finanzen")]))))
  (testing "an unprotected resource lands directly — the solo default protects nothing"
    (is (nil? (policy/protection-block schutz "main" [(row "ri.ot.regal" "ri.p.lager")])))
    (is (nil? (policy/protection-block {:resources #{} :projects #{}} "main"
                                       [(row "ri.ot.werkzeug" "ri.p.lager")]))))
  (testing "off the trunk nothing is protected: a branch is where protected work is prepared"
    (is (nil? (policy/protection-block schutz "feature-x" [(row "ri.ot.werkzeug" "ri.p.lager")]))))
  (testing "the refusal names every protected resource of the write, and only those"
    (let [b (policy/protection-block schutz "main"
                                     [(row "ri.ot.werkzeug" "ri.p.lager")
                                      (row "ri.ot.regal" "ri.p.lager")])]
      (is (= ["ri.ot.werkzeug"] (mapv :rid (:protected b))))
      (is (= 403 (anomaly/status b))))))

(deftest what-the-instance-holds-is-not-held-back-for-a-proposal
  (let [none {:resources #{} :projects #{}}]
    (testing "an organization that widens access is the admin's on main: no proposal carries it"
      (is (nil? (policy/protection-block none "main" [{:rid "ri.o.privat" :section "organizations"
                                                      :name "privat" :expands-access true}]))))
    (testing "a branched resource that widens access still waits for a proposal"
      (is (some? (policy/protection-block none "main" [{:rid "ri.ds.frei" :section "datasets"
                                                       :name "frei" :expands-access true}]))))))

(deftest a-checkpoint-is-a-need-of-a-rule
  (let [rule {:when [:always] :needs [:justification]}
        t    {:task "object-types/x" :requires [(assoc rule :count 1)]}]
    (is (nil? (review/problems [rule])) "a rule may need a justification and name no approver")
    (is (seq (review/unmet t [] {:author "a"})) "without a justification it is open")
    (is (seq (review/unmet t [] {:author "a" :justification "   "})) "blank is no justification")
    (is (empty? (review/unmet t [] {:author "a" :justification "Quartalsabschluss"}))
        "with one it is met — no approver needed")
    (testing "with approvers too, both must hold"
      (let [both {:task "x"
                  :requires [{:when [:always] :by [:other] :count 1 :needs [:justification]}]}]
        (is (seq (review/unmet both [{:who "b"}] {:author "a"})))
        (is (seq (review/unmet both [] {:author "a" :justification "j"})))
        (is (empty? (review/unmet both [{:who "b"}] {:author "a" :justification "j"})))))
    (is (seq (review/problems [{:when [:always] :needs [:freitags]}])) "an unknown need is named")))

(deftest a-waiting-proposal-is-for-the-owners-of-what-it-waits-on
  (let [schema {:projects {"ri.p.lager" {:grants [{:principal "anna" :role :ontology-owner}
                                                  {:principal "lager-team" :role :ontology-owner}
                                                  {:principal "ben" :role :ontology-editor}]}}}
        db     (authority/database (authority/schema-facts schema)
                                   (authority/role-facts grants/standard-roles))
        tasks  [{:task "object-types/ri.ot.a" :name "regal" :project "ri.p.lager" :kind :breaking
                 :consumers ["einkauf"]}
                {:task "object-types/ri.ot.b" :name "fach" :project "ri.p.lager" :kind :accretion}]]
    (is (= #{"anna" "lager-team"} (authority/holders db :manage-permissions "ri.p.lager"))
        "owners are the holders of :manage-permissions — users and groups, editors not")
    (let [n (policy/review-notice {:id "p1" :title "t" :author "carl"} tasks
                                  ["object-types/ri.ot.a"]
                                  #(authority/holders db :manage-permissions %))]
      (is (= "review-requested" (:channel n)))
      (is (= [{:task "object-types/ri.ot.a" :name "regal" :kind "breaking"
               :owners ["anna" "lager-team"] :consumers ["einkauf"]}]
             (get-in n [:payload :tasks]))
          "only the open task, with its owners and whom it breaks"))
    (is (nil? (policy/review-notice {:id "p1"} tasks [] (constantly #{})))
        "nothing waits, nothing is sent")))
