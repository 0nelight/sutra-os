(ns ^:unit platform.git-pr-parity-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.branch-policy :as policy]))

(deftest the-new-policy-keys-default-to-off
  (testing "an instance that never configures them behaves exactly as before"
    (is (false? (:require-checks policy/default-policy)))
    (is (= [:squash :merge :fast-forward] (:merge-modes policy/default-policy))
        "every mode allowed by default; :squash first because it is the primary")))

(deftest require-checks-refuses-until-green
  (let [pol {:require-checks true}]
    (testing "a failed check blocks, and names which one"
      (let [r (policy/checks-block pol "sha1" (constantly [{:check "plan" :status :failed}]))]
        (is (str/includes? (:error r) "plan failed"))))
    (testing "a QUEUED check blocks too"
      (is (some? (policy/checks-block pol "sha1" (constantly [{:check "plan" :status :queued}])))))
    (testing "no check at all blocks — an unbuilt commit is not a green one"
      (is (some? (policy/checks-block pol "sha1" (constantly [])))))
    (testing "all passed clears"
      (is (nil? (policy/checks-block pol "sha1" (constantly [{:check "plan" :status :passed}])))))))

(deftest require-checks-is-a-no-op-without-a-repository
  (testing "most branches are not backed by a code repository, and those must not
            become unmergeable because the policy key exists"
    (is (nil? (policy/checks-block {:require-checks true} "sha1" nil)))
    (is (nil? (policy/checks-block {:require-checks true} nil (constantly []))))
    (is (nil? (policy/checks-block {:require-checks false} "sha1" (constantly []))))))

(deftest path-reviewers-name-who-is-missing
  (testing "a code repository's path rules name reviewers; the gate says who has not approved"
    (is (nil? (policy/reviewers-block [] [])))
    (is (nil? (policy/reviewers-block ["sec"] ["sec" "bob"])))
    (is (= ["sec"] (:missing-reviewers (policy/reviewers-block ["sec" "ops"] ["ops"]))))
    (is (some? (policy/reviewers-block ["sec"] ["a" "b" "c"]))
        "three approvals from others do not stand in for the named one")))

(deftest merge-mode-is-checked-against-the-target-policy
  (is (nil? (policy/merge-mode-block {:merge-modes [:squash :merge]} :squash)))
  (is (some? (policy/merge-mode-block {:merge-modes [:squash]} :merge)))
  (testing "a nil mode means the author expressed no preference and takes the
            policy's own — an installation that allows only :squash never has to
            say so twice"
    (is (nil? (policy/merge-mode-block {:merge-modes [:squash]} nil))))
  (testing "the refusal lists what IS allowed"
    (is (= [:squash] (:allowed (policy/merge-mode-block {:merge-modes [:squash]} :merge))))))

(deftest approval-policy-selects-reviewers-by-file-regex
  (let [policy [{:paths ["transforms/.*\\.py"] :reviewers ["data-sec"]}
                {:paths ["sutra\\.repo\\.edn"] :reviewers ["repo-admin"]}]]
    (testing "rules are ADDITIVE — a change touching two governed areas needs both
              reviews, not the last-declared one"
      (is (= #{"data-sec" "repo-admin"}
             (set (policy/approval-policy-reviewers
                   policy ["transforms/x.py" "sutra.repo.edn"])))))
    (testing "and a reviewer named by two rules is demanded once"
      (is (= ["r"] (policy/approval-policy-reviewers
                    [{:paths ["a"] :reviewers ["r"]} {:paths ["b"] :reviewers ["r"]}]
                    ["a" "b"]))))))

(deftest a-broken-regex-does-not-wedge-the-branch
  (testing "the policy file is authored by hand and is not otherwise validated;
            a typo in it must not make every proposal unmergeable, including the
            one that would fix the typo. This is a deliberate fail-OPEN, in
            exactly one narrow place — the fail-CLOSED rule (read it from the
            BASE branch) is what actually stops self-exemption."
    (is (= [] (policy/approval-policy-reviewers
               [{:paths ["*unclosed["] :reviewers ["x"]}] ["a.py"])))))

(deftest an-empty-policy-demands-nothing
  (is (= [] (policy/approval-policy-reviewers [] ["transforms/x.py"])))
  (is (= [] (policy/approval-policy-reviewers nil ["transforms/x.py"]))))

(def ^:private regel [{:when [:always] :by [:other] :count 1}])

(def ^:private aufgaben
  [{:task "objects/lieferant" :fingerprint "f1" :requires regel}
   {:task "objects/rechnung"  :fingerprint "f2" :requires regel}])

(defn- review [wer & pairs]
  {:approver wer :task-verdicts (into {} (for [[t v fp] (partition 3 pairs)]
                                           [t {:verdict v :fingerprint fp}]))})

(def ^:private no-owner (constantly false))

(defn- p [& reviews] {:author "a" :reviews (vec reviews)})

(deftest one-rejected-task-blocks-the-whole-merge
  (let [b (policy/tasks-block (p (review "b" "objects/rechnung" "rejected" "f2")) aufgaben
                              no-owner)]
    (is (some? b))
    (is (= ["objects/rechnung"] (:rejected-tasks b)))
    (testing "and it names the RESOURCE, not the proposal"
      (is (str/includes? (:error b) "objects/rechnung")))))

(deftest every-task-needs-its-own-approval
  (testing "approving one resource does not approve the other"
    (let [b (policy/tasks-block (p (review "b" "objects/lieferant" "approved" "f1")) aufgaben
                                no-owner)]
      (is (= ["objects/rechnung"] (:open-tasks b)))))
  (testing "all of them approved clears the gate"
    (is (nil? (policy/tasks-block (p (review "b"
                                          "objects/lieferant" "approved" "f1"
                                          "objects/rechnung"  "approved" "f2"))
                               aufgaben no-owner)))))

(deftest an-approval-does-not-survive-the-resource-changing
  (testing "approve, then edit, then merge is the hole this closes"
    (let [b (policy/tasks-block (p (review "b" "objects/lieferant" "approved" "f1"))
                             [{:task "objects/lieferant" :fingerprint "f2" :requires regel}]
                                no-owner)]
      (is (= ["objects/lieferant"] (:stale-tasks b)))
      (is (str/includes? (:error b) "stale")
          "and it says WHY, rather than reporting the approval as missing"))))

(deftest without-tasks-the-gate-is-a-no-op
  (testing "a proposal whose content is not ontological has none, and must still merge"
    (is (nil? (policy/tasks-block (p) nil no-owner)))
    (is (nil? (policy/tasks-block (p) [] no-owner))))
  (testing "a task no rule requires still refuses a rejection — an objection is not a count"
    (let [frei (mapv #(assoc % :requires []) aufgaben)]
      (is (some? (policy/tasks-block (p (review "b" "objects/lieferant" "rejected" "f1")) frei
                                     no-owner)))
      (is (nil? (policy/tasks-block (p) frei no-owner))))))
