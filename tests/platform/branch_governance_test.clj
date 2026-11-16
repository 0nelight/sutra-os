(ns ^:unit platform.branch-governance-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing]]
            [sutra.foundation.side-effect :as fx]
            [sutra.catalog.branch-policy :as policy]
            [sutra.branching.push]
            [sutra.store.records :as records]
            [sutra.types.review]))

(def ^:private ->policy #'policy/->policy)
(def ^:private effective #'policy/effective)

(deftest a-null-column-does-not-override-the-default
  (testing "NULL means 'not set here', not false/0 — that is what makes a named
            row an OVERRIDE of the `*` default rather than a replacement"
    (is (= {} (->policy {:protected nil :review nil :retention-days nil :inactive-days nil})))
    (is (= {:protected true} (->policy {:protected 1 :review nil})))
    (is (= {:protected false :review [{:when [:always] :by [:other]}]}
           (->policy {:protected 0 :review "[{:when [:always] :by [:other]}]"})))))

(deftest enrollment-default-applies-to-every-branch
  (let [rows {"*" {:branch "*" :protected 1 :review "[{:when [:always] :by [:other]}]"}}]
    (doseq [b ["anything" "main"]]
      (is (= (assoc policy/default-policy :protected true :review [{:when [:always] :by [:other]}])
             (effective rows b))
          (str b " inherits the enrollment default")))))

(deftest a-branch-row-overrides-the-default-field-by-field
  (let [rows {"*"    {:branch "*" :protected 1 :review "[]" :retention-days 30
                      :inactive-days 7}
              "spike" {:branch "spike" :protected 0}}]
    (testing "the named row wins where it speaks"
      (is (false? (:protected (effective rows "spike")))))
    (testing "and inherits where it is silent — the trap a REPLACE semantics would spring"
      (is (= [] (:review (effective rows "spike"))))
      (is (= 30 (:retention-days (effective rows "spike"))))
      (is (= 7  (:inactive-days (effective rows "spike")))))))

(deftest with-no-rows-at-all-nothing-is-governed
  (is (= policy/default-policy (effective {} "main")))
  (is (false? (:protected (effective {} "main")))))

(deftest an-undeclared-review-is-the-default-rule
  (testing "only a breaking change to a resource its author does not own waits"
    (is (= sutra.types.review/default-rules (policy/review-rules policy/default-policy))))
  (testing "an empty :review is a decision too — nothing waits"
    (is (= [] (policy/review-rules {:review []})))))

(deftest auto-land-is-off-until-somebody-says-otherwise
  (testing "collapsing the handshake is a decision about who is in the room, so an
            instance that never states it keeps the three steps"
    (is (false? (:auto-land policy/default-policy)))
    (is (= {} (->policy {:auto-land nil})) "a NULL column states nothing")
    (is (= {:auto-land true} (->policy {:auto-land 1})))))

(def ^:private wartet
  {:task "object-types/x" :fingerprint "f" :project "lager"
   :requires [{:when [:breaking :foreign] :by [:owner] :count 1}]})

(defn- urteil [wer v fp]
  {:approver wer :task-verdicts {"object-types/x" {:verdict v :fingerprint fp}}})

(defn- nur [& owner] (fn [who project] (and (= "lager" project) (contains? (set owner) who))))

(deftest auto-land-reaches-no-gate-that-judges-the-change
  (let [solo (assoc policy/default-policy :auto-land true :require-checks true)]
    (testing "red checks still refuse"
      (is (some? (policy/checks-block solo "abc123" (constantly [{:check "ci" :status :failed}])))))
    (testing "a rejected task still refuses, even one no rule requires"
      (is (some? (policy/tasks-block {:author "me"
                                      :reviews [(urteil "me" policy/rejected-verdict "f")]}
                                     [(assoc wartet :requires [])] (nur)))))
    (testing "a task no rule requires needs no verdict"
      (is (nil? (policy/tasks-block {:author "me" :reviews []} [(assoc wartet :requires [])]
                                    (nur)))))))

(deftest a-breaking-foreign-change-waits-for-an-owner
  (testing "open until an owner signs"
    (is (some? (policy/tasks-block {:author "me" :reviews []} [wartet] (nur "owner")))))
  (testing "an approval from someone who does not own the resource does not count"
    (is (some? (policy/tasks-block {:author "me"
                                    :reviews [(urteil "other" policy/approved-verdict "f")]}
                                   [wartet] (nur "owner")))))
  (testing "an owner's fresh approval clears it"
    (is (nil? (policy/tasks-block {:author "me"
                                   :reviews [(urteil "owner" policy/approved-verdict "f")]}
                                  [wartet] (nur "owner")))))
  (testing "an owner's approval of an earlier version is stale, and the refusal says so"
    (is (re-find #"stale"
                 (:error (policy/tasks-block {:author "me"
                                              :reviews
                                              [(urteil "owner" policy/approved-verdict "alt")]}
                                             [wartet] (nur "owner")))))))

(deftest whether-an-approval-counts-is-decided-at-landing
  (testing "race: the owner approves, then loses the role before the merge — the approval no longer counts"
    (let [p {:author "me" :reviews [(urteil "owner" policy/approved-verdict "f")]}]
      (is (nil? (policy/tasks-block p [wartet] (nur "owner"))) "while they own it")
      (is (some? (policy/tasks-block p [wartet] (nur))) "after the grant is gone")))
  (testing "and the other way: someone approves, then is made owner — it counts from then on"
    (let [p {:author "me" :reviews [(urteil "neu" policy/approved-verdict "f")]}]
      (is (some? (policy/tasks-block p [wartet] (nur "owner"))))
      (is (nil? (policy/tasks-block p [wartet] (nur "owner" "neu")))))))

(deftest a-rule-tightened-after-approval-reopens-the-task
  (testing "race: approved under the default, then main is set to four eyes with two approvers"
    (let [p  {:author "me" :reviews [(urteil "owner" policy/approved-verdict "f")]}
          zwei (assoc wartet :requires [{:when [:always] :by [:other] :count 2}])]
      (is (nil? (policy/tasks-block p [wartet] (nur "owner"))))
      (is (re-find #"2× \[:other\]" (:error (policy/tasks-block p [zwei] (nur "owner"))))))))

(defn- b [m] (merge {:name "feature/x" :age-days 100 :protected false} m))

(deftest the-windows-are-the-documented-ones
  (testing ""
    (is (= 7  (:inactive-days policy/default-policy)))
    (is (= 28 (:retention-days policy/default-policy))))
  (testing "fixed,
            not a policy field"
    (is (= 7 policy/purge-after-days))
    (is (not (contains? policy/default-policy :purge-days)))))

(deftest a-branch-with-no-window-is-never-stale
  (testing "no --days and no configured retention ⇒ nothing is stale, however old"
    (is (false? (policy/stale? (b {:age-days 3650}) nil))))
  (testing "an explicit sweep argument overrides the branch's own window"
    (is (true?  (policy/stale? (b {:age-days 10 :retention-days 30}) 7)))
    (is (false? (policy/stale? (b {:age-days 10 :retention-days 5}) 30)))))

(deftest the-sweep-never-touches-what-it-must-not
  (testing "the trunk"
    (is (false? (policy/stale? (b {:name "main"}) 30))))
  (testing "a protected branch — it is protected precisely because it matters"
    (is (false? (policy/stale? (b {:protected true}) 30))))
  (testing "an already-closed or merged branch (idempotent sweeps)"
    (is (false? (policy/stale? (b {:closed-at "2026-01-01"}) 30)))
    (is (false? (policy/stale? (b {:merged-at "2026-01-01"}) 30))))
  (testing "a branch whose tip commit date could not be read"
    (is (false? (policy/stale? (b {:age-days nil}) 30)))))

(deftest staleness-is-inclusive-at-the-boundary
  (is (false? (policy/stale? (b {:age-days 29}) 30)))
  (is (true?  (policy/stale? (b {:age-days 30}) 30))))

(deftest retention-runs-in-two-stages
  (testing "Active → Inactive at the inactive window, → Closed at the retention one"
    (is (= :mark-inactive
           (policy/retention-action (b {:age-days 10 :inactive-days 7 :retention-days 28}) {})))
    (is (= :close
           (policy/retention-action (b {:age-days 30 :inactive-days 7 :retention-days 28}) {}))))
  (testing "closing WINS over the label — a branch past both goes straight out"
    (is (= :close
           (policy/retention-action (b {:age-days 30 :inactive-days 7 :retention-days 28
                                        :inactive-at "2026-01-01"}) {}))))
  (testing "the label is idempotent: an already-labelled branch is not re-labelled"
    (is (nil? (policy/retention-action (b {:age-days 10 :inactive-days 7
                                           :inactive-at "2026-01-01"}) {})))))

(deftest a-branch-that-moves-again-is-reactivated
  (testing ""
    (is (= :reactivate
           (policy/retention-action (b {:age-days 1 :inactive-days 7
                                        :inactive-at "2026-01-01"}) {}))))
  (testing "and the label never outlives the rule that produced it"
    (is (= :reactivate
           (policy/retention-action (b {:age-days 900 :inactive-days nil
                                        :inactive-at "2026-01-01"}) {})))))

(deftest the-inactive-stage-is-opt-in-too
  (testing "no :inactive-days ⇒ no label, however old, so arming the runner on an
            unconfigured instance does nothing"
    (is (nil? (policy/retention-action (b {:age-days 3650}) {}))))
  (testing "and the exemptions hold at BOTH stages"
    (doseq [ex [{:name "main"} {:protected true} {:closed-at "2026-01-01" :ended-days 1}]]
      (is (nil? (policy/retention-action (b (assoc ex :age-days 100 :inactive-days 7)) {}))
          (str "exempt: " (pr-str ex))))))

(deftest an-owner-may-land-their-branch-and-a-stranger-may-not
  (is (true?  (policy/owner-allowed? ["alice"] "alice" false)))
  (is (false? (policy/owner-allowed? ["alice"] "bob" false))))

(deftest the-admin-has-the-owner-role-everywhere
  (testing ""
    (is (true? (policy/owner-allowed? ["alice"] "bob" true)))))

(deftest a-branch-with-no-owner-has-only-the-admin
  (testing "main has no owner row until one is named: nobody may make themselves its owner"
    (is (false? (policy/owner-allowed? [] "anyone" false)))
    (is (false? (policy/owner-allowed? nil "anyone" false)))
    (is (true? (policy/owner-allowed? [] "admin" true))))
  (testing "a refusal names the branch and the act"
    (is (= "main" (:branch (policy/owner-refusal [] "bob" "main" "change its roles" false))))))

(deftest an-unprotected-branch-is-not-blocked
  (let [policy {:protected false}]
    (is (nil? (policy/write-block policy "feature/x")))))

(deftest a-protected-branch-refuses-a-direct-change-and-says-how-to-proceed
  (let [blk (policy/write-block {:protected true} "main")]
    (is (some? blk))
    (is (= "main" (:branch blk)))
    (is (re-find #"proposal" (:error blk))
        "the refusal must name the remedy, not just say no")
    (is (re-find #"propose" (:remedy blk)))))

(deftest no-branch-means-nothing-to-block
  (let [policy {:protected true}]
    (is (nil? (policy/write-block policy nil)))))

(deftest a-saved-graph-lands-on-a-protected-trunk
  (testing "a lineage graph is a Kompass FILE: no exporter writes it to a .ent
            and no push can carry it, so the proposal route the refusal names
            does not exist — the gate would make it unsavable, not reviewable"
    (let [policy {:protected true}]
      (is (nil? (policy/write-block policy "main" [{:type "graph" :verb "create"}
                                                   {:type "graph" :verb "update"}]))))))

(deftest one-ontology-row-takes-the-whole-batch-back-through-the-gate
  (let [policy {:protected true}]
    (is (some? (policy/write-block policy "main" [{:type "graph"       :verb "create"}
                                                  {:type "object-type" :verb "create"}]))
        "a mixed batch commits as ONE commit — the exemption cannot be per-row")))

(deftest an-empty-batch-is-not-an-exemption
  (let [policy {:protected true}]
    (is (some? (policy/write-block policy "main" [])))
    (is (some? (policy/write-block policy "main" nil))
        "'nothing staged' must not read as 'safe to commit on a protected branch'")))

(deftest on-the-trunk-every-side-effect-fires
  (testing "suppression is about a TESTING environment; main is not one"
    (doseq [k (keys fx/side-effect-kinds)]
      (is (= :allow (fx/side-effect-verdict "main" #{} k)) (str k " on main")))))

(deftest with-no-request-in-flight-nothing-is-suppressed
  (testing "a nil branch is a platform path (bootstrap, migration, a background
            poll, a test), not a user testing something on a branch"
    (doseq [k (keys fx/side-effect-kinds)]
      (is (= :allow (fx/side-effect-verdict nil #{} k))))))

(deftest on-a-branch-webhooks-and-notifications-are-skipped
  (is (= :suppress (fx/side-effect-verdict "feature/x" #{} :webhooks)))
  (is (= :suppress (fx/side-effect-verdict "feature/x" #{} :notifications))))

(deftest but-an-external-call-fails-outright
  (testing ""
    (is (= :fail (fx/side-effect-verdict "feature/x" #{} :external-calls)))))

(deftest the-per-action-opt-in-restores-one-class-only
  (is (= :allow    (fx/side-effect-verdict "feature/x" #{:webhooks} :webhooks)))
  (is (= :suppress (fx/side-effect-verdict "feature/x" #{:webhooks} :notifications))
      "opting a webhook in must not also un-suppress notifications")
  (is (= :allow    (fx/side-effect-verdict "feature/x" #{:external-calls} :external-calls))))

(def ^:private on-feature {:branch "feature/x" :allowed #{}})

(deftest a-suppressed-effect-is-recorded-not-silently-dropped
  (let [fx (fx/side-effect on-feature :webhooks {:webhook :notify-erp})]
    (is (false? (fx/admit! fx)))
    (is (= {:kind :webhooks :verdict :suppress :branch "feature/x"
            :detail {:webhook :notify-erp}}
           fx)
        "the record must identify WHICH effect, or the report is useless")))

(deftest an-external-call-on-a-branch-throws-a-403
  (let [e (is (thrown? clojure.lang.ExceptionInfo
                       (fx/admit!
                        (fx/side-effect on-feature :external-calls {:source :mro}))))]
    (is (= 403 (anomaly/status (ex-data e))))
    (is (re-find #":on-branch" (:remedy (ex-data e)))
        "the refusal must name the opt-in that would permit it")))

(deftest the-gate-carries-the-opt-ins
  (is (true? (fx/admit! (fx/side-effect (assoc on-feature :allowed #{:webhooks})
                                        :webhooks {:webhook :a}))))
  (is (true? (fx/admit! (fx/side-effect {:branch nil :allowed #{}} :external-calls {})))
      "no request in flight judges as the trunk")
  (is (= {:branch "feature/x" :allowed #{:webhooks}} (policy/request-gate "feature/x" [:webhooks]))
      "the request's branch goes into the gate a run carries"))

(deftest the-report-is-a-function-of-the-records
  (testing "one run's report is built from its own effects, so it never carries
            another action's skips"
    (let [hook  (fx/side-effect on-feature :webhooks {:webhook :a})
          fired (fx/side-effect {:branch "main" :allowed #{}} :webhooks {:webhook :b})]
      (is (= {:suppressed-side-effects [hook]} (policy/held-back [hook fired])))
      (is (= {} (policy/held-back [fired]))
          "nothing held back, nothing reported"))))

(deftest the-file-plane-verdict-is-its-own-answer
  (testing "no repository took part"
    (is (= :none (policy/file-plane-status nil)))
    (is (= :none (policy/file-plane-status {}))))
  (testing "a half is owed but has not run — the merge records it and returns"
    (is (= :pending (policy/file-plane-status nil true)))
    (is (= :pending (policy/file-plane-status {} true))
        ":none would read as 'no repository took part', which is the opposite"))
  (testing "the git half threw — nothing was written on that side"
    (is (= :error (policy/file-plane-status {:error "remote rejected"}))))
  (testing "a conflict is not an error: the TARGET's files silently stand"
    (is (= :conflicts (policy/file-plane-status {"ri.repo.a" {:conflicts ["x.py"]}})))
    (is (= :conflicts (policy/file-plane-status {"ri.repo.a" {:sha "abc"}
                                                 "ri.repo.b" {:conflicts ["x.py"]}}))
        "one conflicted repo is enough — a partial landing is not a landing"))
  (testing "every participating repo landed"
    (is (= :ok (policy/file-plane-status {"ri.repo.a" {:sha "abc"}})))))

(def ^:private absagen #'sutra.branching.push/pre-receive-rejections)

(def ^:private repos
  (doto (java.io.File/createTempFile "sutra-gov-git" "") (.delete) (.mkdirs)))

(defn- absage
  [branch {:keys [offen? geschützt? old]}]
  (get (absagen repos "ri.repo.x"
                [{:ref (str "refs/heads/" branch) :old old :new "abc123"}]
                {:protected?       (constantly (boolean geschützt?))
                 :changeset-open? (constantly (boolean offen?))})
       (str "refs/heads/" branch)))

(deftest ein-zweig-ohne-changeset-wird-nicht-angenommen
  (testing "no open proposal — refused, and the refusal names the one door"
    (let [why (absage "mein-zweig" {:offen? false})]
      (is (some? why))
      (is (re-find #"no open proposal" why))
      (is (re-find #"propose --source mein-zweig" why))))

  (testing "with one, the push is this gate's business no longer"
    (is (nil? (absage "mein-zweig" {:offen? true}))))

  (testing "an UPDATE to the branch is judged the same as its creation — a
            changeset that was withdrawn must not leave the branch pushable"
    (is (some? (absage "mein-zweig" {:offen? false :old "aaa111"}))))

  (testing "the trunk is exempt here and refused one clause earlier, for the
            better reason: it is protected, which no changeset would change"
    (is (re-find #"is protected" (absage records/trunk {:offen? false :geschützt? true})))
    (is (nil? (absage records/trunk {:offen? false :geschützt? false}))))

  (testing "NO valve. `sketch/` exempts a push from the GATES — it says nothing
            about whether the work belongs to a change, and a prefix anyone can
            type would make this a request rather than a rule"
    (is (some? (absage "sketch/versuch" {:offen? false}))))

  (testing "and a caller that does not hand the question in is not answered by
            a guess — an older caller keeps the behaviour it was written against"
    (is (nil? (get (absagen repos "ri.repo.x"
                            [{:ref "refs/heads/mein-zweig" :new "abc123"}]
                            {:protected? (constantly false)})
                   "refs/heads/mein-zweig")))))

(deftest ended-branches-lose-their-data-seven-days-later
  (testing "the clock runs from the
            END, not from the last commit"
    (is (= :purge (policy/retention-action (b {:closed-at "2026-01-01" :ended-days 7}) {})))
    (is (= :purge (policy/retention-action (b {:merged-at "2026-01-01" :ended-days 9}) {}))))
  (testing "inside the seven days the ended branch is left alone"
    (is (nil? (policy/retention-action (b {:closed-at "2026-01-01" :ended-days 3}) {}))))
  (testing "a landing carries the schema, not the rows: a merged branch waits the same seven days"
    (is (nil? (policy/retention-action (b {:merged-at "2026-01-01" :ended-days 0}) {})))
    (is (= :purge (policy/retention-action (b {:merged-at "2026-01-01" :ended-days 7}) {})))
    (is (nil? (policy/retention-action (b {:merged-at "2026-01-01" :ended-days 9})
                                       {:open-proposals #{"feature/x"}}))))
  (testing "a purged branch is only its name now — nothing left to delete"
    (is (nil? (policy/retention-action (b {:closed-at "2026-01-01" :ended-days 900
                                           :purged-at "2026-01-08" :age-days nil}) {}))))
  (testing "a closed branch whose data is already gone still purges its remains"
    (is (= :purge (policy/retention-action (b {:closed-at "2026-01-01" :ended-days 30
                                               :age-days nil}) {}))))
  (testing "the trunk and a protected branch are exempt here too"
    (doseq [ex [{:name "main"} {:protected true}]]
      (is (nil? (policy/retention-action (b (assoc ex :closed-at "2026-01-01" :ended-days 900)) {}))
          (str "exempt: " (pr-str ex))))))

(deftest an-open-proposal-holds-the-branch-open
  (testing "neither closed nor emptied"
    (let [opts {:open-proposals #{"feature/x"}}]
      (is (nil? (policy/retention-action (b {:age-days 30 :retention-days 28}) opts)))
      (is (nil? (policy/retention-action (b {:closed-at "2026-01-01" :ended-days 900}) opts)))))
  (testing "the label still lands, because that is what asks the author to act"
    (is (= :mark-inactive
           (policy/retention-action (b {:age-days 10 :inactive-days 7})
                                    {:open-proposals #{"feature/x"}}))))
  (testing "and a branch nobody proposed is swept as before"
    (is (= :close (policy/retention-action (b {:age-days 30 :retention-days 28})
                                           {:open-proposals #{"other"}})))))

(deftest a-conflicting-git-half-is-a-verdict-not-a-retry
  (testing "the git half of a landing had two outcomes — gone, or try again — and a
            conflict fell into 'gone': dropped, unrecorded, and the projection stayed
            behind the ontology with nothing to say so. It is a third outcome."
    (is (= :done (policy/git-half-outcome {:ri/onto {:sha "abc"}})))
    (is (= :unresolved (policy/git-half-outcome {:ri/onto {:conflicts ["notes/a.md"]}}))
        "and at once — no attempt is spent on a verdict no retry can change")
    (testing "every other failure keeps waiting — it never gives up, it waits longer"
      (is (= :retry (policy/git-half-outcome {:error "git down"})))
      (is (= :retry (policy/git-half-outcome {:error "git down"}))
          "an outage of any length still heals itself; a ceiling here would park a half
           that a later round would have carried, and the drift is told by the
           git-projection-lag check, not by this counter"))
    (testing "the reason survives with it"
      (is (= "git down" (policy/git-half-reason {:error "git down"})))
      (is (re-find #"notes/a\.md"
                   (policy/git-half-reason {:ri/onto {:conflicts ["notes/a.md"]}}))))))

(deftest the-first-retry-does-not-wait-the-later-ones-do
  (testing "the tick's own round IS the first wait — a backoff on top of it would only
            double the delay before the common case (a blip) heals"
    (is (= 0 (policy/git-half-backoff-ms 1)))
    (is (= 60000 (policy/git-half-backoff-ms 2)))
    (is (= 120000 (policy/git-half-backoff-ms 3)))
    (is (= 3600000 (policy/git-half-backoff-ms 99)) "capped at an hour, as a dead effect is")
    (testing "the cap IS the brake: an hour between tries, not a count that gives up"
      (is (= 3600000 (policy/git-half-backoff-ms 1000))))))

(deftest a-preview-that-cannot-be-judged-keeps-its-own-name
  (testing "the state a merge check reports is the state the preview was in — only
            genuine emptiness becomes :empty, because only :empty means there is
            nothing to land"
    (doseq [store [:gone :no-base :base-unreadable]]
      (is (= store
             (:status (policy/merge-preview-of {:status store :diff [] :conflicts []})))
          (str (name store) " is not folded into :empty")))
    (is (= :empty
           (:status (policy/merge-preview-of {:status :clean :diff [] :conflicts []}))))
    (is (= :clean
           (:status (policy/merge-preview-of
                     {:status :clean :diff [{:table "x"}] :conflicts []}))))
    (is (= :conflicts
           (:status (policy/merge-preview-of
                     {:status :clean :diff [] :conflicts [{:table "x"}]}))))))

(deftest a-merge-check-blocks-the-landing-and-names-the-way-out
  (testing "an unjudged proposal never lands — and the refusal says why and what to do,
            because a refusal that only says no sends the author to the wrong place"
    (doseq [store [:gone :no-base :base-unreadable]]
      (let [b (policy/merge-check-block store)]
        (is (some? b) (str (name store) " blocks"))
        (is (seq (:error b)))
        (is (seq (:remedy b)) (str (name store) " names a remedy"))
        (is (= (name store) (:preview-status b))))))
  (testing "nothing else blocks here: an empty merge has nothing to land, which is a
            verdict, not a failure to reach one"
    (doseq [store [:empty :clean :conflicts nil]]
      (is (nil? (policy/merge-check-block store))
          (str (pr-str store) " is not a merge-check block")))))
