(ns ^:integration platform.scenario-lifecycle
  (:require [sutra.store.access.scope :as ctx]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.catalog.grants :as grants]
            [sutra.actions.dispatch :as c]
            [sutra.governance.branches :as onto-branches]
            [sutra.scenarios.apply :as scenario]
            [sutra.store.seat :as seat]
            [sutra.actions.action :as action]
            [sutra.store.world :as world]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-stores-only)

(defn- scenario-name []
  (str "scenario/test-app/" (subs (str (random-uuid)) 0 8)))

(defn- fresh-scenario
  []
  (let [nm (scenario-name)]
    (scenario/create-scenario-branch! (ctx/store) {:parent "main" :name nm})
    nm))

(defn- drop! [nm] (try (onto-branches/drop-branch! (ctx/store) nm) (catch Throwable _ nil)))
(defn- branch-exists? [nm] (world/exists? (ctx/store) nm))

(deftest seal-is-durable
  (let [nm (fresh-scenario)]
    (try
      (is (false? (action/scenario-sealed? (ctx/store nm)))
          "a fresh scenario is not sealed")
      (scenario/seal-scenario! (ctx/store nm))
      (is (true? (action/scenario-sealed? (ctx/store nm)))
          "and the marker survives a re-read — it is a log row, not memory")
      (finally (drop! nm)))))

(deftest a-sealed-scenario-refuses-actions
  (testing "two planners comparing A vs B must see the same A — enforced at
            dispatch-action!, the one place every action arrives"
    (let [nm (fresh-scenario)]
      (try
        (let [branch nm]
          (scenario/seal-scenario! (ctx/store nm))
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo #"is sealed"
               (c/dispatch-action! (ctx/at (ctx/store "main" grants/system-authority) branch)
                                   (seat/branch-schema (ctx/store branch)) :whatever {}))
              "and refused BEFORE the unknown-action error — the gate runs first"))
        (finally (drop! nm))))))

(deftest only-a-scenario-branch-can-be-sealed
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"only a scenario branch"
                        (scenario/seal-scenario! (ctx/store)))
      "on main — sealing a development branch would be a different feature"))

(deftest the-seal-marker-is-not-part-of-the-definition
  (testing "in :opaque it would make every sealed scenario un-replayable — the
            exact opposite of what sealing is for"
    (let [nm (fresh-scenario)]
      (try
        (scenario/seal-scenario! (ctx/store nm))
        (let [d (scenario/scenario-definition (ctx/store nm) (seat/branch-schema (ctx/store nm)))]
          (is (= [] (:replayable d)))
          (is (= [] (:opaque d)) "the seal is bookkeeping ABOUT the scenario"))
        (finally (drop! nm))))))

(deftest replay-may-write-a-sealed-scenario
  (testing "a rebuild re-applies the sealed list onto a fresh fork, so exactly
            one writer must get past the gate — otherwise a sealed scenario
            could never be re-evaluated, which is the whole point"
    (let [nm (fresh-scenario)]
      (try
        (scenario/seal-scenario! (ctx/store nm))
        (is (= [] (scenario/replay-actions! (ctx/store nm) (seat/branch-schema (ctx/store nm))
                                     {:replayable [] :opaque []})))
        (finally (drop! nm))))))

(deftest rebuild-reproduces-the-scenario-and-keeps-its-seal
  (testing "'the same scenario, on today's data' — the operation the old
            branch-as-scenario design could not express at all"
    (let [nm (fresh-scenario)]
      (try
        (scenario/seal-scenario! (ctx/store nm))
        (let [r (scenario/rebuild-scenario! (ctx/store) nm "main")]
          (is (= nm (:branch r)))
          (is (true? (:sealed? r)) "a sealed scenario rebuilds sealed")
          (is (branch-exists? nm))
          (is (true? (action/scenario-sealed? (ctx/store nm)))
              "and is sealed again on the republished fork"))
        (finally (drop! nm))))))

(deftest a-failed-rebuild-leaves-the-original-untouched
  (testing "We cannot span
            one transaction over several actions, so the atomicity is structural:
            replay onto a throwaway fork, publish only on success. The first cut
            dropped the branch BEFORE replaying, which turned any replay failure
            into a deleted scenario."
    (let [nm (fresh-scenario)]
      (try
        (scenario/seal-scenario! (ctx/store nm))
        (action/log-action! (ctx/store nm) :nothing-resolves-to-this {})
        (is (thrown? clojure.lang.ExceptionInfo (scenario/rebuild-scenario! (ctx/store) nm "main")))
        (is (branch-exists? nm) "the scenario the planner had is still there")
        (is (true? (action/scenario-sealed? (ctx/store nm)))
            "and still sealed — nothing about it moved")
        (finally (drop! nm))))))

(deftest a-failed-duplicate-leaves-no-half-built-scenario
  (testing "a half-built duplicate is worse than none: someone finds it, and it
            looks like a plan somebody authored"
    (let [src (fresh-scenario)
          dst (scenario-name)]
      (try
        (is (thrown? clojure.lang.ExceptionInfo
                     (scenario/duplicate-scenario! (ctx/store)
                                            src "main" dst
                                            (fn [_]
                                              {:replayable
                                                 [{:seq 1 :action :no-such-action :args {}}]
                                               :opaque []})
                                            {})))
        (is (false? (branch-exists? dst)) "the failed duplicate is gone")
        (is (branch-exists? src) "and the source is untouched")
        (finally (drop! src) (drop! dst))))))

(deftest the-action-cap-is-enforced-before-anything-is-applied
  (testing "a scenario is an overlay, not a second world — and the cap must bite
            before the first write, or the refusal comes too late to mean
            anything"
    (let [nm (fresh-scenario)
          over (mapv (fn [i] {:seq i :action :no-such-action :args {}})
                     (range (inc scenario/max-scenario-actions)))]
      (try
        (is (re-find #"over the"
                     (try
                       (scenario/replay-actions! (ctx/store nm) (seat/branch-schema (ctx/store nm))
                                          {:replayable over :opaque []})
                       ""
                       (catch clojure.lang.ExceptionInfo e (.getMessage e))))
            "the cap message, not 'Unknown action' — proves nothing dispatched")
        (finally (drop! nm))))))

(deftest only-a-scenario-branch-can-be-applied
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a scenario branch"
                        (scenario/apply-scenario! (ctx/store) "main" "main"))))

(deftest apply-runs-the-list-on-the-base-and-then-seals-the-scenario
  (testing ""
    (let [nm (fresh-scenario)]
      (try
        (let [r (scenario/apply-scenario! (ctx/store) nm "main")]
          (is (= {:branch nm :target "main" :applied 0 :sealed? true} r))
          (is (true? (scenario/scenario-applied? (ctx/store nm))))
          (is (true? (action/scenario-sealed? (ctx/store nm)))))
        (finally (drop! nm))))))

(deftest applying-a-second-time-is-refused-unless-asked-for
  (testing "every action would run again — the way to double what a scenario
            created, and nothing in the result would say so"
    (let [nm (fresh-scenario)]
      (try
        (scenario/apply-scenario! (ctx/store) nm "main")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"applied already"
                              (scenario/apply-scenario! (ctx/store) nm "main")))
        (is (= 0 (:applied (scenario/apply-scenario! (ctx/store) nm "main" {:again? true})))
            "and :again? is the deliberate repeat")
        (finally (drop! nm))))))

(deftest a-scenario-that-does-not-apply-leaves-the-base-and-itself-alone
  (testing "the validation fork is the atomicity: an action that cannot run
            against today's base fails there, before the base has seen anything"
    (let [nm (fresh-scenario)]
      (try
        (action/log-action! (ctx/store nm) :no-such-action {})
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not apply"
                              (scenario/apply-scenario! (ctx/store) nm "main")))
        (is (false? (scenario/scenario-applied? (ctx/store nm)))
            "and the scenario is not marked as applied")
        (is (false? (action/scenario-sealed? (ctx/store nm)))
            "nor sealed — the planner can still fix it")
        (is (empty? (filter #(str/starts-with? (str (:name %)) "scenario/apply-")
                            (onto-branches/list-branches (ctx/store))))
            "and the throwaway validation fork is gone")
        (finally (drop! nm))))))

(deftest the-applied-marker-is-not-part-of-the-definition
  (testing "in :opaque it would make an applied scenario un-rebuildable"
    (let [nm (fresh-scenario)]
      (try
        (scenario/apply-scenario! (ctx/store) nm "main")
        (let [d (scenario/scenario-definition (ctx/store nm) (seat/branch-schema (ctx/store nm)))]
          (is (= [] (:replayable d)))
          (is (= [] (:opaque d))))
        (finally (drop! nm))))))
