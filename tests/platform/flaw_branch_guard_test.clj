(ns ^:integration platform.flaw-branch-guard-test
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [sutra.store.access.scope :as ctx]
            [sutra.governance.branch-state :as branch-state]
            [sutra.governance.guards :as guards]
            [sutra.store.protection :as protection]
            [sutra.governance.protection :as gov-protection]
            [sutra.governance.roles :as roles]
            [sutra.catalog.signature :as signature]
            [sutra.store.world :as world]
            [sutra.actions.action :as action]
            [sutra.actions.dispatch :as c]
            [support.pod :as pod]
            [support.store :as store])
  (:import (java.util.concurrent CyclicBarrier TimeUnit)))

(use-fixtures :once pod/with-mem-stores-only)

(defn- W [nm]
  (ri/derived {:sigma clock/system}
              {:object-types {nm {:object-type-name nm
                                  :attribute-list
                                    [{:attribute-name :nr :attribute-type :string}]
                                  :plural "Objekte"}}}))

(defn- install-through! [store W]
  (world/install! store W {:dataset-decls signature/datasets-of :actor {:principal "test"}}))

(defn- refusal [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest write-after-close
  (store/with-fresh-branch [b "guard-close"]
    (branch-state/close! (ctx/store b) b)
    (let [r (refusal #(guards/write-guard (ctx/store b) (protection/guard-reads (ctx/store b)) b nil))]
      (is (= 409 (anomaly/status r)) "a closed branch takes no write"))))

(deftest an-action-runs-only-on-a-guarded-store
  (store/with-fresh-branch [b "guard-action"]
    (testing "the guard is a property of the store value, for its own branch only"
      (is (not (guards/guarded? (ctx/store b))))
      (is (guards/guarded? (guards/data-guard (ctx/store b))))
      (is (not (guards/guarded? (ctx/at (guards/data-guard (ctx/store b)) "main"))))
      (let [g (guards/data-guard (ctx/store b))]
        (is (identical? g (guards/data-guarded g)) "a run decides the guard once, not per dispatch")
        (is (guards/guarded? (guards/data-guarded (ctx/store b))))))
    (testing "the chokepoint refuses a store that never passed the guard"
      (is (some? (refusal #(action/run-action! {:store (ctx/store b) :action :x})))))
    (testing "a dispatch with no request edge (an automation, an agent tool) is guarded too"
      (branch-state/close! (ctx/store b) b)
      (is (= 409 (anomaly/status (refusal #(c/dispatch-action! (ctx/store b) {} :x {}))))
          "a closed branch takes no action, from a person or from the system"))))

(deftest a-guard-holds-until-the-commit
  (testing "protected-write: the branch is protected between the guard and the commit"
    (store/with-fresh-branch [b "guard-protect"]
      (let [store (guards/write-guard (ctx/store b) (protection/guard-reads (ctx/store b)) b nil)]
        (gov-protection/set-policy! (ctx/store b) b {:protected true})
        (is (= :conflict (:store/refusal (refusal #(install-through! store (W :geschuetzt)))))
            "the write that passed the guard before the change is refused in its commit"))))
  (testing "write-after-close: the branch is closed between the guard and the commit"
    (store/with-fresh-branch [b "guard-closing"]
      (let [store (guards/write-guard (ctx/store b) (protection/guard-reads (ctx/store b)) b nil)]
        (branch-state/close! (ctx/store b) b)
        (is (some? (refusal #(install-through! store (W :zu)))))))))

(deftest protection-holds-until-the-commit
  (let [rid     "ri.ot.guard-schutz"
        touched [{:rid rid :name "schutz" :section "object-types" :project "ri.p.guard"}]
        guarded #(let [st (ctx/store "main")]
                   (-> (guards/write-guard st (protection/guard-reads st) "main" nil)
                       (protection/protection-guard "main" touched)))]
    (testing "unprotected, the guarded write commits"
      (is (nil? (refusal #(install-through! (guarded) (W :ungeschuetzt))))))
    (testing "the resource is protected between the guard and the commit"
      (let [store (guarded)]
        (protection/protect! (ctx/store "main") [rid] true)
        (try
          (is (= :conflict (:store/refusal (refusal #(install-through! store (W :schutz)))))
              "decided on the rows the guard read, the commit refuses once one moved")
          (is (= 403 (anomaly/status (refusal guarded)))
              "a guard read after it refuses outright")
          (finally (protection/protect! (ctx/store "main") [rid] false)))))
    (testing "protection is decided only on a guard's reading"
      (is (thrown? IllegalStateException
                   (protection/protection-guard (ctx/store "main") "main" touched))))))

(deftest orphaned-branch
  (store/with-fresh-branch [b "guard-owners"]
    (let [store (ctx/store b)]
      (roles/assign-role! store b "ava")
      (roles/assign-role! store b "bob")
      (let [bar (CyclicBarrier. 2)
            rs  (mapv deref
                      (mapv (fn [p] (future (.await bar 5 TimeUnit/SECONDS)
                                            (try (roles/revoke-role! store b p)
                                                 (catch Exception e {:error (ex-message e)}))))
                            ["ava" "bob"]))]
        (is (= 1 (count (filter :revoked rs))) (pr-str rs))
        (is (= 1 (count (roles/owners store b))) "one owner is left")))))

(deftest a-drawing-is-not-held-by-protection
  (store/with-fresh-branch [b "guard-view"]
    (gov-protection/set-policy! (ctx/store b) b {:protected true})
    (let [guard (fn [touched]
                  (let [st (ctx/store b)]
                    (refusal #(guards/write-guard st (protection/guard-reads st) b nil {:touched touched}))))]
      (is (nil? (guard [{:section "graphs" :rid "ri.graph.1"}]))
          "a batch of drawings commits on a protected branch")
      (is (= 403 (anomaly/status (guard [{:section "object-types" :rid "ri.ot.1"}])))
          "what a proposal does decide still arrives as one")
      (is (= 403 (anomaly/status (guard [{:section "graphs" :rid "ri.graph.1"}
                                       {:section "object-types" :rid "ri.ot.1"}])))
          "and a batch that mixes the two is judged by the reviewed half"))))
