(ns grammar.plan-pipeline-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.lifecycle :as lc]
            [sutra.types.finding :as finding]
            [sutra.types.plan :as plan]))

(def ^:private err (finding/finding :r/e :error :actions :a {:message "blocks"}))
(def ^:private warn (finding/finding :r/w :warn :actions :b {:message "hints"}))

(deftest a-plan-is-ops-and-findings
  (is (= {:ops [] :findings []} (plan/plan)))
  (is (= {:ops [1 2 3] :findings [warn err]}
         (plan/combine (plan/plan [1] [warn]) (plan/plan [2 3] [err]))))
  (is (= {:ops [] :findings [err warn] :blocked? true}
         (plan/preview (plan/of-findings [warn err])))
      "a preview shows the findings worst first and whether they block")
  (is (false? (:blocked? (plan/preview (plan/of-findings [warn]))))))

(deftest apply-commits-only-what-no-finding-blocks
  (let [seen (atom nil)]
    (is (= :done (plan/apply! (plan/plan [:op] [warn]) #(do (reset! seen %) :done))))
    (is (= [:op] @seen) "a warning does not stop it"))
  (let [seen (atom nil)
        e    (try (plan/apply! (plan/plan [:op] [warn err]) #(reset! seen %)
                               {:data {:sutra/gate :g}})
                  (catch clojure.lang.ExceptionInfo e e))]
    (is (nil? @seen) "nothing committed")
    (is (= :refusal.finding/blocking (:refusal/code (ex-data e))))
    (is (= [err] (:problems (ex-data e))) "the blocking findings ride under :problems")
    (is (= :g (:sutra/gate (ex-data e))))))

(deftest a-code-and-its-params-word-the-refusal
  (let [r (plan/refusal (plan/of-findings [err]) {:code :refusal.authoring/plan-problems
                                                  :params (fn [bs] {:headline (str (count bs))})})]
    (is (= "1" (ex-message r)))
    (is (= :incorrect (anomaly/category (ex-data r))))))

(deftest problems-of-any-producer-become-findings
  (let [[s m f] (plan/problems :x/y ["plain" {:problem :too-big :n 3} warn])]
    (is (= {:rule :x/y :severity :error :message "plain"} s))
    (is (= [:too-big :error 3] [(:rule m) (:severity m) (:n m)]))
    (is (= warn f) "a finding stays itself")))

(def ^:private door
  (lc/table {:name   :door
             :states #{:open :shut}
             :final  #{}
             :edges  {:shut {:from #{:open} :to :shut
                             :guard (fn [r _] (when (:jammed r) "jammed"))
                             :refusal :refusal.door/jammed}}}))

(deftest a-guard-says-no-as-a-finding
  (testing "allowed: the op of the new state"
    (is (= (plan/plan [[:to :shut]])
           (plan/transition door 1 :shut :open {} nil (fn [to] [:to to])))))
  (testing "a guard refusing: a blocking finding under the edge's code, nothing to do"
    (let [p (plan/transition door 1 :shut :open {:jammed true} nil (fn [to] [:to to]))
          f (first (:findings p))]
      (is (= [] (:ops p)))
      (is (plan/blocked? p))
      (is (= [:refusal.door/jammed :door 1 "jammed"] [(:rule f) (:entity-class f) (:entity f)
                                                      (:reason f)]))))
  (testing "a state the edge does not leave: the lifecycle's own code"
    (is (= :refusal.lifecycle/transition-refused
           (:rule (first (:findings (plan/transition door 1 :shut :shut {} nil identity))))))))
