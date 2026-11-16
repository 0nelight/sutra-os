(ns sutra.authoring.install.findings.functions
  (:require [clojure.string :as str]
            [sutra.authoring.install.diff :as diff]
            [sutra.authoring.install.findings :as fnd]
            [sutra.authoring.install.live :as live]
            [sutra.catalog.function-contract :as contract]
            [sutra.catalog.functions :as functions]
            [sutra.types.entities :as ents]))

(set! *warn-on-reflection* true)

(defn- consumer-words [[kind nm]] (str (name kind) " " (name nm)))

(defn- change-findings
  [{:keys [function unbound breaks widenings callers]}]
  (let [base {:entity-class :functions :entity function}]
    (concat
     (for [{:keys [consumer inputs]} unbound]
       (assoc base :rule :function-caller-unbound :severity :error
              ::fnd/ctx {:consumer consumer :inputs inputs}))
     (when (and (seq breaks) (seq callers))
       [(assoc base :rule :function-change-affects-callers :severity :warn
               ::fnd/ctx {:breaks breaks :callers callers})])
     (when (seq widenings)
       [(assoc base :rule :function-input-widened :severity :warn
               ::fnd/ctx {:widenings widenings})]))))

(defn- regressions
  [{:keys [before after] :as d}]
  (let [acts  (diff/changed d :action)
        only  (fn [w] (live/select (:actions w) acts))
        ab    (only before)
        aa    (only after)
        fs    (into #{} (map :function)
                    (concat (contract/calls {:actions ab}) (contract/calls {:actions aa})))
        nodes (diff/fn-nodes d fs)
        world (fn [w actions]
                (diff/sub-world w {:functions {} :transforms {} :applications {}
                                   :actions actions}
                                nodes))]
    (contract/action-regressions (world before ab) (world after aa))))

(defn- function-side
  [d]
  (let [{:keys [names calling]} (diff/index d)
        changed (concat (for [k (diff/changed d :function)] [:functions k])
                        (for [k (diff/changed d :transform)] [:transforms k]))
        ns      (into #{}
                      (keep (fn [[side [sec k]]]
                              (let [s (live/lookup (get-in d [side sec]) k)]
                                (when (or (= :functions sec) (functions/query-function? s))
                                  (:function-name s)))))
                      (for [node changed side [:before :after]] [side node]))
        callers (distinct (concat (for [n ns
                                        c (distinct (keep contract/call-key
                                                          (live/keys-named names n)))
                                        [sec r] (calling c)]
                                    [sec (diff/node-key d sec r)])
                                  (for [k (diff/changed d :action)] [:actions k])
                                  (for [k (diff/changed d :application)] [:applications k])))
        world   (fn [side nodes]
                  (diff/sub-world (get d side)
                                  {:functions {} :transforms {} :actions {} :applications {}}
                                  nodes))
        before  (world :before changed)
        after   (world :after (concat changed callers))]
    [(contract/function-changes before after) (contract/undeclared-outputs before after)]))

(def function-contract
  {:find (fn [{:keys [text] :as d}]
           (let [any?    (fn [& ks] (some #(seq (diff/changed d %)) ks))
                 [fc uo] (when (any? :function :transform) (function-side d))]
             (concat
              (mapcat change-findings fc)
              (when (any? :action)
                (for [{:keys [action function inputs]} (regressions d)]
                  {:rule :action-call-unbound :severity :error :entity-class :actions
                   :entity action ::fnd/ctx {:function function :inputs inputs}}))
              (when (any? :function :transform)
                (for [{:keys [function callers]} uo]
                  {:rule :function-output-undeclared :severity :warn :entity-class :functions
                   :entity function ::fnd/ctx {:callers callers}}))
              (for [[nm f] (:functions text)
                    :when (and (ents/edit-function? f) (seq (:returns f)))]
                {:rule :function-returns-on-edit :entity-class :functions :entity nm}))))
   :says
   {:function-caller-unbound
    (fn [{nm :entity {:keys [consumer inputs]} ::fnd/ctx}]
      (str "function " (name nm) " now requires " (str/join ", " (map pr-str inputs))
           ", and " (consumer-words consumer) " binds nothing to it. Bind it there, or declare"
           " it optional: a Python default, ^:optional on a Clojure parameter, :optional true"
           " on a :scalar input."))
    :function-change-affects-callers
    (fn [{nm :entity {:keys [breaks callers]} ::fnd/ctx}]
      (str "function " (name nm) ": " (str/join "; " breaks) ". Called by "
           (str/join ", " (map consumer-words callers)) "."))
    :function-input-widened
    (fn [{nm :entity {:keys [widenings]} ::fnd/ctx}]
      (str "function " (name nm) ": " (str/join "; " widenings)
           ". A wider numeric input keeps its callers working, but check what the body does"
           " with the new values."))
    :action-call-unbound
    (fn [{nm :entity {:keys [function inputs]} ::fnd/ctx}]
      (str "action " (name nm) " calls function " (name function) " and binds nothing to its"
           " required input(s) " (str/join ", " (map pr-str inputs)) ". Bind them in the"
           " rule's :inputs or as parameters of the action, or declare them optional in the"
           " function."))
    :function-output-undeclared
    (fn [{nm :entity {:keys [callers]} ::fnd/ctx}]
      (str "function " (name nm) " declares no :returns, so a changed or dropped output column"
           " reaches " (str/join ", " (map consumer-words callers)) " unnoticed. Declare"
           " {:spalte :typ}."))
    :function-returns-on-edit
    (fn [{nm :entity}]
      (str "edit function " (name nm) " declares :returns, but an edit function answers with"
           " its edits. :returns belongs on a query function."))}})
