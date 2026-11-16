(ns ^:unit ontology.batched-execution-test
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing]]
            [sutra.parser.spec :as spec]
            [sutra.actions.effects :as action-effects]
            [sutra.catalog.functions :as functions]
            [sutra.actions.operations :as ops]))

(def ^:private retitle-one
  {:function-name :retitle
   :target-type   :ticket
   :language      :kernel
   :param-types   {:ticket :ticket}
   :source        {:params {:ticket {:binding :ticket :type :ticket}}
                   :steps  [{:op :update :target :ticket
                             :values {:title "retitled"}}]}})

(def ^:private retitle-batch
  {:function-name :retitle-batch
   :language      :kernel
   :source        {:params {:batch {:binding :batch}}
                   :steps  [{:op :for-each :over :batch :as :entry
                             :steps [{:op :create :type :ticket
                                      :values {:ticket-id [:ref :entry :ticket-id]
                                               :title     [:ref :entry :title]}}]}]}})

(def ^:private world
  {:object-types
   {:ticket {:object-type-name :ticket
             :attribute-list [{:attribute-name :ticket-id :attribute-type :string}
                              {:attribute-name :title     :attribute-type :string}]
             :plural "Tickets"}}
   :link-types {}
   :functions  {:retitle retitle-one :retitle-batch retitle-batch}
   :actions
   {    :retitle-ticket
    {:rules  [{:op :apply-edit-function :function :retitle}]
     :params {:ticket {:type :ticket}}
     :edits  [{:object-type :ticket}]}
    :retitle-tickets
    {:rules  [{:op :apply-edit-function :function :retitle-batch :batched true
               :inputs {:ticket-id :ticket-id :title :title}}]
     :params {:ticket-id {:type :string} :title {:type :string}}
     :edits  [{:object-type :ticket}]}}})

(defn- logged [res] (mapv (juxt :op :type :payload) (:ops res)))

(defn- ctx []
  (tsys/ensure!)
  {:verbs (merge (tsys/verbs))})

(defn- ticket [n]
  {:id (str "t" n) :object-type-ref :ticket
   :values {:ticket-id (str "T-" n) :title "old"}})

(defn- run-batch
  [action arg-maps]
  (let [res (ops/execute-action-batch world {} (ctx) nil action arg-maps)]
    [(logged res) res]))

(defn- function-runs [res]
  (count (filter #(and (= :function (:op %)) (= :edit (:lifecycle %)))
                 (:side-effects res))))

(defn- fails [f] (try (f) nil (catch Exception e (ex-message e))))

(deftest only-an-unbatched-function-action-is-held-to-twenty-calls
  (is (= 20 (functions/action-batch-limit world :retitle-ticket)))
  (is (= 10000 (functions/action-batch-limit world :retitle-tickets)))
  (is (= 10000 (functions/action-batch-limit world :no-such-action))))

(deftest without-the-flag-the-function-runs-once-per-call
  (let [[log res] (run-batch :retitle-ticket
                             [{:ticket (ticket 1)} {:ticket (ticket 2)}
                              {:ticket (ticket 3)}])]
    (is (= 3 (function-runs res))
        "")
    (is (= 3 (count log)) "…and every call's edits land in the ONE run's :ops")
    (is (= #{"t1" "t2" "t3"} (set (map #(:id (nth % 2)) log)))
        "one transaction, applied at the Action boundary — not three actions")))

(deftest with-the-flag-the-whole-batch-is-one-execution
  (let [[log res] (run-batch :retitle-tickets
                             [{:ticket-id "T-1" :title "A"}
                              {:ticket-id "T-2" :title "B"}
                              {:ticket-id "T-3" :title "C"}])]
    (is (= 1 (function-runs res))
        "ONE function execution for the whole batch — the point of the flag")
    (is (= 3 (count log)) "which still performs every entry's edit")
    (is (= {"T-1" "A" "T-2" "B" "T-3" "C"}
           (into {} (for [[_ _ p] log]
                      [(:ticket-id (:values p)) (:title (:values p))])))
        "each entry's own struct fields reached the function, unmixed")))

(deftest a-single-call-of-a-batched-action-gets-a-one-entry-list
  (testing ""
    (let [[log res] (run-batch :retitle-tickets [{:ticket-id "T-9" :title "solo"}])]
      (is (= 1 (function-runs res)))
      (is (= 1 (count log)))
      (is (= "solo" (:title (:values (nth (first log) 2))))
          "the signature must not depend on how the caller arrived"))))

(deftest each-entry-is-the-struct-its-inputs-describe
  (testing "field → that call's own value, and an object handle passes through
            untouched (the id→object resolution itself needs a transaction and
            is covered end-to-end in api.batched-execution-test)"
    (let [entries ((deref #'action-effects/batch-entries)
                   :retitle-batch {:ticket-id :ticket :title :title}
                   [{:ticket (ticket 1) :title "A"} {:ticket (ticket 2) :title "B"}]
                   {:params {:ticket {:type :ticket} :title {:type :string}}}
                   world nil)]
      (is (= 2 (count entries)) "one struct per Action call")
      (is (= ["A" "B"] (mapv :title entries)) "…carrying that call's own values")
      (is (= "t1" (:id (:ticket-id (first entries))))
          "an object-typed parameter reaches the function AS the object — a raw
           primary key here would make every property read in the function body
           return nothing, silently"))))

(deftest a-batched-function-must-take-a-single-parameter
  (let [w   (assoc-in world [:functions :retitle-batch :source :params]
                      {:batch {:binding :batch} :extra {:binding :extra}})
        msg (fails #(ops/execute-action-batch
                     w {} (ctx) nil :retitle-tickets
                     [{:ticket (ticket 1) :title "A"}]))]
    (is (re-find #"SINGLE parameter" msg))
    (is (re-find #"list of structs" msg) "and it says what that parameter is")))

(deftest a-batched-rule-must-name-the-struct-fields
  (testing ":inputs names the FIELDS; without them every entry would be empty"
    (is (re-find #":inputs"
                 (spec/function-rule-problem
                  {:rules [{:op :apply-edit-function :function :f :batched true}]
                   :edits [{:object-type :ticket}]})))
    (is (nil? (spec/function-rule-problem
               {:rules [{:op :apply-edit-function :function :f :batched true
                         :inputs {:ticket :ticket}}]
                :edits [{:object-type :ticket}]})))))

(deftest only-the-function-rule-takes-batched
  (testing "a query function returns rows to the NEXT rules — it receives no
            batch of Action calls, so :batched there is a misunderstanding"
    (is (re-find #"only the function rule"
                 (spec/function-dep-problem
                  {:op :invoke-function :function :f :batched true})))
    (is (nil? (spec/function-dep-problem
               {:op :apply-edit-function :function :f :batched true})))))

(deftest every-entry-is-validated-on-its-own
  (testing "a submission criterion is not something a batch can dilute"
    (let [w (assoc-in world [:actions :retitle-tickets :submission-criteria]
                      [{:test {:t [:get :arg0 :title]} :pred [:!= :t "verboten"]}])]
      (is (re-find #"Vorbedingung"
                   (fails #(ops/execute-action-batch
                            w {} (ctx) nil :retitle-tickets
                            [{:ticket-id "T-1" :title "A"}
                             {:ticket-id "T-2" :title "verboten"}])))
          "one bad entry refuses the batch — before any of it is applied"))))
