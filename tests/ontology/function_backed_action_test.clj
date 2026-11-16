(ns ^:unit ontology.function-backed-action-test
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.kernel :as kernel]
            [sutra.catalog.functions :as functions]
            [sutra.parser.spec :as spec]
            [sutra.parser.resolve :as res]
            [sutra.parser.project-emit :as pemit]
            [sutra.actions.operations :as ops]))

(def ^:private retitle-fn
  {:function-name :retitle
   :target-type   :ticket
   :language      :kernel
   :param-types   {:ticket :ticket}
   :source        {:params {:ticket {:binding :ticket :type :ticket}}
                   :steps  [{:op :update :target :ticket
                             :values {:title "retitled"}}]}})

(def ^:private world
  {:object-types
   {:ticket  {:object-type-name :ticket
              :attribute-list [{:attribute-name :ticket-id :attribute-type :string}
                               {:attribute-name :title     :attribute-type :string}]
              :plural "Tickets"}
    :invoice {:object-type-name :invoice
              :attribute-list [{:attribute-name :nr :attribute-type :string}]
              :plural "Invoices"}}
   :link-types {}
   :functions  {:retitle retitle-fn}
   :transforms {:count-tickets {:function-name :count-tickets
                                :language :python :code "…"}}
   :actions
   {:retitle-ticket {:rules [{:op :apply-edit-function :function :retitle}]
                     :params {:ticket {:type :ticket}}
                     :edits  [{:object-type :ticket}]}}})

(defn- logged [res] (mapv (juxt :op :type :payload) (:ops res)))

(defn- ctx []
  (tsys/ensure!)
  {:verbs (merge (tsys/verbs))})

(defn- run-rule
  ([rule bindings] (run-rule world rule bindings))
  ([w rule bindings]
   (let [res (kernel/eval-step rule w bindings (ctx))]
     [(logged res) res])))

(defn- fails [f]
  (try (f) nil (catch Exception e (ex-message e))))

(def ^:private a-ticket
  {:id "t1" :object-type-ref :ticket :values {:ticket-id "T-1" :title "old"}})

(deftest function-rule-applies-the-functions-edits
  (let [[log res] (run-rule {:op :apply-edit-function :function :retitle}
                            {:ticket a-ticket})]
    (testing "the function's :update reached the ACTION's store"
      (is (= 1 (count log)))
      (is (= :update-object (ffirst log)))
      (is (= {:title "retitled"} (:values (nth (first log) 2)))))
    (testing "the edit is reported as the action's own side-effect"
      (is (some #(= :update (:op %)) (:side-effects res))))
    (testing "and the run is announced as an EDIT-function run"
      (is (some #(and (= :function (:op %)) (= :edit (:lifecycle %)))
                (:side-effects res))))))

(deftest inputs-default-to-the-identically-named-action-parameter
  (testing ""
    (let [[log] (run-rule {:op :apply-edit-function :function :retitle}
                          {:ticket a-ticket})]
      (is (= "t1" (:id (nth (first log) 2)))))))

(deftest inputs-may-be-mapped-explicitly
  (testing ":inputs maps a function parameter onto a differently-named param"
    (let [[log] (run-rule {:op :apply-edit-function :function :retitle
                           :inputs {:ticket :selected}}
                          {:selected a-ticket})]
      (is (= "t1" (:id (nth (first log) 2)))))))

(deftest an-input-the-function-never-declared-is-refused
  (is (re-find #"Unknown function input"
               (fails #(run-rule {:op :apply-edit-function :function :retitle
                                  :inputs {:tickets a-ticket}}
                                 {})))))

(deftest a-query-function-cannot-back-an-action
  (let [msg (fails #(run-rule {:op :apply-edit-function :function :count-tickets} {}))]
    (is (re-find #"invoke-function" msg) "and it names the step that DOES take one")))

(deftest an-edit-function-cannot-be-invoked-for-rows
  (let [msg (fails #(run-rule {:op :invoke-function :function :retitle} {}))]
    (is (re-find #"apply-edit-function" msg) "and it names the rule that backs an action")))

(deftest an-unknown-function-is-refused-by-name
  (is (re-find #"unknown edit function"
               (fails #(run-rule {:op :apply-edit-function :function :nope} {})))))

(defn- problem [action] (spec/function-rule-problem action))

(deftest a-function-rule-excludes-every-other-rule
  (testing "function code alone handles everything"
    nil)
  (testing "…including a function rule hidden inside control flow"
    (is (some? (problem {:rules [{:op :if :test {} :pred [:= 1 1]
                                  :then [{:op :apply-edit-function :function :retitle}]}]
                         :edits [{:object-type :ticket}]})))))

(deftest a-function-backed-action-must-declare-its-provenance
  (is (re-find #":edits provenance"
               (problem {:rules [{:op :apply-edit-function :function :retitle}]})))
  (is (nil? (problem (get-in world [:actions :retitle-ticket])))
      "a well-formed one has no problem"))

(deftest a-function-rule-must-name-a-function
  (is (re-find #"names no :function"
               (problem {:rules [{:op :apply-edit-function}] :edits [{:object-type :ticket}]}))))

(deftest simple-rule-actions-are-untouched
  (testing "no function rule ⇒ the phantom-edit invariant still governs"
    (is (nil? (problem {:rules [{:op :update :object-type :ticket}]})))
    (is (false? (spec/edits-consistent? {:rules [{:op :update :object-type :ticket}]
                                         :edits [{:object-type :invoice}]})))
    (is (true? (spec/edits-consistent? {:rules [{:op :update :object-type :ticket}]
                                        :edits [{:object-type :ticket}]}))))
  (testing "a function-backed action is exempt from it — there is nothing to derive"
    (is (true? (spec/edits-consistent? (get-in world [:actions :retitle-ticket]))))))

(defn- violations [action events]
  (functions/provenance-violations world action events))

(deftest edits-outside-the-declared-provenance-are-violations
  (let [action (get-in world [:actions :retitle-ticket])]
    (is (empty? (violations action [{:op :update :target {:type :ticket :id "t1"}}])))
    (is (= [{:object-type :invoice}]
           (violations action [{:op :update  :target {:type :ticket  :id "t1"}}
                               {:op :create  :target {:type :invoice :id "i1"}}])))
    (testing "a link edit is provenance too"
      (is (= [{:link-type :ticket-owner}]
             (violations action [{:op :create-link :target {:type :ticket-owner :id "l1"}}]))))
    (testing "non-edit events carry no provenance"
      (is (empty? (violations action [{:op :query :object-set :all-tickets}
                                      {:op :notify :channel "ops"}]))))))

(deftest simple-rule-actions-are-not-held-to-runtime-provenance
  (testing "their rules already name what they edit; :edits is legitimately absent"
    (is (empty? (violations {:rules [{:op :update :object-type :ticket}]}
                            [{:op :create :target {:type :invoice :id "i1"}}])))))

(deftest an-interface-provenance-covers-its-implementers
  (let [w (assoc world
                 :interfaces {:has-title {:interface-name :has-title}}
                 :object-types (assoc-in (:object-types world) [:ticket :implements] [:has-title]))
        action {:rules [{:op :apply-edit-function :function :retitle}]
                :edits [{:interface :has-title}]}]
    (is (empty? (functions/provenance-violations
                 w action [{:op :update :target {:type :ticket :id "t1"}}])))
    (is (seq (functions/provenance-violations
              w action [{:op :update :target {:type :invoice :id "i1"}}])))))

(deftest the-action-refuses-a-run-that-left-its-provenance
  (testing ""
    (let [w   (assoc-in world [:functions :retitle :source]
                        {:params {:ticket {:binding :ticket :type :ticket}}
                         :steps  [{:op :create :type :invoice
                                   :values {:nr "X"} :as :new-invoice}]})
          msg (fails #(ops/execute-action w {} (ctx) nil :retitle-ticket
                                          {:arg0 {:ticket a-ticket}}))]
      (is (re-find #"outside its declared :edits provenance" msg))
      (is (re-find #":invoice" msg) "and it names what was touched"))))

(defn- finding-msgs
  [schema]
  (mapv :message (filter #(= :aktion/function-rule (:rule %)) (res/findings schema))))

(def ^:private authored
  {:objects   {:ticket {:attributes {:ticket-id :string :title :string} :plural "Tickets"}}
   :functions {:retitle retitle-fn}
   :actions   {:retitle-ticket {:rules [{:op :apply-edit-function :function :retitle}]
                                :edits [{:object-type :ticket}]}}})

(deftest the-resolver-names-the-action-and-the-problem
  (is (empty? (finding-msgs authored)) "the well-formed one is quiet")
  (testing "an unknown function is caught before it can be submitted"
    (is (re-find #"not a declared function"
                 (first (finding-msgs (assoc-in authored [:actions :retitle-ticket :rules]
                                                [{:op :apply-edit-function :function :nope}]))))))
  (testing "exclusivity and the missing provenance are reported with a subject"
    (is (re-find #"retitle-ticket"
                 (first (finding-msgs (update-in authored [:actions :retitle-ticket :rules]
                                                 conj {:op :update :object-type :ticket})))))
    (is (re-find #":edits provenance"
                 (first (finding-msgs (update-in authored [:actions :retitle-ticket]
                                                 dissoc :edits)))))))

(deftest a-function-backed-actions-provenance-survives-the-export
  (testing "…because the install door REQUIRES it — dropping it would emit a
            file that cannot be re-installed"
    (let [a (get-in world [:actions :retitle-ticket])]
      (is (= [{:object-type :ticket}] (:edits (#'pemit/emit-action {} a))))))
  (testing "a simple-rule action still exports WITHOUT :edits (it is derivable)"
    (is (nil? (:edits (#'pemit/emit-action {} {:rules [{:op :update :object-type :ticket}]
                                            :edits [{:object-type :ticket}]}))))))

(deftest a-run-within-its-provenance-is-applied
  (let [res (ops/execute-action world {} (ctx) nil :retitle-ticket
                                {:arg0 {:ticket a-ticket}})]
    (is (= [:update-object] (mapv first (logged res))))
    (is (some #(= :update (:op %)) (:side-effects res)))))
