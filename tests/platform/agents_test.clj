(ns ^:api platform.agents-test
  (:require [sutra.serve.errors :as errors]
            [support.system :as system]
            [sutra.store.access.scope :as ctx]
            [support.wire :refer [ok? eval-run!]]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.wire.api :as api]
            [sutra.agents.http :as agents]
            [support.pod :as pod]))

(use-fixtures :once pod/with-mem-pod)

(def ^:private schema
  {:projects   {:rechnung {}}
   :llm-models {"grok-4" {:in 0.1 :out 0.2 :currency "USD"}}
   :objects    {:invoice {:name "Invoice" :primary-key :pk
                          :attributes [[:pk :string] [:amount :number]]
                          :project :rechnung
                          :plural "Invoices"}}
   :actions    {:record-invoice
                {:name  :record-invoice
                 :project :rechnung
                 :params {:pk {:name "Pk" :type :string} :amount {:name "Amount" :type :number}}
                 :rules [{:op :create :object-type :invoice
                          :id :pk :values {:pk :pk :amount :amount}}]
                 :edits [{:object-type :invoice}]}}
   :agents     {:clerk {:display-name  "Invoice clerk"
                        :project       :rechnung
                        :model         "grok-4"
                        :source        :lms
                        :writes        :apply
                        :temperature   0.2
                        :system-prompt "You manage invoices."
                        :tools [{:kind :action :action :record-invoice}
                                {:kind :object-query :object-type :invoice}]}
                :reviewer {:model   "grok-4"
                           :project :rechnung
                           :source  :lms
                           :tools   [{:kind :action :action :record-invoice}
                                     {:kind :ontology}]}}})

(defn- call [id nm args]
  {:id id :type "function" :function {:name nm :arguments (json/write-str args)}})

(defn- scripted!
  [turns]
  (let [n (atom 0) seen (atom [])]
    (system/provide! :llm/complete
                   (fn [req]
                     (swap! seen conj req)
                     (let [i (dec (swap! n inc))]
                       {:request-id (str "r" i) :message (nth turns (min i (dec (count turns))))})))
    seen))

(deftest agent-install-and-invoke
  (testing "install the agents + their type/model/action"
    (is (ok? (api/mutate :create-llm-model {:name "grok-4" :in 0.1 :out 0.2 :currency "USD"}
                         {:stage? true}))
        "the model the agents use is registered")
    (is (ok? (api/post! :install {:schema schema})) "install stages")
    (is (ok? (api/post! :staging)) "commit — reconcile accepts the :agents section"))

  (testing "the agent persists into the readable schema"
    (let [a (get-in (:body (api/get! :agents)) [:agents :clerk])]
      (is (some? a) "agent persisted")
      (is (= "grok-4" (:model a)))
      (is (= :apply (:writes a)))
      (is (= 2 (count (:tools a))) "both tool bindings round-tripped")))

  (testing "an Action tool call with JSON arguments, as a model sends them, is applied"
    (let [seen
            (scripted! [{:tool_calls [(call "t1" "apply_record-invoice" {:pk "inv-1" :amount 42})]}
                        {:content "Recorded invoice inv-1."}])
          b    (:body (api/post! :agent-invoke {:name :clerk}
                                 {:input "record invoice inv-1 for 42"}))]
      (is (= "Recorded invoice inv-1." (:answer b)) "the model's final answer is returned")
      (is (= "apply_record-invoice" (:tool (first (:trace b)))) "the action tool ran")
      (is (= {:status "applied"} (:summary (first (:trace b)))))
      (is (some #(= "inv-1" (get-in % [:values :pk])) (:rows (:body (api/list-objects :invoice))))
          "the agent's Action tool created the invoice object")
      (is (some? (:thread b)) "the run is kept in a thread")
      (let [req (first @seen)]
        (is (= "agent:clerk" (:caller req)) "every model call names its agent")
        (is (= (:run-id b) (:run-id req)) "…and carries the run's id into the audit")
        (is (= :lms (:source req)))
        (is (some #(= "apply_record-invoice" (get-in % ["function" "name"]))
                  (get-in req [:request "tools"])))
        (is (= #{"pk" "amount" "_reason"}
               (set (keys (get-in (first (get-in req [:request "tools"]))
                                  ["function" "parameters" "properties"]))))
            "an Action tool declares the action's parameters"))))

  (testing "a query tool filters in the store and the answer cites what it read"
    (scripted! [{:tool_calls [(call "q1" "query_invoice"
                                    {:where {:amount {">=" 40}} :show ["pk" "amount"]})]}
                {:content "There is one: [[invoice:inv-1]]."}])
    (let [b (:body (api/post! :agent-invoke {:name :clerk}
                              {:input "which invoices are 40 or more?"}))]
      (is (= "query_invoice" (:tool (first (:trace b)))))
      (is (= {:count 1 :returned 1} (:summary (first (:trace b)))) "the filter reached the store")
      (is (= ["inv-1"] (mapv :id (:sources b))) "the cited object is a source")
      (is (empty? (:unverified b)))))

  (testing "a citation no tool returned is reported, not trusted"
    (scripted! [{:content "It is [[invoice:made-up]]."}])
    (let [b (:body (api/post! :agent-invoke {:name :clerk} {:input "which one?"}))]
      (is (= ["invoice:made-up"] (:unverified b)))
      (is (empty? (:sources b)))))

  (testing "an agent that proposes: the action is staged on the thread, accepting applies it"
    (scripted! [{:tool_calls [(call "t1" "apply_record-invoice"
                                    {:pk "inv-2" :amount 7 :_reason "asked"})]}
                {:content "Proposed invoice inv-2."}])
    (let [b    (:body (api/post! :agent-invoke {:name :reviewer} {:input "record inv-2 for 7"}))
          tid  (:thread b)
          view (:body (api/get! :agent-thread {:id tid}))
          p    (first (:proposals view))]
      (is (= 1 (count (:proposals b))) "the run says what it proposed")
      (is (= "open" (:status p)))
      (is (= "asked" (:reason p)))
      (is (not-any? #(= "inv-2" (get-in % [:values :pk]))
                    (:rows (:body (api/list-objects :invoice))))
          "nothing is applied before the asker decides")
      (is (ok? (api/post! :agent-thread-proposal-accept {:id tid :proposal (:id p)} {})))
      (is (some #(= "inv-2" (get-in % [:values :pk])) (:rows (:body (api/list-objects :invoice))))
          "accepting the proposal applies it")))

  (testing "the ontology tools list the readable types and query any of them"
    (scripted! [{:tool_calls [(call "l" "list_object_types" {})
                              (call "q" "query_objects"
                                    {:type "invoice" :aggregate [{:fn "sum" :attr "amount"}]})]}
                {:content "49 in total."}])
    (let [b (:body (api/post! :agent-invoke {:name :reviewer} {:input "total?"}))]
      (is (= ["list_object_types" "query_objects"] (mapv :tool (butlast (:trace b)))))
      (is (= {:types 1} (:summary (first (:trace b)))))
      (is (= {:groups 1} (:summary (second (:trace b)))))))

  (testing "threads: asked, listed, answered asynchronously"
    (scripted! [{:content "Hello."}])
    (let [r   (api/post! :agent-threads {:agent "clerk" :input "hi"})
          tid (get-in r [:body :thread :id])]
      (is (ok? r))
      (is (some #(= tid (:id %)) (:threads (:body (api/get! :agent-threads)))))
      (is (= "hi" (get-in (api/get! :agent-thread {:id tid}) [:body :thread :title]))
          "the first question titles it")
      (is (ok? (api/delete! :agent-thread {:id tid})))))

  (testing "an eval suite runs an agent dry and grades answer, tools, proposed edits and a judge's verdict"
    (scripted! [{:tool_calls [(call "t1" "apply_record-invoice" {:pk "inv-9" :amount 5})]}
                {:content "Recorded inv-9 for 5."}
                {:content "{\"score\": 0.75, \"reason\": \"right invoice, terse\"}"}])
    (let [before (count (:rows (:body (api/list-objects :invoice))))
          r      (eval-run!
                            {:suite {:name "clerk-smoke" :agent :clerk :iterations 1
                                     :cases [{:id "c1" :input "record inv-9 for 5"
                                              :expected
                                                {:answer ["inv-9"] :tools ["apply_record-invoice"]
                                                 :proposed
                                                   [{:action "record-invoice" :args {:pk "inv-9"}}]
                                                 :reference "Recorded invoice inv-9."}}]
                                     :evaluators [{:name :says-id :kind :contains :field :answer}
                                                  {:name :used-tool :kind :tool-use :field :tools}
                                                  {:name :edits :kind :proposed-actions
                                                   :field :proposed}
                                                  {:name :judge :kind :llm-judge :field :answer
                                                   :expected :reference :model "grok-4"}]}})
          by     (into {} (map (juxt (comp keyword :name) :value)) (get-in r [:body :metrics]))]
      (is (ok? r))
      (is (= {:says-id 1.0 :used-tool 1.0 :edits 1.0 :judge 0.75} by))
      (is (= before (count (:rows (:body (api/list-objects :invoice)))))
          "a dry run applies nothing")))

  (testing "error paths: unknown agent, missing input, no model door"
    (is (not (ok? (api/post! :agent-invoke {:name :nope} {:input "hi"}))) "unknown agent ⇒ 400")
    (is (not (ok? (api/post! :agent-invoke {:name :clerk} {:input ""}))) "blank input ⇒ 400")
    (system/provide! :llm/complete nil)
    (is (not (ok? (api/post! :agent-invoke {:name :clerk} {:input "hi"})))
        "no model door ⇒ the run fails, reported")))

(deftest agents-are-scoped-by-owning-project
  (let [restricted {:project-operations {"ri.p.other" #{:read}}}]
    (testing "an unrestricted principal still sees everything"
      (is (contains? (:agents (:body (api/get! :agents))) :clerk)
          "admin/dev (project-operations nil) is unaffected by the gate"))

    (testing "a principal granted only OTHER projects sees no agent"
      (is (empty? (:agents (:body (agents/list-agents {:sutra/store (ctx/store "main" restricted)
                                                       :sutra/schema (system/seated-schema)}))))
          "an unowned agent must be invisible to a restricted principal"))

    (testing "…and cannot invoke it either"
      (system/provide! :llm/complete (fn [_] {:message {:content "should never run"}}))
      (let [r ((errors/wrap-exceptions agents/invoke-agent) {:path-params {:name "clerk"}
                                    :body-params {:input "hi"}
                                    :sutra/store    (ctx/store "main" restricted)
                                    :sutra/schema (system/seated-schema)
                                    :sutra/branch "main"})]
        (is (not= 200 (:status r))
            "an agent the caller may not see must not be runnable")
        (is (re-find #"no such agent" (str (get-in r [:body :error])))
            "and it reports 404-shaped 'no such agent' rather than confirming
             the agent exists in a project the caller has no access to")))))
