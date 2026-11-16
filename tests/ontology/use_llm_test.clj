(ns ^:unit ontology.use-llm-test
  (:require [support.system :as tsys]
            [support.replay :as replay]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.kernel :as kernel]
            [sutra.catalog.structure :as structure]
            [sutra.catalog.grants :as grants]
            [sutra.actions.logic :as logic]
            [sutra.catalog.program :as prog]
            [sutra.types.llm-output :as llm-out]
            [sutra.actions.execution]
            [sutra.types.object-set :as os]))

(def ^:private world
  {:object-types
   {:invoice {:object-type-name :invoice
              :attribute-list [{:attribute-name :nr :attribute-type :string}
                               {:attribute-name :text :attribute-type :string}
                               {:attribute-name :cost-center :attribute-type :string}]
              :plural "Invoices"}}
   :link-types {}
   :llm-models {"gpt-test" {:in 1 :out 1}}
   :actions
   {:assign-cost-center {:params {:invoice {:type :invoice :required true}
                                  :cost-center {:type :string :required true}}
                         :rules [{:op :update :target [:get :invoice :id]
                                  :values {:cost-center [:get :cost-center]}}]}}})

(defn- logged [res] (mapv (juxt :op :type :payload) (:ops res)))

(def ^:private an-invoice
  {:id "i1" :object-type-ref :invoice
   :values {:nr "R-1" :text "Splinte, Dichtmittel" :cost-center nil}})

(def ^:private params {:invoice {:binding :invoice :type :invoice}})

(defn- ctx [& {:keys [propose?]}]
  (cond-> {:verbs (merge logic/verbs (tsys/verbs)) :store {:authority grants/system-authority}}
    propose? logic/proposing))

(defn- call [id nm args]
  {:id id :type "function" :function {:name nm :arguments (json/write-str args)}})

(defn- scripted [& messages]
  (let [q (atom (vec messages)) seen (atom [])]
    [(fn [req]
       (swap! seen conj req)
       (let [m (first @q)]
         (swap! q (comp vec rest))
         {:request-id (str "req-" (count @seen)) :message m}))
     seen]))

(defn- run [steps bindings c]
  (let [compiled (prog/compile-function world steps params)]
    (replay/evaluate c #(kernel/eval-steps (:steps compiled) world bindings %1 %2))))

(defn- fails [f] (try (f) nil (catch Exception e (ex-message e))))

(deftest output-grammar
  (is (nil? (llm-out/problem [:enum "a" "b"])))
  (is (nil? (llm-out/problem {:struct {:cc :string :sure :boolean}})))
  (is (nil? (llm-out/problem [:list :integer])))
  (is (re-find #"unknown output type" (llm-out/problem :date)))
  (is (= {:value {:cc "4711" :sure true}}
         (llm-out/coerce {:struct {:cc :string :sure :boolean}} {"cc" "4711" "sure" "true"})))
  (is (= [:cc] (:path (llm-out/coerce {:struct {:cc :string}} {}))))
  (is (= {:value :b} (llm-out/coerce [:enum :a :b] "b"))))

(deftest compile-refuses-what-cannot-run
  (testing "a prompt variable that names no binding"
    (is (re-find #"Unknown binding"
                 (fails #(prog/compile-function world [{:op :use-llm :model "gpt-test" :source :lms
                                                        :prompt "{{rechnung.nr}}"}] params)))))
  (testing "an output type outside the grammar"
    (is (re-find #":output"
                 (fails #(prog/compile-function world [{:op :use-llm :model "gpt-test" :source :lms
                                                        :prompt "x" :output :date}] params)))))
  (testing "no model"
    (is (re-find #":model"
                 (fails #(prog/compile-function world [{:op :use-llm :source :lms :prompt "x"}]
                                                params))))))

(deftest use-llm-binds-a-typed-answer-and-logs-the-chain
  (let [[f seen] (scripted {:tool_calls [(call "c1" "calculator" {"expression" "(2+3)*4"})]}
                           {:tool_calls
                              [(call "c2" "final_answer" {"value" {"cc" "4711" "sure" true}})]})
        r (tsys/with-provided {:llm/complete f}
            (run [{:op :use-llm :model "gpt-test" :source :lms :tools [:calculator]
                   :prompt "Rechnung {{invoice.nr}}: {{invoice.text}}"
                   :output {:struct {:cc :string :sure :boolean}} :as :answer}]
                 {:invoice an-invoice} (ctx)))
        decisions (:decisions r)]
    (testing "the prompt is interpolated from the object"
      (is (= "Rechnung R-1: Splinte, Dichtmittel"
             (get-in (first @seen) [:request "messages" 0 "content"]))))
    (testing "the calculator ran and its result went back to the model"
      (is (= "{\"result\":20.0}" (get-in (second @seen) [:request "messages" 2 "content"]))))
    (testing "the answer is coerced to the declared struct and bound"
      (is (some #(= {:cc "4711" :sure true} %) (vals (:bindings r)))))
    (testing "the decision log carries every turn and request id"
      (is (= ["req-1" "req-2"] (keep :request-id (:log (first decisions)))))
      (is (= {:cc "4711" :sure true} (some :output (:log (first decisions))))))
    (is (empty? (logged r)))))

(deftest a-retry-recovers-an-invalid-answer
  (let [[f] (scripted {:tool_calls [(call "c1" "final_answer" {"value" "vielleicht"})]}
                      {:tool_calls [(call "c2" "final_answer" {"value" "b"})]})
        r (tsys/with-provided {:llm/complete f}
            (run [{:op :use-llm :model "gpt-test" :source :lms :prompt "?"
                   :output [:enum "a" "b"] :as :pick}]
                 {:invoice an-invoice} (ctx)))]
    (is (some #{"b"} (vals (:bindings r))))))

(deftest apply-action-tool-in-propose-mode-stages-instead-of-editing
  (let [[f] (scripted {:tool_calls [(call "c1" "apply_assign-cost-center"
                                          {"invoice" "i1" "cost-center" "4711"
                                           "_reason" "Splinte = Werkstatt"})]}
                      {:tool_calls [(call "c2" "final_answer" {"value" "done"})]})
        r   (tsys/with-provided {:llm/complete f}
              (run [{:op :use-llm :model "gpt-test" :source :lms :prompt "{{invoice}}"
                     :tools [{:apply-action :assign-cost-center}] :as :x}]
                   {:invoice an-invoice} (ctx :propose? true)))]
    (is (= [{:action :assign-cost-center :args {:invoice "i1" :cost-center "4711"}
             :reason "Splinte = Werkstatt"}]
           (:proposals r)))
    (is (empty? (logged r)))))

(deftest apply-action-tool-applies-through-the-action
  (let [[f] (scripted {:tool_calls [(call "c1" "apply_assign-cost-center"
                                          {"invoice" "i1" "cost-center" "4711"})]}
                      {:tool_calls [(call "c2" "final_answer" {"value" "done"})]})
        r   (tsys/with-provided {:llm/complete f}
              (with-redefs [sutra.actions.execution/lookup
                              (fn [_ _ {id :id}] (if id [(assoc an-invoice :id id)] []))]
              (run [{:op :use-llm :model "gpt-test" :source :lms :prompt "?"
                     :tools [{:apply-action :assign-cost-center}] :as :x}]
                   {:invoice an-invoice} (assoc (ctx) :reads? true))))]
    (is (= [[:update-object :invoice {:id "i1" :values {:cost-center "4711"}}]]
           (mapv (fn [[op t p]] [op t (select-keys p [:id :values])]) (logged r))))))

(deftest deterministic-apply-action-step
  (let [steps [{:op :apply-action :action :assign-cost-center
                :params {:invoice :invoice :cost-center "4711"}}]]
    (testing "applied, it runs the action"
      (let [r (run steps {:invoice an-invoice} (ctx))]
        (is (= [:update-object] (mapv first (logged r))))))
    (testing "proposed, it records the action with the object's id and edits nothing"
      (let [r   (run steps {:invoice an-invoice} (ctx :propose? true))]
        (is (= [{:action :assign-cost-center :args {:invoice "i1" :cost-center "4711"}}]
               (:proposals r)))
        (is (empty? (logged r)))))
    (testing "it counts as an edit for an edit function"
      (is (prog/writes-an-edit? steps)))))

(deftest propose-mode-refuses-a-direct-edit
  (is (re-find #"propose mode"
               (fails #(run [{:op :update :target :invoice :values {:cost-center "1"}}]
                            {:invoice an-invoice} (ctx :propose? true))))))

(deftest calculator
  (is (= 20.0 (logic/calculate "(2+3)*4")))
  (is (= -1.5 (logic/calculate "-3/2")))
  (is (fails #(logic/calculate "(java.lang.System/exit 0)"))))

(deftest clojure-source-form
  (let [{:keys [steps]} (prog/parse-source
                         {:language :clojure
                          :source (pr-str '(defn classify [^Invoice invoice]
                                             (let [cc
                                                     (OntologyClient/use-llm
                                                      {:model "gpt-test" :source :lms
                                                       :prompt "{{invoice.text}}"
                                                       :output :string})]
                                               (OntologyClient/apply-action :assign-cost-center
                                                                            {:invoice invoice
                                                                             :cost-center cc}))))})]
    (is (= [:use-llm :apply-action] (mapv :op steps)))))

(deftest install-validation-names-unknown-tools-and-models
  (let [compiled (prog/compile-function world [{:op :use-llm :model "nope" :source :lms :prompt "x"
                                                :tools [{:apply-action :missing}
                                                        {:query-objects :ghost :properties [:a]}]}]
                                        params)]
    (is (= #{[:unregistered-model "nope"] [:unknown-action :missing] [:unknown-type :ghost]}
           (set (map (juxt :kind :ref) (structure/function-schema-violations world compiled)))))))

(deftest query-objects-tool-reads-a-bounded-filtered-set
  (let [asked (atom nil)
        [f
         seen] (scripted {:tool_calls [(call "c1" "query_invoice"
                                             {"where" {"nr" "R-1" "secret" "x"} "limit" 500})]}
                           {:tool_calls [(call "c2" "final_answer" {"value" "ok"})]})]
    (tsys/with-provided {:llm/complete f}
      (with-redefs [sutra.actions.execution/read-store (fn [_ _ q] (reset! asked q)
                                                    [(assoc-in an-invoice [:values :cost-center]
                                                               "9")])]
      (run [{:op :use-llm :model "gpt-test" :source :lms :prompt "?"
             :tools [{:query-objects :invoice :properties [:nr :text] :limit 10}] :as :x}]
           {:invoice an-invoice} (ctx))))
    (testing "only declared properties filter, and the model cannot raise the bound"
      (is (= [:select (os/qt->ir {:root :invoice :limit 10 :where [:and [:= :nr "R-1"]]})] @asked)))
    (testing "only declared properties are returned"
      (is (= {"objects" [{"nr" "R-1" "text" "Splinte, Dichtmittel" "id" "i1"}]}
             (json/read-str (get-in (second @seen) [:request "messages" 2 "content"])))))))
