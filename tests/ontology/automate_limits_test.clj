(ns ^:unit ontology.automate-limits-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.automation.rules :as rules]
            [sutra.automation.runs :as autos]
            [sutra.automation.activity :as activity]
            [sutra.catalog.functions :as functions]
            [support.logic-ledger :as ledger]
            [sutra.actions.dispatch :as c]
            [sutra.objects.reads :as objects]
            [sutra.store.seat :as seat]
            [sutra.actions.action :as action]
            [sutra.store.access.scope :as ctx]
            [sutra.types.run-limits :as limits]))

(def ^:private schema
  {:object-types {}
   :actions   {:assign {:rules []}}
   :functions {:ri.fn {:function-name :classify :source {} :target-type :invoice
                       :param-types {:invoice :invoice} :timeout-ms 1000}}})

(defn- objects [n]
  (mapv (fn [i] {:id (str "i" i) :values {:nr (str "R-" i)}}) (range n)))

(defn- run-automation [effect matched opts & {:keys [run-logic dispatch-batch]}]
  (let [recorded (atom nil) calls (atom 0)]
    (with-redefs-fn (second (ledger/fake))
      (fn [] (with-redefs [ctx/store            (constantly nil)
                           seat/branch-schema   (constantly schema)
                           objects/objects-of-type (fn [& _] matched)
                           c/run-logic!
                             (fn [& _] (swap! calls inc) ((or run-logic (constantly {}))))
                           c/dispatch-action-batch!
                             (fn [& _] (swap! calls inc) ((or dispatch-batch (constantly nil))))
                           autos/mark-fired! (fn [& _])
rules/owner-authority (constantly {:user "u-owner" :username "owner"})
                           autos/record-run! (fn [_ _ _ r] (reset! recorded r))
                           activity/after-run! (fn [& _])]
               (let [r (rules/run-one! nil "main"
                                       {:id "a1" :owner "owner" :on "invoice"
                                        :effect-spec (pr-str effect)} opts)]
                 {:result r :recorded @recorded :calls @calls}))))))

(deftest a-hung-logic-run-cannot-hold-the-poller
  (let [interrupted (promise)
        t0 (System/currentTimeMillis)
        {:keys [result recorded]}
        (with-redefs [limits/await-margin-ms 300]
          (run-automation {:function :classify} (objects 2) {}
                          :run-logic #(try (Thread/sleep 60000) {}
                                           (catch InterruptedException _ (deliver interrupted true)
                                             {}))))
        took (- (System/currentTimeMillis) t0)]
    (is (< took 5000)
        "the evaluation returns after the time limit plus margin, not when the model does")
    (is (= 2 (count (:errors result))))
    (is (every? #(re-find #"did not finish within 1300 ms" %) (:errors result)))
    (is (= "error" (:status recorded)))
    (is (true? (deref interrupted 2000 false)) "the straggler is cancelled, not left running")))

(deftest over-a-thousand-matched-objects-is-an-error-run
  (let [n (inc (:scheduled-per-object-max limits/automate))]
    (testing "a scheduled per-object Logic evaluation"
      (let [{:keys [result recorded calls]} (run-automation {:function :classify} (objects n) {})]
        (is (zero? calls) "nothing runs — not a partial run")
        (is (= "error" (:status recorded)))
        (is (re-find #"1001 objects matched, more than the 1000" (:detail recorded)))
        (is (= 0 (:applied result)))))
    (testing "a manual run takes the larger manual limit"
      (let [{:keys [calls]} (run-automation {:function :classify} (objects n) {:manual? true})]
        (is (= n calls))))
    (testing "a batched action effect is capped at the batch size"
      (with-redefs [functions/batched-action? (constantly true)]
        (let [{:keys [recorded
                      calls]} (run-automation {:action :assign :args {:invoice {:self :id}}}
                                                       (objects n) {:manual? true})]
          (is (zero? calls))
          (is (re-find #"batch size of 1000" (:detail recorded))))))))

(defn- kfn [steps & {:as extra}]
  (merge {:language :kernel :source {:params {} :steps steps}} extra))

(deftest the-run-policy-follows-the-function
  (let [world {:actions   {:ask-act {:rules [{:op :apply-edit-function :function :asker}]}}
               :functions {:ri.a (assoc (kfn [{:op :use-llm :model "m" :source :s :prompt "?"}])
                                        :function-name :asker)}}
        pol   #(:timeout-ms (action/run-policy world %))]
    (testing "a function behind an action and a plain edit function both get the 60 s default"
      (is (= 60000 (pol {:action :ask-act})))
      (is (= 60000 (pol {:spec (kfn [{:op :search :type :t :as :x}])}))))
    (testing ":timeout-ms on the function wins, clamped to the grammar's bounds"
      (is (= 600000 (pol {:spec (kfn [] :timeout-ms 9999999)}))))))
