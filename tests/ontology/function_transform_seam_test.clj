(ns ^:unit ontology.function-transform-seam-test
  (:require [sutra.kit.anomaly :as anomaly]
            [support.replay :as replay]
            [support.system :as tsys]
            [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [sutra.catalog.functions :as functions]
            [sutra.actions.effects :as action-effects]
            [sutra.authoring.install :as install]
            [sutra.compute.runner :as runner]
            [sutra.types.rid :as rid]))

(def ^:private build-rid (rid/mint-for :transforms (random-uuid)))
(def ^:private py-fn-rid (rid/mint-for :transforms (random-uuid)))
(def ^:private clj-fn-rid (rid/mint-for :transforms (random-uuid)))
(def ^:private sql-fn-rid (rid/mint-for :transforms (random-uuid)))
(def ^:private edit-rid  (rid/mint-for :functions (random-uuid)))

(def ^:private world
  {:transforms
   {build-rid   {:id build-rid :transform-name :dedup-rows
                 :language :python :code "def f(): pass"
                 :inputs {:locations {:kind :object-type :object-type :location}}
                 :output {:kind :dataset :dataset :dedup_rows}}
    py-fn-rid   {:id py-fn-rid :transform-name :dedup-plan :function-name :dedup-plan
                 :language :python :code "def f(): pass" :inputs {}}
    clj-fn-rid  {:id clj-fn-rid :transform-name :survivor-plan :function-name :survivor-plan
                 :language :clojure :code "(fn [_] [])" :inputs {}}
    sql-fn-rid  {:id sql-fn-rid :transform-name :sql-plan :function-name :sql-plan
                 :language :pg-sql :query "select 1" :inputs {}}}
   :functions
   {edit-rid {:id edit-rid :function-name :approve-invoice
              :language :clojure :source "(fn [_] [])"}}})

(defn- invoke-function
  [step]
  (tsys/ensure!)
  (let [verb (get (tsys/verbs) :invoke-function)]
    (is (some? verb) ":invoke-function must be contributed by the action-effects module")
    (replay/evaluate {} #(verb step world {} %1 %2))))

(defn- refusal
  [step]
  (try (invoke-function step) nil
       (catch clojure.lang.ExceptionInfo e e)))

(deftest a-name-resolves-to-exactly-one-lifecycle
  (testing "a stamped transform entry is a QUERY function"
    (is (= :query (:kind (functions/function-by-name world :dedup-plan))))
    (is (= py-fn-rid (:rid (functions/function-by-name world :dedup-plan)))))

  (testing "the `:functions` section holds EDIT functions"
    (is (= :edit (:kind (functions/function-by-name world :approve-invoice)))))

  (testing "a build transform is NOT a Function — this is the resolution that drifted"
    (is (nil? (functions/function-by-name world :dedup-rows))
        "scanning `:transforms` by :transform-name would have matched it")
    (is (some? (functions/build-transform-by-name world :dedup-rows))))

  (testing "and the complement holds: a query function is not a build transform"
    (is (nil? (functions/build-transform-by-name world :dedup-plan))))

  (testing "the two partitions of the shared store are disjoint and total"
    (is (= (set (keys (:transforms world)))
           (into (set (keys (functions/query-functions world)))
                 (keys (functions/build-transforms world)))))
    (is (empty? (set/intersection
                 (set (keys (functions/query-functions world)))
                 (set (keys (functions/build-transforms world))))))))

(deftest invoking-a-build-transform-is-refused-by-name
  (let [e (refusal {:function :dedup-rows})]
    (is (some? e) "a build transform must not be runnable as a Function")
    (is (= :build-transform (:lifecycle (ex-data e))))
    (is (re-find #"BUILD transform" (ex-message e))
        "the message must name WHICH of the two lifecycles was hit — reporting it
         as unknown would send the author looking for a typo")))

(deftest invoking-an-edit-function-is-refused-by-name
  (let [e (refusal {:function :approve-invoice})]
    (is (some? e))
    (is (= :edit (:lifecycle (ex-data e))))
    nil))

(deftest an-unknown-name-is-still-just-unknown
  (let [e (refusal {:function :no-such-thing})]
    (is (some? e))
    (is (nil? (:lifecycle (ex-data e)))
        "no lifecycle tag — nothing by that name exists in either")))

(deftest a-sql-body-cannot-be-a-function
  (let [e (refusal {:function :sql-plan})]
    (is (some? e))
    (is (= :pg-sql (:language (ex-data e))))
    (is (re-find #"views" (ex-message e))
        "the reason is that :sql resolves its inputs as views over BUILT tables,
         which an action step does not have")))

(deftest a-clojure-function-runs-in-process-not-in-the-python-jail
  (with-redefs [runner/run-code! (fn [lang _ _]
                                   (if (= :clojure lang)
                                     {:rows [{:id 1} {:id 2}]}
                                     (throw
                                      (AssertionError.
                                       "the python runner was reached for a :clojure Function"))))]
    (let [{:keys [bindings side-effects]} (invoke-function {:function :survivor-plan})]
      (is (= [{:id 1} {:id 2}] (:function-result bindings))
          "the BINDING is the rows — not the {:rows …} payload map the runners return")
      (is (= 2 (:result-count (first side-effects)))
          "so the audit event counts rows, not payload keys"))))

(deftest a-python-function-still-ships-to-the-jail
  (with-redefs [runner/run-code!
                (fn [lang _ _]
                  (if (= :python lang)
                    {:rows [{:id 1}]}
                    (throw (AssertionError.
                            "the in-process runner was reached for a :python Function"))))]
    (is (= [{:id 1}] (-> (invoke-function {:function :dedup-plan}) :bindings :function-result)))))

(deftest a-function-returns-one-result-not-several-named-outputs
  (with-redefs [runner/run-code! (fn [_ _ _] {:outputs {:a [] :b []}})]
    (let [e (refusal {:function :dedup-plan})]
      (is (some? e) "multi-output is a build transform's shape, not a Function's")
      (is (= [:a :b] (:outputs (ex-data e)))))))

(deftest run-query-function-returns-rows-and-reuses-the-refusals
  (testing "returns the rows a query function produced, not the {:rows …} payload"
    (with-redefs [runner/run-code! (fn [_ _ _] {:rows [{:id 1} {:id 2}]})]
      (is (= [{:id 1} {:id 2}] (action-effects/run-query-function nil world :survivor-plan))
          "the standalone runner binds ITS OWN result — a row seq")))
  (testing "an empty-input query function needs no tx (nil is fine)"
    (with-redefs [runner/run-code! (fn [_ _ _] {:rows []})]
      (is (= [] (action-effects/run-query-function nil world :dedup-plan)))))
  (testing "it refuses exactly what the step verb refuses, with the same ex-data"
    (is (= :edit
           (:lifecycle (ex-data (try (action-effects/run-query-function nil world :approve-invoice)
                                     (catch clojure.lang.ExceptionInfo e e)))))
        "an EDIT function")
    (is (= :build-transform
           (:lifecycle (ex-data (try (action-effects/run-query-function nil world :dedup-rows)
                                     (catch clojure.lang.ExceptionInfo e e)))))
        "a build transform")
    (is (= :pg-sql (:language (ex-data (try (action-effects/run-query-function nil world :sql-plan)
                                         (catch clojure.lang.ExceptionInfo e e)))))
        "a :sql body")
    (is (nil? (:lifecycle (ex-data (try (action-effects/run-query-function nil world :no-such-thing)
                                        (catch clojure.lang.ExceptionInfo e e)))))
        "an unknown name is still just unknown")))

(deftest a-sql-query-function-is-refused-by-the-install-plan
  (let [desired {:objects   {:widget {:attributes [[:id :string]] :plural "Widgets"}}
                 :functions {:sql-plan {:language :pg-sql :query "select 1"
                                        :inputs {:w {:kind :object-type :object-type :widget}}}}}
        e (try (doall (install/plan-schema desired {} {})) nil
               (catch clojure.lang.ExceptionInfo ex ex))]
    (is (some? e)
        "a :sql body installs fine as a transform and can never run as a Function
         — catching that at publish beats catching it mid-transaction in prod")
    (is (= :pg-sql (:language (ex-data e))))))

(deftest a-query-function-may-not-declare-an-output
  (doseq [[label out] [["single :output" {:output {:kind :object-type :object-type :widget}
                                          :primary-key :id}]
                       ["multi :outputs" {:outputs [{:kind :dataset :dataset :d :name :d}]}]]]
    (testing label
      (let [desired {:objects   {:widget {:attributes [[:id :string]] :plural "Widgets"}}
                     :functions {:plan (merge {:language :python :code "def f(): pass"
                                               :inputs {:w {:kind :object-type
                                                            :object-type :widget}}}
                                              out)}}
            e (try (doall (install/plan-schema desired {} {})) nil
                   (catch clojure.lang.ExceptionInfo ex ex))]
        (is (some? e))
        (is (some? (:output (ex-data e))))
        (is (re-find #"backing transform" (ex-message e))
            "the message must say what an output would MAKE it, not just that it is refused")))))

(deftest a-scalar-arg-reaches-a-clojure-function-body
  (let [w (-> world
              (assoc-in [:transforms clj-fn-rid :code] "(fn [{:keys [jahr]}] [{:jahr jahr}])")
              (assoc-in [:transforms clj-fn-rid :inputs] {:jahr {:kind :scalar}}))]
    (is (= [{:jahr 2026}] (action-effects/run-query-function nil w :survivor-plan {:jahr 2026}))
        "the caller's scalar arg binds to the function's declared :scalar input and
         reaches the in-process body — the whole point of passing a year in")
    (let [e (try (action-effects/run-query-function nil w :survivor-plan) nil
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? e)
          "a declared :scalar the call bound NOTHING to is refused. Binding nil
           instead made 'no argument' and 'the argument was nil' the same value
           to the body — which left a call with a literal argument no spelling
           from an Action at all")
      (is (anomaly/client-error? (ex-data e)) "it is the caller's mistake — a 400, not a 500")
      (is (= :jahr (:alias (ex-data e))) "and the refusal names the input"))))

(deftest a-scalar-arg-reaches-a-python-function-manifest
  (let [w    (assoc-in world [:transforms py-fn-rid :inputs] {:jahr {:kind :scalar}})
        seen (atom nil)]
    (with-redefs [runner/run-code! (fn [_ _ opts] (reset! seen opts) {:rows []})]
      (action-effects/run-query-function nil w :dedup-plan {:jahr 2026}))
    (is (= {:kind :scalar :value 2026} (get-in @seen [:inputs-manifest :jahr]))
        "the scalar travels to the sandbox as its value, not as an empty row file")
    (is (empty? (:input-paths @seen)) "and binds no file")))

(deftest a-code-query-function-plans-normally
  (let [desired {:objects   {:widget {:attributes [[:id :string]] :plural "Widgets"}}
                 :functions {:plan {:language :python :code "def f(): pass"
                                    :inputs {:w {:kind :object-type :object-type :widget}}}}}]
    (is (some? (seq (filter #(= :functions (:phase %))
                            (install/plan-schema desired {} {}))))
        "the gate must not swallow the lifecycle it exists to protect")))

(deftest a-scalar-input-query-function-plans-normally
  (let [desired {:objects   {:widget {:attributes [[:id :string]] :plural "Widgets"}}
                 :functions {:plan {:language :clojure :code "(fn [{:keys [jahr]}] [])"
                                    :inputs {:w    {:kind :object-type :object-type :widget}
                                             :jahr {:kind :scalar}}}}}]
    (is (some? (seq (filter #(= :functions (:phase %))
                            (install/plan-schema desired {} {}))))
        "a :scalar input must not fail the install-plan input validator")))

(deftest the-unbound-scalar-refusal-names-its-call-site
  (let [w       (-> world
                    (assoc-in [:transforms clj-fn-rid :code]
                              "(fn [{:keys [sicht]}] [{:sicht sicht}])")
                    (assoc-in [:transforms clj-fn-rid :inputs] {:sicht {:kind :scalar}}))
        refusal (fn [site] (try (action-effects/run-query-function nil w :survivor-plan {} site) nil
                                (catch clojure.lang.ExceptionInfo ex ex)))]
    (testing "from an app variable it points at the variable's :args, not at an action"
      (let [e (refusal {:call-site :app-variable :application :kasse})]
        (is (= :app-variable (:call-site (ex-data e))))
        (is (re-find #":args \{:sicht" (ex-message e)))
        (is (re-find #"app kasse" (ex-message e)))
        (is (not (re-find #"(?i)action|rule" (ex-message e))))))
    (testing "from an action rule it points at the parameter and the rule's :inputs"
      (let [e (refusal {:call-site :action-rule})]
        (is (re-find #"action a :sicht parameter" (ex-message e)))
        (is (re-find #":inputs \{:sicht" (ex-message e)))))))
