(ns ^:unit platform.eval-parity
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.evals.cases :as cases]
            [sutra.evals.evaluators :as ev]
            [sutra.evals.experiments :as exp]
            [sutra.evals.suites :as evals]
            [sutra.evals.validate :as validate]))

(def ^:private score-actuals #'evals/score-actuals)
(def ^:private metric-of #'evals/metric-of)
(def ^:private case-results #'evals/case-results)
(def ^:private models-of #'evals/models-of)

(defn- judge [e a x] (:value (ev/judge e {:actual a :expected x})))

(deftest evaluators-judge-one-case
  (testing "generic exact match coerces numbers and dates"
    (is (true? (judge {:kind :exact-match :field :n} {:n "12.50"} {:n 12.5})))
    (is (true? (judge {:kind :exact-match :field :d} {:d "2026-06-25T00:00:00Z"}
                      {:d "2026-06-25"})))
    (is (true? (judge {:kind :exact-match :field :s :ignore-case true} {:s "AB"} {:s "ab"}))))
  (testing "no expected value: the case is not graded"
    (is (nil? (ev/judge {:kind :exact-match :field :n} {:actual {:n 1} :expected {}}))))
  (testing "ranges around the expected"
    (is (true? (judge {:kind :numeric-range :field :netto :tolerance 0.05}
                      {:netto 100.04} {:netto 100})))
    (is (false? (judge {:kind :numeric-range :field :netto :tolerance-pct 1}
                       {:netto 102} {:netto 100})))
    (is (true? (judge {:kind :temporal-range :field :datum :tolerance-days 2}
                      {:datum "2026-06-27"} {:datum "2026-06-25"}))))
  (testing "text distance and patterns"
    (is (= 1 (judge {:kind :levenshtein :field :nr} {:nr "RE-1234"} {:nr "RE-1235"})))
    (is (true? (judge {:kind :regex :field :nr :pattern "RE-\\d+"} {:nr "RE-1234"} {})))
    (is (true? (judge {:kind :array-match :field :xs :ordered? false}
                      {:xs ["b" "a"]} {:xs ["a" "b"]})))
    (is (< 0.99 (judge {:kind :rouge :field :t} {:t "the cat sat"} {:t "The cat sat."}))))
  (testing "an action's planned edits contain the expected one (a partial map)"
    (is (true? (judge {:kind :edits :field :plan}
                      {:plan [{:op :modify :target {:type "rechnung"} :values {:status "gebucht"
                                                                               :x 1}}]}
                      {:plan [{:target {:type "rechnung"} :values {:status "gebucht"}}]})))))

(deftest objectives-are-per-case
  (testing "a boolean metric passes on true by default; a number needs an objective"
    (is (true? (ev/passes? {:kind :exact-match} true)))
    (is (nil? (ev/passes? {:kind :levenshtein} 3)))
    (is (false? (ev/passes? {:kind :levenshtein :objective {:max 2}} 3)))
    (is (true? (ev/passes? {:kind :exact-match :objective {:expect false}} false)))))

(def ^:private suite
  {:name "kopf"
   :function :kopf-lesen
   :cases [{:id "a" :input {:pdf "a"} :expected {:netto 10 :nr "R1"}}
           {:id "b" :input {:pdf "b"} :expected {:netto 20 :nr "R2"}}
           {:id "c" :input {:pdf "c"} :expected {:netto 30}}]
   :evaluators [{:name :netto :kind :numeric-range :field :netto :tolerance 0.01}
                {:name :nr :kind :levenshtein :field :nr :objective {:max 1}}]})

(deftest a-case-passes-when-every-metric-passes-in-every-iteration
  (let [it1      [{:netto 10 :nr "R1"} {:netto 20 :nr "XX9"} {:netto 31}]
        it2      [{:netto 10 :nr "R1"} {:netto 20 :nr "R2"} {:netto 30}]
        per-iter [(score-actuals suite it1 nil) (score-actuals suite it2 nil)]
        rows     (case-results suite [it1 it2] per-iter {} {})]
    (is (= [true false false] (mapv :passed rows))
        "b fails its name in iteration 1, c its amount: one failed iteration fails the case")
    (let [netto (metric-of (first (:evaluators suite)) per-iter)
          nr    (metric-of (second (:evaluators suite)) per-iter)]
      (is (= "ungraded" (:status netto)) "a metric has no verdict of its own, only its cases")
      (is (< (abs (- (/ 5.0 6) (:value netto))) 1e-9)
          "the mean over both iterations, shown, not judged")
      (is (== 1.0 (:pass-rate nr))))))

(deftest a-metric-that-graded-nothing-is-no-pass
  (let [s  {:name "x" :evaluators [{:name :nr :kind :exact-match :field :nr}]
            :cases [{:input {} :expected {}}]}
        m  (metric-of (first (:evaluators s)) [(score-actuals s [{:nr "1"}] nil)])]
    (is (nil? (:value m)))
    (is (= "no-cases" (:status m)))))

(def ^:private schema {:actions {:notiz-anlegen {:name :notiz-anlegen}}})

(deftest validation-names-the-mistake
  (let [ps (validate/problems schema
                              {:name "probe" :action :notiz-anlegen
                               :cases [{:input {:text "x"} :expected {:plan []}}]
                               :evaluators [{:name "plan" :kind :edits :field "pla"}
                                            {:name "netto" :kind :exact-match :field "plan"
                                             :objective {:op :>= :value 0.9}}
                                            {:name "mittel" :kind :exact-match :field "plan"
                                             :target {:min 0.9}}
                                            {:name "z" :kind :fuzzy}]})
        txt (str/join "\n" ps)]
    (is (str/includes? txt "did you mean \"plan\""))
    (is (str/includes? txt "does not exist"))
    (is (str/includes? txt "evaluator mittel: :target is removed"))
    (is (str/includes? txt "unknown :kind :fuzzy")))
  (is (str/includes? (str/join "\n" (validate/problems schema
                                                       {:name "alt" :transform :grok
                                                        :match :quell_pdf
                                                        :evaluators [{:name "n"
                                                                      :kind :exact-match
                                                                      :field "netto"}]}))
                     "a transform as eval target is removed"))
  (is (empty? (validate/problems schema
                                 {:name "ok" :action :notiz-anlegen
                                  :evaluators [{:name "p" :kind :edits :field "plan"}]}))))

(deftest experiments-run-the-grid
  (is (= #{{:model "a" :p "x"} {:model "a" :p "y"} {:model "b" :p "x"} {:model "b" :p "y"}}
         (set (exp/grid {:model ["a" "b"] :p ["x" "y"]})))))

(def ^:private experiment-schema
  {:llm-models {"lesemodell" {:in 1 :out 2} "anderes" {:in 1 :out 2}}
   :functions
   {:ri.f {:function-name :lesen :language :clojure :kind :edit
           :source "(defn lesen [d] (OntologyClient/use-llm {:model \"lesemodell\" :prompt \"x\"}))"}}
   :transforms
   {:ri.q {:function-name :antwort :language :python
           :inputs {:llm {:kind :language-model :model "lesemodell" :source :xai}
                    :kontext {:kind :scalar :type :string}}}}})

(deftest an-experiment-swaps-the-model-the-function-really-calls
  (testing "a use-llm literal follows the run's model"
    (let [s (exp/at-model experiment-schema {:function :lesen} "anderes" nil)]
      (is (= ["anderes"] (models-of s (get-in s [:functions :ri.f]))))))
  (testing "a language-model input follows it, through the source the registry names"
    (let [s (exp/at-model experiment-schema {:function :antwort} "anderes" nil)]
      (is (= {:kind :language-model :model "anderes"}
             (get-in s [:transforms :ri.q :inputs :llm])))))
  (testing "a language-model input by its name swaps that input"
    (let [{:keys [model-inputs]} (exp/apply-combination
                                  experiment-schema
                                  {:function :antwort :cases []} {:llm "anderes"})]
      (is (= {:llm "anderes"} model-inputs))
      (is (= "anderes" (get-in (exp/at-model experiment-schema {:function :antwort}
                                             nil model-inputs)
                               [:transforms :ri.q :inputs :llm :model])))))
  (testing "no model asked: the schema stays"
    (is (= experiment-schema (exp/at-model experiment-schema {:function :lesen} nil nil))))
  (testing "a model the source computes is refused, not recorded as if it had run"
    (let [s (assoc-in experiment-schema [:functions :ri.f :source]
                      "(defn lesen [m] (OntologyClient/use-llm {:model m :prompt \"x\"}))")]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"as a literal"
                            (exp/at-model s {:function :lesen} "anderes" nil))))))

(deftest experiment-values-override-the-cases
  (let [suite {:input-map {:kontext :spalte}
               :cases [{:id "a" :input {:spalte "alt" :x 1}}]}
        {s :suite :as at} (exp/apply-combination
                           experiment-schema
                           (assoc suite :function :antwort)
                           {:kontext 5 :model "anderes" :version "1.0.0"})]
    (is (= {:x 1 :kontext "5"} (:input (first (:cases s))))
        "the value replaces the column it read, as the input's declared type")
    (is (empty? (:input-map s)))
    (is (= ["anderes" "1.0.0"] [(:model at) (:version at)]))))

(deftest an-experiment-is-refused-before-it-costs
  (let [problems #(exp/problems experiment-schema %1 {:params %2})]
    (is (empty? (problems {:function :lesen} {:model ["lesemodell" "anderes"]})))
    (is (re-find #"needs a list" (first (problems {:function :lesen} {:model "anderes"}))))
    (is (re-find #"needs a list" (first (problems {:function :lesen} {:model []}))))
    (is (re-find #"not in the LLM registry"
                 (first (problems {:function :lesen} {:model ["gibt-es-nicht"]}))))
    (is (seq (exp/problems experiment-schema {:function :lesen} {:params {}})))))

(deftest input-mapping-renames-case-columns
  (is (= {:rechnung "R1" :other 1}
         (cases/mapped-input {:rechnung :rechnungs-id} {:rechnungs-id "R1" :other 1}))))

(def ^:private at-version #'evals/at-version)

(def ^:private versioned-schema
  {:functions {:ri.f {:function-name :lesen :language :clojure :version "1.1.0"
                      :source "(defn lesen [d] (OntologyClient/use-llm {:model \"lesemodell\"}))"
                      :releases {"1.0.0" {:source "(defn lesen [d] :alt)"}
                                 "1.1.0" {:source "(defn lesen [d] :neu)"}}}}})

(deftest a-run-measures-the-release-it-names
  (testing "a release replaces the measured function; the last saved state stays without one"
    (is (= "(defn lesen [d] :alt)"
           (get-in (at-version versioned-schema {:function :lesen} "1.0.0")
                   [:functions :ri.f :source])))
    (is (= versioned-schema (at-version versioned-schema {:function :lesen} nil)))
    (is (= versioned-schema (at-version versioned-schema {:function :lesen} "published"))))
  (testing "a release the function never had is refused, naming the ones it has"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"no release 9.9.9 — published: 1.0.0, 1.1.0"
                          (at-version versioned-schema {:function :lesen} "9.9.9")))))

(deftest a-logic-function-names-its-model-in-its-body
  (is (= ["lesemodell"]
         (models-of versioned-schema (get-in versioned-schema [:functions :ri.f])))))
