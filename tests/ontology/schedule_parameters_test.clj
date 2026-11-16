(ns ontology.schedule-parameters-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.kit.refusal :as refusal]
            [sutra.builds.run.setup :as run-setup]
            [sutra.schedules.dispatch :as dispatch]
            [sutra.schedules.run-setups :as run-setups]
            [sutra.schedules.schedules :as schedules]
            [sutra.compute.external :as ext]
            [sutra.compute.sandbox :as sandbox]
            [sutra.compute.runner :as runner]
            [sutra.kit.system :as sys]
            [sutra.parser.params :as params]
            [sutra.foundation.side-effect :as fx]
            [sutra.catalog.branch-policy :as policy]
            [support.replay :as replay]
            [support.system :as tsys]
            [sutra.builds.plan]))

(def ^:private transforms
  {:fares   {:transform-name :fares :language :python
             :parameters {:suche_id {:type :string :default nil}
                          :max     {:type :number :default 12}}}
   :shaping {:transform-name :shaping :language :pg-sql :query "select 1"}})

(deftest ^:unit binding-gives-each-declared-parameter-its-value
  (let [bound (params/bind transforms {:suche_id "S-1" :max 3})]
    (is (= "S-1" (get-in bound [:fares :parameters :suche_id :default])))
    (is (= 3 (get-in bound [:fares :parameters :max :default])))
    (is (= (:shaping transforms) (:shaping bound)) "a transform declaring none stays as it is"))
  (is (identical? transforms (params/bind transforms {})) "no values, no change")
  (is (= [:typo] (params/undeclared transforms {:suche_id "x" :typo 1}))))

(deftest ^:unit a-parallelized-schedule-runs-only-by-its-run-setups
  (let [s {:name "preise" :target "fares" :parameters {:suche_id nil :max 12}
           :run-setup-key :suche_id}]
    (is (nil? (schedules/mode-problem s)))
    (is (nil? (schedules/mode-problem {:name "plain" :target "x" :cron "0 2 * * *"}))
        "a schedule without a run-setup key is not parallelized")
    (is (re-find #"none of the schedule's :parameters"
                 (schedules/mode-problem (assoc s :run-setup-key :other))))
    (doseq [k [:cron :every-ms :trigger]]
      (is (re-find #"only by its run setups"
                   (schedules/mode-problem (assoc s k ({:cron "0 2 * * *" :every-ms 1000
                                                        :trigger {:kind :cadence :every-ms 1}} k))))
          (str k " is an automated trigger, which a parallelized schedule does not take")))))

(deftest ^:unit a-run-setup-is-the-schedule-parameters-overridden-by-its-values
  (let [s {:parameters {:suche_id nil :max 12} :run-setup-key :suche_id}]
    (is (= {:key :suche_id :value "S-1" :parameters {:suche_id "S-1" :max 12}}
           (schedules/run-setup s {:suche_id "S-1"})))
    (is (re-find #"needs a value" (:problem (schedules/run-setup s {:max 3}))))
    (is (re-find #"declares no parameter :nope"
                 (:problem (schedules/run-setup s {:suche_id "S" :nope 1}))))))

(deftest ^:unit a-schedule-holds-at-most-a-thousand-run-setups
  (let [full (mapv #(hash-map :key-value (str %)) (range run-setups/max-per-schedule))]
    (is (re-find #"at most" (apply refusal/message (run-setups/room-problem full {:value "new"}))))
    (is (nil? (run-setups/room-problem full {:value "7"}))
        "an existing one is updated, not added")))

(deftest ^:unit a-run-setup-owns-its-share-of-the-output
  (let [rs {:key :suche_id :value "S-1"}]
    (is (run-setup/scopes? rs (:fares transforms)))
    (is (not (run-setup/scopes? rs (:shaping transforms))))
    (is (not (run-setup/scopes? nil (:fares transforms)))
        "a build without a run setup shares nothing")
    (is (= [{:a 1 :suche_id "S-1"}] (run-setup/stamp rs [{:a 1}])))
    (is (run-setup/in-share? rs {:suche_id "S-1"}))
    (is (not (run-setup/in-share? rs {:suche_id "S-2"})))
    (is (not= (run-setup/share-key rs "x") (run-setup/share-key {:value "S-2"} "x"))
        "two shares minting the same key stay two rows")
    (is (= ["b"] (run-setup/stale ["a" "b"] ["a" "c"])))))

(deftest ^:unit the-build-request-carries-the-run-setup-values
  (with-redefs [sutra.builds.plan/schedule-targets (fn [_ _] {:only ["fares"]})]
    (let [s {:name "preise" :parameters {:suche_id nil :max 12} :run-setup-key :suche_id}
          rs {:key :suche_id :value "S-1" :parameters {:suche_id "S-1" :max 12}}]
      (is (= {:origin "schedule preise · S-1" :only-types ["fares"]
              :parameters {:suche_id "S-1" :max 12}
              :run-setup {:key :suche_id :value "S-1"} :run-id "r"}
             (dispatch/build-request {} s rs "r")))
      (is (= {:suche_id nil :max 12} (:parameters (dispatch/build-request {} s nil nil)))
          "fired by itself a schedule passes its own values"))))

(deftest ^:unit a-build-is-no-side-effect-a-branch-holds-back
  (is (= :allow (fx/side-effect-verdict "feature" #{} :builds))))

(deftest ^:unit a-schedule-rule-starts-the-build-and-binds-its-run
  (tsys/ensure!)
  (let [asked (atom nil)
        verb  (get (tsys/verbs) :run-schedule)
        real  sys/required]
    (is (some? verb) ":run-schedule is an action verb")
    (with-redefs [sys/required (fn [s k] (if (= k :schedule/start)
                                           (fn [_ branch req] (reset! asked [branch req])
                                             {:run-id "run-1"})
                                           (real s k)))]
      (let [{:keys [bindings side-effects]}
            (replay/evaluate {}
                             #(verb {:schedule "preise" :parameters {:suche_id [:get :suche]}
                                     :as :lauf}
                                    {} {:suche "S-1"} %1 %2))]
        (is (= "run-1" (:lauf bindings)) "the run id is bound for the rules after it")
        (is (= ["main" {:schedule "preise" :parameters {:suche_id "S-1"}}] @asked)
            "parameter values are resolved from the action's bindings")
        (is (= :schedule-run (:op (first side-effects))))))))

(def ^:private param-code
  "from sutra_transforms import transform, Input, StringParam, IntegerParam, BooleanParam

@transform(inputs={'stadt': StringParam('Seattle'), 'risiko': IntegerParam(5),
                   'aktiv': BooleanParam(False)})
def go(stadt, risiko, aktiv):
    return [{'stadt': stadt.value, 'risiko': risiko.value, 'aktiv': aktiv.value}]
")

(deftest ^:integration python-parameters-take-their-default-or-the-build-value
  (if-not (sandbox/available?)
    (println "  (schedule-parameters SKIPPED — bwrap unavailable)")
    (do
      (testing "introspection reads the declared parameters"
        (is (= {:stadt  {:type :string :default "Seattle"}
                :risiko {:type :number :default 5}
                :aktiv  {:type :boolean :default false}}
               (:parameters (ext/discover-transform-spec {:transform-name :p :code param-code})))))
      (let [run (fn [ps]
                  (:rows (runner/run-code! :python
                                           {:transform-name :p :code param-code :parameters ps}
                                           {})))]
        (testing "without values the defaults hold"
          (is (= [{:stadt "Seattle" :risiko 5 :aktiv false}] (run nil))))
        (testing "a bound value reaches .value, coerced to the parameter's type"
          (is (= [{:stadt "Wien" :risiko 7 :aktiv true}]
                 (run
                  (get-in (params/bind {:p {:parameters {:stadt  {:type :string :default "Seattle"}
                                                 :risiko {:type :number :default 5}
                                                 :aktiv  {:type :boolean :default false}}}}
                               {:stadt "Wien" :risiko "7" :aktiv "true"})
                          [:p :parameters])))))))))
