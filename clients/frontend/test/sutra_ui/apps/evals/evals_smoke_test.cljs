(ns sutra-ui.apps.evals.evals-smoke-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [reagent.dom.server :as server]
            [sutra-ui.apps.evals.core :as core]
            [sutra-ui.apps.evals.model :as m]
            [sutra-ui.apps.evals.results :as results]))

(defn- run [id model kontext cost]
  {:id id :run-at (str "2026-11-04T1" id) :ref "main" :iterations 2
   :summary {:passed 1 :total 2 :failed 1 :pass-rate 0.5
             :timing {:mean-ms 120}
             :usage {:calls 4 :prompt-tokens 100 :completion-tokens 20
                     :cost cost :currency "USD"}
             :experiment {:id "e1" :name "modelle" :params {:model model :kontext kontext}}}
   :metrics [{:metric "treffer" :value 0.5 :variance 0.1 :status "ungraded"}]})

(defn- kase [cid a b]
  {:case-id cid :passed false :input {:text "t"} :expected {:soll 9} :actual {:x b}
   :metrics {:treffer [{:value 1 :pass? true} {:value 0 :pass? false :debug "warum"}]}
   :trace [{:ms 5 :spans [{:name "fn" :ms 4}] :log ["zeile"] :actual {:x a}}
           {:ms 6 :actual {:x b}}]})

(def ^:private suite {:id "s1" :name "beleg"})

(defn- reset-ui! [m]
  (reset! m/ui (merge {:runs {"s1" [(run "a" "m1" "x" 0.01) (run "b" "m2" "y" 0.03)]}
                       :cases {"a" [(kase "c1" 1 2)] "b" [(kase "c1" 3 4)]}
                       :compare ["a" "b"] :filter :all :diff? true
                       :group-cases? true :group-iterations? true
                       :hidden-runs #{} :hidden-cases #{} :iteration {}}
                      m)))

(defn- html [c] (server/render-to-static-markup c))

(deftest the-runs-table-shows-parameters-groups-and-filters
  (reset-ui! {})
  (let [h (html [results/runs-table suite])]
    (is (str/includes? h "kontext") "a parameter is a column")
    (is (str/includes? h "0.0100 USD") "a run's cost shows"))
  (testing "group by a parameter gives an aggregate row per value"
    (reset-ui! {:group-by :kontext})
    (is (str/includes? (html [results/runs-table suite]) "<b>x</b>")))
  (testing "the filter narrows to one experiment"
    (reset-ui! {:experiment "andere"})
    (is (not (str/includes? (html [results/runs-table suite]) "m1"))))
  (testing "a hidden column goes"
    (reset-ui! {:hidden-runs #{"cost"}})
    (is (not (str/includes? (html [results/runs-table suite]) "0.0100 USD")))))

(deftest the-test-cases-table-groups-cases-and-iterations
  (testing "grouped: one row per case, its iterations in a selector, on the last one"
    (reset-ui! {})
    (let [h (html [results/compare-view suite])]
      (is (str/includes? h "<select"))
      (is (str/includes? h "{:x 2}"))
      (is (not (str/includes? h "{:x 1}")))))
  (testing "the selector shows the iteration it names"
    (reset-ui! {:iteration {["a" "c1"] 0}})
    (is (str/includes? (html [results/compare-view suite]) "{:x 1}")))
  (testing "iterations listed: every output shows"
    (reset-ui! {:group-iterations? false})
    (let [h (html [results/compare-view suite])]
      (is (str/includes? h "{:x 1}"))
      (is (str/includes? h "{:x 2}"))))
  (testing "cases apart: a row per case and run, each with its run's parameters"
    (reset-ui! {:group-cases? false})
    (let [h (html [results/compare-view suite])]
      (is (str/includes? h "kontext=x"))
      (is (str/includes? h "kontext=y"))))
  (testing "a hidden output column goes"
    (reset-ui! {:hidden-cases #{"output"}})
    (is (not (str/includes? (html [results/compare-view suite]) "{:x 2}")))))

(deftest the-debug-drawer-compares-when-grouped
  (testing "alone: one run's logs"
    (reset-ui! {:debug ["a" "c1" nil]})
    (let [h (html [results/debug-view suite])]
      (is (str/includes? h "zeile"))
      (is (not (str/includes? h "aev-cols")))))
  (testing "from a grouped table: its runs side by side"
    (reset-ui! {:debug ["a" "c1" nil] :debug-runs ["a" "b"]})
    (is (str/includes? (html [results/debug-view suite]) "aev-cols"))))

(deftest the-run-dialog-sends-the-grid
  (let [body (#'core/run-body {:name "beleg"}
                              {:exp-on? true :exp-name "modelle"
                               :exp-rows [{:param "model" :values "a\n b \n\n"}
                                          {:param "temperatur" :values "0.7\n1"}
                                          {:param "" :values "bleibt weg"}]})]
    (is (= {:name "modelle" :params {:model ["a" "b"] :temperatur [0.7 1]}}
           (:experiment body))))
  (testing "experiments off: a single run"
    (is (nil? (:experiment (#'core/run-body {:name "beleg"}
                                            {:exp-rows [{:param "model" :values "a"}]})))))
  (testing "the preview lists every combination"
    (let [h (html [#'core/combination-table
                   {:runs 2 :answers 8 :combinations [{:model "a"} {:model "b"}]}])]
      (is (str/includes? h ">b</td>")))))
