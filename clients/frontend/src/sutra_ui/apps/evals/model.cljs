(ns sutra-ui.apps.evals.model
  (:require [reagent.core :as r]
            [sutra-ui.apps.evals.events]
            [sutra-ui.state.machine :as machine]
            [sutra.types.eval-runs :as er]))

(defonce ui
  (r/atom {:suites :loading :sel nil :tab :results :runs {} :cases {}
           :run nil :compare [] :debug nil :group-by nil :filter :all :diff? true
           :experiment nil :finished nil :hidden-runs #{} :hidden-cases #{}
           :group-cases? true :group-iterations? true :iteration {}}))

(defonce evals (machine/frame ui {:name :eval-suites}))

(defn send! [event] (machine/dispatch event {:frame evals}))

(defn at
  [k]
  @(machine/subscribe [:eval-suites/at k] {:frame evals}))

(defn err-text [e] (or (.-message e) (str e)))

(defn suites [] (let [s (:suites @ui)] (when (sequential? s) s)))

(defn suite-named [nm] (some #(when (= nm (:name %)) %) (suites)))

(defn selected [] (suite-named (:sel @ui)))

(defn runs-of [suite-id]
  (let [rs (get-in @ui [:runs suite-id])]
    (when (sequential? rs) rs)))

(defn ensure-cases! [run-id] (send! [:eval-suites/ensure-cases run-id]))

(defn cases-of [run-id]
  (let [cs (get-in @ui [:cases run-id])]
    (when (sequential? cs) cs)))

(defn run-by-id [suite-id run-id]
  (some #(when (= run-id (:id %)) %) (runs-of suite-id)))

(defn draft-case!
  [c]
  (send! [:eval-suites/draft-case c]))

(defn select-suite! [nm] (send! [:eval-suites/select nm]))

(defn open-run! [run-id] (send! [:eval-suites/open-run run-id]))

(defn pct [v] (when (number? v) (str (.toFixed (* 100 v) 1) " %")))

(defn fmt-value
  [v]
  (cond (boolean? v) (if v "✓" "✗")
        (and (number? v) (<= 0 v 1)) (pct v)
        (number? v) (str (/ (js/Math.round (* 1000 v)) 1000))
        (nil? v) "—"
        :else (str v)))

(defn show [v]
  (cond (nil? v) ""
        (string? v) v
        :else (pr-str v)))

(defn clip [v n]
  (let [s (show v)] (if (> (count s) n) (str (subs s 0 n) "…") s)))

(defn run-experiment-id [run] (get-in run [:summary :experiment :id]))

(defn param-names
  [runs]
  (vec (sort (distinct (for [r runs [k] (er/run-params r)] (name k))))))

(defn experiments-of
  [runs]
  (vec (for [[id rs] (group-by run-experiment-id (filter run-experiment-id runs))]
         {:id id :name (get-in (first rs) [:summary :experiment :name]) :runs (count rs)})))

(defn shown-runs
  [runs]
  (if-let [id (:experiment @ui)]
    (filterv #(= id (run-experiment-id %)) runs)
    runs))

(defn usage [run] (get-in run [:summary :usage]))

(defn mean-ms [run] (get-in run [:summary :timing :mean-ms]))

(defn fmt-cost [cost currency]
  (when (number? cost) (str (.toFixed cost 4) " " (or currency ""))))

(defn parse-value
  [s]
  (cond (re-matches #"-?\d+(\.\d+)?" s) (js/parseFloat s)
        (= "true" s)  true
        (= "false" s) false
        :else         s))

(defn run-failed? [run]
  (let [s (:summary run)]
    (boolean (or (pos? (or (:failed s) 0)) (seq (:targets-failed s))))))

(defn run-label [run]
  (str (:run-at run)
       (when-let [m (er/run-model run)] (str " · " m))
       (when-let [v (er/run-version run)] (str " · v" v))
       (when (seq (er/run-params run)) (str " · " (er/params-text run)))))

(defn metric-names
  [runs]
  (vec (distinct (for [r runs m (:metrics r)] (name (:metric m))))))

(defn metric-of [run nm] (some #(when (= nm (name (:metric %))) %) (:metrics run)))

(defn case-metric
  [c nm]
  (some (fn [[k v]] (when (= nm (name k)) v)) (:metrics c)))

(defn iteration-output
  [c i]
  (let [t (get (:trace c) i)]
    (if (contains? t :actual) (:actual t) (:actual c))))

(defn iteration-verdict
  [c i]
  (let [verdicts (keep #(:pass? (nth % i nil)) (vals (:metrics c)))]
    (when (seq verdicts) (every? true? verdicts))))

(defn toggle-in [s x] (if (contains? s x) (disj s x) (conj s x)))

(defn case-by-id [cases id] (some #(when (= id (:case-id %)) %) cases))
