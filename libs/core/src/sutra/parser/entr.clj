(ns sutra.parser.entr
  (:require [sutra.kit.refusal :as refusal]
            [sutra.parser.ent-errors :refer [ent-errors-params]]
            [sutra.parser.operational :as op]
            [sutra.parser.step-verbs :as sv]
            [malli.core :as m]
            [malli.error :as me]))

(set! *warn-on-reflection* true)

(defn- with-opt [verb args] [:cat [:= verb] [:? args]])
(defn- with-req [verb args] [:cat [:= verb] args])

(def ^:private op-schemas
  {[:vector :op/automation]   [:vector op/EntAutomation]
   [:vector :op/schedule]     [:vector op/EntSchedule]
   [:vector :op/health-check] [:vector op/EntHealthCheck]})

(def Step
  (-> [:multi {:dispatch (fn [s] (when (and (sequential? s) (seq s)) (first s)))}]
      (into (map (fn [[verb arity args]]
                   [verb ((if (= :req arity) with-req with-opt) verb (op-schemas args args))]))
            sv/steps)
      (conj [::m/default [:fn {:error/message "unknown runbook step verb"} (constantly false)]])))

(def Runbook
  [:map {:closed true}
   [:title  {:optional true} :string]
   [:steps  [:vector Step]]])

(def ^:private compiled-runbook (delay (m/schema Runbook)))

(defn explain
  [data]
  (some-> (m/explain @compiled-runbook data) me/humanize))

(declare validate!)

(def ^:private step-schemas
  (memoize (fn [verb]
             (some (fn [e] (when (and (vector? e) (= verb (first e))) (m/schema (second e))))
                   (drop 2 Step)))))

(defn step-form
  [verb]
  (when-let [[_ cat] (some #(when (and (vector? %) (= verb (first %))) %) (drop 2 Step))]
    (let [tail (nth cat 2 nil)
          opt? (and (vector? tail) (= :? (first tail)))]
      [opt? (if opt? (second tail) tail)])))

(defn validate-step!
  [verb args source]
  (if-let [sch (step-schemas verb)]
    (let [step [verb args]]
      (when-let [errs (some-> (m/explain sch step) me/humanize)]
        (refusal/refuse! :refusal.parser/invalid-text {:source source :errors {:steps [errs]}}
                         (ent-errors-params "Invalid .entr" {:steps [errs]})))
      [[verb args]])
    (validate! {:steps [[verb args]]} source)))

(defn validate!
  [data source]
  (when-not (and (map? data) (contains? data :steps))
    (refusal/refuse! :refusal.parser/runbook-shape {:source source}))
  (if-let [errs (explain data)]
    (refusal/refuse! :refusal.parser/invalid-text {:source source :errors errs}
                     (ent-errors-params "Invalid .entr" errs))
    data))
