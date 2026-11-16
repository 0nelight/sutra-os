(ns sutra.kit.lifecycle
  (:require [sutra.kit.refusal :as refusal]))

(defn table
  [{:keys [name states final edges] :as t}]
  (doseq [[event {:keys [from to]}] edges
          s (cond-> (vec from) (keyword? to) (conj to))]
    (when-not (contains? states s)
      (throw (ex-info (str "lifecycle " name ": " event " names unknown state " s)
                      {:lifecycle name :event event :state s})))
    (when (and (contains? final s) (contains? from s))
      (throw (ex-info (str "lifecycle " name ": " event " leaves final state " s)
                      {:lifecycle name :event event :state s}))))
  (when-not (every? states final)
    (throw (ex-info (str "lifecycle " name ": a final state is no state") {:lifecycle name})))
  t)

(defn- from-ok? [from state]
  (if (empty? from) (nil? state) (contains? from state)))

(defn refusal
  [lifecycle id event from why]
  (refusal/refusal :refusal.lifecycle/transition-refused
                   {:lifecycle lifecycle :id id :event event :from (or from :absent)
                    :reason why}
                   {:why (if why (str " — " why) "")}))

(defn- verdict
  [edges event from record input]
  (let [{:keys [guard] :as edge} (get edges event)]
    (cond
      (nil? edge)                        [false "no such transition"]
      (not (from-ok? (:from edge) from)) [false nil]
      :else                              (if-let [why (some-> guard (apply [record input]))]
                                           [false why]
                                           [true nil]))))

(defn why-not
  [{:keys [edges]} event from record input]
  (second (verdict edges event from record input)))

(defn decide
  [{:keys [name edges]} id event from record input]
  (let [[ok? why] (verdict edges event from record input)
        {:keys [to]} (get edges event)]
    (if ok?
      (if (fn? to) (to from input) to)
      (throw (refusal name id event from why)))))

(defn next-state
  [{:keys [edges]} event from record input]
  (when (first (verdict edges event from record input))
    (let [{:keys [to]} (get edges event)]
      (if (fn? to) (to from input) to))))

(defn state-names
  [{:keys [states]}]
  (into #{} (map name) states))

(defn allowed?
  [{:keys [edges]} event from]
  (boolean (some-> (get edges event) :from (from-ok? from))))
