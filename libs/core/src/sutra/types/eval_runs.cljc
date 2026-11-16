(ns sutra.types.eval-runs
  (:require [clojure.string :as str]))

(defn captured [run] (get-in run [:summary :captured]))

(defn captured-version
  [c]
  (some-> (:version c) str))

(defn run-model
  [run]
  (or (get-in run [:summary :model]) (:model (captured run))))

(defn run-version [run] (captured-version (captured run)))

(defn run-params [run] (get-in run [:summary :experiment :params]))

(defn param-of [run nm]
  (some (fn [[k v]] (when (= (name nm) (name k)) v)) (run-params run)))

(defn params-text
  [run]
  (str/join ", " (for [[k v] (sort-by (comp str key) (run-params run))]
                   (str (name k) "=" v))))

(defn run-experiment
  [run]
  (when-let [{nm :name :keys [params]} (get-in run [:summary :experiment])]
    (str nm (when (seq params) (str " (" (params-text run) ")")))))

(defn group-of
  [run by]
  (when by
    (let [p (some-> (param-of run by) str)]
      (case (name by)
        "model"      (or p (run-model run))
        "version"    (or p (run-version run))
        "experiment" (run-experiment run)
        "ref"        (some-> (:ref run) str)
        p))))

(defn groups
  [by runs]
  (sort-by (comp str key) (group-by #(group-of % by) runs)))

(defn mean [xs]
  (let [xs (filter number? xs)]
    (when (seq xs) (/ (reduce + xs) (double (count xs))))))
