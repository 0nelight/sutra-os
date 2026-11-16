(ns sutra.store.storage.command
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.store.records :as r]))

(set! *warn-on-reflection* true)

(defmulti plan
  (fn [_st fx _at] (:op fx)))

(defmethod plan :default
  [_ {:keys [op] :as fx} _]
  (anomaly/raise! :fault (str "no planner for effect " (pr-str op)) {:effect fx}))

(defn execute!
  [store effects]
  (r/plan! store (mapv (fn [fx] #(plan %1 fx %2)) (remove nil? effects))))

(defn run-command!
  [store read decide]
  (execute! store (decide (read store))))
