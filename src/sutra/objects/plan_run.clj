(ns sutra.objects.plan-run
  (:require [sutra.objects.planner :as planner]
            [sutra.store.access.substrate :as sub]))

(set! *warn-on-reflection* true)

(defn execute
  [reads {:keys [ir]}]
  (sub/select reads ir))

(defn explain
  [store {:keys [ir] :as plan}]
  (assoc (planner/describe plan) :store (sub/explain store (sub/query-request nil ir))))
