(ns sutra.actions.ports
  (:require [sutra.foundation.ops :as op]
            [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn action-verbs [] (sys/table (sys/process) :action/verbs))

(defn llm-complete [store] (sys/required (sys/of store) :llm/complete))

(defn run-performers [store] (sys/table (sys/of store) :run/performers))

(defn schedule-start [store] (sys/required (sys/of store) :schedule/start))

(defn required [store k] (sys/required (sys/of store) k))

(defn builds-landing [store k] (get (sys/required (sys/of store) :builds/landing) k))

(defn scenario-state [store branch]
  (op/query {:store store} :scenarios/state {:branch (str branch)}))

(defn scenario-append-fence [store branch]
  (op/query {:store store} :scenarios/append-fence {:branch (str branch)}))

(defn announce-action! [branch-store branch schema result]
  ((builds-landing branch-store :announce-action!) branch-store branch schema result))

(defn locked-projects
  [store branch schema]
  (op/query {:store store} :authoring/locked-projects {:branch (str branch) :schema schema}))
