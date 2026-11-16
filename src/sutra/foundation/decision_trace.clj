(ns sutra.foundation.decision-trace
  (:require [sutra.foundation.armed :as armed]
            [sutra.kit.decisions :as decisions]))

(set! *warn-on-reflection* true)

(declare start! stop!)

(defonce ^:private switch
  (armed/switch {:label "decision-trace" :on-arm #'start! :on-disarm #'stop!
                 :ttl-seconds 1800}))

(defn- start! [{:keys [kinds]}] (decisions/start! kinds))

(defn- stop! [] (decisions/stop!) (decisions/clear!) {})

(defn armed? [] (armed/armed? switch))

(defn arm! [opts] (armed/arm! switch opts))

(defn disarm! [] (armed/disarm! switch))

(defn node-report
  []
  (merge {:armed (armed?) :lifecycle (armed/status switch)} (decisions/status)))

(defn records-text
  []
  (let [rs (decisions/records)]
    (when (seq rs) (decisions/text rs))))
