(ns sutra.schedules.api
  (:require [sutra.schedules.facts :as facts]
            [sutra.schedules.poller :as poller]
            [sutra.schedules.schedules :as schedules]
            [sutra.schedules.triggers :as triggers]))

(set! *warn-on-reflection* true)

(defn schedule-fact [runs s] (facts/schedule-fact runs s))

(defn targets [s] (facts/targets s))

(defn trigger-word [s] (facts/trigger-word s))

(defn each-due-across [tag list-due fire label] (poller/each-due-across tag list-due fire label))

(defn poke-job! [k] (poller/poke-job! k))

(defn enriched-schedules [store schema branch] (schedules/enriched-schedules store schema branch))

(defn list-all [store branch] (schedules/list-all store branch))

(defn build-closure [schema plan s] (schedules/schedule-build-closure schema plan s))

(defn watched-parents [s] (triggers/watched-parents s))

(defn watching [store branch dataset] (schedules/watching store branch dataset))
