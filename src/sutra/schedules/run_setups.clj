(ns sutra.schedules.run-setups
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(def max-per-schedule
  1000)

(defn- row [schedule-id {:keys [value parameters]}]
  {:schedule-id schedule-id :key-value (str value) :parameters parameters})

(defn list-of
  [store branch schedule-id]
  (ctl/list store :schedule-run-setups {:branch branch :schedule-id schedule-id}))

(defn room-problem
  [standing {:keys [value]}]
  (when (and (>= (count standing) max-per-schedule)
             (not-any? #(= (str value) (:key-value %)) standing))
    [:refusal.schedules/run-setups-full {:max max-per-schedule}]))

(defn upsert!
  [store branch schedule-id rs]
  (when-let [[code params] (room-problem (list-of store branch schedule-id) rs)]
    (refusal/refuse! code {:schedule-id schedule-id} params))
  (ctl/upsert! store :schedule-run-setups (assoc (row schedule-id rs) :branch branch)))

(defn mark-started!
  [store branch schedule-id {:keys [value]} run-id]
  (ctl/update! store :schedule-run-setups
               {:branch branch :schedule-id schedule-id :key-value (str value)}
               {:last-run-id (str run-id)}))
