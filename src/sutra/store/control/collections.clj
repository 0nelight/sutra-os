(ns sutra.store.control.collections
  (:require [sutra.store.control.tables :as cc]))

(set! *warn-on-reflection* true)

(def consumer-marks
  {:coll  :consumer/mark
   :owner 'sutra.builds.ledger.marks
   :scope :global
   :key   [:tuple :string :string]
   :value :string})

(def outbox-cursors
  {:coll  :outbox/cursor
   :owner 'sutra.store.access.run-log
   :scope :global
   :key   [:tuple :string :string]
   :value [:tuple :int :int]})

(def event-cursors
  {:coll  :ops/cursor
   :owner 'sutra.store.access.events
   :scope :global
   :key   [:tuple :string :string]
   :value [:tuple :int :int]})

(def parked-events
  {:coll  :ops/parked
   :owner 'sutra.store.access.events
   :scope :global
   :key   [:tuple :string :string :int :int]
   :value :map})

(def stream-index-marks
  {:coll  :stream/index-mark
   :owner 'sutra.builds.run.stream-index
   :scope :global
   :key   [:tuple :string :string :string]
   :value :map})

(def all (-> [consumer-marks outbox-cursors event-cursors parked-events stream-index-marks]
             (into cc/all) (into cc/ledger)))
