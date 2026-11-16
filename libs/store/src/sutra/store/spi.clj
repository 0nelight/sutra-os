(ns sutra.store.spi
  (:require [sutra.store.api :as api]
            [sutra.store.shapes :as shapes]))

(set! *warn-on-reflection* true)

(defprotocol Engine
  (-profile [e])

  (-begin [e mode lane])
  (-lock! [e h ks])
  (-next-rev [e h])
  (-commit! [e h])
  (-rollback! [e h])

  (-kv-read [e h ks])
  (-kv-write! [e h writes])
  (-kv-scan [e h space part opts])
  (-kv-parts [e h space])

  (-ns-exists? [e h ns])
  (-ns-fork! [e h from to cx])
  (-ns-drop! [e h ns])

  (-layout! [e h cx change])
  (-inspect [e h cx])

  (-rows [e h cx T ids])
  (-links [e h cx L where])

  (-write-rows! [e h cx T rows])
  (-delete-rows! [e h cx T ids])
  (-write-links! [e h cx L links])
  (-delete-links! [e h cx L ids]))

(defprotocol Pushdown
  (-scan [e h cx scan])
  (-execute [e h cx fragment])
  (-explain [e h cx fragment]))

(defprotocol Snapshots
  (-storage-snapshot [e h basis]))

(defprotocol Records
  (-rec-colls [e])
  (-rec-get [e h cx coll k])
  (-rec-put! [e h cx coll kvs])
  (-rec-scan [e h cx coll])
  (-rec-copy! [e h coll from to]))

(defprotocol RecordsClock
  (-rec-clock [e h coll]))

(def decline ::decline)

(def reserved-keys api/reserved-keys)

(def profile-schema (shapes/schema :driver/profile))

(defn capabilities
  [{:keys [history? scan plan explain?]}]
  (cond-> #{}
    history?                 (conj :objects/history)
    (seq (:sources scan))    (conj :scan)
    (seq (:ops plan))        (conj :select)
    explain?                 (conj :select/explain)))

(def layout-schema (shapes/schema :driver/layout))

(def plan
  ::plan)

(def signature-schema
  (shapes/schema :driver/signature))

(def row-schema (shapes/schema :driver/row))

(def link-schema (shapes/schema :driver/link))
