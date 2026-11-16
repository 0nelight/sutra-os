(ns sutra.store.shadow
  (:require [sutra.store.api :as api]
            [sutra.store.judge :as judge]
            [sutra.store.kv :as kv]
            [sutra.store.kv-engine :as kv-engine]
            [sutra.store.spi :as spi]))

(set! *warn-on-reflection* true)

(def ^:dynamic *on*
  (contains? #{"on" "true" "1"} (System/getenv "SUTRA_STORE_SHADOW")))

(defn- differ! [what {:keys [msg data]}]
  (throw (api/refusal :unavailable (str "store engine answered " what ": " msg)
                      (assoc data :store/engine-contract :duty/shadow))))

(defn- same-rows! [what T a b]
  (some->> (judge/same-rows T a b) (differ! what))
  a)

(defrecord Shadow [e r]
  spi/Engine
  (-profile [_] (spi/-profile e))
  (-begin [_ mode lane] (spi/-begin e mode lane))
  (-lock! [_ h ks] (spi/-lock! e h ks))
  (-next-rev [_ h] (spi/-next-rev e h))
  (-commit! [_ h] (spi/-commit! e h))
  (-rollback! [_ h] (spi/-rollback! e h))
  (-kv-read [_ h ks] (spi/-kv-read e h ks))
  (-kv-write! [_ h writes] (spi/-kv-write! e h writes))
  (-kv-scan [_ h space part opts] (spi/-kv-scan e h space part opts))
  (-kv-parts [_ h space] (spi/-kv-parts e h space))
  (-ns-exists? [_ h ns]
    (let [a (spi/-ns-exists? e h ns) b (spi/-ns-exists? r h ns)]
      (when (not= (boolean a) (boolean b))
        (differ! "-ns-exists?" {:msg "otherwise than the reference" :data {:ns ns :engine a}}))
      a))
  (-ns-fork! [_ h from to cx] (spi/-ns-fork! e h from to cx) (spi/-ns-fork! r h from to cx))
  (-ns-drop! [_ h ns] (spi/-ns-drop! e h ns) (spi/-ns-drop! r h ns))
  (-layout! [_ h cx change] (spi/-layout! e h cx change) (spi/-layout! r h cx change))
  (-inspect [_ h cx] (spi/-inspect e h cx))
  (-rows [_ h cx T ids]
    (same-rows! "-rows" T (spi/-rows e h cx T ids) (spi/-rows r h cx T ids)))
  (-links [_ h cx L where]
    (same-rows! "-links" L (spi/-links e h cx L where) (spi/-links r h cx L where)))
  (-write-rows! [_ h cx T rows] (spi/-write-rows! e h cx T rows) (spi/-write-rows! r h cx T rows))
  (-delete-rows! [_ h cx T ids] (spi/-delete-rows! e h cx T ids) (spi/-delete-rows! r h cx T ids))
  (-write-links! [_ h cx L ls] (spi/-write-links! e h cx L ls) (spi/-write-links! r h cx L ls))
  (-delete-links! [_ h cx L ids]
    (spi/-delete-links! e h cx L ids)
    (spi/-delete-links! r h cx L ids))

  spi/Pushdown
  (-scan [_ h cx s] (spi/-scan e h cx s))
  (-execute [_ h cx fragment] (spi/-execute e h cx fragment))
  (-explain [_ h cx fragment] (spi/-explain e h cx fragment))

  spi/Snapshots
  (-storage-snapshot [_ h basis]
    (when (satisfies? spi/Snapshots e) (spi/-storage-snapshot e h basis)))

  spi/Records
  (-rec-colls [_] (if (satisfies? spi/Records e) (spi/-rec-colls e) #{}))
  (-rec-get [_ h cx coll k] (spi/-rec-get e h cx coll k))
  (-rec-put! [_ h cx coll kvs] (spi/-rec-put! e h cx coll kvs))
  (-rec-scan [_ h cx coll] (spi/-rec-scan e h cx coll))
  (-rec-copy! [_ h coll from to] (spi/-rec-copy! e h coll from to))

  spi/RecordsClock
  (-rec-clock [_ h coll]
    (when (satisfies? spi/RecordsClock e) (spi/-rec-clock e h coll))))

(defn wrap [e]
  (if (instance? Shadow e) e (->Shadow e (kv-engine/engine (kv/of-engine e) "sutra.shadow"))))
