(ns sutra.store.guard
  (:require [sutra.store.api :as api]
            [sutra.store.judge :as judge]
            [sutra.store.spi :as spi])
  (:import [java.util.concurrent.atomic AtomicLong]))

(set! *warn-on-reflection* true)

(def duties
  #{:duty/clock :duty/kv-scan :duty/kv-parts :duty/kv-read :duty/rows :duty/links
    :duty/read-stable :duty/write-own :duty/record-auto})

(def ^:dynamic *on*
  (contains? #{"on" "true" "1"} (System/getenv "SUTRA_STORE_GUARD")))

(defn- check! [finding answer]
  (when-let [{:keys [duty msg data]} finding]
    (throw (api/refusal :unavailable (str "store engine broke " (name duty) ": " msg)
                        (assoc data :store/engine-contract duty))))
  answer)

(defn- next-rev! [^AtomicLong last rev]
  (check! (judge/clock nil rev) rev)
  (let [prior (.getAndAccumulate last rev (reify java.util.function.LongBinaryOperator
                                            (applyAsLong [_ a b] (max a b))))]
    (check! (judge/clock prior rev) rev)))

(defn- stable! [h q answer]
  (when-let [seen (::seen h)]
    (let [d (hash answer)]
      (check! (judge/stable q (get (swap! seen update q #(or % d)) q) d) answer)))
  answer)

(defn- own-read! [e h cx T xs deleted?]
  (when (= :write (::mode h))
    (let [back (spi/-rows e h (select-keys cx [:ns :sig]) T (if deleted? xs (map :id xs)))]
      (check! (if deleted? (judge/own-delete T xs back) (judge/own-write T xs back)) nil))))

(defn- rec-keys! [^java.util.Map autos coll kvs ks]
  (doseq [[[k _] assigned] (map vector kvs ks) :when (= api/auto k)]
    (check! (judge/record-key coll (.get autos coll) assigned) nil)
    (.put autos coll assigned))
  ks)

(defrecord Guard [e ^AtomicLong clock ^java.util.Map autos]
  spi/Engine
  (-profile [_] (spi/-profile e))
  (-begin [_ mode lane]
    (cond-> (assoc (spi/-begin e mode lane) ::mode mode)
      (= :read mode) (assoc ::seen (atom {}))))
  (-lock! [_ h ks] (spi/-lock! e h ks))
  (-next-rev [_ h] (next-rev! clock (spi/-next-rev e h)))
  (-commit! [_ h] (spi/-commit! e h))
  (-rollback! [_ h] (spi/-rollback! e h))
  (-kv-read [_ h ks]
    (let [a (spi/-kv-read e h ks)] (stable! h [:kv-read ks] (check! (judge/kv-read ks a) a))))
  (-kv-write! [_ h writes] (spi/-kv-write! e h writes))
  (-kv-scan [_ h space part opts]
    (let [a (spi/-kv-scan e h space part opts)]
      (stable! h [:kv-scan space part opts] (check! (judge/kv-scan opts a) a))))
  (-kv-parts [_ h space] (let [a (spi/-kv-parts e h space)] (check! (judge/kv-parts a) a)))
  (-ns-exists? [_ h ns] (spi/-ns-exists? e h ns))
  (-ns-fork! [_ h from to cx] (spi/-ns-fork! e h from to cx))
  (-ns-drop! [_ h ns] (spi/-ns-drop! e h ns))
  (-layout! [_ h cx change] (spi/-layout! e h cx change))
  (-inspect [_ h cx] (spi/-inspect e h cx))
  (-rows [_ h cx T ids]
    (let [a (spi/-rows e h cx T ids)]
      (stable! h [:rows (:ns cx) (:as-of cx) T ids] (check! (judge/rows (:sig cx) T ids a) a))))
  (-links [_ h cx L where]
    (let [a (spi/-links e h cx L where)]
      (stable! h [:links (:ns cx) (:as-of cx) L where] (check! (judge/links L where a) a))))
  (-write-rows! [_ h cx T rows]
    (let [r (spi/-write-rows! e h cx T rows)]
      (own-read! e h cx T rows false)
      r))
  (-delete-rows! [_ h cx T ids]
    (let [r (spi/-delete-rows! e h cx T ids)]
      (own-read! e h cx T ids true)
      r))
  (-write-links! [_ h cx L links] (spi/-write-links! e h cx L links))
  (-delete-links! [_ h cx L ids] (spi/-delete-links! e h cx L ids))

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
  (-rec-put! [_ h cx coll kvs]
    (rec-keys! autos coll kvs (spi/-rec-put! e h cx coll kvs)))
  (-rec-scan [_ h cx coll] (spi/-rec-scan e h cx coll))
  (-rec-copy! [_ h coll from to] (spi/-rec-copy! e h coll from to))

  spi/RecordsClock
  (-rec-clock [_ h coll]
    (when (satisfies? spi/RecordsClock e) (spi/-rec-clock e h coll))))

(defn wrap [e]
  (if (instance? Guard e)
    e
    (->Guard e (AtomicLong. Long/MIN_VALUE) (java.util.concurrent.ConcurrentHashMap.))))
