(ns sutra.store.kv
  (:require [sutra.store.api :as api]
            [sutra.store.model :as model]
            [sutra.store.spi :as spi])
  (:import [java.util.concurrent Semaphore TimeUnit]))

(set! *warn-on-reflection* true)

(defprotocol Kv
  (-profile [s])
  (-begin [s mode lane])
  (-lock! [s h ks])
  (-next-rev [s h])
  (-commit! [s h])
  (-rollback! [s h])
  (-read [s h ks])
  (-write! [s h writes])
  (-scan [s h space part opts])
  (-parts [s h space]))

(defrecord MemKv [state ^Semaphore permit revs]
  Kv
  (-profile [_] {:consistency :transactional})
  (-begin [_ mode _lane]
    (when (= :write mode)
      (when-not (.tryAcquire permit 10 TimeUnit/SECONDS)
        (api/refuse! :unavailable "another writer holds the store")))
    {:mode mode :st (atom @state)})
  (-lock! [_ _ _] nil)
  (-next-rev [_ _] (swap! revs inc))
  (-commit! [_ {:keys [mode st]}]
    (when (= :write mode) (reset! state @st) (.release permit)))
  (-rollback! [_ {:keys [mode]}]
    (when (= :write mode) (.release permit)))
  (-read [_ h ks] (model/kv-read @(:st h) ks))
  (-write! [_ h writes] (swap! (:st h) model/kv-write writes) nil)
  (-scan [_ h space part opts] (model/kv-scan @(:st h) space part opts))
  (-parts [_ h space] (model/kv-parts @(:st h) space)))

(defn mem []
  (->MemKv (atom {:kv {}}) (Semaphore. 1) (atom 0)))

(defn of-engine
  [e]
  (reify Kv
    (-profile [_] (select-keys (spi/-profile e) [:consistency]))
    (-begin [_ mode lane] (spi/-begin e mode lane))
    (-lock! [_ h ks] (spi/-lock! e h ks))
    (-next-rev [_ h] (spi/-next-rev e h))
    (-commit! [_ h] (spi/-commit! e h))
    (-rollback! [_ h] (spi/-rollback! e h))
    (-read [_ h ks] (spi/-kv-read e h ks))
    (-write! [_ h writes] (spi/-kv-write! e h writes))
    (-scan [_ h space part opts] (spi/-kv-scan e h space part opts))
    (-parts [_ h space] (spi/-kv-parts e h space))))
