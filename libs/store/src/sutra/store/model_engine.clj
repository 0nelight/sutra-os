(ns sutra.store.model-engine
  (:require [sutra.store.api :as api]
            [sutra.store.model :as model]
            [sutra.store.spi :as spi])
  (:import [java.util.concurrent Semaphore TimeUnit]))

(set! *warn-on-reflection* true)

(defrecord ModelEngine [state ^Semaphore permit revs colls autos]
  spi/Engine
  (-profile [_]
    {:driver :model :history? true :consistency :transactional :reads #{:self}
     :semantics-overrides {}})
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
  (-kv-read [_ h ks] (model/kv-read @(:st h) ks))
  (-kv-write! [_ h writes] (swap! (:st h) model/kv-write writes) nil)
  (-kv-scan [_ h space part opts] (model/kv-scan @(:st h) space part opts))
  (-kv-parts [_ h space] (model/kv-parts @(:st h) space))
  (-ns-exists? [_ h ns] (contains? (:ns @(:st h)) ns))
  (-ns-fork! [_ h from to _cx] (swap! (:st h) model/ns-fork from to) nil)
  (-ns-drop! [_ h ns] (swap! (:st h) model/ns-drop ns) nil)
  (-layout! [_ h cx change] (swap! (:st h) model/layout cx change) nil)
  (-inspect [_ h cx] (model/inspect @(:st h) cx))
  (-rows [_ h cx T ids] (model/rows @(:st h) cx T ids))
  (-links [_ h cx L where] (model/links @(:st h) cx L where))
  (-write-rows! [_ h cx T xs] (swap! (:st h) model/write :objects cx T xs) nil)
  (-delete-rows! [_ h cx T ids] (swap! (:st h) model/erase :objects cx T ids) nil)
  (-write-links! [_ h cx L xs] (swap! (:st h) model/write :links cx L xs) nil)
  (-delete-links! [_ h cx L ids] (swap! (:st h) model/erase :links cx L ids) nil)

  spi/Records
  (-rec-colls [_] colls)
  (-rec-get [_ h cx coll k] (model/rec-get @(:st h) cx coll k))
  (-rec-put! [_ h cx coll kvs]
    (let [next-key #(get (swap! autos update % (fnil inc 0)) %)
          [st ks] (model/rec-put @(:st h) cx coll kvs next-key)]
      (reset! (:st h) st)
      ks))
  (-rec-scan [_ h cx coll] (model/rec-scan @(:st h) cx coll))
  (-rec-copy! [_ h coll from to] (swap! (:st h) model/rec-copy coll from to) nil))

(defn engine
  ([] (engine #{}))
  ([colls]
   (->ModelEngine (atom (model/empty-state)) (Semaphore. 1) (atom 0) (set colls) (atom {}))))
