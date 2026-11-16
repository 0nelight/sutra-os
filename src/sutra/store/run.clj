(ns sutra.store.run
  (:refer-clojure :exclude [run!])
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.store.control :as ctl]
            [sutra.store.machine :as machine]
            [sutra.store.records :as records]))

(set! *warn-on-reflection* true)

(defmulti after!
  (fn [_store job] (:kind job)))

(defmethod after! :default
  [_ job]
  (anomaly/raise! :fault (str "no interpreter for a job of kind " (pr-str (:kind job)))
                         {:job job}))

(def ^:private default-actor {:principal "sutra.store.run"})

(defn- with-actor [{:keys [tx] :as d}]
  (cond-> d tx (update :tx #(update % :actor (fnil identity default-actor)))))

(defn run!
  [store {:keys [branch fences decide]}]
  (let [d (machine/command! (:db store)
                            {:branch (str (or branch (:branch store) "main")) :fences fences}
                            (comp with-actor decide))]
    (doseq [job (:after d)] (after! store job))
    (:result d)))

(defn- commit-op
  [store {:keys [auth]} decide]
  (run! store {:branch records/trunk
               :decide (fn [snap]
                         (let [s (records/deciding store snap)
                               d (decide s)]
                           (ctl/judge! s auth (get-in d [:tx :records]))
                           d))}))

(def module
  {:name     :store-run
   :provides {:ops/commit commit-op}})
