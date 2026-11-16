(ns sutra.schedules.poller
  (:require [sutra.store.access.scope :as ctx]
            [sutra.builds.api :as builds]
            [sutra.foundation.periodic :as periodic]
            [sutra.store.seat :as seat]))

(set! *warn-on-reflection* true)

(defn stop!
  [slot]
  (periodic/stop! slot 30000))

(defn first-tick-ms
  [interval-ms]
  (min (long interval-ms) 60000))

(defn start!
  ([slot tag interval-ms tick] (start! slot tag interval-ms tick (first-tick-ms interval-ms)))
  ([slot tag interval-ms tick initial-delay-ms]
   (stop! slot)
   (periodic/start! slot tag initial-delay-ms interval-ms
                    (let [f tick]
                      (fn [] (builds/as-leader #(f (builds/fenced (ctx/system-store) %))))))
   (println (str "[" tag "] armed: poll every " interval-ms "ms"
                 " (runs only on the singleton-lease holder)"))))

(defn poke!
  [slot]
  (periodic/poke! slot))

(defonce ^:private job-slots (atom {}))

(defn- job-slot [k]
  (or (get @job-slots k)
      (get (swap! job-slots update k #(or % (periodic/slot))) k)))

(defn- start-tick!
  [{k :name tick :tick first-tick :first-tick} ms]
  (start! (job-slot k) (name k) ms tick ((or first-tick first-tick-ms) ms)))

(defn- stop-tick! [{k :name}] (stop! (job-slot k)))

(defn poke-job! [k] (poke! (job-slot k)))

(defn quiesce!
  [ms]
  (periodic/drain! (vals @job-slots) ms))

(defn each-due
  [tag list-due fire label]
  (seat/on-branch tag
             (fn [store branch]
               (doseq [item (list-due store branch)]
                 (try (fire store branch item)
                      (catch InterruptedException e (throw e))
                      (catch Exception t
                        (println (str "[" tag "] '" (label item) "' failed: "
                                      (.getMessage t)))))))))

(defn each-due-across
  [tag list-due fire label]
  (fn [store]
    (try
      (doseq [[branch items] (group-by :branch (list-due store))
              item items]
        (try (fire (ctx/at store branch) branch item)
             (catch InterruptedException e (throw e))
             (catch Exception t
               (println (str "[" tag "] '" (label item) "' on " branch " failed: "
                             (.getMessage t))))))
      (catch InterruptedException e (throw e))
      (catch Exception t
        (println (str "[" tag "] tick error: " (.getMessage t)))))))

(def module
  {:name     :poller
   :provides {:jobs/tick {:start start-tick! :stop stop-tick!}}})
