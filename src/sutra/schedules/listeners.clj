(ns sutra.schedules.listeners
  (:require [sutra.schedules.listener-events :as listener-events]
            [sutra.schedules.schedules :as schedules]
            [sutra.schedules.triggers :as triggers]
            [sutra.schedules.poller :as poller]
            [sutra.sources.api :as sources]
            [sutra.foundation.env :as env]
            [sutra.catalog.values :as values]
            [sutra.store.seat :as seat]))

(set! *warn-on-reflection* true)

(def ^:private window-ms
  (or (some-> (env/getenv "SUTRA_LISTENER_WINDOW_MS") parse-long) (* 9 60 1000)))

(def gap-ms 10000)

(def backoff-ms 5000)

(def backoff-max-ms (* 5 60 1000))

(defonce ^:private listening
  (atom {}))

(defn settle
  [entry outcome now]
  (let [entry (dissoc entry :inflight)]
    (if-let [err (:error outcome)]
      (let [n (inc (or (:failures entry) 0))]
        (assoc entry :failures n :error err
               :next-at (+ now (min backoff-max-ms (* backoff-ms (bit-shift-left 1 (dec n)))))))
      (-> entry
          (dissoc :error)
          (assoc :failures 0 :state (:state outcome)
                 :next-at (if (:changed outcome) (+ now gap-ms) now))))))

(defn due?
  [entry now]
  (and (not (:inflight entry)) (<= (or (:next-at entry) 0) now)))

(defn- window!
  [l spec]
  (let [outcome (try (sources/listen! spec {:folders   (:folders l)
                                       :window-ms window-ms
                                       :state     (get-in @listening [l :state])})
                     (catch InterruptedException e (throw e))
                     (catch Exception t {:error (or (ex-message t) (str (class t)))}))]
    (swap! listening update l settle outcome (System/currentTimeMillis))
    (if (:error outcome)
      (println (str "[listeners] " (name (:source l)) ": window failed — " (:error outcome)))
      (when (:changed outcome)
        (listener-events/heard! (:source l))
        (println (str "[listeners] " (name (:source l)) ": new mail — waking the scheduler"))
        (poller/poke-job! :scheduler)))))

(defn- open-due!
  [store branch]
  (let [sources (values/sources->datasources (seat/branch-schema store))
        now     (System/currentTimeMillis)]
    (doseq [l (triggers/listeners (remove :paused (schedules/list-all store branch)))
            :let [spec (or (get sources (:source l)) (get sources (name (:source l))))]
            :when (and spec (due? (get @listening l) now))]
      (swap! listening update l assoc :inflight true)
      (future (window! l spec)))))

(defn status
  []
  (into {} (map (fn [[l e]] [(str (name (:source l)) (when (seq (:folders l))
                                                       (str " " (:folders l))))
                             (dissoc e :state)]))
        @listening))

(def module
  {:name :listeners
   :jobs [{:name    :listeners :every "SUTRA_LISTENER_MS" :default 5000
           :tick    (seat/on-branch "listeners" open-due!)}]})
