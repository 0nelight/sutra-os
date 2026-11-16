(ns sutra.foundation.periodic
  (:require [sutra.kit.noted :as noted])
  (:import (java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit)
           (java.util.concurrent.locks LockSupport)))

(set! *warn-on-reflection* true)

(defn slot [] (atom nil))

(defn- daemon-executor ^ScheduledExecutorService [thread-name]
  (Executors/newSingleThreadScheduledExecutor
   (reify ThreadFactory
     (newThread [_ r] (doto (Thread. ^Runnable r ^String thread-name) (.setDaemon true))))))

(defn start!
  [slot thread-name initial-ms period-ms f]
  (locking slot
    (when-not (:ex @slot)
      (let [ex (daemon-executor thread-name)]
        (.scheduleWithFixedDelay ex ^Runnable f (long initial-ms) (long period-ms)
                                 TimeUnit/MILLISECONDS)
        (reset! slot {:ex ex :f f})))))

(defn stop!
  ([slot] (stop! slot 0))
  ([slot await-ms]
   (locking slot
     (when-let [^ScheduledExecutorService ex (:ex @slot)]
       (.shutdownNow ex)
       (when (pos? (long await-ms))
         (.awaitTermination ex (long await-ms) TimeUnit/MILLISECONDS))
       (reset! slot nil)))))

(defn poke!
  [slot]
  (let [{:keys [^ScheduledExecutorService ex f]} @slot]
    (when (and ex (not (.isShutdown ex)))
      (.execute ex ^Runnable f)
      true)))

(defn drain!
  [slots ms]
  (let [exs      (keep #(:ex @%) slots)
        deadline (+ (System/currentTimeMillis) (long ms))]
    (doseq [^ScheduledExecutorService ex exs] (.shutdown ex))
    (every? (fn [^ScheduledExecutorService ex]
              (.awaitTermination ex (max 0 (- deadline (System/currentTimeMillis)))
                                 TimeUnit/MILLISECONDS))
            (doall exs))))

(defn beating
  [thread-name period-ms renew!]
  (let [on? (atom true)
        t   (doto (Thread.
                   ^Runnable
                   (fn []
                     (loop []
                       (LockSupport/parkNanos (* 1000000 (long period-ms)))
                       (when @on?
                         (when (noted/or-else (str thread-name " renewal") true (some? (renew!)))
                           (recur)))))
                   ^String thread-name)
              (.setDaemon true)
              (.start))]
    (fn stop! [] (reset! on? false) (.interrupt t))))

(defn renewing
  [thread-name period-ms renew! f]
  (let [stop (beating thread-name period-ms renew!)]
    (try (f) (finally (stop)))))
