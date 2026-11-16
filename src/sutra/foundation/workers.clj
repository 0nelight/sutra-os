(ns sutra.foundation.workers
  (:import (java.util.concurrent LinkedBlockingQueue ThreadFactory ThreadPoolExecutor
                                 TimeUnit)))

(set! *warn-on-reflection* true)

(defn pool
  ^ThreadPoolExecutor [prefix n]
  (let [n       (int (max 1 n))
        counter (atom 0)]
    (doto (ThreadPoolExecutor.
           n n 60 TimeUnit/SECONDS (LinkedBlockingQueue.)
           (reify ThreadFactory
             (newThread [_ r]
               (doto (Thread. ^Runnable r (str prefix (swap! counter inc)))
                 (.setDaemon true)))))
      (.allowCoreThreadTimeOut true))))

(defn sized!
  ^ThreadPoolExecutor [^ThreadPoolExecutor pool n]
  (let [n (int (max 1 n))]
    (when (not= n (.getMaximumPoolSize pool))
      (if (> n (.getMaximumPoolSize pool))
        (do (.setMaximumPoolSize pool n) (.setCorePoolSize pool n))
        (do (.setCorePoolSize pool n) (.setMaximumPoolSize pool n))))
    pool))
