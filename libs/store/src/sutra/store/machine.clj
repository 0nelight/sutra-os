(ns sutra.store.machine
  (:require [sutra.store.api :as api]))

(set! *warn-on-reflection* true)

(def retried #{:conflict :stale-object :snapshot-expired :prepare-expired})

(defn- attempt
  [db branch lane fences decide]
  (let [[{:keys [tx] :as d} basis]
        (with-open [s ^java.io.Closeable (api/snapshot
                                          db (cond-> {:branch branch} lane (assoc :lane lane)))]
          [(decide s) (api/basis s)])]
    (if tx
      (assoc d :committed (api/transact! db (cond-> (merge {:basis basis} tx)
                                              (seq fences) (update :fences (fnil into []) fences)
                                              lane         (assoc :lane lane))))
      d)))

(defn retrying
  [{:keys [retries retry? backoff?]} f]
  (loop [n 0]
    (let [r (try {:ok (f)}
                 (catch clojure.lang.ExceptionInfo e
                   (if (and (< n retries) (retry? e)) {:retry e} (throw e))))]
      (if (contains? r :ok)
        (:ok r)
        (do (when backoff? (Thread/sleep (long (rand-int (* 2 (bit-shift-left 1 (min n 6)))))))
            (recur (inc n)))))))

(defn command!
  [db {:keys [branch lane retries fences] :or {branch "main" retries 8}} decide]
  (retrying {:retries retries :backoff? true :retry? #(retried (:store/refusal (ex-data %)))}
            #(attempt db branch lane fences decide)))
