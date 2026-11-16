(ns sutra.compute.cores
  (:require [clojure.string :as str]
            [sutra.foundation.env :as env])
  (:import (java.util.concurrent Semaphore)))

(set! *warn-on-reflection* true)

(def default-cores
  2)

(defn parse-cpu-list
  [s]
  (->> (str/split (str/trim (str s)) #",")
       (remove str/blank?)
       (mapcat (fn [part]
                 (let [[a b] (map #(Long/parseLong (str/trim %)) (str/split part #"-" 2))]
                   (range a (inc (or b a))))))
       distinct
       sort
       vec))

(defn format-cpu-list [ids] (str/join "," ids))

(defn- own-cpus
  []
  (or (some-> (env/getenv "SUTRA_TRANSFORM_CPUS") not-empty parse-cpu-list not-empty)
      (vec (range (.availableProcessors (Runtime/getRuntime))))))

(defn queue
  [cpus]
  {:cpus (vec cpus)
   :permits (Semaphore. (count cpus) true)
   :free (atom (into (sorted-set) cpus))})

(defonce ^:private worker-queue (delay (queue (own-cpus))))

(defn worker
  []
  @worker-queue)

(defn grant
  [q n]
  (-> (long (or n default-cores)) (max 1) (min (count (:cpus q)))))

(defn- take-cores! [q k]
  (let [taken (volatile! nil)]
    (swap! (:free q) (fn [free]
                       (let [ids (vec (take k free))]
                         (vreset! taken ids)
                         (apply disj free ids))))
    @taken))

(defn with-cores*
  [q n f]
  (let [k (grant q n)
        ^Semaphore s (:permits q)]
    (.acquire s (int k))
    (let [ids (take-cores! q k)]
      (try (f ids)
           (finally
             (swap! (:free q) into ids)
             (.release s (int k)))))))

(defmacro with-cores
  [[ids n] & body]
  `(with-cores* (worker) ~n (fn [~ids] ~@body)))

(defn status
  []
  (let [q (worker)]
    {:cpus (count (:cpus q))
     :free (count @(:free q))
     :waiting (.getQueueLength ^Semaphore (:permits q))}))
