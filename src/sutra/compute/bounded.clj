(ns sutra.compute.bounded
  (:require [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(def max-abandoned 4)

(defonce ^:private abandoned (atom #{}))

(defn abandoned-count
  []
  (count (swap! abandoned (fn [ts] (into #{} (filter #(.isAlive ^Thread %)) ts)))))

(defn start
  [label f]
  (when (>= (abandoned-count) max-abandoned)
    (refusal/refuse! :refusal.compute/sandbox-saturated {:reason :sandbox-saturated}
                     {:n max-abandoned}))
  (let [p (promise)
        g (bound-fn* f)
        t (Thread. ^Runnable (fn [] (deliver p (try [:ok (g)] (catch Throwable e [:err e]))))
                   (str "sandbox-" label))]
    (.setDaemon t true)
    (.start t)
    {:thread t :result p}))

(defn abandon!
  [{:keys [^Thread thread result]}]
  (when-not (realized? result)
    (.interrupt thread)
    (.setPriority thread Thread/MIN_PRIORITY)
    (swap! abandoned conj thread))
  nil)

(defn outcome
  [[k v]]
  (if (= :ok k) v (throw v)))

(defn call
  [label ms f]
  (let [{:keys [result] :as run} (start label f)
        r (deref result ms ::timeout)]
    (if (= ::timeout r)
      (do (abandon! run) ::timeout)
      (outcome r))))
