(ns sutra.foundation.armed
  (:import (java.util.concurrent Executors ScheduledExecutorService ThreadFactory
                                 TimeUnit Future)))

(set! *warn-on-reflection* true)

(defonce ^:private timer
  (delay (Executors/newSingleThreadScheduledExecutor
          (reify ThreadFactory
            (newThread [_ r] (doto (Thread. r "sutra-deadman") (.setDaemon true)))))))

(def ^:const default-ttl-seconds 1800)

(defn switch [{:keys [label on-arm on-disarm ttl-seconds]}]
  {:label     (str label)
   :on-arm    (or on-arm (constantly nil))
   :on-disarm (or on-disarm (constantly nil))
   :ttl       (long (or ttl-seconds default-ttl-seconds))
   :lock      (Object.)
   :state     (atom {:armed? false :generation 0 :deadline nil :expiry nil :ttl nil})})

(defn armed? [{:keys [state]}]
  (:armed? @state))

(defn expires-in-ms [{:keys [state]}]
  (when-let [deadline (:deadline @state)]
    (max 0 (- (long deadline) (System/currentTimeMillis)))))

(defn- cancel-deadman! [state]
  (some-> ^Future (:expiry @state) (.cancel false))
  (swap! state assoc :expiry nil :deadline nil :ttl nil))

(defn- disarm* [{:keys [state on-disarm]}]
  (let [was (:armed? @state)]
    (cancel-deadman! state)
    (swap! state assoc :armed? false)
    (merge {:armed false :was-armed was} (on-disarm))))

(defn disarm! [{:keys [lock] :as sw}]
  (locking lock (disarm* sw)))

(defn arm!
  ([sw] (arm! sw nil))
  ([{:keys [lock state on-arm ttl] :as sw} {:keys [ttl-seconds] :as opts}]
   (locking lock
     (disarm* sw)
     (let [ttl (long (or ttl-seconds ttl))
           g   (:generation (swap! state update :generation inc))
           _   (swap! state assoc :armed? true :ttl ttl
                      :deadline (+ (System/currentTimeMillis) (* 1000 ttl)))
           res (try (on-arm opts)
                    (catch Throwable t (disarm* sw) (throw t)))]
       (swap! state assoc :expiry
              (.schedule ^ScheduledExecutorService @timer
                         ^Runnable (fn [] (locking lock
                                            (when (= g (:generation @state)) (disarm* sw))))
                         ttl TimeUnit/SECONDS))
       (merge {:armed true :ttl-seconds ttl} res)))))

(defn status [{:keys [state] :as sw}]
  (let [{:keys [armed? ttl]} @state]
    (cond-> {:armed (boolean armed?)}
      armed? (assoc :ttl-seconds ttl :expires-in-ms (expires-in-ms sw)))))

(defn off! [{:keys [lock state]}]
  (locking lock
    (cancel-deadman! state)
    (swap! state assoc :armed? false)
    nil))
