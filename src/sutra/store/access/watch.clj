(ns sutra.store.access.watch
  (:refer-clojure :exclude [name])
  (:require [sutra.kit.pubsub :as pubsub]
            [sutra.kit.system :as sys]
            [sutra.kit.noted :as noted]
            [clojure.core :as core]))

(set! *warn-on-reflection* true)

(defonce ^:private subscribers
  (pubsub/registry))

(defonce ^:private changed
  (atom {}))

(defn- topic
  [branch object-type]
  [(str branch) (core/name object-type)])

(defn subscribe!
  [branch object-type sink]
  (pubsub/subscribe! subscribers (topic branch object-type) sink))

(defn subscriber-count
  [branch object-type]
  (pubsub/count subscribers (topic branch object-type)))

(defn drop-subscribers!
  []
  (pubsub/clear! subscribers))

(defn forget-changes!
  []
  (reset! changed {})
  nil)

(defn changed-at
  [branch object-type]
  (get @changed (topic branch object-type)))

(defn- notify-here!
  [branch object-type payload]
  (let [k (topic branch object-type)]
    (swap! changed assoc k (System/currentTimeMillis))
    (pubsub/publish! subscribers k (assoc payload :object-type (core/name object-type)))
    (doseq [[_ hook] (sys/table (sys/process) :watch/hooks)]
      (noted/or-else (str "watch hook on " branch "/" (core/name object-type)) nil
        (hook branch object-type)))))

(defn- wake-here!
  []
  (when-let [f (sys/provided (sys/process) :watch/wake)]
    (noted/or-else "waking the scheduler" nil (f)))
  nil)

(defn- tell-others!
  [topic message]
  (when-let [broadcast (sys/provided (sys/process) :cluster/broadcast)]
    (noted/or-else (str "broadcasting " topic) nil
      (or (broadcast topic message)
          (broadcast topic (update message :payload dissoc :ids))))))

(defn notify!
  [branch object-type payload]
  (notify-here! branch object-type payload)
  (tell-others! :watch/notify
                {:branch (str branch) :object-type (core/name object-type) :payload payload}))

(defn wake!
  []
  (wake-here!)
  (tell-others! :watch/wake {})
  nil)

(def module
  {:name        :watch
   :contributes {:cluster/listeners
                 {:watch/notify (fn [{:keys [branch object-type payload]}]
                                  (notify-here! branch object-type payload))
                  :watch/wake   (fn [_] (wake-here!))}}})
