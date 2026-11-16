(ns sutra.foundation.scan-progress
  (:require [sutra.foundation.live-trace :as live]))

(set! *warn-on-reflection* true)

(def ^:private trace-every-ms 5000)

(defonce ^:private scans (atom {}))

(defn- trace! [m final?]
  (live/emit! (cond-> (assoc (select-keys m [:source :table :folders :files :mb :denied :current])
                             :kind :build/scan)
                final? (assoc :final? true))))

(defn- due? [{:keys [traced-at]} now]
  (or (nil? traced-at) (>= (- now traced-at) trace-every-ms)))

(defn note! [source m]
  (let [now  (System/currentTimeMillis)
        was  (get @scans source)
        show (due? was now)
        m    (assoc m :source source :at (str (java.time.Instant/now))
                    :traced-at (if show now (:traced-at was)))]
    (swap! scans assoc source m)
    (when show (trace! m false))))

(defn end! [source]
  (when-let [m (get @scans source)]
    (trace! m true))
  (swap! scans dissoc source))

(defn running [] (mapv #(dissoc % :traced-at) (vals @scans)))
