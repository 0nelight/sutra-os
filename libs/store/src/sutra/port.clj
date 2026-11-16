(ns sutra.port
  (:require [sutra.kit.anomaly :as anomaly]
            [malli.core :as m]))

(set! *warn-on-reflection* true)

(def ^:private validator (memoize m/validator))

(defn asked!
  [port schema x what]
  (when-let [e (and (not ((validator schema) x)) (m/explain schema x))]
    (anomaly/raise! :incorrect
                    (str (name port) ": malformed " (name what))
                    {:explain e what x}))
  x)

(defn answered!
  ([port schema x what] (answered! port schema x what (constantly true)))
  ([port schema x what ok?]
   (when-not (and ((validator schema) x) (ok? x))
     (anomaly/raise! :unavailable
                     (str (name port) " driver answered a malformed " (name what))
                     {:store/refusal :unavailable :port/driver-contract what :port port
                      :explain (m/explain schema x)}))
   x))
