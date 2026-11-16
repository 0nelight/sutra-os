(ns support.check
  (:require [sutra.foundation.contracts]
            [sutra.store.core]
            [sutra.store.core.engine]
            [sutra.store.guard]))

(defn enable!
  [config]
  (alter-var-root #'sutra.foundation.contracts/*check* (constantly true))
  (alter-var-root #'sutra.store.core.engine/*verify* (constantly true))
  (alter-var-root #'sutra.store.core.engine/*one-handle-per-lane* (constantly true))
  (alter-var-root #'sutra.store.guard/*on* (constantly true))
  (println "[support.check] seam contract guards: ON for this run")
  config)
