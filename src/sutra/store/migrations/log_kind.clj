(ns sutra.store.migrations.log-kind
  (:require [sutra.store.core.log-kind :as log-kind]))

(set! *warn-on-reflection* true)

(defn index!
  [store]
  (log-kind/index! (:db store)))
