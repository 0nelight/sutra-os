(ns sutra.store.migrations.schema-log
  (:require [sutra.store.core.schema-log :as schema-log]))

(set! *warn-on-reflection* true)

(defn index!
  [store]
  (schema-log/index! (:db store)))
