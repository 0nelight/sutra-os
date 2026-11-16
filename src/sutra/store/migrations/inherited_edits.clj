(ns sutra.store.migrations.inherited-edits
  (:require [sutra.store.core.inherited-edits :as inherited]))

(set! *warn-on-reflection* true)

(defn compact!
  [store]
  (inherited/compact! (:db store)))
