(ns sutra-ui.ui.sel
  (:require [sutra.capps.variables :refer [deref*]]))

(defn rd
  [sel] (when (some? sel) (deref* sel)))

(defn wr!
  [sel val] (when (satisfies? IReset sel) (reset! sel val)))

(defn as-set
  [x]
  (cond (set? x)  x
        (nil? x)  #{}
        (coll? x) (set x)
        :else     #{x}))
