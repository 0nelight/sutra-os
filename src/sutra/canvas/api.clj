(ns sutra.canvas.api
  (:require [sutra.canvas.graphs :as graphs]))

(set! *warn-on-reflection* true)

(defn read-graph-side-data [store graph-rid] (graphs/read-graph-side-data store graph-rid))
