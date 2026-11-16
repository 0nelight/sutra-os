(ns sutra.lineage.api
  (:require [sutra.lineage.facts :as facts]
            [sutra.lineage.reach :as reach]
            [sutra.lineage.render :as render]))

(set! *warn-on-reflection* true)

(defn graph-svg [spec] (render/graph-svg spec))

(defn edges [schema automations] (facts/edges schema automations))

(defn facts [schema answers opts] (facts/facts schema answers opts))

(defn nodes [schema es] (facts/nodes schema es))

(defn resolve-nodes [schema ns handles] (facts/resolve-nodes schema ns handles))

(defn named-nodes [schema] (facts/named-nodes schema))

(defn known-node [known section t] (facts/known-node known section t))

(defn node-id-of [schema] (reach/node-id-of schema))
