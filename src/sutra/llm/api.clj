(ns sutra.llm.api
  (:require [sutra.llm.cost :as cost-log]
            [sutra.llm.forecast :as forecast]
            [sutra.llm.media :as llm-media]
            [sutra.llm.rates :as rates]
            [sutra.llm.wire :as wire]))

(set! *warn-on-reflection* true)

(defn audit [store schema opts] (cost-log/audit store schema opts))

(defn cost [store schema opts] (cost-log/cost store schema opts))

(defn run-calls [store schema run-id] (cost-log/run-calls store schema run-id))

(defn trace [store schema request-id] (cost-log/trace store schema request-id))

(defn usage-by-node [store schema] (cost-log/usage-by-node store schema))

(defn estimate [store schema closure opts] (forecast/estimate store schema closure opts))

(defn llm-transforms [schema closure] (forecast/llm-transforms schema closure))

(defn elide [x] (llm-media/elide x))

(defn hashes-of [v] (llm-media/hashes-of v))

(defn parts [store hash-list opts] (llm-media/parts store hash-list opts))

(defn record-llm-rates! [store head at] (rates/record-llm-rates! store head at))

(def builtin wire/builtin)

(defn embed-request [wire base key payload] (wire/embed-request wire base key payload))

(defn local-endpoint? [base-url] (wire/local-endpoint? base-url))

(defn request [wire base key payload] (wire/request wire base key payload))

(defn response [wire body] (wire/response wire body))

(defn url [base path] (wire/url base path))

(defn wire-for [wires spec base-url] (wire/wire-for wires spec base-url))
