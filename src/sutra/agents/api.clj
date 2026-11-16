(ns sutra.agents.api
  (:require [sutra.agents.runtime :as runtime]))

(set! *warn-on-reflection* true)

(defn agent-of
  ([schema nm] (runtime/agent-of schema nm))
  ([schema nm version] (runtime/agent-of schema nm version)))

(defn answer! [store schema agent opts] (runtime/answer! store schema agent opts))

(defn builtin [schema] (runtime/builtin schema))

(defn source-of [schema agent] (runtime/source-of schema agent))

(defn with-model [schema agent model] (runtime/with-model schema agent model))
