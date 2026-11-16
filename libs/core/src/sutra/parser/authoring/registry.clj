(ns sutra.parser.authoring.registry
  (:require [sutra.parser.authoring :as authoring]
            [sutra.parser.authoring.languages :as languages]
            [sutra.parser.ent :as ent]))

(set! *warn-on-reflection* true)

(def registry
  {:languages languages/languages
   :kinds     ent/kinds})

(defn formats
  [texts]
  (authoring/formats registry texts))

(defn projection
  [schema nm]
  (some (fn [[k v]] (when (= (name nm) (name k)) v)) (:projections schema)))

(defn texts
  [schema nm]
  (formats (:filename (projection schema nm))))
