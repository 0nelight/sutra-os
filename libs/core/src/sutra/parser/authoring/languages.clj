(ns sutra.parser.authoring.languages
  (:require [sutra.parser.authoring.edn :as edn]
            [sutra.parser.authoring.yaml :as yaml]))

(def languages
  {:edn  edn/language
   :yaml yaml/language})
