(ns sutra.parser.authoring.edn
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.parser.canonical-print :as cp]))

(set! *warn-on-reflection* true)

(defn- parse [text] (when-not (str/blank? text) (edn/read-string text)))

(def language {:parse parse :print cp/canonical-ent-str})
