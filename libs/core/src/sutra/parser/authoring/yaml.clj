(ns sutra.parser.authoring.yaml
  (:require [clj-yaml.core :as yaml]
            [sutra.parser.authoring.data :as data]))

(set! *warn-on-reflection* true)

(defn- parse [text]
  (when-let [doc (yaml/parse-string text :keywords true)]
    (data/->document doc)))

(defn- emit [doc]
  (yaml/generate-string (data/->data doc) :dumper-options {:flow-style :auto}))

(def language {:parse parse :print emit})
