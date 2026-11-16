(ns sutra.parser.ent-file
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [sutra.parser.ent :as ent]))

(set! *warn-on-reflection* true)

(def fs
  {:read      (comp edn/read-string slurp)
   :canonical #(.getCanonicalPath (io/file %))
   :resolve   (fn [base rel] (.getCanonicalPath (io/file (.getParent (io/file base)) rel)))})

(defn read-ent [path] (ent/load-ent fs path))
