(ns sutra.parser.authoring.workflow
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def standard
  (edn/read-string (slurp (io/resource "sutra/authoring/workflow.edn"))))

(defn- fill [s params]
  (str/replace s #"\{(\w+)\}" (fn [[m k]] (str (get params (keyword k) m)))))

(defn lines
  ([way params] (lines standard way params))
  ([{:keys [steps ways]} way params]
   (mapv #(fill (:line (get steps %)) params) (get ways way))))

(defn sentence
  [way params]
  (str/join " → " (map #(str "`" % "`") (lines way params))))
