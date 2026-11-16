(ns sutra.store.shapes
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [malli.core :as m]))

(set! *warn-on-reflection* true)

(def registry
  (edn/read-string (slurp (io/resource "sutra/store/driver.malli.edn"))))

(def ^:private opts {:registry (merge (m/default-schemas) registry)})

(def schema
  (memoize (fn [k]
             (when-not (contains? registry k)
               (throw (ex-info (str "no driver shape " k) {:shape k})))
             (m/schema [:ref k] opts))))

(def ^:private validator (memoize (fn [k] (m/validator (schema k)))))

(defn valid? [k x] ((validator k) x))

(defn explain [k x] (m/explain (schema k) x))
