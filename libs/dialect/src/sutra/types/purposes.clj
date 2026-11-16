(ns sutra.types.purposes
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.types.entities :as ents]
            [sutra.types.table :as tbl]))

(set! *warn-on-reflection* true)

(def ^:private purposes-path "sutra/types/purposes.edn")

(defn- read-purposes
  [path]
  (let [r (or (io/resource path)
              (anomaly/raise! :fault
                              (str "no " path " on the classpath — the prose of the resources"
                                   " is missing, and nothing can be explained to a reader")
                              {:path path}))
        m (edn/read-string (slurp r))]
    (when-not (map? m)
      (anomaly/raise! :fault (str "invalid " path ": a map of section → one sentence")
                      {:path path}))
    m))

(def purposes
  (read-purposes purposes-path))

(defn coverage-gaps
  [sections prose]
  (let [sections (set sections)]
    (vec (concat (for [s (sort-by str sections) :when (not (contains? prose s))]
                   {:section s :verdict :no-purpose})
                 (for [[s v] (sort-by (comp str key) prose)]
                   (cond
                     (not (contains? sections s)) {:section s :verdict :no-such-resource}
                     (str/blank? (str v))         {:section s :verdict :empty-purpose}))))))

(def ^:private sections (mapv :section ents/entities))

(def ^:private gaps-now (remove nil? (coverage-gaps sections purposes)))

(tbl/holds! "every resource can be explained, and every sentence is about one that exists"
            (mapv (juxt :section :verdict) gaps-now))

(defn reader-table
  []
  (vec (for [row (ents/regime-report)]
         (assoc row :purpose (get purposes (:resource row))))))
