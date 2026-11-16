(ns sutra.parser.presets
  (:require [sutra.kit.coerce :refer [->kw]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.parser.ent :as ent]
            [sutra.parser.ent-file :as ent-file]))

(set! *warn-on-reflection* true)

(def ^:private stufen "presets/standard/")

(defn app-dateien
  []
  (let [u (io/resource (str stufen "apps"))]
    (if (and u (= "file" (.getProtocol u)))
      (->> (.listFiles (io/file u))
           (filter #(str/ends-with? (.getName ^java.io.File %) ".enta"))
           (sort-by #(.getName ^java.io.File %))
           vec)
      [])))

(defn passt?
  [schema app]
  (let [typen   (set (map ->kw (keys (:objects schema))))
        actions (set (map ->kw (keys (:actions schema))))]
    (and (every? (comp typen ->kw) (:object-types app))
         (every? (comp actions ->kw :action) (:actions app)))))

(defn startanwendungen
  [schema]
  (vec (for [^java.io.File f (app-dateien)
             :let [roh (edn/read-string (slurp f))]
             :when (passt? schema roh)]
         {:datei (.getName f)
          :schema (:schema (ent/parse-enta roh (.getPath f)))})))

(defn mit-startanwendungen
  [schema]
  (:schema (ent/merge-entity-schemas
            (into [{:schema schema}] (map #(select-keys % [:schema]))
                  (startanwendungen schema)))))

(defn paket-schema
  [datei]
  (mit-startanwendungen
   (:schema (ent-file/read-ent (.getPath (io/file (io/resource (str stufen datei))))))))
