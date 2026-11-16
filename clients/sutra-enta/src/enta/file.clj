(ns enta.file
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [sutra.parser.ent :as ent]
            [sutra.parser.canonical-print :as cp]))

(defn read-enta
  [path]
  (let [f (io/file path)]
    (when-not (.isFile f)
      (throw (ex-info (str "keine Datei: " path) {:path path :exit 2})))
    (try (edn/read-string (slurp f))
         (catch Exception e
           (throw (ex-info (str path " ist kein lesbares EDN: " (.getMessage e))
                           {:path path :exit 2}))))))

(defn validate
  [app path]
  (try (ent/parse-enta app path) nil
       (catch Exception e (.getMessage e))))

(defn write-enta!
  [app path]
  (when-let [msg (validate app path)]
    (throw (ex-info msg {:path path :exit 1})))
  (spit (io/file path) (cp/canonical-ent-str app))
  app)

(defn derived-module [app] (:derived-properties app))

(defn put-module [app module]
  (if (seq module)
    (assoc app :derived-properties module)
    (dissoc app :derived-properties)))

(defn object-types [app] (vec (:object-types app)))
