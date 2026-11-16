(ns sutra-ui.maplibre-assets
  (:require [clojure.java.io :as io]))

(def ^:private files
  [["maplibre-gl/dist/maplibre-gl.mjs" "maplibre-gl.mjs"]
   ["maplibre-gl/dist/maplibre-gl-worker.mjs" "maplibre-gl-worker.mjs"]
   ["maplibre-gl/dist/maplibre-gl.css" "maplibre-gl.css"]])

(defn- same?
  [^java.io.File src ^java.io.File dst]
  (and (.exists dst) (= (.length src) (.length dst))))

(def ^:private boot
  (str "import * as m from '/js/maplibre/maplibre-gl.mjs';\n"
       "window.maplibregl = m;\n"))

(defn copy!
  [output-dir]
  (let [b (io/file output-dir "maplibre" "boot.mjs")]
    (when-not (and (.exists b) (= boot (slurp b)))
      (io/make-parents b)
      (spit b boot)))
  (doseq [[from to] files
          :let [src (io/file "node_modules" from)
                dst (io/file output-dir "maplibre" to)]
          :when (and (.exists src) (not (same? src dst)))]
    (io/make-parents dst)
    (io/copy src dst)))

(defn hook
  {:shadow.build/stage :flush}
  [build-state & _]
  (try (copy! (get-in build-state [:shadow.build/config :output-dir])) (catch Throwable _ nil))
  build-state)
