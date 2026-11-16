(ns sutra-ui.vega-assets
  (:require [clojure.java.io :as io]))

(def ^:private files
  [["vega/build/vega.min.js" "vega.min.js"]
   ["vega-lite/build/vega-lite.min.js" "vega-lite.min.js"]
   ["vega-embed/build/vega-embed.min.js" "vega-embed.min.js"]])

(defn- same?
  [^java.io.File src ^java.io.File dst]
  (and (.exists dst) (= (.length src) (.length dst))))

(defn copy!
  [output-dir]
  (doseq [[from to] files
          :let [src (io/file "node_modules" from)
                dst (io/file output-dir "vega" to)]
          :when (and (.exists src) (not (same? src dst)))]
    (io/make-parents dst)
    (io/copy src dst)))

(defn hook
  {:shadow.build/stage :flush}
  [build-state & _]
  (try (copy! (get-in build-state [:shadow.build/config :output-dir])) (catch Throwable _ nil))
  build-state)
