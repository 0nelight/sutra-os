(ns sutra-ui.pdf-worker
  (:require [clojure.java.io :as io]))

(def ^:private files
  ["pdf.min.mjs" "pdf.worker.min.mjs"])

(def ^:private boot
  (str "import * as p from '/js/pdfjs/pdf.min.mjs';\n"
       "p.GlobalWorkerOptions.workerSrc = '/js/pdfjs/pdf.worker.min.mjs';\n"
       "window.pdfjsLib = p;\n"))

(defn- same?
  [^java.io.File src ^java.io.File dst]
  (and (.exists dst) (= (.length src) (.length dst))))

(defn copy!
  [output-dir]
  (let [b (io/file output-dir "pdfjs" "boot.mjs")]
    (when-not (and (.exists b) (= boot (slurp b)))
      (io/make-parents b)
      (spit b boot)))
  (seq
   (for [f files
         :let [src (io/file "node_modules/pdfjs-dist/build" f)
               dst (io/file output-dir "pdfjs" f)]
         :when (.exists src)]
     (do (when-not (same? src dst)
           (io/make-parents dst)
           (io/copy src dst))
         dst))))

(defn hook
  {:shadow.build/stage :flush}
  [build-state & _]
  (try (copy! (get-in build-state [:shadow.build/config :output-dir])) (catch Throwable _ nil))
  build-state)
