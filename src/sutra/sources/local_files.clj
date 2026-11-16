(ns sutra.sources.local-files
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.foundation.confine :as confine]
            [sutra.foundation.env :as env]
            [sutra.kit.refusal :as refusal]))

(set! *warn-on-reflection* true)

(def ^:private roots
  (delay (->> (str/split (or (env/getenv "SUTRA_CSV_ROOTS") "/var/sutra/reach:/tmp/sutra-reach")
                         #":")
              (remove str/blank?)
              (mapv #(.getCanonicalPath (io/file %))))))

(defn path
  ([p] (path :refusal.foreign/path-outside p))
  ([refusal-key p]
   (let [f     (io/file (str p))
         canon (.getCanonicalFile f)]
     (when-not (and (.isAbsolute f) (some #(confine/within? (io/file %) canon) @roots))
       (refusal/refuse! refusal-key {:path (str p) :roots @roots}
                        {:dirs (str/join ", " @roots)}))
     (str canon))))
