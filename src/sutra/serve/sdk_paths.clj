(ns sutra.serve.sdk-paths
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.serve.server :as server]
            [sutra.types.ir :as tir]))

(set! *warn-on-reflection* true)

(def file "clients/python-platform-sdk/sutra_platform_sdk/paths.py")

(defn- block-bounds
  [text]
  (let [i (.indexOf ^String text "ENDPOINTS = {\n")
        j (.indexOf ^String text "\n}\n" (int i))]
    (when (and (<= 0 i) (< i j)) [i (+ j 3)])))

(defn committed
  []
  (let [t (slurp file)]
    (when-let [[i j] (block-bounds t)] (subs t i j))))

(defn fresh [] (tir/python-endpoints (server/ir)))

(defn -main
  [& _]
  (let [t      (slurp file)
        [i j]  (or (block-bounds t) (anomaly/raise! :fault (str "no ENDPOINTS block in " file) {}))]
    (spit file (str (subs t 0 i) (fresh) (subs t j)))
    (println "wrote" file)
    (shutdown-agents)
    (System/exit 0)))
