(ns sutra.authoring.install.findings
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)

(defn render
  [says f]
  (-> (if (:message f)
        f
        (let [say (or (get says (:rule f))
                      (anomaly/raise! :fault
                                      (str "no words for finding " (:rule f)) {:rule (:rule f)}))]
          (assoc f :message (say f))))
      (dissoc ::ctx)))

(defn scan
  [{:keys [find says]} d]
  (map #(render says %) (find d)))

(defn scan-all
  [checks d]
  (mapcat #(scan % d) checks))

(defn where
  [& parts]
  (str/join " > " (map #(if (keyword? %) (name %) (str %)) parts)))
