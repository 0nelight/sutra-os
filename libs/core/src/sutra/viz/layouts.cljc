(ns sutra.viz.layouts
  (:require [clojure.string :as str]
            [sutra.kit.refusal :as refusal]
            [sutra.viz.graph-layout :as gl]))

(def ^:private richtungen #{:links-rechts :oben-unten})

(def built-in
  {:hierarchie {:label  "Hierarchie"
                :layout gl/hierarchie
                :doc    "Schaltschrank, Abhängigkeiten, alles Gerichtete: Schichten nach Tiefe, in
                         jeder Schicht nach Baryzentrum geordnet."}
   :baum       {:label  "Baum"
                :layout gl/baum
                :doc    "Strenge Bäume — Kinder nebeneinander, das Elternteil mittig darüber."}})

(def default-layout :hierarchie)

(defn labels
  [table]
  (update-vals table :label))

(defn entry
  [table k]
  (let [m table
        k (or k default-layout)]
    (or (get m k)
        (refusal/refuse! :refusal.viz/unknown-layout
                         {:layout k :layouts (vec (sort (keys m)))}
                         {:installed (str/join ", " (sort (map name (keys m))))}))))

(defn place
  [table nodes edges opts]
  (let [{:keys [richtung]} opts]
    (when (and richtung (not (richtungen richtung)))
      (refusal/refuse! :refusal.viz/unknown-richtung {:richtung richtung}))
    (if (empty? nodes)
      []
      (vec ((:layout (entry table (:layout opts)))
            (vec nodes)
            (vec edges)
            (dissoc (or opts {}) :layout))))))
