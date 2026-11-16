(ns sutra.types.planes
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [sutra.types.entities :as ents]
            [sutra.types.purposes :as purposes]
            [sutra.types.table :as tbl]))

(set! *warn-on-reflection* true)

(def ^:private planes-path "sutra/types/planes.edn")

(def planes
  (let [r (or (io/resource planes-path)
              (anomaly/raise! :fault (str "no " planes-path " on the classpath")
                              {:path planes-path}))
        m (edn/read-string (slurp r))]
    (when-not (map? m)
      (anomaly/raise! :fault
                      (str "invalid " planes-path ": a map of plane → row") {:path planes-path}))
    m))

(def review-doors
  #{"propose" "approve" "reject" "merge-proposal" "git push"})

(def plane-verdicts
  {:no-regime                     "names no regime — nothing says who may say no to a change"
   :unknown-regime                "names a regime the vocabulary does not know"
   :no-door
   "names no door — a resource nobody can write is a resource nobody has"
   :no-purpose                    "cannot be explained to a reader"
   :no-counterpart
   "names no counterpart, so conformance is asserted and not readable"
   :content-through-the-contract-door
   "not :declared and yet named a door of the review path — that is the detour this table was written for"
   :content-without-a-folder-door
   ":own-history and no folder sync: content that is a file on disk has to be handed over one call at a time"})

(defn plane-gaps
  [planes regimes]
  (vec (for [[nm {:keys [purpose regime doors folder-sync counterpart]}] (sort-by (comp str key) planes)
             :let [v (cond
                       (nil? regime)                       :no-regime
                       (not (contains? regimes regime))    :unknown-regime
                       (empty? doors)                      :no-door
                       (str/blank? (str purpose))          :no-purpose
                       (str/blank? (str counterpart))          :no-counterpart
                       (and (not (ents/reviewed regime))
                            (some review-doors doors))     :content-through-the-contract-door
                       (and (= :own-history regime)
                            (str/blank? (str folder-sync))) :content-without-a-folder-door)]
             :when v]
         {:plane nm :verdict v :regime regime :doors (vec doors) :why (plane-verdicts v)})))

(def ^:private gaps-now (plane-gaps planes ents/regimes))

(tbl/holds!
 "every plane is decided, writable, explainable, and keeps content out of the review path"
 (mapv (juxt :plane :verdict)
       (remove #(= :content-without-a-folder-door (:verdict %)) gaps-now)))

(defn all-resources
  []
  (vec (concat (purposes/reader-table)
               (for [[nm {:keys [purpose regime doors counterpart]}] (sort-by (comp str key) planes)
                     :let [r (get ents/regimes regime)]]
                 {:resource nm :purpose purpose :regime regime
                  :label (:label r) :why (:why r) :undo (:undo r)
                  :doors (vec doors) :counterpart counterpart}))))
