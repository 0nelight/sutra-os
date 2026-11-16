(ns sutra.parser.authoring.standard
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [sutra.parser.checkout :as co]
            [sutra.parser.sugar :as sugar]))

(set! *warn-on-reflection* true)

(def ^:private source
  (edn/read-string (slurp (io/resource "sutra/authoring/standard.edn"))))

(def standard
  (dissoc source :supersedes))

(def projections (:projections standard))

(defn entry-ids
  [fragment]
  (into (sorted-set) (for [[section entries] fragment, [id _] entries] [section (name id)])))

(defn fingerprint
  [entry]
  (co/fingerprint (select-keys entry [:description :filename :select :edits :elements])))

(defn- superseded?
  [section id entry]
  (and (map? entry)
       (contains? (get-in source [:supersedes section (keyword (name id))] #{})
                  (fingerprint entry))))

(defn missing
  [schema adopted]
  (into {}
        (keep (fn [[section entries]]
                (let [have (into {} (map (fn [[k v]] [(name k) [k v]])) (get schema section))
                      todo (into (sorted-map)
                                 (keep (fn [[id e]]
                                         (if-let [[k v] (have (name id))]
                                           (when (superseded? section id v) [k e])
                                           (when-not (adopted [section (name id)]) [id e]))))
                                 entries)]
                  (when (seq todo) [section todo]))))
        standard))

(defn migrate
  [schema adopted]
  (let [drop-in (fn [m k] (update-vals m #(cond-> % (map? %) (dissoc k))))]
    (sugar/derive-projects
     (cond-> (reduce-kv (fn [s section es] (update s section merge es))
                        (dissoc schema :authoring)
                        (missing schema adopted))
       (:projects schema)    (update :projects #(-> % (drop-in :concern) (drop-in :stage)))
       (:projections schema) (update :projections drop-in :layout)))))
