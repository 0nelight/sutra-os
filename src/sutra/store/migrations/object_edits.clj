(ns sutra.store.migrations.object-edits
  (:require [sutra.kit.coll :as coll]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.storage.instance-config :as ic]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.store.world :as world]
            [sutra.types.naming :as naming]))

(set! *warn-on-reflection* true)

(defn- done-key [b] (str "migrations.object-edits-adopted." b))

(defn- table-of
  [T]
  (str naming/dataset-prefix (str/replace (peek (str/split (str T) #"\.")) "-" "_")))

(defn- epoch-ms [s]
  (when s (.toEpochMilli (java.time.Instant/parse s))))

(defn- entry
  [T {:keys [pk edit-kind edited-values updated-at]}]
  (let [vs (update-vals (edn/read-string (or edited-values "{}")) :v)]
    (cond-> {:type T :id pk :kind (keyword edit-kind)}
      (seq vs)   (assoc :values vs)
      updated-at (assoc :at (epoch-ms updated-at)))))

(defn adopt!
  [store]
  (let [b (str (:branch store))]
    (when-not (ic/get-cfg store (done-key b))
      (let [types (coll/index-by table-of (keys (:object-types (world/authored store))))
            es    (for [{:keys [values]} (dsets/rows store sysds/edit-provenance-rid)
                        :when (= b (:branch values))
                        :let [T (types (:obj-table values))]
                        :when T]
                    (entry T values))]
        (world/adopt-edits! store es)
        (ic/set-cfg! store (done-key b) "true")
        (count es)))))
