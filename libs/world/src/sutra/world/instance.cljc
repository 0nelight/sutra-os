(ns sutra.world.instance
  (:require [sutra.world.db :as kdb]
            [sutra.world.delta :as d]
            [sutra.types.entities :as ents]))

(def sections
  ents/instance-sections)

(defn held
  [db]
  (let [own (kdb/nodes db :instance [:held])]
    (into own (for [[s k] own n (ents/instance-via s (kdb/entry db [s k]))] n))))

(defn- entry? [e] (or (nil? e) (map? e)))

(defn overlaid
  [db home]
  (let [W  (kdb/world home)
        cs (into [] (comp (filter #(and (entry? (kdb/entry db %)) (entry? (kdb/entry home %))))
                          (mapcat #(d/entry-delta % (kdb/entry db %) (kdb/entry home %))))
                 (sort-by str (into (held db) (held home))))
        fs (d/frame-delta (select-keys (kdb/world db) sections)
                          (select-keys W sections))]
    (kdb/db-with db (d/around fs cs))))
