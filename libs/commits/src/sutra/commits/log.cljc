(ns sutra.commits.log
  (:require [sutra.world.db :as db]
            [sutra.world.delta :as d]))

(defn frame-changes
  [db frame]
  (d/frame-delta (db/world db) frame))

(defn upgraded
  [upgrade [sec k] v]
  (let [[[k' e]] (seq (get (upgrade {sec {k (or v {})}}) sec))]
    [[sec k'] (when (some? v) e)]))

(defn changes
  [db upgrade vs]
  (into []
        (mapcat (fn [[id v]]
                  (let [[node e] (upgraded upgrade id v)]
                    (d/entry-delta node (db/entry db node) e))))
        vs))

(defn- entry? [e] (or (nil? e) (map? e)))

(defn stepped
  [db W]
  (let [W0 (db/world db)
        cs (into [] (for [s (distinct (concat (keys W0) (keys W)))
                          :let [a (get W0 s) b (get W s)]
                          :when (and (not (identical? a b)) (or (map? a) (map? b)))
                          :let [a (if (map? a) a {}) b (if (map? b) b {})]
                          k (distinct (concat (keys a) (keys b)))
                          :let [x (get a k) y (get b k)]
                          :when (and (not= x y) (entry? x) (entry? y))
                          c (d/entry-delta [s k] x y)]
                      c))]
    (db/db-with db (d/around (d/frame-delta W0 W) cs))))
