(ns sutra.service.merge
  (:require [sutra.world.db :as db]
            [sutra.world.delta :as d]
            [sutra.types.entities :as ents]
            [sutra.types.tree :as tree]))

(def ^:private merged-sections
  (set ents/sections))

(defn touched
  [& deltas]
  (into #{} (comp (mapcat d/nodes) (filter (comp merged-sections first))) deltas))

(defn- body [x [sec k :as node]]
  (if-some [e (db/entry x node)] {sec {k e}} {}))

(defn- frame-merged
  [base mine theirs [_ s :as node]]
  (let [a (db/value base node) m (db/value mine node) t (db/value theirs node)]
    (if (and (contains? merged-sections s) (= a t)) m t)))

(defn- holding
  [W fcs]
  (into [] (keep (fn [[n f a b :as c]]
                   (if (and (not (map? b)) (seq (let [x (get W (second n))] (when (map? x) x))))
                     (when (not= a {}) [n f a {}])
                     c)))
        fcs))

(defn merge3
  [base mine theirs nodes opts]
  (let [{:keys [frame changes conflicts]}
        (reduce (fn [acc [sec k :as node]]
                  (if (d/frame-node? node)
                    (let [v (frame-merged base mine theirs node)]
                      (cond-> acc (not= v (db/value theirs node))
                              (update :frame conj [node :value (db/value theirs node) v])))
                    (let [{b :body cs :conflicts}
                          (tree/merge3 (body base node) (body mine node) (body theirs node)
                                       opts)]
                      (-> acc
                          (update :changes into
                                  (d/entry-delta node (db/entry theirs node) (get-in b [sec k])))
                          (update :conflicts into cs)))))
                {:frame [] :changes [] :conflicts []}
                nodes)]
    {:changes   (d/around (holding (d/apply-to (db/world theirs) changes) frame) changes)
     :conflicts (vec (sort-by (comp pr-str :node) conflicts))}))

(defn rebased
  [base mine theirs mine-delta theirs-delta opts]
  (let [{:keys [changes conflicts]}
        (merge3 base mine theirs (touched mine-delta theirs-delta) opts)]
    {:db (db/db-with theirs changes) :conflicts conflicts}))
