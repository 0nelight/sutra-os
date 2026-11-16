(ns support.merge-orakel
  (:require [sutra.catalog.kernel :as kernel]
            [sutra.catalog.shape :as shape]
            [sutra.governance.merge :as merge]
            [sutra.ref.install :as ri]
            [sutra.store.lib :as lib]
            [sutra.store.world :as world]
            [sutra.types.entities :as ents]
            [sutra.types.rid :as rid]
            [sutra.types.shape :as tshape]
            [sutra.types.tree :as tree]))

(def ^:private opts {:cut tree/lists-of-maps :fold tshape/fold :first :theirs})

(defn- sections [w] (select-keys w ents/sections))

(defn- cut-to
  [scope w]
  (if scope
    (tree/select (sections w) (into #{} (map (fn [[s k]] [s (rid/->name k)])) scope))
    (sections w)))

(defn- merged [base mine theirs resolve scope]
  (tree/merge3 (cut-to scope base) (cut-to scope mine) (cut-to scope theirs)
               (assoc opts :resolve resolve)))

(defn check
  ([base mine theirs] (check base mine theirs nil))
  ([base mine theirs scope]
   (if (nil? base)
     {:status :no-base :conflicts [] :mine {}}
     (let [{:keys [conflicts]} (merged base mine theirs nil scope)
           [bi mi ti] (map (comp tree/index (partial cut-to scope)) [base mine theirs])
           side       (fn [i n] (if-let [e (get i n)] (:value e) :removed))
           clashes    (vec (for [[[class :as n] cs] (sort-by (comp str key)
                                                     (group-by :node conflicts))
                                 :let [fields (vec (sort (keep (comp tree/label :field) cs)))]]
                             (cond-> {:key    (:key (or (get mi n) (get ti n) (get bi n)))
                                      :mine   (side mi n)
                                      :theirs (side ti n)
                                      :class  class}
                               (seq fields) (assoc :fields fields))))]
       {:status    (if (seq clashes) :conflicts :clean)
        :conflicts clashes
        :mine      (lib/diff-paths (cut-to scope base) (cut-to scope mine))}))))

(defn rebase
  ([base mine theirs resolutions] (rebase base mine theirs resolutions nil))
  ([base mine theirs resolutions scope]
   (let [body (:body (merged base mine theirs (merge/resolver resolutions) scope))]
     (if scope
       (reduce (fn [w [s k]]
                 (if-some [v (get-in body [s k])]
                   (assoc-in w [s k] v)
                   (cond-> w (contains? (get w s) k) (update s dissoc k))))
               theirs scope)
       (merge theirs body)))))

(defn worlds
  [store parent staging]
  (let [{:keys [branch rev]} (:base (world/branch store staging))
        theirs (or (not-empty (world/world (world/at store parent))) shape/empty-schema-map)
        base   (world/world (world/at store branch) rev)]
    {:base          (cond-> base
                      (world/instance-home (world/at store staging))
                      (world/with-instance (world/instance-keys store base theirs) theirs))
     :mine-schema   (or (not-empty (world/world (world/at store staging)))
                        shape/empty-schema-map)
     :theirs-schema theirs
     :scope         (let [m (world/changed-resources (world/at store staging) nil)
                          t (world/changed-resources (world/at store parent) rev)]
                      (when (and m t) (into m t)))}))

(defn rows
  [d base mine]
  (#'merge/entity-diff->rows d (#'merge/world-access d base mine) nil))

(defn verdict
  [merged target]
  (let [{:keys [ok reasons] :as v} (kernel/verdict (ri/ensure-actions merged)
                                                   (some-> target not-empty ri/ensure-actions))]
    (cond-> v (not ok) (-> (dissoc :reasons) (assoc :error reasons)))))
