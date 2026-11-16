(ns sutra.builds.import.changelog
  (:require [sutra.builds.ports :as ports]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.store.world :as world]
            [sutra.store.access.datasets :as dsets]))

(set! *warn-on-reflection* true)

(defn- unchanged?
  [changed type id]
  (when-let [ids (get changed type)] (not (contains? ids (str id)))))

(defn publish-changes
  [decls changed]
  (into [] (remove (fn [{:keys [kind verb type row]}]
                     (and (= :objects kind) (= :create verb) (unchanged? changed type (:id row)))))
        decls))

(defn changed-keys
  [pk-index changed]
  (into {}
        (map (fn [[type pk->id]]
               [type (into {} (remove #(unchanged? changed type (val %))) pk->id)]))
        pk-index))

(defn unindexed-ids
  [store obj-type ds rows]
  (if (< (sub/stored-count store obj-type) (dsets/table-count store ds))
    (let [held (into #{} (map (comp str :id)) (sub/read-type (ctx/as-system store) obj-type nil))]
      (into #{} (comp (map (comp str :id)) (remove held)) rows))
    #{}))

(defn edit-exempt-pks
  [store obj-type-kw]
  (into #{} (keep (fn [[id {:keys [created?]}]] (when created? id)))
        (world/edited store obj-type-kw)))

(defn- kept-pks
  [store obj-type-kw]
  (into (edit-exempt-pks store obj-type-kw) (map str)
        (ports/foreign-owned-ids store obj-type-kw)))

(defn object-removals
  ([store obj-type-kw t-name] (object-removals store obj-type-kw t-name nil))
  ([store obj-type-kw t-name {:keys [propagate-empty? low produced known-removed]}]
   (let [exempt  (delay (kept-pks store obj-type-kw))
         removed (fn [via ks] {:removed (into [] (remove @exempt) ks) :via via})]
     (cond
       (and (not propagate-empty?) (zero? (dsets/table-count store t-name)))
       {:removed [] :via :empty-source-valve}

       produced
       (removed :full-compare
                (into [] (remove produced)
                      (mapv :id
                            (sub/read-type (ctx/as-system store) obj-type-kw nil))))

       :else
       (if-let [rm (or known-removed (dsets/removed-pks store t-name low))]
         (removed :changelog rm)
         (let [produced (into #{} (dsets/table-pks store t-name))]
           (removed :full-compare
                    (into [] (remove produced)
                          (mapv :id
                                (sub/read-type (ctx/as-system store) obj-type-kw
                                               nil))))))))))

(defn link-edge-removals
  ([store conn-ref source-ids] (link-edge-removals store conn-ref source-ids nil))
  ([store conn-ref source-ids {:keys [propagate-empty? low edge-dataset]}]
   (let [ids       (into #{} (map str) source-ids)
         edits     (delay (world/edited store conn-ref))
         tombstoned (vec (keep (fn [[id e]] (when (:deleted? e) id)) @edits))
         connected (delay (into #{}
                                (keep (fn [[id e]]
                                        (when (and (:created? e) (not (:deleted? e))) id))) @edits))
         removed   (fn [via edge-ids]
                     {:removed (into [] (remove @connected) edge-ids)
                      :tombstoned tombstoned
                      :via via})]
     (cond
       (and (empty? ids) (not propagate-empty?))
       {:removed [] :tombstoned tombstoned :via :empty-source-valve}

       :else
       (if-let [rm (and edge-dataset (dsets/removed-pks store edge-dataset low))]
         (removed :changelog rm)
         (removed :full-compare
                  (into [] (remove ids)
                        (mapv :id
                              (sub/read-links (ctx/as-system store) conn-ref
                                              nil)))))))))

(defn unedited-links
  [store conn-ref]
  (let [connected (into #{} (keep (fn [[id e]] (when (and (:created? e) (not (:deleted? e))) id)))
                        (world/edited store conn-ref))]
    (into [] (remove (comp connected str :id))
          (sub/read-links (ctx/as-system store) conn-ref nil))))

(defn link-unregister-removals
  [store conn-ref]
  {:removed    (mapv :id (unedited-links store conn-ref))
   :tombstoned []
   :via        :unregister})
