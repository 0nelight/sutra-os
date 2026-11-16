(ns sutra.projects.entity-resolution
  (:require [sutra.store.control :as ctl]
            [sutra.store.records :as r]))

(set! *warn-on-reflection* true)

(defn entity-state-by-type [store branch]
  (->> (ctl/list store :entity-crosswalk {:branch (or branch ctl/trunk)})
       (group-by :object-type)
       (map (fn [[t rows]]
              {:object-type t :rows (clojure.core/count rows)
               :entities (clojure.core/count (distinct (map :entity-id rows)))}))
       (sort-by (comp - :rows))
       vec))

(defn entity-state-elsewhere [store branch]
  (let [b  (or branch ctl/trunk)
        cw (frequencies (keep #(when (not= b (:branch %)) (:branch %))
                              (r/list store (ctl/decl :entity-crosswalk) {} {:all-branches? true})))
        pv (frequencies (keep #(when (not= b (:branch %)) (:branch %))
                              (r/list store (ctl/decl :entity-provenance) {}
                                      {:all-branches? true})))]
    (->> (distinct (concat (keys cw) (keys pv))) sort
         (mapv (fn [x]
                 {:branch x :crosswalk (long (clojure.core/get cw x 0))
                  :provenance (long (clojure.core/get pv x 0))})))))

(defn sweep-entity-resolution!
  ([store branch resolved-rids] (sweep-entity-resolution! store branch resolved-rids {}))
  ([store branch resolved-rids {:keys [apply?]}]
   (let [b     (or branch ctl/trunk)
         keep  (set resolved-rids)
         cw    (ctl/list store :entity-crosswalk {:branch b})
         kept  (filter #(keep (:object-type %)) cw)
         gone  (remove #(keep (:object-type %)) cw)
         surv  (into #{} (map :entity-id) kept)
         pv    (remove #(surv (:entity-id %)) (ctl/list store :entity-provenance {:branch b}))]
     (when (and apply? (seq (concat gone pv)))
       (ctl/commit! store
                    (concat
                     (when (seq pv)
                       [(fn [s _] (ctl/deleting s :entity-provenance
                                                {:seq (into #{} (map :seq) pv)}))])
                     (for [c gone]
                       (fn [s _] (ctl/deleting s :entity-crosswalk
                                               (select-keys c [:branch :object-type :id-kind
                                                               :id-value])))))))
     {:crosswalk (clojure.core/count gone) :provenance (clojure.core/count pv)
      :kept (clojure.core/count kept)})))
