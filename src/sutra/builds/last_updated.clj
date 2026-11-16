(ns sutra.builds.last-updated
  (:require [sutra.builds.ledger :as ledger]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.outputs :as outputs]
            [sutra.kit.time :as time]
            [sutra.projects.api :as projects]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(defn- on-branch
  [store f]
  (let [branch (or (:branch store) ctl/trunk)]
    (cond-> (f ctl/trunk) (not= ctl/trunk branch) (merge (f branch)))))

(defn- times
  [store]
  {:index (on-branch store #(projects/index-times (ctx/at store %)))
   :txns  (on-branch store #(ledger/last-txn-per-dataset store %))
   :jobs  (on-branch store #(:ok (ledger/last-jobs-per-transform store %)))})

(defn- job-ended
  [{:keys [jobs]} transforms]
  (->> transforms
       (keep #(some-> (get jobs %) :ended-at time/->instant))
       sort
       last))

(defn- source-at
  [schema {:keys [txns] :as ts} resource]
  (let [ds (lookup/->type-rid schema :datasets resource)
        tf (lookup/->type-rid schema :transforms resource)]
    (or (some-> (get txns ds) :ended_at time/->instant)
        (job-ended ts (if ds (outputs/dataset-producers schema ds) (when tf [tf]))))))

(defn- blocked?
  [auth rid]
  (contains? (into (grants/hidden-object-types auth) (grants/blocked-resources auth)) rid))

(defn- source-rid
  [schema resource]
  (or (lookup/->type-rid schema :datasets resource)
      (lookup/->type-rid schema :transforms resource)))

(defn of-items
  [store schema auth items]
  (let [ts (times store)]
    (vec
     (distinct
      (concat
       (for [{:keys [object-type]} items
             :let  [rid (lookup/->type-rid schema :object-types object-type)
                    ot  (get-in schema [:object-types rid])]
             :when (and ot (not (blocked? auth rid)))]
         {:kind "object-type" :name (name object-type)
          :at (some-> (get (:index ts) rid) time/->instant str)})
       (for [{:keys [sources]} items
             {:keys [resource]} sources
             :let  [rid (source-rid schema resource)]
             :when (and rid (not (blocked? auth rid)))]
         {:kind "source" :name (name resource)
          :at (some-> (source-at schema ts resource) str)}))))))
