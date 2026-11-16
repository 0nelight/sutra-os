(ns sutra.authoring.preview
  (:require [sutra.projects.api :as projects]
            [sutra.foundation.ops :as op]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.actions :as actions]
            [sutra.catalog.signature :as signature]
            [sutra.types.schema-reach :as reach]
            [sutra.objects.api :as objects]
            [sutra.actions.api :as actions-api]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(defn- staged-types
  [staged-rows]
  (reduce (fn [acc {:keys [action args]}]
            (if-let [section (:section (actions/schema-action-kinds action))]
              (update acc section (fnil conj #{}) (some-> (:name args) clojure.core/name keyword))
              acc))
          {:object-types #{} :link-types #{} :interfaces #{}}
          staged-rows))

(defn preview-projection
  [store {:keys [read live?]}]
  (let [store       (ctx/at store read)
        schema      (actions-api/staging-schema store)
        staged-rows (if live? (actions-api/list-staged store) [])
        body        {:schema         (reach/catalog schema)
                     :schema-derived (objects/derive-all schema)
                     :staged-types   (staged-types staged-rows)
                     :staged-ids     (into #{} (keep #(get-in % [:args :id])) staged-rows)
                     :integrity      (objects/tableless schema (sub/layout store))}]
    (assoc body :version (str (hash body)))))

(op/defop :authoring/preview-projection [{:keys [store]} view]
  (preview-projection store view))

(defn preview-schema
  [store view]
  (actions-api/staging-schema (ctx/at store (:read view))))

(defn- drift
  [schema layout datasets]
  (let [orphans (vec (concat (for [{:keys [name rows]} (:orphans layout)]
                               {:table name :store :objects :rows rows})
                             (for [{:keys [name rows]} (:orphans datasets)]
                               {:table name :store :datasets :rows rows})))
        missing (objects/tableless schema layout)]
    (cond-> {}
      (seq orphans) (assoc :orphan-tables orphans)
      missing       (assoc :tableless-types missing))))

(defn- drift-under
  [store schema]
  (drift schema (sub/layout store) (dsets/inspect store (signature/datasets-of schema))))

(defn- branch-drift
  [store]
  (drift-under store (actions-api/staging-schema store)))

(defn orphan-tables
  [store schema]
  (:orphan-tables (drift-under store schema)))

(defn preview-physical-drift
  [store view]
  (branch-drift (ctx/at store (:read view))))

(defn- prune-branch!
  [store b]
  (let [store      (ctx/at store b)
        orphans (:orphan-tables (branch-drift store))]
    (when (some #(= :objects (:store %)) orphans)
      (world/drop-unclaimed! store))
    (into (mapv :table (filter #(= :objects (:store %)) orphans))
          (dsets/drop-datasets! store (mapv :table (filter #(= :datasets (:store %)) orphans))))))

(defn prune-orphan-tables!
  [store {:keys [parent staging live? read]} {:keys [dry-run?]}]
  (let [preview (:orphan-tables (branch-drift (ctx/at store read)))]
    (if (or dry-run? (empty? preview))
      {:dry-run? true :orphans preview :dropped []}
      (let [dropped (prune-branch! store parent)
            staged  (when live? (prune-branch! store staging))]
        {:orphans  preview
         :dropped  (vec (distinct (concat dropped staged)))
         :branches (cond-> [parent] live? (conj staging))}))))
