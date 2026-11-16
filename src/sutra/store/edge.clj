(ns sutra.store.edge
  (:require [sutra.store.protection :as protection]
            [sutra.store.world :as world]
            [sutra.types.branch-kinds :as branch-kinds]
            [sutra.store.seat :as seat]))

(set! *warn-on-reflection* true)

(defn- pinned!
  [store held b row]
  (cond
    (= "main" b) (world/pin! store (world/basis (world/at held "main")))
    row          (world/pin! store {:branch b :rev (:rev row)})))

(defn read-edge
  ([store b user-id] (read-edge store b user-id nil))
  ([store b user-id {:keys [writes?]}]
   (let [b (str b)
         s (when user-id (branch-kinds/staging-branch b user-id))
         {:keys [basis staging] :as read}
         (world/holding
          store "main"
          (fn [held]
            (let [row  (when (not= "main" b) (world/branch held b))
                  srow (when s (world/branch held s))]
              (cond-> {:branch  b
                       :basis   (pinned! store held b row)
                       :staging (when s
                                  {:branch s :live? (some? srow) :base (:base srow)
                                   :basis  (pinned! store held s srow)})}
                writes? (merge (protection/guard-reads held))))))]
     (assoc read
            :exists?  (some? basis)
            :staging  (some-> staging (dissoc :basis))
            :schema   (when basis (seat/branch-schema (world/at store b)))))))

(defn staging-view
  [{:keys [branch staging]}]
  (let [{s :branch :keys [live? base]} staging]
    {:parent branch :staging s :live? (boolean live?) :base base
     :read (if live? s branch)}))

(defn guarding
  [e]
  (select-keys e [:governance :protection-rows]))
