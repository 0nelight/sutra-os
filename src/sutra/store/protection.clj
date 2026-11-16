(ns sutra.store.protection
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.coll :as coll]
            [sutra.foundation.ops :as op]
            [sutra.store.control :as ctl]
            [sutra.store.served :as served]
            [sutra.store.api :as api]
            [sutra.store.world :as world]
            [sutra.types.authority :as authority]
            [sutra.catalog.branch-policy :refer
             [actor-name container-policy-change container-policy-of protected-row?
              protection-block]]))

(set! *warn-on-reflection* true)

(defn protection-rows
  [store]
  {:protections        (coll/index-by (comp str :resource) (ctl/list store :protections {}))
   :container-policies (coll/index-by (comp str :container)
                                      (ctl/list store :container-policies {}))})

(defn- released? [row] (some-> (:released row) long pos?))

(defn- protection-in
  [{:keys [protections container-policies]}]
  {:resources (into #{} (keep (fn [[r row]] (when-not (released? row) r))) protections)
   :released  (into #{} (keep (fn [[r row]] (when (released? row) r))) protections)
   :projects  (into #{} (keep (fn [[c r]] (when (some-> (:protect-all r) long pos?) c)))
                    container-policies)})

(defn protection
  [store]
  (protection-in (protection-rows store)))

(defn repository-protected?
  [rows schema repo-rid]
  (protected-row? (protection-in rows)
                  {:rid repo-rid :project (get-in schema [:repositories repo-rid :project])}))

(defn protect!
  [store resources on?]
  (let [rs (into #{} (map str) resources)
        by (actor-name store)]
    (when (seq rs)
      (ctl/plan! store
                 (for [r rs]
                   (fn [s at]
                     (ctl/swapping s :protections {:resource r}
                                   (fn [_] {:protected-by by :protected-at ctl/now
                                            :released (if on? 0 1)})
                                   at))))))
  (protection store))

(defn release-unmarked!
  [store resources]
  (let [held (:protections (protection-rows store))
        rs   (remove #(contains? held (str %)) resources)]
    (when (seq rs) (protect! store rs false))))

(defn container-policy
  [store container]
  (container-policy-of container (ctl/get store :container-policies {:container (str container)})))

(defn container-policies
  [store containers]
  (into {} (for [c (distinct (map str containers))] [c (:review (container-policy store c))])))

(defn set-container-policy!
  [store container opts]
  (let [{:keys [row] :as change} (container-policy-change (container-policy store container) opts)]
    (if (:error change)
      change
      (do (ctl/upsert! store :container-policies
                       (assoc row :updated-by (actor-name store)))
          (container-policy store container)))))

(def space
  (str authority/default-space))

(defn context-project
  [store schema asked]
  (or (some-> asked str not-empty)
      (:default-project (container-policy store space))
      (let [ps (:projects schema)]
        (when (= 1 (count ps)) (some-> (val (first ps)) :project-name name)))))

(defn record-fence
  [coll k row]
  (if row
    (let [e (ctl/expecting coll row)] [:record (:coll e) (:key e) (:expect e)])
    [:record (keyword "ctl" (name coll)) k api/absent]))

(defn- protection-fences
  [{:keys [protections container-policies]} touched]
  (vec (concat
        (for [rid (distinct (keep #(some-> (:rid %) str) touched))]
          (record-fence :protections rid (get protections rid)))
        (for [p (distinct (keep #(some-> (:project %) str) touched))]
          (record-fence :container-policies p (get container-policies p))))))

(defn protection-guard
  [store branch touched]
  (when-not (contains? store ::protection)
    (throw (IllegalStateException.
            "protection is decided on write-guard's reading: guard the store first")))
  (if-let [rows (::protection store)]
    (do (when-let [block
                   (protection-block (protection-in rows) branch touched)]
          (refusal/refuse! (:refusal/code block)
                           (dissoc block :error :refusal/code :refusal/params)
                           (:refusal/params block)))
        (update store :db served/fenced (protection-fences rows touched)))
    store))

(defn guard-reads
  [store]
  (world/holding store "main"
                 (fn [held] {:governance      (op/query {:store held} :governance/policies {})
                             :protection-rows (protection-rows held)})))
