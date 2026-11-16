(ns sutra.governance.guards
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.anomaly :as anomaly]
            [sutra.store.served :as served]
            [sutra.catalog.grants :as grants]
            [sutra.types.entities :as ents]
            [sutra.catalog.branch-policy :refer
             [default-branch-name policy-of write-block instance-block]]
            [sutra.store.protection :as protection]))

(set! *warn-on-reflection* true)

(defn- guard-fences
  [gov branch]
  (vec (for [k (distinct [(str branch) "*"])]
         (protection/record-fence :branch-governance k (get gov k)))))

(defn- instance-batch?
  [touched]
  (boolean (and (seq touched) (every? :instance touched))))

(defn- unreviewed-batch?
  [touched]
  (boolean (and (seq touched)
                (not-any? #(ents/reviewed-section? (keyword (:section %))) touched))))

(defn- unbranched-batch?
  [touched]
  (boolean (and (seq touched)
                (some :instance touched)
                (every? #(or (:instance %)
                             (not (ents/reviewed-section? (keyword (:section %)))))
                        touched))))

(defn write-guard
  ([store seen branch staged] (write-guard store seen branch staged nil))
  ([store {gov :governance :keys [protection-rows] :as seen} branch staged
    {:keys [data? touched instance? open-trunk?]}]
   (when-not (contains? seen :governance)
     (anomaly/raise! :fault "a write guard is decided on governance read at the edge; none given"
                     {:branch branch}))
   (let [prot      (when (and (not data?) (= default-branch-name (str branch)))
                     protection-rows)
         p         (cond-> (policy-of gov branch)
                     (or data? (unreviewed-batch? touched)) (dissoc :protected))
         instance? (or instance? (instance-batch? touched) (unbranched-batch? touched))]
     (when-let [block
                (if (and instance? (or (:protected p)
                                       (and (not data?)
                                            (= default-branch-name (str branch)))))
                  (instance-block branch (grants/all-projects? (grants/read-rights store)))
                  (write-block (cond-> p open-trunk? (dissoc :protected))
                               branch (when (and (:protected p) (not open-trunk?))
                                        (force staged))))]
       (refusal/refuse! (:refusal/code block)
                        (dissoc block :error :refusal/code :refusal/params)
                        (:refusal/params block)))
     (cond-> (-> store
                 (update :db served/fenced (guard-fences gov branch))
                 (assoc ::protection/protection prot
                        ::guarded (str (or branch default-branch-name))))
       (and touched (not data?)) (protection/protection-guard branch touched)))))

(defn guarded?
  [store]
  (= (::guarded store) (str (or (:branch store) default-branch-name))))

(defn data-guard
  [store]
  (write-guard store (protection/guard-reads store) (:branch store) nil {:data? true}))

(defn data-guarded
  [store]
  (if (guarded? store) store (data-guard store)))
