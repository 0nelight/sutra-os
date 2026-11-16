(ns sutra.builds.import.reconcile
  (:require [sutra.store.access.datasets :as dsets]
            [sutra.catalog.backing :as backing]
            [sutra.catalog.system-datasets :as sysds]
            [sutra.types.naming :as naming]
            [sutra.catalog.shape :as shape]
            [sutra.builds.import.changelog :as changelog]
            [sutra.builds.plan.producers :as producers]
            [sutra.builds.ledger :as ledger]
            [sutra.store.world :as world]))

(set! *warn-on-reflection* true)

(defn- delete-ops
  [op type ids]
  (mapv (fn [id] {:op op :type type :payload {:id (str id)}}) ids))

(defn- replaced?
  [{:keys [snapshot? producer]} snapshot-run]
  (or snapshot? (contains? snapshot-run producer)))

(defn already-landed?
  [{landed-mode :mode landed-rows :rows} mode rows]
  (and (= landed-mode mode) (identical? landed-rows rows)))

(defn- object-write!
  [store schema snapshot-run landed [obj-type obj-rows]]
  (let [{:keys [dataset] :as backing} (backing/object-backing schema obj-type)
        replace? (replaced? backing snapshot-run)
        mode     (if replace? :snapshot :append)]
    (when-not (or (= (naming/dataset-table dataset) (naming/obj-table obj-type))
                  (already-landed? (get landed obj-type) mode obj-rows))
      (dsets/write-rows! store dataset mode obj-rows))
    (producers/write-fact dataset (not replace?) {:written (count obj-rows) :deleted 0})))

(defn- link-write!
  [store schema [conn-type pairs]]
  (let [{:keys [dataset]} (backing/backing-of schema :link-type conn-type)]
    (dsets/write-edges! store dataset
                        (mapv #(assoc % :id (shape/link-id-for schema conn-type %)) pairs))
    {:dataset-rid dataset :incremental? false :written (count pairs) :deleted 0}))

(defn- landed
  [store schema run-id kind write! [target :as entry]]
  (when-let [{:keys [producer]} (backing/backing-of schema kind target)]
    (assoc (write! store schema entry)
           :job-rid (ledger/job-rid run-id producer)
           :transform-rid producer)))

(defn write-object-tables!
  [store schema run-id snapshot-run staged-rows rows]
  (into [] (keep #(landed store schema run-id :object-type
                          (fn [s sch entry] (object-write! s sch snapshot-run staged-rows entry))
                          %))
        (remove #(:authored? (backing/object-backing schema (key %))) rows)))

(defn write-link-tables!
  [store schema run-id conn-pairs]
  (into [] (keep #(landed store schema run-id :link-type link-write! %)) conn-pairs))

(defn still-known-removed
  [store dataset {:keys [signature removed]}]
  (when (and (some? signature)
             (not (sysds/control-resident? dataset))
             (= signature (dsets/content-signature store (naming/dataset-table dataset))))
    removed))

(defn reconcile-object-tables!
  ([store branch schema rows low snapshot-run]
   (reconcile-object-tables! store branch schema rows low snapshot-run nil))
  ([store branch schema rows low snapshot-run known-removals]
   (vec
    (for [[obj-type _] rows
          :let [backing (backing/object-backing schema obj-type)]
          :when (replaced? backing snapshot-run)
          :let [{:keys [dataset external? authored?]} backing
                {:keys [removed via]} (changelog/object-removals
                                       store obj-type dataset
                                       (cond-> {:propagate-empty? (not external?) :low low
                                                :known-removed
                                                (still-known-removed store dataset
                                                                     (get known-removals obj-type))}
                                         authored? (assoc :produced (into #{} (map (comp str :id))
                                                                          (get rows obj-type)))))]]
      (do (when (seq removed)
            (world/index! (world/at store branch) (delete-ops :delete-object obj-type removed)))
          {:target obj-type :via via :removed (count removed)})))))

(defn reconcile-link-tables!
  [store branch schema conn-pairs low]
  (vec
   (for [[conn-type pairs] conn-pairs
         :when (shape/owns-edge-rows? schema conn-type)
         :let [{:keys [dataset external? snapshot?]}
               (backing/backing-of schema :link-type conn-type)]
         :when snapshot?
         :let [at  (world/at store branch)
               ids (map #(shape/link-id-for schema conn-type %) pairs)
               {:keys [removed via]} (changelog/link-edge-removals at conn-type ids
                                                                   {:propagate-empty?
                                                                    (not external?)
                                                                    :low low
                                                                    :edge-dataset dataset})]]
     (do (when (seq removed)
           (world/index! at (delete-ops :delete-link conn-type removed)))
         {:target conn-type :via via :removed (count removed)}))))

(defn- adopt-ops
  [conn-rid links]
  (mapv (fn [{:keys [id from-object-id to-object-id markings]}]
          {:op :create-link :type conn-rid
           :payload (cond-> {:id (str id) :from-id (str from-object-id)
                             :to-id (str to-object-id)}
                      (seq markings) (assoc :markings (vec markings)))})
        links))

(defn reregister-link-backing!
  [store schema conn-rid owed]
  (when (and (contains? (:link-types schema) conn-rid) (shape/owns-edge-rows? schema conn-rid))
    (case owed
      :retract (let [{:keys [removed]} (changelog/link-unregister-removals store conn-rid)]
                 (when (seq removed)
                   (world/index! store (delete-ops :delete-link conn-rid removed))))
      :adopt   (let [basis (world/head store)]
                 (when-let [links (seq (changelog/unedited-links store conn-rid))]
                   (world/commit! store {:ops     (adopt-ops conn-rid links)
                                         :actor   {:principal "sutra.builds.reconcile"}
                                         :message (str "edges of " conn-rid
                                                       " kept as edits: a producer backs it")
                                         :basis   basis
                                         :fences  [[:type conn-rid]]}))))))
