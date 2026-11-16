(ns sutra.builds.plan.producers
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.datasets :as dsets]
            [sutra.catalog.retirement :as retirement]
            [sutra.catalog.transforms :as transforms]
            [sutra.types.languages :as langs]
            [sutra.types.naming :as naming]
            [sutra.foundation.encoding :as enc]
            [sutra.builds.plan.expectations :as expect]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.builds.import.materialize :as mat]
            [sutra.store.access.substrate :as sub]
            [sutra.builds.plan :as plan]
            [sutra.builds.run.setup :as run-setup]
            [sutra.types.column :as column]))

(set! *warn-on-reflection* true)

(defn- write-mode
  [replace? t]
  (if (or replace? (transforms/snapshot-write? t)) :replace :append))

(defn- replaces?
  [{:keys [full-refresh?] :as ctx} t]
  (or full-refresh? (inputs/snapshot-run? ctx t)))

(defn write-fact
  ([ds modify? counts] (write-fact ds modify? false counts))
  ([ds modify? upsert? {:keys [written deleted]}]
   {:dataset-rid ds :incremental? modify? :upsert? upsert?
    :written written :deleted deleted}))

(defn- materialize-object-type!
  [store schema ds-rid ot]
  (let [props (get-in schema [:datasets ds-rid :properties])
        keep? (when (seq props) (into #{} (map column/attr->col) props))
        rows  (cond->> (mat/object-rows (sub/reader store) schema ot)
                keep? (mapv (fn [r] (update r :values select-keys keep?))))]
    (dsets/write-rows! store ds-rid :replace rows)
    [{:dataset-rid ds-rid :incremental? false
      :written (count rows) :deleted 0}]))

(defn- bind-incremental
  [store full-refresh? from out-ds]
  (if-let [{:keys [column initial]} (:incremental from)]
    (let [mark (if full-refresh? initial (or (dsets/high-water store out-ds column) initial))]
      (when (nil? mark)
        (refusal/refuse! :refusal.builds/incremental-pull-unbound {:dataset out-ds :column column}))
      (assoc from ::inputs/params [mark]))
    from))

(defn- land-share!
  [store rs out-ds pk rows producer]
  (let [row-id  #(enc/pk-uuid out-ds (pr-str (run-setup/share-key rs (get % pk))))
        rows    (run-setup/stamp rs rows)
        written (dsets/land-rows! store out-ds :append row-id rows producer)
        share   (->> (dsets/rows-with store out-ds [(run-setup/column rs)])
                     (filter #(run-setup/in-share? rs (:values %)))
                     (map :id))
        dead    (run-setup/stale share (map (comp str row-id) rows))]
    (dsets/delete-rows! store out-ds dead)
    (update written :deleted + (count dead))))

(defn- land-dataset-outputs!
  [{:keys [store schema full-refresh? run-setup] :as ctx} t-kw t from ds-outs]
  (if-let [ot (:object-type from)]
    (let [outs (mapv :dataset ds-outs)]
      (when (not= 1 (count outs))
        (refusal/refuse! :refusal.builds/materialization-outputs
                         {:transform t-kw :outputs outs} {:n (count outs)}))
      (materialize-object-type! store schema (first outs) ot))
    (vec
     (for [out ds-outs
           :let [out-ds (:dataset out)
                 pk     (:primary-key out)
                 from   (bind-incremental store
                                          (or full-refresh? (inputs/snapshot-run? ctx t))
                                          from out-ds)
                 rows   (inputs/read-source-rows ctx from (:name out))
                 shared (and pk (run-setup/scopes? run-setup t))
                 _      (when pk
                          (inputs/check-strict-append!
                           ctx t out-ds (map #(enc/pk-uuid out-ds (get % pk)) rows)))]]
       (if shared
         (write-fact out-ds true true
                     (land-share! store run-setup out-ds pk rows (or (:transform-name t) t-kw)))
         (write-fact out-ds (and (not (replaces? ctx t)) (transforms/modify-write? t))
                     (and (boolean pk) (not (transforms/strict-append? t)))
                     (dsets/land-rows! store out-ds (write-mode (replaces? ctx t) t)
                                       (when pk #(enc/pk-uuid out-ds (get % pk)))
                                       rows (or (:transform-name t) t-kw))))))))

(defn- intermediate-plan
  [t-kw t]
  (let [inc?  (transforms/added-read? t)
        snap? (transforms/snapshot-write? t)]
    {:compute    (cond-> (plan/compute t t-kw (if (langs/query-language? (:language t)) :sql-run :plan))
                   inc? (assoc :incremental true))
     :prev-table (when inc? (naming/trans-table t-kw))
     :merge-key  (when-not snap?
                   (if-let [pk (:primary-key t)]
                     (if (sequential? pk) (vec pk) [pk])
                     (refusal/refuse! :refusal.builds/incremental-intermediate-no-key
                                      {:transform t-kw})))
     :writer     (or (:transform-name t) t-kw)}))

(defn- materialize-intermediate!
  [{:keys [store] :as ctx} t-kw t]
  (let [{:keys [compute prev-table merge-key writer]} (intermediate-plan t-kw t)
        rows (inputs/select-output (inputs/run-compute! ctx compute {:prev-table prev-table})
                                   nil)
        _    (when merge-key
               (inputs/check-strict-append! ctx t t-kw
                                            (map (dsets/merge-key-id t-kw merge-key) rows)))]
    [(write-fact t-kw (and (not (inputs/snapshot-run? ctx t)) (transforms/modify-write? t))
                 (dsets/land-rows! store t-kw (write-mode (replaces? ctx t) t)
                                   (when merge-key (dsets/merge-key-id t-kw merge-key))
                                   rows writer))]))

(defn- orphan-skips
  [schema ds-outs-of]
  (let [orphans (into {} (remove (comp seq ds-outs-of key)) (retirement/orphaned-producers schema))]
    (doseq [[t-kw refs] orphans]
      (println
       (format "  ⚠ %s übersprungen: verwaist — sein Output-Typ %s existiert nicht mehr (bleibt so, bis das Code-Repo den Transform weglässt)"
               (name (or (get-in schema [:transforms t-kw :transform-name]) t-kw))
               (pr-str (vec (sort (map str refs)))))))
    (set (keys orphans))))

(defn producer-nodes
  [schema dataset? materialization?]
  (let [ts         (:transforms schema)
        ds-outs-of (into {} (for [[k _ outs] (plan/dataset-output-producers schema)] [k outs]))
        orphan?    (orphan-skips schema ds-outs-of)]
    (for [t-kw (apply concat (transforms/topo-levels schema))
          :when (not (orphan? t-kw))
          :let [t       (get ts t-kw)
                ds-outs (get ds-outs-of t-kw)
                do-ds?  (and (seq ds-outs) (dataset? t-kw))
                do-mat? (and (:output-columns t) (materialization? t-kw))]
          :when (or do-ds? do-mat?)
          :let [from (when do-ds? (:from (plan/walk-chain ts t-kw t)))]]
      {:kind :producer :key t-kw :t-kw t-kw
       :spec {:from from :ds-outs ds-outs :dataset? (boolean do-ds?)
              :materialize? (boolean do-mat?)}})))

(defn producer-job
  [{:keys [ctx sql-cache schema]} _ {t-kw :key {:keys [from ds-outs dataset? materialize?]} :spec}]
  (let [t (get-in schema [:transforms t-kw])]
    {:facts (cond-> []
              dataset?     (into (let [facts (land-dataset-outputs! ctx t-kw t from ds-outs)]
                                   (run! #(expect/check-landed-dataset! ctx t-kw t %) ds-outs)
                                   facts))
              materialize?
              (into (materialize-intermediate! (assoc ctx :mat-cache sql-cache) t-kw t)))}))
