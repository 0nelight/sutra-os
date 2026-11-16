(ns sutra.catalog.system-datasets
  (:require [sutra.capps.variables :as vars]
            [sutra.capps.refs :as refs]
            [sutra.catalog.shape :as shape]
            [sutra.catalog.lookup :as lookup]
            [sutra.ref.install :as ri]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(def system-object-types
  #{:user :marking})

(def edit-provenance-rid
  "ri.sutra.main.dataset.e6d5c4b3-a2f1-3098-8765-4321fedcba98")

(def link-edit-provenance-rid
  "ri.sutra.main.dataset.f7e6d5c4-b3a2-3109-9876-5432fedcba09")

(def system-datasets
  (into {}
        (map (fn [[rid ds]] [rid (assoc ds :id rid)]))
        {"ri.sutra.main.dataset.3c8b083e-3717-3380-9ff2-f2a4dd40e203"
         {:dataset-name :llm_call_log
          :description "Audit ledger: one row per LLM chat-completions call (input/output/tokens/cost), so any extracted value is traceable to the call that produced it."
          :kind :standalone
          :columns
          [{:attribute-name :ts                :attribute-type :string}
           {:attribute-name :request_id        :attribute-type :string}
           {:attribute-name :caller            :attribute-type :string}
           {:attribute-name :run_id            :attribute-type :string}
           {:attribute-name :source            :attribute-type :string}
           {:attribute-name :model             :attribute-type :string}
           {:attribute-name :input_json        :attribute-type :text}
           {:attribute-name :output_json       :attribute-type :text}
           {:attribute-name :prompt_tokens     :attribute-type :integer}
           {:attribute-name :completion_tokens :attribute-type :integer}
           {:attribute-name :latency_ms        :attribute-type :integer}
           {:attribute-name :error             :attribute-type :text}
           {:attribute-name :input_sha         :attribute-type :string}]}

         "ri.sutra.main.dataset.225d57a0-b16c-38ad-892c-d7da11d6f4aa"
         {:dataset-name :llm_model_rates
          :description "Rate-change ledger: one append-only row per (LLM model, rate) with the date it took effect, so pricing history is a queryable table."
          :kind :standalone
          :columns
          [{:attribute-name :model          :attribute-type :string}
           {:attribute-name :rate_in        :attribute-type [:decimal 18 8]}
           {:attribute-name :rate_out       :attribute-type [:decimal 18 8]}
           {:attribute-name :currency       :attribute-type :string}
           {:attribute-name :effective_from :attribute-type :string}
           {:attribute-name :reason         :attribute-type :text}
           {:attribute-name :deprecated     :attribute-type :boolean}]}

         "ri.sutra.main.dataset.b09e4c10-98c8-3452-a011-b640f3fa6790"
         {:dataset-name :dataset_transactions
          :description "Catalog transaction log: one typed transaction (SNAPSHOT/APPEND/DELETE) per dataset per build Job, with its snapshot_id, row_delta and row_count — the domain-level view of the snapshot graph."
          :kind :standalone
          :store :control
          :branch-model :event
          :columns
          [{:attribute-name :txn_rid       :attribute-type :string}
           {:attribute-name :dataset_rid   :attribute-type :string}
           {:attribute-name :branch        :attribute-type :string}
           {:attribute-name :run_id        :attribute-type :string}
           {:attribute-name :job_rid       :attribute-type :string}
           {:attribute-name :snapshot_id   :attribute-type :string}
           {:attribute-name :prev_txn_rid  :attribute-type :string}
           {:attribute-name :type          :attribute-type :string}
           {:attribute-name :status        :attribute-type :string}
           {:attribute-name :schema_commit :attribute-type :string}
           {:attribute-name :row_count     :attribute-type :long}
           {:attribute-name :row_delta     :attribute-type :long}
           {:attribute-name :started_at    :attribute-type :string}
           {:attribute-name :ended_at      :attribute-type :string}]}

         "ri.sutra.main.dataset.ad94deca-83fc-3d6b-bb24-b03e85929ddc"
         {:dataset-name :dataset_consumption
          :description "Incremental consumer-pointer: per (consumer transform, input dataset, branch), the last-consumed snapshot_id — the low bound for the next delta read (dataframe('added'))."
          :kind :standalone
          :store :control
          :branch-model :event
          :columns
          [{:attribute-name :consumer_rid      :attribute-type :string}
           {:attribute-name :input_dataset_rid :attribute-type :string}
           {:attribute-name :branch            :attribute-type :string}
           {:attribute-name :last_txn_rid      :attribute-type :string}
           {:attribute-name :last_snapshot_id  :attribute-type :string}
           {:attribute-name :updated_at        :attribute-type :string}]}

         "ri.sutra.main.dataset.7b3c9d1e-4f52-3a8b-9c1d-2e3f4a5b6c7d"
         {:dataset-name :jobs
          :description "Job execution log: one row per transform-execution in a build (status, produced snapshot_id, outputs, attempt)."
          :kind :standalone
          :store :control
          :branch-model :event
          :columns
          [{:attribute-name :job_rid       :attribute-type :string}
           {:attribute-name :run_id        :attribute-type :string}
           {:attribute-name :transform_rid :attribute-type :string}
           {:attribute-name :branch        :attribute-type :string}
           {:attribute-name :status        :attribute-type :string}
           {:attribute-name :attempt       :attribute-type :integer}
           {:attribute-name :snapshot_id   :attribute-type :string}
           {:attribute-name :rows_in       :attribute-type :integer}
           {:attribute-name :rows_out      :attribute-type :integer}
           {:attribute-name :pending_observed :attribute-type :integer}
           {:attribute-name :code_sha      :attribute-type :string}
           {:attribute-name :jobspec_id    :attribute-type :string}
           {:attribute-name :fingerprint   :attribute-type :string}
           {:attribute-name :outputs       :attribute-type :text}
           {:attribute-name :error         :attribute-type :text}
           {:attribute-name :job_log       :attribute-type :text}
           {:attribute-name :job_spans     :attribute-type :text}
           {:attribute-name :consumed_cursors :attribute-type :text}
           {:attribute-name :started_at    :attribute-type :string}
           {:attribute-name :ended_at      :attribute-type :string}]}

         edit-provenance-rid
         {:dataset-name :object_edits
          :description "Per-row user-edit provenance: one row per edited (object-type, pk) with its edit_kind (create/update/delete), so source-reconcile can exempt user-owned rows."
          :kind :standalone
          :store :control
          :branch-model :state
          :branch-key [:obj_table :pk]
          :columns
          [{:attribute-name :obj_table    :attribute-type :string}
           {:attribute-name :pk           :attribute-type :string}
           {:attribute-name :branch       :attribute-type :string}
           {:attribute-name :edit_kind    :attribute-type :string}
           {:attribute-name :edited_values :attribute-type :string}
           {:attribute-name :last_txn_rid :attribute-type :string}
           {:attribute-name :edited_by    :attribute-type :string}
           {:attribute-name :updated_at   :attribute-type :string}]}

         link-edit-provenance-rid
         {:dataset-name :link_edits
          :description "Per-edge user-edit provenance: one row per edited (link-type, link-id) with its edit_kind (connect/disconnect), so a source rebuild exempts user-connected edges and honors user-disconnect tombstones."
          :kind :standalone
          :store :control
          :branch-model :state
          :branch-key [:link_table :link_id]
          :columns
          [{:attribute-name :link_table :attribute-type :string}
           {:attribute-name :link_id    :attribute-type :string}
           {:attribute-name :branch     :attribute-type :string}
           {:attribute-name :from_id    :attribute-type :string}
           {:attribute-name :to_id      :attribute-type :string}
           {:attribute-name :edit_kind  :attribute-type :string}
           {:attribute-name :markings   :attribute-type :string}
           {:attribute-name :edited_by  :attribute-type :string}
           {:attribute-name :updated_at :attribute-type :string}]}}))

(defn control-resident-rids
  []
  (into [] (keep (fn [[rid ds]] (when (= :control (:store ds)) rid)))
        system-datasets))

(defn control-resident?
  [rid]
  (= :control (get-in system-datasets [(str rid) :store])))

(defn branch-model
  [rid]
  (get-in system-datasets [(str rid) :branch-model]))

(defn branch-key
  [rid]
  (get-in system-datasets [(str rid) :branch-key]))

(defn ensure-system-datasets
  [schema]
  (update schema :datasets #(merge % system-datasets)))

(defn readable
  [W]
  (-> (or (not-empty W) shape/empty-schema-map)
      ensure-system-datasets
      vars/lift-schema-applications
      refs/lift-schema-applications))

(defn- system-type-rids-of
  [schema]
  (into #{} (keep #(lookup/rid-of schema :object-types %)) system-object-types))

(defn system-type-rids
  [schema]
  (sch/column schema ::system-type-rids system-type-rids-of))

(defn as-staging-schema
  [w]
  (ensure-system-datasets (ri/ensure-actions (or (not-empty w) shape/empty-schema-map))))
