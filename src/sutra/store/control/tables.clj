(ns sutra.store.control.tables
  (:require [sutra.kit.coll :as coll]
            [clojure.string :as str]
            [sutra.store.records :as records]))

(set! *warn-on-reflection* true)

(def ts "TIMESTAMP(6)")
(def ts-now "TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP")
(def identity-seq "BIGINT GENERATED ALWAYS AS IDENTITY")
(def branch-col [:branch "VARCHAR(255) NOT NULL"])
(def branch-col-null [:branch "VARCHAR(255)"])

(def graph-node-kinds
  #{:object-type :object :connection :datasource :dataset :transform :application :action
    :function :group})

(def tables
  [{:name :blobs :sql-only? true :table "control_blobs" :scope :global :key [:hash]
    :cols [[:hash "CHAR(64) NOT NULL"] [:size "BIGINT NOT NULL"] [:stored-at ts-now]]}
   {:name :blob-chunks :sql-only? true :table "control_blob_chunks" :scope :global :key [:hash :n]
    :cols [[:hash "CHAR(64) NOT NULL"] [:n "INTEGER NOT NULL"] [:bytes "BYTEA NOT NULL"]]}
   {:name :staged-actions :table "control_staged_actions" :scope :branch :key [:seq]
    :order [[:seq :asc]]
    :on-drop :keep-for-staging
    :cols [[:seq identity-seq] branch-col [:action "VARCHAR(255) NOT NULL" :keyword]
           [:args "TEXT NOT NULL" :edn] [:tx-vector "TEXT" :edn] [:created-at ts-now]
           [:author "VARCHAR(255)"] [:origin "VARCHAR(64)"] [:origin-ref "TEXT"]
           [:delta "TEXT" :edn]]
    :indexes [["control_staged_actions_branch_idx" "branch, seq"]]}
   {:name :action-requests :table "control_action_requests" :scope :branch
    :key [:branch :actor :request-key]
    :cols [branch-col [:actor "VARCHAR(255) NOT NULL"] [:request-key "VARCHAR(255) NOT NULL"]
           [:response "TEXT" :edn] [:created-at ts-now]]}
   {:name :writeback-claims :table "control_writeback_claims" :scope :global
    :key [:source :target-table :match-value]
    :cols [[:source "VARCHAR(255) NOT NULL"] [:target-table "VARCHAR(255) NOT NULL"]
           [:match-value "VARCHAR(512) NOT NULL"] [:claimed-at ts-now :touch]]}
   {:name :outbox-dead :table "control_outbox_dead" :scope :branch :key [:branch :id]
    :cols [branch-col [:id "VARCHAR(64) NOT NULL"] [:state "VARCHAR(16) NOT NULL DEFAULT 'dead'"]
           [:entry "TEXT" :edn] [:error "TEXT"] [:attempts "INT NOT NULL DEFAULT 0"]
           [:next-at ts] [:updated-at ts-now :touch]]}
   {:name :git-pending :table "control_git_pending" :scope :global :key [:proposal-id]
    :cols [[:proposal-id "VARCHAR(64) NOT NULL"] [:source "VARCHAR(255) NOT NULL"]
           [:target "VARCHAR(255) NOT NULL"] [:merge-mode "VARCHAR(32)"] [:actor "VARCHAR(255)"]
           [:authority "TEXT" :edn] [:state "VARCHAR(16) NOT NULL DEFAULT 'retry'"]
           [:attempts "INT NOT NULL DEFAULT 0"] [:error "TEXT"] [:next-at ts]
           [:updated-at ts-now :touch]]}
   {:name :runs :table "control_runs" :scope :branch :key [:id] :order [[:started-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:kind "VARCHAR(32) NOT NULL" :keyword]
           [:actor "VARCHAR(255)"] [:inputs-hash "VARCHAR(64) NOT NULL DEFAULT ''"]
           [:inputs "TEXT" :edn] [:started-at ts-now] [:finished-at ts]
           [:status "VARCHAR(16) NOT NULL DEFAULT 'running'"] [:objects-count "INT"]
           [:links-count "INT"] [:summary "TEXT" :edn] [:snapshot-id "VARCHAR(64)"]
           [:parent-run-id "VARCHAR(36)"] [:working-branch "VARCHAR(255)"] [:error-message "TEXT"]
           [:retry-count "INT"] [:retry-backoff-ms "BIGINT"] [:abort-on-failure "BOOLEAN"]
           [:owner "VARCHAR(128)"] [:lease-until ts] [:targets "TEXT" :edn]
           [:progress "TEXT" :edn] [:cancel-requested "VARCHAR(32)" :keyword]]
    :indexes [["control_runs_branch_started_idx" "branch, started_at DESC"]
              ["control_runs_branch_kind_idx" "branch, kind"]]}
   {:name :import-keys :table "control_import_keys" :scope :branch
    :key [:branch :object-type :pk-value]
    :cols [branch-col [:object-type "VARCHAR(255) NOT NULL"] [:pk-value "VARCHAR(512) NOT NULL"]
           [:uuid "VARCHAR(36) NOT NULL"] [:imported-at ts-now :touch]]
    :indexes [["control_import_keys_uuid_idx" "uuid"]]}
   {:name :transform-versions :table "control_transform_versions" :scope :branch
    :key [:branch :transform]
    :cols [branch-col [:transform "VARCHAR(255) NOT NULL"] [:semantic-version "INT NOT NULL"]
           [:reset-at ts-now :touch]]}
   {:name :link-backings :table "control_link_backings" :scope :branch :key [:branch :link-type]
    :cols [branch-col [:link-type "VARCHAR(255) NOT NULL"] [:fingerprint "VARCHAR(64) NOT NULL"]
           [:registered-at ts-now :touch]]}
   {:name :index-times :table "control_index_times" :scope :branch :key [:branch :object-type]
    :cols [branch-col [:object-type "VARCHAR(255) NOT NULL"] [:run-id "VARCHAR(36)"]
           [:indexed-at "TIMESTAMP(6) NOT NULL"]]}
   {:name :build-schedules :table "control_build_schedules" :scope :branch :key [:id :branch]
    :order [[:created-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:name "VARCHAR(255) NOT NULL"] [:target "TEXT"]
           [:cron "VARCHAR(128)"] [:every-ms "BIGINT"] [:trigger "TEXT" :edn]
           [:scope "VARCHAR(32)" :keyword] [:build-from "TEXT"]
           [:paused "BOOLEAN NOT NULL DEFAULT FALSE" :bool] [:last-fired-at ts]
           [:full "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:retry-count "INT NOT NULL DEFAULT 0"] [:retry-backoff-ms "BIGINT NOT NULL DEFAULT 0"]
           [:abort-on-failure "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:parameters "TEXT" :edn] [:run-setup-key "VARCHAR(255)" :keyword]
           [:last-run-id "VARCHAR(36)"] [:created-at ts-now] [:updated-at ts-now :touch]]
    :indexes [["control_build_schedules_branch_idx" "branch, created_at"]]}
   {:name :schedule-run-setups :table "control_schedule_run_setups" :scope :branch
    :key [:branch :schedule-id :key-value] :order [[:updated-at :desc]]
    :cols [branch-col [:schedule-id "VARCHAR(36) NOT NULL"] [:key-value "VARCHAR(255) NOT NULL"]
           [:parameters "TEXT" :edn] [:last-run-id "VARCHAR(36)"]
           [:created-at ts-now] [:updated-at ts-now :touch]]}
   {:name :leases :sql-only? true :table "control_leases" :scope :global :key [:name]
    :cols [[:name "VARCHAR(64) NOT NULL"] [:holder "VARCHAR(128)"]
           [:epoch "BIGINT NOT NULL DEFAULT 0"] [:since ts]]}
   {:name :llm-budget :table "control_llm_budget" :scope :global :key [:day]
    :cols [[:day "VARCHAR(10) NOT NULL"] [:reserved "TEXT" :edn] [:spent "DOUBLE PRECISION"]
           [:spent-by "TEXT" :edn] [:spent-by-source "TEXT" :edn]
           [:updated-at ts-now :touch]]}
   {:name :build-slots :table "control_build_slots" :scope :global :key [:name]
    :cols [[:name "VARCHAR(64) NOT NULL"] [:running "TEXT" :edn] [:updated-at ts-now :touch]]}
   {:name :build-claims :table "control_build_claims" :scope :global :key [:run-id]
    :order [[:queued-at :asc]] :branch-verbatim? true
    :cols [[:run-id "VARCHAR(36) NOT NULL"] branch-col-null
           [:status "VARCHAR(16) NOT NULL DEFAULT 'queued'"]
           [:reads "TEXT" :edn] [:writes "TEXT" :edn] [:origin "VARCHAR(255)"]
           [:targets "TEXT" :edn] [:owner "VARCHAR(128)"] [:lease-until ts]
           [:queued-at ts-now] [:admitted-at ts]]}
   {:name :build-requests :table "control_build_requests" :scope :global :key [:run-id]
    :order [[:created-at :asc]] :branch-verbatim? true
    :cols [[:run-id "VARCHAR(36) NOT NULL"] branch-col-null
           [:status "VARCHAR(16) NOT NULL DEFAULT 'queued'"]
           [:preset "TEXT" :edn] [:context "TEXT" :edn] [:origin "TEXT"]
           [:owner "VARCHAR(128)"]
           [:claim-id "VARCHAR(36)"] [:claimed-until ts]
           [:result "TEXT" :edn] [:error "TEXT"] [:error-data "TEXT" :edn]
           [:created-at ts-now] [:updated-at ts-now :touch]]
    :enums [["control_build_requests_status_chk" :status
             #{"queued" "running" "done" "failed" "cancelled"}]]
    :indexes [["control_build_requests_status_idx" "status, created_at"]]}
   {:name :automations :table "control_automations" :scope :branch :key [:id :branch]
    :order [[:created-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:name "VARCHAR(255) NOT NULL"]
           [:on-type "VARCHAR(255) NOT NULL"] [:when-spec "TEXT"] [:effect-spec "TEXT NOT NULL"]
           [:every-ms "BIGINT"] [:paused "BOOLEAN NOT NULL DEFAULT FALSE" :bool] [:last-fired-at ts]
           [:last-matched "BIGINT"] [:created-at ts-now] [:updated-at ts-now :touch]
           [:owner "VARCHAR(255)"] [:live "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:auto-mute "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:muted "BOOLEAN NOT NULL DEFAULT FALSE" :bool] [:muted-by "VARCHAR(255)"]
           [:muted-at ts]]
    :indexes [["control_automations_branch_idx" "branch, created_at"]]}
   {:name :automation-subscriptions :table "control_automation_subscriptions" :scope :branch
    :key [:automation-id :branch :username]
    :cols [[:automation-id "VARCHAR(36) NOT NULL"] branch-col [:username "VARCHAR(255) NOT NULL"]
           [:created-at ts-now]]}
   {:name :notifications :table "control_notifications" :scope :global :key [:id]
    :order [[:created-at :desc]] :branch-verbatim? true
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col-null [:recipient "VARCHAR(255) NOT NULL"]
           [:heading "TEXT NOT NULL"] [:message "TEXT"]
           [:object-type "VARCHAR(255)"] [:object-id "VARCHAR(255)"]
           [:automation-id "VARCHAR(36)"] [:created-at ts-now] [:read-at ts]
           [:source "VARCHAR(32)"] [:link-url "TEXT"] [:link-label "VARCHAR(255)"]]
    :indexes [["control_notifications_recipient_idx" "recipient, created_at DESC"]]}
   {:name :notification-preferences :table "control_notification_preferences" :scope :global
    :key [:username]
    :cols [[:username "VARCHAR(255) NOT NULL"] [:settings "TEXT NOT NULL" :edn]
           [:updated-at ts-now :touch]]}
   {:name :health-watches :table "control_health_watches" :scope :branch
    :key [:check-id :branch :username]
    :cols [[:check-id "VARCHAR(255) NOT NULL"] branch-col [:username "VARCHAR(255) NOT NULL"]
           [:level "VARCHAR(16) NOT NULL DEFAULT 'all'"] [:created-at ts-now]]}
   {:name :health-snoozes :table "control_health_snoozes" :scope :branch :key [:check-id :branch]
    :cols [[:check-id "VARCHAR(255) NOT NULL"] branch-col [:until ts] [:reason "TEXT"]
           [:snoozed-by "VARCHAR(255)"] [:created-at ts-now]]}
   {:name :health-check-groups :table "control_health_check_groups" :scope :branch
    :key [:id :branch]
    :cols [[:id "VARCHAR(64) NOT NULL"] branch-col [:name "VARCHAR(255) NOT NULL"]
           [:description "TEXT"] [:check-ids "TEXT NOT NULL" :edn] [:created-by "VARCHAR(255)"]
           [:created-at ts-now] [:updated-at ts-now :touch]]}
   {:name :health-group-subscriptions :table "control_health_group_subscriptions"
    :scope :branch :key [:group-id :branch :username]
    :cols [[:group-id "VARCHAR(64) NOT NULL"] branch-col [:username "VARCHAR(255) NOT NULL"]
           [:every-ms "BIGINT NOT NULL"] [:only-failing "BOOLEAN NOT NULL DEFAULT TRUE" :bool]
           [:last-sent-at ts] [:created-at ts-now]]}
   {:name :package-installs :table "control_package_installs" :scope :branch :key [:package :branch]
    :fallback :trunk
    :cols [[:package "VARCHAR(64) NOT NULL"] branch-col [:version "VARCHAR(32) NOT NULL"]
           [:release "TEXT NOT NULL" :edn] [:installed-by "VARCHAR(255)"]
           [:installed-at ts-now :touch]]}
   {:name :products :table "control_products" :scope :global :key [:product :version]
    :cols [[:product "VARCHAR(128) NOT NULL"] [:version "VARCHAR(32) NOT NULL"]
           [:channel "VARCHAR(16) NOT NULL" :keyword] [:state "VARCHAR(16) NOT NULL" :keyword]
           [:description "TEXT"] [:bundle "TEXT NOT NULL" :edn] [:digest "CHAR(64) NOT NULL"]
           [:requires "TEXT" :edn] [:origin "VARCHAR(255)"] [:published-by "VARCHAR(255)"]
           [:published-at ts-now] [:updated-at ts-now :touch]]}
   {:name :product-installs :table "control_product_installs" :scope :global
    :key [:install-id]
    :cols [[:install-id "VARCHAR(36) NOT NULL"] [:product "VARCHAR(128) NOT NULL"]
           [:version "VARCHAR(32) NOT NULL"] branch-col [:projects "TEXT" :edn]
           [:prefix "VARCHAR(64)"] [:suffix "VARCHAR(64)"] [:bindings "TEXT" :edn]
           [:release "TEXT" :edn] [:locked "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:channel "VARCHAR(16) NOT NULL DEFAULT 'stable'" :keyword]
           [:auto-upgrade "BOOLEAN NOT NULL DEFAULT FALSE" :bool] [:window "TEXT" :edn]
           [:linked "TEXT" :edn] [:last-error "TEXT"] [:installed-by "VARCHAR(255)"]
           [:state "VARCHAR(16) NOT NULL DEFAULT 'installed'" :keyword]
           [:documents "TEXT" :edn]
           [:installed-at ts-now] [:updated-at ts-now :touch]]}
   {:name :peer-connections :table "control_peer_connections" :scope :global
    :key [:connection-id]
    :cols [[:connection-id "VARCHAR(36) NOT NULL"] [:name "VARCHAR(128) NOT NULL"]
           [:role "VARCHAR(16) NOT NULL" :keyword] [:status "VARCHAR(16) NOT NULL" :keyword]
           [:trust "VARCHAR(64)"] [:remote-fingerprint "VARCHAR(64)"] [:space "VARCHAR(255)"]
           [:remote-space "VARCHAR(255)"] [:data-types "TEXT NOT NULL" :edn]
           [:ceiling "TEXT" :edn] [:pending-ceiling "TEXT" :edn]
           [:disabled-local "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:disabled-remote "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:ingress "TEXT" :edn] [:marking-map "TEXT" :edn] [:invite "TEXT"]
           [:profile-id "VARCHAR(36)"]
           [:created-by "VARCHAR(255)"] [:created-at ts-now] [:updated-at ts-now :touch]]}
   {:name :peer-mappings :table "control_peer_mappings" :scope :global
    :key [:connection-id :local-type]
    :cols [[:connection-id "VARCHAR(36) NOT NULL"] [:local-type "VARCHAR(255) NOT NULL"]
           [:remote-type "VARCHAR(255) NOT NULL"] [:properties "TEXT NOT NULL" :edn]
           [:links "TEXT" :edn]
           [:source-data "VARCHAR(8) NOT NULL DEFAULT 'none'" :keyword]
           [:action-data "VARCHAR(8) NOT NULL DEFAULT 'none'" :keyword]
           [:source-rev "TEXT" :edn] [:action-rev "TEXT" :edn]
           [:created-by "VARCHAR(255)"] [:created-at ts-now] [:updated-at ts-now :touch]]}
   {:name :peer-jobs :table "control_peer_jobs" :scope :global :key [:job-id]
    :order [[:at :desc]]
    :cols [[:job-id "VARCHAR(36) NOT NULL"] [:connection-id "VARCHAR(36) NOT NULL"]
           [:data-type "VARCHAR(16) NOT NULL" :keyword] [:way "VARCHAR(4) NOT NULL" :keyword]
           [:local-type "VARCHAR(255)"] [:state "VARCHAR(16) NOT NULL" :keyword]
           [:items "INTEGER NOT NULL DEFAULT 0"] [:held "INTEGER NOT NULL DEFAULT 0"]
           [:superseded "INTEGER NOT NULL DEFAULT 0"] [:error "TEXT"]
           [:latency-ms "BIGINT"] [:at ts-now]]
    :indexes [["control_peer_jobs_conn_idx" "connection_id, at"]]}
   {:name :peer-outbox :table "control_peer_outbox" :scope :global :key [:item-id]
    :order [[:seq :asc]]
    :cols [[:item-id "VARCHAR(36) NOT NULL"] [:connection-id "VARCHAR(36) NOT NULL"]
           [:seq "BIGINT NOT NULL"] [:local-type "VARCHAR(255) NOT NULL"]
           [:payload "TEXT NOT NULL" :edn] [:attempts "INTEGER NOT NULL DEFAULT 0"]
           [:next-at "BIGINT NOT NULL DEFAULT 0"] [:last-error "TEXT"] [:created-at ts-now]]}
   {:name :peer-crosswalk :table "control_peer_crosswalk" :scope :global
    :key [:connection-id :local-id]
    :cols [[:connection-id "VARCHAR(36) NOT NULL"] [:local-id "VARCHAR(255) NOT NULL"]
           [:remote-id "VARCHAR(255) NOT NULL"] [:local-type "VARCHAR(255) NOT NULL"]
           [:edit-at "BIGINT"] [:edit-from "VARCHAR(64)"] [:updated-at ts-now :touch]]
    :indexes [["control_peer_crosswalk_remote_idx" "connection_id, remote_id"]]}
   {:name :peer-shares :table "control_peer_shares" :scope :global
    :key [:connection-id :dataset]
    :cols [[:connection-id "VARCHAR(36) NOT NULL"] [:dataset "VARCHAR(255) NOT NULL"]
           [:created-by "VARCHAR(255)"] [:created-at ts-now]]}
   {:name :peer-media-mappings :table "control_peer_media_mappings" :scope :global
    :key [:connection-id :local-set]
    :cols [[:connection-id "VARCHAR(36) NOT NULL"] [:local-set "VARCHAR(255) NOT NULL"]
           [:remote-set "VARCHAR(255) NOT NULL"] [:direction "VARCHAR(8) NOT NULL" :keyword]
           [:sent "TEXT" :edn] [:created-by "VARCHAR(255)"] [:created-at ts-now]
           [:updated-at ts-now :touch]]}
   {:name :peer-profiles :table "control_peer_profiles" :scope :global :key [:profile-id]
    :order [[:name :asc]]
    :cols [[:profile-id "VARCHAR(36) NOT NULL"] [:pair "VARCHAR(255) NOT NULL"]
           [:name "VARCHAR(255) NOT NULL"] [:side "VARCHAR(8) NOT NULL" :keyword]
           [:space "VARCHAR(255)"] [:ceiling "TEXT" :edn] [:data-types "TEXT" :edn]
           [:types "TEXT" :edn] [:strategy "TEXT" :edn]
           [:auto-connect "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:created-by "VARCHAR(255)"] [:created-at ts-now] [:updated-at ts-now :touch]]}
   {:name :peer-mesh-nodes :table "control_peer_mesh_nodes" :scope :global :key [:trust]
    :cols [[:trust "VARCHAR(255) NOT NULL"] [:labels "TEXT" :edn] [:updated-at ts-now :touch]]}
   {:name :peer-offers :table "control_peer_offers" :scope :global :key [:connection-id]
    :cols [[:connection-id "VARCHAR(36) NOT NULL"] [:trust "VARCHAR(255) NOT NULL"]
           [:pair "VARCHAR(255) NOT NULL"] [:invite "TEXT NOT NULL"] [:error "TEXT"]
           [:at ts-now]]}
   {:name :peer-pending-links :table "control_peer_pending_links" :scope :global
    :key [:connection-id :link-type :from-id :to-id]
    :order [[:since :asc]]
    :cols [[:connection-id "VARCHAR(36) NOT NULL"] [:link-type "VARCHAR(160) NOT NULL"]
           [:from-id "VARCHAR(255) NOT NULL"] [:to-id "VARCHAR(255) NOT NULL"]
           [:since ts-now]]}
   {:name :import-plans :table "control_import_plans" :scope :branch :key [:branch] :fallback :trunk
    :cols [branch-col [:plan "TEXT NOT NULL" :edn] [:updated-at ts-now :touch]]}
   {:name :atlas-nodes :table "control_atlas_nodes" :scope :branch :key [:rid :branch]
    :order [[:position :asc] [:name :asc]]
    :cols [[:rid "VARCHAR(96) NOT NULL"] branch-col [:node-kind "VARCHAR(32) NOT NULL" :keyword]
           [:parent-rid "VARCHAR(96)"] [:name "VARCHAR(255) NOT NULL"] [:ref "VARCHAR(160)"]
           [:position "INT NOT NULL DEFAULT 0"] [:created-at ts-now] [:updated-at ts-now :touch]
           [:project "VARCHAR(255)"]]
    :owner {:project :project :own-if [:node-kind :folder] :parent [:parent-rid :rid]}
    :enums [["control_atlas_nodes_kind_chk" :node-kind #{:folder :placement}]]
    :unique [["control_atlas_nodes_ref_uniq" :branch :ref]]
    :indexes [["control_atlas_nodes_parent_idx" "branch, parent_rid"]]}
   {:name :atlas-annotations :table "control_atlas_annotations" :scope :branch
    :key [:ref :branch]
    :order [[:ref :asc]]
    :cols [[:ref "VARCHAR(160) NOT NULL"] branch-col [:tags "TEXT" :edn]
           [:trashed "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:curated "BOOLEAN NOT NULL DEFAULT FALSE" :bool] [:doc "TEXT"]
           [:pinned "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:promoted "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:updated-at ts-now :touch]]}
   {:name :portfolios :table "control_portfolios" :scope :global :key [:rid]
    :order [[:name :asc]]
    :cols [[:rid "VARCHAR(96) NOT NULL"] [:name "VARCHAR(255) NOT NULL"] [:description "TEXT"]
           [:projects "TEXT" :edn] [:created-by "VARCHAR(255)"] [:created-at ts-now]
           [:updated-at ts-now :touch]]}
   {:name :collections :table "control_collections" :scope :global :key [:rid]
    :order [[:name :asc]]
    :cols [[:rid "VARCHAR(96) NOT NULL"] [:name "VARCHAR(255) NOT NULL"] [:description "TEXT"]
           [:refs "TEXT" :edn] [:created-by "VARCHAR(255)"] [:created-at ts-now]
           [:updated-at ts-now :touch]]}
   {:name :files :table "control_files" :scope :global :key [:id]
    :order [[:name :asc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] [:name "VARCHAR(700) NOT NULL"] [:project "VARCHAR(255)"]
           [:address "VARCHAR(96) NOT NULL"] [:size "BIGINT NOT NULL DEFAULT 0"]
           [:mime "VARCHAR(255)"] [:author "VARCHAR(255)"] [:created-at ts-now]
           [:updated-at ts-now :touch]]}
   {:name :file-grants :table "control_file_grants" :scope :global :key [:ref :principal]
    :cols [[:ref "VARCHAR(160) NOT NULL"] [:principal "VARCHAR(255) NOT NULL"]
           [:role "VARCHAR(16) NOT NULL" :keyword] [:via "VARCHAR(16) NOT NULL" :keyword]
           [:granted-by "VARCHAR(255)"] [:project "VARCHAR(255)"] [:created-at ts-now]]}
   {:name :share-links :table "control_share_links" :scope :global :key [:token]
    :cols [[:token "VARCHAR(64) NOT NULL"] [:ref "VARCHAR(160) NOT NULL"]
           [:role "VARCHAR(16) NOT NULL" :keyword] [:created-by "VARCHAR(255)"]
           [:project "VARCHAR(255)"] [:created-at ts-now]]}
   {:name :project-settings :table "control_project_settings" :scope :global :key [:project]
    :cols [[:project "VARCHAR(255) NOT NULL"]
           [:file-grants "BOOLEAN NOT NULL DEFAULT FALSE" :bool] [:updated-at ts-now :touch]]}
   {:name :favorites :table "control_favorites" :scope :global :key [:principal :ref]
    :cols [[:principal "VARCHAR(255) NOT NULL"] [:ref "VARCHAR(160) NOT NULL"]
           [:created-at ts-now]]}
   {:name :entity-crosswalk :table "control_entity_crosswalk" :scope :branch
    :key [:branch :object-type :id-kind :id-value]
    :cols [branch-col [:object-type "VARCHAR(255) NOT NULL"] [:id-kind "VARCHAR(255) NOT NULL"]
           [:id-value "VARCHAR(512) NOT NULL"] [:entity-id "VARCHAR(36) NOT NULL"]
           [:observed-at ts-now :touch]]
    :indexes [["control_entity_crosswalk_entity_idx" "branch, entity_id"]]}
   {:name :entity-provenance :table "control_entity_provenance" :scope :branch :key [:seq]
    :cols [[:seq identity-seq] branch-col [:entity-id "VARCHAR(36) NOT NULL"]
           [:property "VARCHAR(255) NOT NULL" :keyword] [:value "TEXT" :edn]
           [:source "VARCHAR(255)" :keyword] [:run-id "VARCHAR(36)"] [:observed-at ts-now]]
    :indexes [["control_entity_provenance_entity_prop_idx" "branch, entity_id, property"]]}
   {:name :automation-runs :table "control_automation_runs" :scope :branch :key [:seq]
    :order [[:fired-at :desc]]
    :cols [[:seq identity-seq] branch-col [:automation-id "VARCHAR(36) NOT NULL"]
           [:matched "BIGINT"]
           [:applied "BIGINT"] [:affected-ids "TEXT" :edn]
           [:status "VARCHAR(16) NOT NULL DEFAULT 'ok'"]
           [:detail "TEXT"] [:fired-at ts-now] [:proposed "BIGINT"] [:decisions "TEXT" :edn]
           [:run-id "VARCHAR(36)"]]
    :indexes [["control_automation_runs_idx" "branch, automation_id, fired_at DESC"]]}
   {:name :automation-effects :table "control_automation_effects" :scope :branch
    :key [:run-id :object :version] :order [[:at :desc]]
    :cols [[:run-id "VARCHAR(36) NOT NULL"] [:object "VARCHAR(255) NOT NULL"] branch-col
           [:automation-id "VARCHAR(36) NOT NULL"] [:at ts-now]
           [:version "VARCHAR(32) NOT NULL DEFAULT ''"]]
    :indexes [["control_automation_effects_idx" "branch, automation_id, at DESC"]]}
   {:name :automation-executions :table "control_automation_executions" :scope :branch
    :key [:id :branch] :order [[:created-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:automation-id "VARCHAR(36) NOT NULL"]
           [:pending "TEXT" :edn] [:total "INT NOT NULL"] [:batches "INT NOT NULL DEFAULT 0"]
           [:batch-size "INT NOT NULL"] [:parallelism "INT"]
           [:status "VARCHAR(16) NOT NULL DEFAULT 'running'"]
           [:claim-id "VARCHAR(36)"] [:claimed-until ts]
           [:created-at ts-now] [:updated-at ts-now :touch]]
    :enums [["control_automation_executions_status_chk" :status
             #{"running" "done" "cancelled"}]]
    :indexes [["control_automation_executions_idx" "branch, status, created_at"]]}
   {:name :agent-proposals :table "control_agent_proposals" :scope :branch :key [:id :branch]
    :order [[:created-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:automation-id "VARCHAR(36) NOT NULL"]
           [:object-id "VARCHAR(255)"] [:action "VARCHAR(255) NOT NULL" :keyword]
           [:args "TEXT" :edn]
           [:args-digest "VARCHAR(64) NOT NULL"] [:reason "TEXT"] [:decision-log "TEXT" :edn]
           [:status "VARCHAR(16) NOT NULL DEFAULT 'open'"] [:error "TEXT"] [:edited "TEXT" :edn]
           [:created-at ts-now] [:decided-at ts] [:decided-by "VARCHAR(255)"]
           [:source-key "VARCHAR(512)"] [:provenance "TEXT" :edn] [:accepted-args "TEXT" :edn]
           [:versions "TEXT" :edn]]
    :enums [["control_agent_proposals_status_chk" :status #{"open" "applied" "rejected" "failed"}]]
    :unique [["control_agent_proposals_source_key_idx" :branch :source-key]]
    :indexes [["control_agent_proposals_idx" "branch, automation_id, status, created_at DESC"]]}
   {:name :container-images :table "control_container_images" :scope :global :key [:name :tag]
    :order [[:name :asc] [:tag :asc]]
    :cols [[:name "VARCHAR(200) NOT NULL"] [:tag "VARCHAR(128) NOT NULL"]
           [:project "VARCHAR(255) NOT NULL"] [:image-id "CHAR(64) NOT NULL"]
           [:config "TEXT NOT NULL" :edn] [:layers "TEXT NOT NULL" :edn]
           [:size "BIGINT NOT NULL DEFAULT 0"] [:pushed-by "VARCHAR(255)"]
           [:pushed-at ts-now]]
    :owner {:project :project}}
   {:name :agent-threads :table "control_agent_threads" :scope :global :key [:id]
    :order [[:updated-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] [:owner "VARCHAR(255) NOT NULL"]
           [:agent "VARCHAR(255) NOT NULL"]
           [:branch "VARCHAR(255) NOT NULL"] [:title "TEXT"]
           [:created-at ts-now] [:updated-at ts-now :touch]]
    :indexes [["control_agent_threads_owner_idx" "owner, updated_at DESC"]]}
   {:name :agent-messages :table "control_agent_messages" :scope :global :key [:thread-id :seq]
    :order [[:seq :asc]]
    :cols [[:thread-id "VARCHAR(36) NOT NULL"] [:seq "INT NOT NULL"] [:role "VARCHAR(16) NOT NULL"]
           [:status "VARCHAR(16) NOT NULL DEFAULT 'done'"] [:content "TEXT"] [:run-id "VARCHAR(36)"]
           [:detail "TEXT" :edn] [:created-at ts-now] [:updated-at ts-now :touch]]
    :enums [["control_agent_messages_role_chk" :role #{"user" "assistant"}]
            ["control_agent_messages_status_chk" :status #{"running" "done" "failed"}]]
    :indexes [["control_agent_messages_run_idx" "run_id"]]}
   {:name :agent-checkouts :table "control_agent_checkouts" :scope :branch
    :key [:owner :branch :place]
    :order [[:updated-at :desc]]
    :cols [[:owner "VARCHAR(255) NOT NULL"] branch-col [:place "VARCHAR(512) NOT NULL"]
           [:value "TEXT" :edn] [:created-at ts-now] [:updated-at ts-now :touch]]}
   {:name :logic-events :table "control_logic_events" :scope :branch
    :key [:branch :automation-id :object-id :fingerprint]
    :order [[:updated-at :desc]]
    :cols [branch-col [:automation-id "VARCHAR(36) NOT NULL"] [:object-id "VARCHAR(255) NOT NULL"]
           [:fingerprint "VARCHAR(64) NOT NULL"] [:event-id "VARCHAR(36) NOT NULL"]
           [:claim-id "VARCHAR(36)"]
           [:status "VARCHAR(16) NOT NULL"] [:attempts "INT NOT NULL DEFAULT 0"]
           [:error "TEXT"] [:transient "BOOLEAN NOT NULL DEFAULT FALSE" :bool] [:next-at ts]
           [:outcome "TEXT" :edn] [:fallback "TEXT" :edn] [:started-at ts] [:finished-at ts]
           [:created-at ts-now] [:updated-at ts-now :touch]]
    :enums [["control_logic_events_status_chk" :status #{"running" "done" "failed"}]]
    :indexes [["control_logic_events_idx" "branch, automation_id, status, updated_at DESC"]]}
   {:name :branch-governance :table "control_branch_governance" :scope :branch :key [:branch]
    :on-drop :keep
    :merge {:closed-at :coalesce-old :merged-at :coalesce-old :purged-at :coalesce-old}
    :cols [branch-col [:protected "SMALLINT"] [:review "TEXT"] [:retention-days "INT"]
           [:inactive-days "INT"] [:require-checks "SMALLINT"]
           [:merge-modes "VARCHAR(128)"] [:inactive-at ts]
           [:closed-at ts] [:merged-at ts] [:purged-at ts]
           [:updated-at ts-now :touch] [:updated-by "VARCHAR(255)"] [:auto-land "SMALLINT"]
           [:land-pushes "SMALLINT"] [:reactivated-at ts] [:space "VARCHAR(255)"]]
    :moved-cols [[:archived-at :closed-at]]
    :dropped-cols [:purge-days :min-approvals :allow-contributor-approval :required-reviewers]}
   {:name :retention-policies :table "control_retention_policies" :scope :global :key [:name]
    :cols [[:name "VARCHAR(255) NOT NULL"] [:datasets "TEXT NOT NULL" :edn]
           [:selectors "TEXT NOT NULL" :edn]
           [:allow-latest-view "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:updated-at ts-now :touch] [:updated-by "VARCHAR(255)"]]}
   {:name :snapshot-requests :table "control_snapshot_requests" :scope :branch
    :key [:branch :transform-rid]
    :cols [branch-col [:transform-rid "VARCHAR(255) NOT NULL"] [:dataset-rid "VARCHAR(255)"]
           [:requested-by "VARCHAR(255)"] [:message "TEXT"] [:requested-at ts-now]]}
   {:name :retention-expiries :table "control_retention_expiries" :scope :branch
    :key [:branch :table-name :version]
    :cols [branch-col [:table-name "VARCHAR(255) NOT NULL"] [:version "BIGINT NOT NULL"]
           [:rows "BIGINT NOT NULL"] [:expired-at ts-now]]}
   {:name :proposals :table "control_proposals" :scope :global :key [:id]
    :cols [[:id "VARCHAR(36) NOT NULL"] [:title "VARCHAR(255) NOT NULL"] [:description "TEXT"]
           [:source-branch "VARCHAR(255) NOT NULL"] [:target-branch "VARCHAR(255) NOT NULL"]
           [:author "VARCHAR(255)"] [:status "VARCHAR(16) NOT NULL DEFAULT 'open'"]
           [:created-at ts-now]
           [:resolved-at ts] [:resolved-by "VARCHAR(255)"] [:merge-commit "VARCHAR(64)"]
           [:merge-mode "VARCHAR(16)"] [:review-requested-at ts]
           [:review-requested-by "VARCHAR(255)"]
           [:reviewers "TEXT"] [:justification "TEXT"]]
    :indexes [["control_proposals_status_idx" "status"]
              ["control_proposals_target_idx" "target_branch"]]}
   {:name :proposal-verdicts :table "control_proposal_verdicts" :scope :global :key [:seq]
    :order [[:seq :asc]]
    :cols [[:seq identity-seq] [:proposal-id "VARCHAR(36) NOT NULL"]
           [:approver "VARCHAR(255) NOT NULL"]
           [:task "VARCHAR(255)"] [:verdict "VARCHAR(16) NOT NULL"] [:fingerprint "VARCHAR(64)"]
           [:source-rev "BIGINT"] [:note "TEXT"] [:at ts-now]]
    :indexes [["control_proposal_verdicts_proposal_idx" "proposal_id"]]}
   {:name :proposal-comments :table "control_proposal_comments" :scope :global :key [:id]
    :cols [[:id "VARCHAR(36) NOT NULL"] [:proposal-id "VARCHAR(36) NOT NULL"] [:task "VARCHAR(255)"]
           [:author "VARCHAR(255) NOT NULL"] [:body "TEXT NOT NULL"] [:created-at ts-now]]
    :indexes [["control_proposal_comments_proposal_idx" "proposal_id"]]}
   {:name :object-comments :table "control_object_comments" :scope :global :key [:id]
    :cols [[:id "VARCHAR(36) NOT NULL"] [:object-type "VARCHAR(255) NOT NULL"]
           [:object-id "VARCHAR(255) NOT NULL"] [:author "VARCHAR(255) NOT NULL"]
           [:body "TEXT NOT NULL"] [:attachments "TEXT"] [:created-at ts-now]]
    :indexes [["control_object_comments_object_idx" "object_type, object_id"]]}
   {:name :branch-roles :table "control_branch_roles" :scope :branch :key [:branch :principal]
    :cols [branch-col [:principal "VARCHAR(255) NOT NULL"]
           [:role "VARCHAR(32) NOT NULL DEFAULT 'owner'"]
           [:granted-at ts-now] [:granted-by "VARCHAR(255)"]]}
   {:name :access-requests :table "control_access_requests" :scope :global :key [:id]
    :cols [[:id "VARCHAR(36) NOT NULL"] [:kind "VARCHAR(64) NOT NULL"]
           [:status "VARCHAR(16) NOT NULL DEFAULT 'open'"]
           [:author "VARCHAR(255)"] [:payload "TEXT"] [:justification "TEXT"] [:created-at ts-now]
           [:resolved-at ts] [:resolved-by "VARCHAR(255)"] [:applied "TEXT"]]
    :indexes [["control_access_requests_status_idx" "status"]]}
   {:name :protections :table "control_protections" :scope :global :key [:resource]
    :cols [[:resource "VARCHAR(255) NOT NULL"] [:protected-by "VARCHAR(255)"]
           [:protected-at ts-now] [:released "SMALLINT"]]}
   {:name :proposal-merge-attempts :table "control_proposal_merge_attempts" :scope :global
    :key [:seq] :order [[:seq :asc]]
    :cols [[:seq identity-seq] [:proposal-id "VARCHAR(36) NOT NULL"]
           [:actor "VARCHAR(255)"] [:outcome "VARCHAR(16) NOT NULL"] [:error "TEXT"]
           [:at ts-now]]
    :indexes [["control_proposal_merge_attempts_proposal_idx" "proposal_id"]]}
   {:name :container-policies :table "control_container_policies" :scope :global :key [:container]
    :cols [[:container "VARCHAR(255) NOT NULL"] [:protect-all "SMALLINT"] [:review "TEXT"]
           [:default-project "VARCHAR(255)"]
           [:updated-by "VARCHAR(255)"] [:updated-at ts-now :touch]]}
   {:name :graph-layouts :table "control_graph_layouts" :scope :branch
    :key [:branch :graph-id :node-kind :node-key]
    :cols [branch-col [:graph-id "VARCHAR(128) NOT NULL"]
           [:node-kind "VARCHAR(32) NOT NULL" :keyword]
           [:node-key "VARCHAR(255) NOT NULL"] [:x "DECIMAL(38,10) NOT NULL"]
           [:y "DECIMAL(38,10) NOT NULL"]]
    :enums [["control_graph_layouts_node_kind_chk" :node-kind
             graph-node-kinds]]}
   {:name :color-schemes :table "control_color_schemes" :scope :branch :key [:id :branch]
    :conflict [:branch :graph-id :name] :merge {:id :keep} :order [[:name :asc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:graph-id "VARCHAR(128) NOT NULL"]
           [:name "VARCHAR(255) NOT NULL"] [:rules "TEXT NOT NULL" :edn]
           [:is-default "BOOLEAN NOT NULL DEFAULT FALSE" :bool] [:created-at ts-now]
           [:updated-at ts-now :touch]]
    :unique [["control_color_schemes_graph_name_uniq" :branch :graph-id :name]]}
   {:name :releases :table "control_releases" :scope :branch :key [:branch :section :name :version]
    :cols [branch-col [:section "VARCHAR(64) NOT NULL"] [:name "VARCHAR(255) NOT NULL"]
           [:version "VARCHAR(64) NOT NULL"] [:digest "VARCHAR(64) NOT NULL"] [:created-at ts-now]]}
   {:name :scenarios :table "control_scenarios" :scope :global :key [:branch]
    :cols [[:branch "VARCHAR(255) NOT NULL"] [:state "VARCHAR(16) NOT NULL"]
           [:updated-at ts-now :touch]]}
   {:name :graph-defaults :table "control_graph_defaults" :scope :branch :key [:branch :graph-id]
    :cols [branch-col [:graph-id "VARCHAR(128) NOT NULL"] [:default-scheme "VARCHAR(36) NOT NULL"]
           [:updated-at ts-now :touch]]}
   {:name :explorer-views :table "control_object_workspace_views" :scope :branch :key [:id :branch]
    :conflict [:branch :object-type :name] :merge {:id :keep}
    :order [[:object-type :asc] [:name :asc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:object-type "VARCHAR(255) NOT NULL" :keyword]
           [:view-type "VARCHAR(64) NOT NULL" :keyword] [:name "VARCHAR(255) NOT NULL"]
           [:config "TEXT NOT NULL" :edn] [:created-by "VARCHAR(255)"] [:created-at ts-now]
           [:updated-at ts-now :touch]]
    :unique [["control_obj_explorer_views_ot_name_uniq" :branch :object-type :name]]
    :indexes [["control_obj_explorer_views_ot_idx" "branch, object_type"]]}
   {:name :explorations :table "control_explorations" :scope :global :key [:id]
    :order [[:updated-at :desc]]
    :cols [[:id "VARCHAR(96) NOT NULL"] [:kind "VARCHAR(16) NOT NULL" :keyword]
           [:object-type "VARCHAR(255) NOT NULL" :keyword] [:name "VARCHAR(255) NOT NULL"]
           [:description "TEXT"] [:state "TEXT NOT NULL" :edn]
           [:definition "TEXT NOT NULL" :edn] [:project "VARCHAR(255)"]
           [:home "VARCHAR(255)"] [:created-by "VARCHAR(255)"] [:created-at ts-now]
           [:updated-at ts-now :touch]]}
   {:name :explorer-dashboards :table "control_object_workspace_dashboards" :scope :branch
    :key [:object-type :branch]
    :cols [[:object-type "VARCHAR(255) NOT NULL" :keyword] branch-col [:layout "TEXT NOT NULL" :edn]
           [:updated-at ts-now :touch]]}
   {:name :media-set-items :table "control_media_set_items" :scope :branch
    :key [:branch :media-set :path]
    :cols [branch-col [:media-set "VARCHAR(255) NOT NULL" :keyword]
           [:path "VARCHAR(700) NOT NULL"] [:address "VARCHAR(64) NOT NULL"]
           [:size "BIGINT NOT NULL DEFAULT 0"] [:mime "VARCHAR(255)"]
           [:filename "VARCHAR(700)"] [:landed-by "VARCHAR(255)"]
           [:source "VARCHAR(255)"] [:physical-path "VARCHAR(700)"]
           [:source-modified "VARCHAR(64)"]
           [:landed-at ts-now] [:updated-at ts-now :touch]]
    :indexes [["control_media_set_items_addr_idx" "branch, address"]
              ["control_media_set_items_any_addr_idx" "address"]
              ["control_media_set_items_set_idx" "branch, media_set, landed_at DESC"]]}
   {:name :projection-placements :table "control_projection_placements" :scope :branch
    :key [:projection :branch]
    :cols [[:projection "VARCHAR(255) NOT NULL" :keyword] branch-col [:memory "TEXT NOT NULL" :edn]
           [:updated-at ts-now :touch]]}
   {:name :projection-elements :table "control_projection_elements" :scope :branch
    :key [:projection :branch :element]
    :cols [[:projection "VARCHAR(255) NOT NULL" :keyword] branch-col [:element "TEXT NOT NULL"]
           [:node "TEXT NOT NULL"] [:path "TEXT NOT NULL"] [:updated-at ts-now :touch]]
    :indexes [["control_projection_elements_node_idx" "projection, branch, node"]
              ["control_projection_elements_path_idx" "projection, branch, path"]]}
   {:name :egress-policies :table "control_egress_policies" :scope :branch :key [:id :branch]
    :cols [[:id "VARCHAR(255) NOT NULL"] branch-col
           [:address "VARCHAR(255) NOT NULL"] [:port "VARCHAR(16) NOT NULL"]
           [:state "VARCHAR(16) NOT NULL" :keyword] [:paused "BOOLEAN NOT NULL DEFAULT FALSE"]
           [:justification "TEXT"] [:proposed-by "VARCHAR(255)"] [:decided-by "VARCHAR(255)"]
           [:decided-at "TIMESTAMPTZ"] [:agents "TEXT" :edn] [:updated-at ts-now :touch]]}
   {:name :documents :table "control_documents" :scope :branch :key [:id :branch]
    :order [[:updated-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:name "VARCHAR(700) NOT NULL"]
           [:version "BIGINT NOT NULL DEFAULT 0"]
           [:format "VARCHAR(16) NOT NULL DEFAULT 'markdown'" :keyword]
           [:body "TEXT NOT NULL DEFAULT ''"]
           [:template "VARCHAR(255)"] [:template-version "VARCHAR(64)"]
           [:parameters "TEXT" :edn] [:page "TEXT" :edn] [:links "TEXT" :edn]
           [:author "VARCHAR(255)"] [:created-at ts-now] [:updated-at ts-now :touch]
           [:project "VARCHAR(255)"]]
    :owner {:project :project}
    :indexes [["control_documents_branch_idx" "branch, updated_at DESC"]
              ["control_documents_template_idx" "branch, template"]]}
   {:name :widget-sets :table "control_widget_sets" :scope :global :key [:name]
    :order [[:name :asc]]
    :cols [[:name "VARCHAR(255) NOT NULL"] [:description "TEXT"] [:project "VARCHAR(255)"]
           [:author "VARCHAR(255)"] [:created-at ts-now] [:updated-at ts-now :touch]]}
   {:name :widget-set-releases :table "control_widget_set_releases" :scope :global
    :key [:set-name :version] :order [[:created-at :desc]]
    :cols [[:set-name "VARCHAR(255) NOT NULL"] [:version "VARCHAR(255) NOT NULL"]
           [:manifest "TEXT NOT NULL" :edn] [:files "TEXT NOT NULL" :edn] [:notes "TEXT"]
           [:author "VARCHAR(255)"] [:created-at ts-now] [:updated-at ts-now :touch]]}
   {:name :document-versions :table "control_document_versions" :scope :branch :key [:seq]
    :order [[:seq :desc]]
    :cols [[:seq identity-seq] branch-col [:document-id "VARCHAR(36) NOT NULL"]
           [:body "TEXT NOT NULL DEFAULT ''"] [:label "VARCHAR(255)"]
           [:endorsed "BOOLEAN NOT NULL DEFAULT FALSE" :bool]
           [:author "VARCHAR(255)"] [:created-at ts-now]]
    :indexes [["control_document_versions_doc_idx" "branch, document_id, seq DESC"]]}
   {:name :document-live :table "control_document_live" :scope :branch
    :key [:document-id :branch]
    :cols [[:document-id "VARCHAR(36) NOT NULL"] branch-col
           [:log "TEXT NOT NULL DEFAULT '[]'" :edn] [:updated-at ts-now :touch]]}
   {:name :health-checks :table "control_health_checks" :scope :branch :key [:id :branch]
    :order [[:created-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:name "VARCHAR(255) NOT NULL"]
           [:node-kind "VARCHAR(32) NOT NULL"] [:node-key "VARCHAR(255) NOT NULL"]
           [:kind "VARCHAR(32) NOT NULL DEFAULT 'row-count'"] [:query "TEXT"] [:expects "TEXT" :edn]
           [:enabled "BOOLEAN NOT NULL DEFAULT TRUE" :bool] [:created-at ts-now]
           [:updated-at ts-now :touch]]
    :indexes [["control_health_checks_node_idx" "branch, node_kind, node_key"]]}
   {:name :health-check-results :table "control_health_check_results" :scope :branch :key [:seq]
    :order [[:run-at :desc] [:seq :desc]]
    :cols [[:seq identity-seq] branch-col [:check-id "VARCHAR(36) NOT NULL"]
           [:status "VARCHAR(16) NOT NULL"]
           [:value "TEXT"] [:detail "TEXT"] [:run-at ts-now]]
    :indexes [["control_health_check_results_check_idx" "branch, check_id, run_at DESC"]]}
   {:name :linter-recommendations :table "control_linter_recommendations" :scope :branch
    :key [:rule :resource :branch]
    :cols [[:rule "VARCHAR(64) NOT NULL"] [:resource "VARCHAR(512) NOT NULL"] branch-col
           [:mode "VARCHAR(32) NOT NULL"] [:project "VARCHAR(255)"] [:detail "TEXT"]
           [:first-seen ts-now] [:last-seen ts-now] [:resolved-at ts]
           [:snoozed-until ts] [:snooze-reason "TEXT"] [:assignee "VARCHAR(255)"]]}
   {:name :eval-suites :table "control_eval_suites" :scope :branch :key [:id :branch]
    :order [[:created-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:name "VARCHAR(255) NOT NULL"]
           [:transform "VARCHAR(255) NOT NULL"] [:spec "TEXT NOT NULL" :edn]
           [:enabled "BOOLEAN NOT NULL DEFAULT TRUE" :bool] [:created-at ts-now]
           [:updated-at ts-now :touch]]
    :unique [["control_eval_suites_branch_name_uniq" :branch :name]]}
   {:name :eval-runs :table "control_eval_runs" :scope :branch :key [:id] :order [[:run-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:suite-id "VARCHAR(36) NOT NULL"]
           [:against-ref "VARCHAR(255)"]
           [:ref "VARCHAR(255) NOT NULL"] [:iterations "INT NOT NULL DEFAULT 1"]
           [:summary "TEXT" :edn]
           [:run-at ts-now]]
    :indexes [["control_eval_runs_suite_idx" "branch, suite_id, run_at DESC"]]}
   {:name :eval-results :table "control_eval_results" :scope :branch :key [:seq]
    :cols [[:seq identity-seq] branch-col [:run-id "VARCHAR(36) NOT NULL"]
           [:metric "VARCHAR(255) NOT NULL"]
           [:value "TEXT" :edn] [:variance "TEXT" :edn] [:status "VARCHAR(16) NOT NULL"]
           [:detail "TEXT" :edn]]
    :indexes [["control_eval_results_run_idx" "branch, run_id"]]}
   {:name :eval-jobs :table "control_eval_jobs" :scope :branch :key [:id]
    :order [[:started-at :desc]]
    :cols [[:id "VARCHAR(36) NOT NULL"] branch-col [:suite "VARCHAR(255)"]
           [:status "VARCHAR(16) NOT NULL"] [:run-id "VARCHAR(36)"] [:error "TEXT"]
           [:started-at ts-now] [:ended-at ts] [:owner "VARCHAR(128)"] [:lease-until ts]
           [:progress "TEXT" :edn] [:detail "TEXT" :edn]]}
   {:name :app-previews :table "control_app_previews" :scope :global :key [:id]
    :cols [[:id "VARCHAR(64) NOT NULL"] [:username "VARCHAR(255)"] [:live-name "VARCHAR(255)"]
           [:app "TEXT" :edn] [:created-at ts-now]]}
   {:name :eval-case-results :table "control_eval_case_results" :scope :branch :key [:seq]
    :order [[:seq :asc]]
    :cols [[:seq identity-seq] branch-col [:run-id "VARCHAR(36) NOT NULL"]
           [:case-id "VARCHAR(255) NOT NULL"] [:passed "BOOLEAN" :bool]
           [:input "TEXT" :edn] [:expected "TEXT" :edn] [:actual "TEXT" :edn]
           [:metrics "TEXT" :edn] [:error "TEXT"] [:trace "TEXT" :edn]
           [:intermediates "TEXT" :edn]]
    :indexes [["control_eval_case_results_run_idx" "branch, run_id"]]}
   {:name :begleitungen :table "control_begleitungen" :scope :global :key [:id]
    :order [[:begonnen :desc]]
    :cols [[:id "VARCHAR(64) NOT NULL"] [:benutzer "VARCHAR(255) NOT NULL"] [:aufgabe "TEXT"]
           [:begonnen ts-now] [:beendet ts] [:fazit "TEXT" :edn]
           [:erledigt ts] [:erledigt-von "VARCHAR(255)"]]
    :indexes [["control_begleitungen_benutzer_idx" "benutzer, begonnen"]]}
   {:name :begleitung-ereignisse :table "control_begleitung_ereignisse" :scope :global
    :key [:sitzung :id] :order [[:t :asc]]
    :cols [[:sitzung "VARCHAR(64) NOT NULL"] [:id "VARCHAR(64) NOT NULL"]
           [:art "VARCHAR(16) NOT NULL" :keyword] [:t "BIGINT NOT NULL"]
           [:ereignis "TEXT NOT NULL" :edn] [:empfangen ts-now]]}
   {:name :stream-cursors :table "control_stream_cursors" :scope :branch
    :key [:branch :stream :consumer]
    :cols [branch-col [:stream "VARCHAR(255) NOT NULL"] [:consumer "VARCHAR(32) NOT NULL"]
           [:offsets "TEXT" :edn] [:last-run-at ts] [:updated-at ts-now :touch]]
    :indexes [["control_stream_cursors_branch_idx" "branch, stream"]]}
   {:name :stream-dead-letter :table "control_stream_dead_letter" :scope :branch
    :key [:branch :stream :partition :offset] :order [[:at :desc]]
    :cols [branch-col [:stream "VARCHAR(255) NOT NULL"] [:partition "BIGINT NOT NULL"]
           [:offset "BIGINT NOT NULL"] [:object-type "VARCHAR(255)"] [:record "TEXT" :edn]
           [:reason "TEXT"] [:at ts-now]]
    :indexes [["control_stream_dead_letter_stream_idx" "branch, stream"]]}
   {:name :git-commits :table "control_git_commits" :scope :global :key [:repo-rid :commit-sha]
    :order [[:ordinal :asc]]
    :merge {:seq :coalesce-old :object-blob :coalesce-old :branch :keep :source :keep
            :tree-sha :keep
            :manifest-base :coalesce-old}
    :cols [[:repo-rid "VARCHAR(255) NOT NULL"] branch-col [:seq "BIGINT"]
           [:commit-sha "CHAR(40) NOT NULL"]
           [:source "VARCHAR(16) NOT NULL"] [:tree-sha "CHAR(40) NOT NULL"]
           [:object-blob "CHAR(64)"]
           [:manifest-base "CHAR(40)"]
           [:ordinal identity-seq] [:created-at ts-now]]
    :indexes [["control_git_commits_repo_branch_idx" "repo_rid, branch, created_at"]]}
   {:name :repo-anchor :table "control_repo_anchor" :scope :global :key [:repo-rid :branch]
    :order [[:repo-rid :asc] [:branch :asc]]
    :merge {:watermark :greatest :tip-sha :coalesce-new :tip-seq :coalesce-new}
    :cols [[:repo-rid "VARCHAR(255) NOT NULL"] branch-col [:watermark "BIGINT NOT NULL DEFAULT 0"]
           [:tip-sha "CHAR(40)"] [:tip-seq "BIGINT"] [:generation "VARCHAR(64)"]]}
   {:name :repo-files :table "control_repo_files" :scope :global :key [:repo-rid :commit-sha :path]
    :order [[:path :asc]]
    :cols [[:repo-rid "VARCHAR(255) NOT NULL"] [:commit-sha "CHAR(40) NOT NULL"]
           [:path "VARCHAR(1024) NOT NULL"]
           [:blob-sha "CHAR(64)"] [:mode "VARCHAR(8) NOT NULL DEFAULT '100644'"]
           [:plane "VARCHAR(8) NOT NULL"]]}
   {:name :repo-checks :table "control_repo_checks" :scope :global
    :key [:repo-rid :commit-sha :check-name]
    :order [[:check-name :asc]] :merge {:started-at :coalesce-old :finished-at :coalesce-new}
    :cols [[:repo-rid "VARCHAR(255) NOT NULL"] [:commit-sha "CHAR(40) NOT NULL"]
           [:check-name "VARCHAR(64) NOT NULL"]
           [:status "VARCHAR(16) NOT NULL"] [:detail "TEXT"] [:started-at ts] [:finished-at ts]]}
   {:name :principals :lane :auth :table "principals" :scope :global :key [:username]
    :cols [[:username "VARCHAR(255) NOT NULL"] [:ontology "VARCHAR(255) NOT NULL DEFAULT ''"]
           [:branch "VARCHAR(255) NOT NULL DEFAULT 'main'"] [:user-object-id "VARCHAR(36) NOT NULL"]
           [:pw-hash "VARCHAR(255) NULL"] [:auth-source "VARCHAR(32) NOT NULL DEFAULT 'password'"]
           [:external-subject "VARCHAR(255) NULL"] [:created-at ts-now]
           [:cred-gen "BIGINT NOT NULL DEFAULT 0"]
           [:mfa-seed "VARCHAR(255) NULL"] [:mfa-pending "VARCHAR(255) NULL"]
           [:mfa-last-step "BIGINT NULL"]
           [:pw-must-change "BOOLEAN NOT NULL DEFAULT FALSE" :bool] [:pw-set-at ts]]
    :unique [["uk_external" :auth-source :external-subject]]}
   {:name :sessions :lane :auth :table "sessions" :scope :global :key [:token]
    :cols [[:token "VARCHAR(64) NOT NULL"] [:username "VARCHAR(255) NOT NULL"]
           [:user-id "VARCHAR(36)"] branch-col-null [:scope "TEXT"]
           [:created-at ts-now] [:expires-at "TIMESTAMP(6) NOT NULL"]
           [:cred-gen "BIGINT"] [:client-id "VARCHAR(64)"] [:secret-gen "BIGINT"]
           [:provider "VARCHAR(64)"] [:user-agent "VARCHAR(512)"] [:ip "VARCHAR(64)"]
           [:last-seen-at ts] [:sid "VARCHAR(24)"]]
    :indexes [["sessions_username_idx" "username"] ["sessions_sid_idx" "sid"]]}
   {:name :login-log :lane :auth :table "login_log" :scope :global :key [:id] :order [[:at :desc]]
    :cols [[:id "VARCHAR(64) NOT NULL"] [:at ts-now] [:username "VARCHAR(255)"]
           [:provider "VARCHAR(64) NOT NULL"] [:outcome "VARCHAR(16) NOT NULL"]
           [:subject "VARCHAR(255)"] [:detail "VARCHAR(1024)"]]}
   {:name :oauth-clients :lane :auth :table "oauth_clients" :scope :global :key [:client-id]
    :order [[:created-at :desc]]
    :cols [[:client-id "VARCHAR(64) NOT NULL"] [:app "VARCHAR(255) NOT NULL"]
           [:app-name "VARCHAR(255) NOT NULL"] [:secret-hash "VARCHAR(255)"]
           [:grant-types "TEXT" :edn] [:redirect-uris "TEXT" :edn]
           [:service-user "VARCHAR(255)"] [:created-by "VARCHAR(255)"]
           [:created-at ts-now] [:revoked-at ts] [:secret-gen "BIGINT NOT NULL DEFAULT 0"]]
    :indexes [["idx_oauth_app" "app"]]}
   {:name :oauth-codes :lane :auth :table "oauth_codes" :scope :global :key [:code]
    :cols [[:code "VARCHAR(64) NOT NULL"] [:client-id "VARCHAR(64) NOT NULL"]
           [:user-id "VARCHAR(36)"] [:username "VARCHAR(255)"]
           [:redirect-uri "VARCHAR(1024)"] [:scope "TEXT"]
           [:created-at ts-now] [:expires-at "TIMESTAMP(6) NOT NULL"]
           [:code-challenge "VARCHAR(128)"] [:nonce "VARCHAR(512)"] [:auth-time "BIGINT"]]}
   {:name :custom-app-metrics :lane :auth :table "custom_app_metrics" :scope :global
    :key [:app :day :endpoint] :order [[:day :desc]]
    :cols [[:app "VARCHAR(255) NOT NULL"] [:day "VARCHAR(10) NOT NULL"]
           [:endpoint "VARCHAR(191) NOT NULL"] [:calls "BIGINT NOT NULL DEFAULT 0"]
           [:errors "BIGINT NOT NULL DEFAULT 0"] [:ms-total "BIGINT NOT NULL DEFAULT 0"]
           [:last-at ts-now :touch]]}
   {:name :instance-config :table "instance_config" :scope :global :key [:k]
    :cols [[:k "VARCHAR(191) NOT NULL"] [:v "TEXT"] [:updated-at ts-now :touch]]}
   {:name :object-sets-temp :table "object_sets_temp" :scope :global :key [:rid]
    :order [[:created-at :desc]]
    :cols [[:rid "VARCHAR(128) NOT NULL"] [:created-by "VARCHAR(64) NOT NULL"] branch-col-null
           [:kind "VARCHAR(16) NOT NULL" :keyword] [:definition "TEXT NOT NULL" :edn]
           [:created-at ts-now] [:expires-at "TIMESTAMP(6) NOT NULL"]]}
   {:name :repo-tags :table "control_repo_tags" :scope :global :key [:repo-rid :tag]
    :order [[:tag :asc]]
    :cols [[:repo-rid "VARCHAR(255) NOT NULL"] [:tag "VARCHAR(255) NOT NULL"]
           [:commit-sha "CHAR(40) NOT NULL"]
           [:actor "VARCHAR(255)"] [:created-at ts-now]]}])

(defn- ranged? [order-cols [c t]]
  (or (contains? order-cols c) (boolean (re-find #"^(TIMESTAMP|SMALLINT|INT|BIGINT|DECIMAL)" t))))

(defn- default-value
  [[_ ty & flags]]
  (when-let [[_ d] (re-find #"(?i)\bDEFAULT\s+('(?:[^']|'')*'|-?\d+|TRUE|FALSE|CURRENT_TIMESTAMP)"
                            ty)]
    (let [v (cond (str/starts-with? d
                                    "'")
                  (str/replace (subs d 1 (dec (count d))) "''" "'")
                  (= "CURRENT_TIMESTAMP" (str/upper-case d)) records/now
                  (#{"TRUE" "FALSE"} (str/upper-case d)) (= "TRUE" (str/upper-case d))
                  :else                                 (parse-long d))]
      (if (and (some #{:keyword} flags) (string? v)) (keyword v) v))))

(defn- keyword-cols [cols]
  (into #{} (keep (fn [[c _ & flags]] (when (some #{:keyword} flags) c))) cols))

(defn- value-schema
  [{:keys [enums cols]}]
  (let [enum (into {} (for [[_ c vs] enums] [c (into [:enum] (sort-by str vs))]))
        ks   (keyword-cols cols)
        ents (merge (zipmap ks (repeat :keyword)) enum)]
    (if (seq ents)
      (into [:map] (for [[c s] (sort-by (comp str key) ents)] [c {:optional true} [:maybe s]]))
      :map)))

(defn- collection [{:keys [name scope key cols order] :as t}]
  (let [unique     (mapv (comp vec next) (:unique t))
        defaults   (into {} (keep (fn [[c :as col]] (some->> (default-value col) (vector c)))) cols)
        order-cols (into #{} (map first) order)
        auto       (some (fn [[c ty]] (when (and (= [c] key) (re-find #"IDENTITY" ty)) c)) cols)
        touch      (into [] (keep (fn [[c _ & flags]] (when (some #{:touch} flags) c))) cols)]
    (cond-> (merge {:coll     (keyword "ctl" (clojure.core/name name))
                    :scope    :global :key any? :value (value-schema t)
                    :key-cols key
                    :index    (mapv (fn [[c :as col]] (if (ranged? order-cols col) [c :range] c))
                                    cols)}
                   (select-keys t [:order :fallback :merge :conflict :lane]))
      (= :branch scope) (assoc :branch? true)
      auto              (assoc :auto auto)
      (seq touch)       (assoc :touch touch)
      (seq defaults)    (assoc :defaults defaults)
      (seq unique)      (assoc :unique unique)
      (seq (keyword-cols cols)) (assoc :keywords (keyword-cols cols))
      (and (= :branch scope) (not (#{:retire :keep} (:on-drop t))))
      (assoc :drops-with :branch)
      (= :keep-for-staging (:on-drop t)) (assoc :kept-under "staging/"))))

(def all (into [] (comp (remove :sql-only?) (map collection)) tables))

(def renamed-tables
  [
   {:from "control_notifications" :to "control_notifications_outbox_retired"
    :if-column "dispatched_at"
    :renames [["control_notifications_pkey" "control_notifications_outbox_retired_pkey"]
              ["control_notifications_pending_idx"
               "control_notifications_outbox_retired_pending_idx"]]}])

(def ledger-txns-rid        "ri.sutra.main.dataset.b09e4c10-98c8-3452-a011-b640f3fa6790")
(def ledger-consumption-rid "ri.sutra.main.dataset.ad94deca-83fc-3d6b-bb24-b03e85929ddc")
(def ledger-jobs-rid        "ri.sutra.main.dataset.7b3c9d1e-4f52-3a8b-9c1d-2e3f4a5b6c7d")

(def ledger
  [{:coll :ctl/ledger-txns :scope :global :key any? :value :map :drops-with :branch
    :key-cols [:internal-uuid] :dataset ledger-txns-rid
    :index [:txn-rid :dataset-rid :branch :run-id :job-rid :snapshot-id :type :status
            [:started-at :range] [:ended-at :range]]
    :order [[:started-at :desc]]}
   {:coll :ctl/ledger-consumption :scope :global :key any? :value :map :drops-with :branch
    :key-cols [:internal-uuid] :dataset ledger-consumption-rid
    :index [:consumer-rid :input-dataset-rid :branch [:updated-at :range]]
    :order [[:updated-at :desc]]}
   {:coll :ctl/ledger-jobs :scope :global :key any? :value :map :drops-with :branch
    :key-cols [:internal-uuid] :dataset ledger-jobs-rid
    :index [:job-rid :run-id :transform-rid :branch :status :fingerprint :pending-observed
            [:started-at :range] [:ended-at :range]]
    :order [[:started-at :desc]]}])

(def by-coll (coll/index-by :coll (concat all ledger)))

(def owners
  (into {} (keep (fn [{:keys [name owner]}]
                   (when owner [(keyword "ctl" (clojure.core/name name)) owner])))
        tables))
