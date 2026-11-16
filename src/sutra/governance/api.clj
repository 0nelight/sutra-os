(ns sutra.governance.api
  (:require [sutra.governance.branch-state :as branch-state]
            [sutra.governance.branches :as branches]
            [sutra.governance.guards :as guards]
            [sutra.governance.merge :as merge]
            [sutra.governance.merge-plan :as merge-plan]
            [sutra.governance.proposals :as proposals]
            [sutra.governance.protection :as protection]
            [sutra.governance.requests :as requests]
            [sutra.governance.roles :as roles]))

(set! *warn-on-reflection* true)

(defn close! [store branch] (branch-state/close! store branch))

(defn delete-branch!
  [store branch drop-branch-fn] (branch-state/delete-branch! store branch drop-branch-fn))

(defn ended-branches [store] (branch-state/ended-branches store))

(defn contributors [store branch] (branch-state/contributors store branch))

(defn branch-states
  ([store] (branch-state/list-branches store))
  ([store opts] (branch-state/list-branches store opts)))

(defn retention-sweep! [store opts] (branch-state/retention-sweep! store opts))

(defn set-inactive! [store branch on?] (branch-state/set-inactive! store branch on?))

(defn branch-exists? [store name] (branches/branch-exists? store name))

(defn create-branch! [store opts] (branches/create-branch! store opts))

(defn current-commit [store] (branches/current-commit store))

(defn drop-branch! [store name] (branches/drop-branch! store name))

(defn branch-parents [store] (branches/list-branches store))

(defn data-guard [store] (guards/data-guard store))

(defn data-guarded [store] (guards/data-guarded store))

(defn guarded? [store] (guards/guarded? store))

(defn write-guard
  ([store seen branch staged] (guards/write-guard store seen branch staged))
  ([store seen branch staged opts] (guards/write-guard store seen branch staged opts)))

(defn after-commit-reads [store] (merge/after-commit-reads store))

(defn branch-merge-preview [row check opts] (merge/branch-merge-preview row check opts))

(defn commit-preview [store opts] (merge/commit-preview store opts))

(defn preview-branch-schema-merge
  ([store source-name target-name]
   (merge/preview-branch-schema-merge store source-name target-name))
  ([store source-name target-name opts]
   (merge/preview-branch-schema-merge store source-name target-name opts)))

(defn preview-of
  ([check] (merge/preview-of check))
  ([check opts] (merge/preview-of check opts)))

(defn preview-tasks [preview] (merge/preview-tasks preview))

(defn proposal-tasks
  ([store source target] (merge/proposal-tasks store source target))
  ([store source target opts] (merge/proposal-tasks store source target opts)))

(defn rebase-schema [merged] (merge/rebase-schema merged))

(defn reconcile-after-commit!
  ([store callbacks base] (merge/reconcile-after-commit! store callbacks base))
  ([store callbacks base seen] (merge/reconcile-after-commit! store callbacks base seen)))

(defn resolver [resolutions] (merge/resolver resolutions))

(defn staging-merge-check
  ([store parent staging] (merge/staging-merge-check store parent staging))
  ([store parent staging fork-base] (merge/staging-merge-check store parent staging fork-base)))

(defn touched-rows [before after] (merge/touched-rows before after))

(defn heads-merge [heads resolutions] (merge/heads-merge heads resolutions))

(defn judged [merge] (merge/judged merge))

(defn authored-target [check] (merge/authored-target check))

(defn plan [opts] (merge-plan/plan opts))

(defn refuse-invalid! [opts] (merge-plan/refuse-invalid! opts))

(defn refuse-unclean! [opts parent staging] (merge-plan/refuse-unclean! opts parent staging))

(defn add-comment! [store id opts] (proposals/add-comment! store id opts))

(defn approve!
  ([store id] (proposals/approve! store id))
  ([store id note] (proposals/approve! store id note))
  ([store id note opts] (proposals/approve! store id note opts)))

(defn ask-review!
  ([store id] (proposals/ask-review! store id))
  ([store id reviewers] (proposals/ask-review! store id reviewers))
  ([store id reviewers justification] (proposals/ask-review! store id reviewers justification)))

(defn close-proposal! [store id] (proposals/close-proposal! store id))

(defn comments-of [store id] (proposals/comments-of store id))

(defn delete-comment!
  [store id comment-id admin?] (proposals/delete-comment! store id comment-id admin?))

(defn fetch [store id] (proposals/fetch store id))

(defn landing-records
  [store id p merged-source?] (proposals/landing-records store id p merged-source?))

(defn list-proposals [store opts] (proposals/list-proposals store opts))

(defn merge-committed! [store id rev] (proposals/merge-committed! store id rev))

(defn open-proposal! [store opts] (proposals/open-proposal! store opts))

(defn open-proposal-of [store source target] (proposals/open-proposal-of store source target))

(defn proposal-refusal [opts] (proposals/proposal-refusal opts))

(defn raise-refused! [r ctx fallback] (proposals/raise-refused! r ctx fallback))

(defn reject!
  ([store id] (proposals/reject! store id))
  ([store id note] (proposals/reject! store id note))
  ([store id note opts] (proposals/reject! store id note opts)))

(defn settle-git-half! [store id fp] (proposals/settle-git-half! store id fp))

(defn due-git-halves [store] (proposals/due-git-halves store))

(defn governance [store] (protection/governance store))

(defn policy [store branch] (protection/policy store branch))

(defn protected? [store branch] (protection/protected? store branch))

(defn changeset-offen? [store branch] (protection/changeset-offen? store branch))

(defn auto-land? [store branch] (protection/auto-land? store branch))

(defn set-policy! [store branch opts] (protection/set-policy! store branch opts))

(defn create-request!
  [store fields justification] (requests/create-request! store fields justification))

(defn fetch-request [store id] (requests/fetch-request store id))

(defn judge-request!
  [store id verdict note owns?] (requests/judge-request! store id verdict note owns?))

(defn list-requests [store opts] (requests/list-requests store opts))

(defn resolve-request! [store id to applied] (requests/resolve-request! store id to applied))

(defn assign-role!
  ([store branch principal] (roles/assign-role! store branch principal))
  ([store branch principal role] (roles/assign-role! store branch principal role)))

(defn branch-roles [store branch] (roles/branch-roles store branch))

(defn merge-attempted! [store id error] (proposals/merge-attempted! store id error))

(defn merge-attempts [store id] (proposals/merge-attempts store id))

(defn owner-block [store branch act admin?] (roles/owner-block store branch act admin?))

(defn owners [store branch] (roles/owners store branch))

(defn revoke-role! [store branch principal] (roles/revoke-role! store branch principal))
