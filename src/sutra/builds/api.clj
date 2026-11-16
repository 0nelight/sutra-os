(ns sutra.builds.api
  (:refer-clojure :exclude [get])
  (:require [sutra.builds.import :as import]
            [sutra.builds.import.changelog :as changelog]
            [sutra.builds.import.materialize :as materialize]
            [sutra.builds.import.sources :as import-sources]
            [sutra.builds.jobspecs :as jobspecs]
            [sutra.builds.ledger :as ledger]
            [sutra.builds.last-updated :as last-updated]
            [sutra.builds.ledger.marks :as marks]
            [sutra.builds.ledger.retention :as retention]
            [sutra.builds.plan :as plan]
            [sutra.builds.plan.jobspec :as jobspec]
            [sutra.builds.plan.preview :as transform-preview]
            [sutra.builds.plan.unit-tests :as unit-tests]
            [sutra.builds.plan.virtual :as virtual]
            [sutra.builds.run :as run]
            [sutra.builds.run.claim :as claim]
            [sutra.builds.run.lane :as lane]
            [sutra.builds.run.leader :as leader]
            [sutra.builds.run.lock :as lock]
            [sutra.builds.run.monitor :as monitor]
            [sutra.builds.run.queue :as queue]
            [sutra.builds.run.rows :as rows]))

(set! *warn-on-reflection* true)

(defn build! [store branch preset-block mode] (import/build! store branch preset-block mode))

(defn build-options [opts] (import/build-options opts))

(defn read-spec [store] (import-sources/read-spec store))

(defn object-removals
  ([store obj-type-kw t-name] (changelog/object-removals store obj-type-kw t-name))
  ([store obj-type-kw t-name opts] (changelog/object-removals store obj-type-kw t-name opts)))

(defn with-landing [store schema f] (materialize/with-landing store schema f))

(defn last-job-per-transform [store branch] (ledger/last-job-per-transform store branch))

(defn last-jobs-per-transform [store branch] (ledger/last-jobs-per-transform store branch))

(defn last-txn-per-dataset [store branch] (ledger/last-txn-per-dataset store branch))

(defn last-updated [store schema auth items] (last-updated/of-items store schema auth items))

(defn jobspecs [store] (jobspecs/resolved store))

(defn jobspec-id [schema t] (jobspec/id schema t))

(defn rollback-check [store ds ds-rid ds-name token]
  (jobspecs/rollback-check store ds ds-rid ds-name token))

(defn rewind-producer! [store rewind]
  (ledger/rewind-producer! store rewind))

(defn read-job-state [status] (ledger/read-job-state status))

(defn advance! [store branch owner from observed] (marks/advance! store branch owner from observed))

(defn get [store branch owner] (marks/get store branch owner))

(defn moved? [mark observed] (marks/moved? mark observed))

(defn observe! [store branch owner observed] (marks/observe! store branch owner observed))

(defn delete-policy! [store policy-name] (retention/delete-policy! store policy-name))

(defn plan [store schema opts] (retention/plan store schema opts))

(defn policies [store] (retention/policies store))

(defn retain! [store schema opts] (retention/retain! store schema opts))

(defn set-policy! [store actor p] (retention/set-policy! store actor p))

(defn derive-plan-leniently [schema] (plan/derive-plan-leniently schema))

(defn schedule-targets [schema opts] (plan/schedule-targets schema opts))

(defn scoped-import-plan
  ([schema import-block0 only-types] (plan/scoped-import-plan schema import-block0 only-types))
  ([schema import-block0 only-types opts]
   (plan/scoped-import-plan schema import-block0 only-types opts)))

(defn preview-transform!
  ([store schema transform-name] (transform-preview/preview-transform! store schema transform-name))
  ([store schema transform-name
    opts] (transform-preview/preview-transform! store schema transform-name opts)))

(defn of-transforms
  [store schema transform-refs] (unit-tests/of-transforms store schema transform-refs))

(defn signature
  ([connect schema ds] (virtual/signature connect schema ds))
  ([connect schema ds now?] (virtual/signature connect schema ds now?)))

(defn spec-of [schema ds-ref] (virtual/spec-of schema ds-ref))

(defn announce-action!
  [branch-store branch schema result] (run/announce-action! branch-store branch schema result))

(defn get-run [store run-id] (run/get-run store run-id))

(defn list-recent
  ([store] (run/list-recent store))
  ([store limit] (run/list-recent store limit))
  ([store limit kind] (run/list-recent store limit kind))
  ([store limit kind within-seconds] (run/list-recent store limit kind within-seconds)))

(defmacro with-run! [& args] `(run/with-run! ~@args))

(defn claimed [record claim-id now ms] (claim/claimed record claim-id now ms))

(defn free? [record now] (claim/free? record now))

(defn lapsed? [until now] (claim/lapsed? until now))

(defn mine? [record claim-id] (claim/mine? record claim-id))

(defn released [record] (claim/released record))

(defn renewed [record claim-id now ms] (claim/renewed record claim-id now ms))

(defn fenced [store fences] (lane/fenced store fences))

(defn as-leader [f] (leader/as-leader f))

(defn status [] (leader/status))

(defn lease-renewer [thread-name label renew] (lock/lease-renewer thread-name label renew))

(defn active-builds [store] (monitor/active-builds store))

(defn any-build-active? [store] (monitor/any-build-active? store))

(defn detach! [what f] (queue/detach! what f))

(defn prune-run-payloads!
  ([store] (rows/prune-run-payloads! store))
  ([store opts] (rows/prune-run-payloads! store opts)))
