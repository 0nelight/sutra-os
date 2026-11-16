(ns sutra.actions.api
  (:require [sutra.actions.action :as action]
            [sutra.actions.apply :as apply]
            [sutra.actions.dispatch :as dispatch]
            [sutra.actions.effects :as action-effects]
            [sutra.actions.execution :as execution]
            [sutra.actions.landing :as landing]
            [sutra.actions.logic :as logic]
            [sutra.actions.options :as options]
            [sutra.actions.ports :as ports]
            [sutra.actions.proposals :as proposals]
            [sutra.actions.request :as request]
            [sutra.actions.staged :as staged]
            [sutra.actions.staged-rows :as staged-rows]
            [sutra.actions.staging :as staging]))

(set! *warn-on-reflection* true)

(defn add-unit [acc opts] (action/add-unit acc opts))

(defn list-staged
  [store] (staged-rows/list-staged store))

(defn commit-run! [store opts] (action/commit-run! store opts))

(defn edited-objects [schema tx-vector] (action/edited-objects schema tx-vector))

(defn log-action! [store action-name payload] (action/log-action! store action-name payload))

(defn run-action! [req] (action/run-action! req))

(defn run-held [req action args extra] (action/run-held req action args extra))

(defn run-policy [schema opts] (action/run-policy schema opts))

(defn scenario-sealed? [store] (action/scenario-sealed? store))

(def seal-action-name action/seal-action-name)

(defn apply-action [req] (apply/apply-action req))

(defn dispatch-action!
  ([store schema action-name args] (dispatch/dispatch-action! store schema action-name args))
  ([store schema action-name args opts]
   (dispatch/dispatch-action! store schema action-name args opts)))

(defn dispatch-action-batch!
  ([store schema action-name arg-maps]
   (dispatch/dispatch-action-batch! store schema action-name arg-maps))
  ([store schema action-name arg-maps opts]
   (dispatch/dispatch-action-batch! store schema action-name arg-maps opts)))

(defn run-logic!
  [store schema fn-name inputs opts] (dispatch/run-logic! store schema fn-name inputs opts))

(defn run-query-function
  ([store world fn-name] (action-effects/run-query-function store world fn-name))
  ([store world fn-name args] (action-effects/run-query-function store world fn-name args))
  ([store world fn-name args
    site] (action-effects/run-query-function store world fn-name args site)))

(defn object-reader [store] (action-effects/object-reader store))

(defn run-query-function-logged
  ([store world fn-name args site]
   (action-effects/run-query-function-logged store world fn-name args site))
  ([store world fn-name args site read]
   (action-effects/run-query-function-logged store world fn-name args site read)))

(defn ask [ctx io request] (execution/ask ctx io request))

(def after-commit-callbacks landing/after-commit-callbacks)

(defn branch-of [opts] (landing/branch-of opts))

(defn commit-branch! [store seen branch] (landing/commit-branch! store seen branch))

(def commit-callbacks landing/commit-callbacks)

(defn land [store seen branch] (landing/land store seen branch))

(defn land! [req resp] (landing/land! req resp))

(defn locked-projects [store branch schema] (ports/locked-projects store branch schema))

(defn land-asked? [seen branch asked] (landing/land-asked? seen branch asked))

(defn land? [req asked] (landing/land? req asked))

(defn action-tool [world action] (logic/action-tool world action))

(defn calculate [expression] (logic/calculate expression))

(defn evaluate-action-params!
  ([store schema action-name args] (options/evaluate-action-params! store schema action-name args))
  ([store schema action-name args search]
   (options/evaluate-action-params! store schema action-name args search)))

(defn options-page [label rows] (options/options-page label rows))

(defn param-option-query [sch pspec] (options/param-option-query sch pspec))

(defn accept!
  ([store branch schema automation-id id] (proposals/accept! store branch schema automation-id id))
  ([store branch schema automation-id id edits]
   (proposals/accept! store branch schema automation-id id edits)))

(defn counts-for [store branch automation-id] (proposals/counts-for store branch automation-id))

(defn list-for
  ([store branch automation-id status] (proposals/list-for store branch automation-id status))
  ([store branch automation-id status
    opts] (proposals/list-for store branch automation-id status opts)))

(defn propose! [store branch p] (proposals/propose! store branch p))

(defn reject! [store branch automation-id id] (proposals/reject! store branch automation-id id))

(defn with-refs [query schema proposals] (proposals/with-refs query schema proposals))

(defn request-on [store m] (request/request-on store m))

(defn with-fresh-id [action args] (request/with-fresh-id action args))

(defn discard-staged! [store opts seq] (staged/discard-staged! store opts seq))

(defn judge-stage! [plan] (staged/judge-stage! plan))

(defn plan-actions [store calls asked] (staged/plan-actions store calls asked))

(defn planned-rows [plan] (staged/planned-rows plan))

(defn planned-schema [opts] (staged/planned-schema opts))

(defn publish-requests! [req calls callbacks] (staged/publish-requests! req calls callbacks))

(defn stage-actions!
  ([store calls] (staged/stage-actions! store calls))
  ([store calls asked] (staged/stage-actions! store calls asked))
  ([store calls asked opts] (staged/stage-actions! store calls asked opts)))

(defn clear-staged! [store opts] (staging/clear-staged! store opts))

(defn decide! [store event input state] (staging/decide! store event input state))

(defn list-staged-enriched [store] (staging/list-staged-enriched store))

(defn of-staging-schema [store k f] (staging/of-staging-schema store k f))

(defn open-transaction [store] (staging/open-transaction store))

(defn staged-creates [store] (staging/staged-creates store))

(defn staging-schema [store] (staging/staging-schema store))

(defn staging-view [store] (staging/staging-view store))
