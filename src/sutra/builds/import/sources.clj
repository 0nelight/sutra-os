(ns sutra.builds.import.sources
  (:require [sutra.sources.api :as sources]
            [sutra.catalog.grants :as grants]
            [sutra.catalog.values :as values]
            [sutra.store.access.scope :as ctx]
            [sutra.store.world :as world]
            [sutra.builds.ports :as ports]
            [sutra.builds.plan :as plan]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.builds.plan.virtual :as virtual]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]))

(set! *warn-on-reflection* true)

(defn read-spec
  [store]
  (when-let [schema (not-empty (world/world store))]
    {:datasources {}
     :import (plan/derive-plan-from-schema schema)}))

(defn source-signature
  [store schema branch]
  (when-let [{:keys [datasources import]} (read-spec (ctx/at store branch))]
    (let [eff-ds  (merge datasources (values/sources->datasources schema))
          froms   (->> (concat (vals (:objects import)) (vals (:links import)))
                       (mapcat (fn [e] (if (:sources e) (map :from (:sources e)) [(:from e)])))
                       (remove nil?)
                       (remove :compute)
                       distinct)
          connect  (ctx/connect (grants/source-authority store))
          read-one (fn [from]
                     (if-let [vds (some->> (:dataset from) (virtual/spec-of schema))]
                       (virtual/signature (ctx/connect (grants/source-authority store))
                                          schema vds)
                       (when-let [spec (get eff-ds (:datasource from))]
                         (sources/with-source [s connect spec] (inputs/select-rows s from)))))]
      (hash (mapv (fn [from] [from (read-one from)]) froms)))))

(defn- tx-datasource-refs
  [spec]
  (set
   (concat
    (keep (fn [[_ in]] (when (= :datasource (:kind in)) (:datasource in))) (:inputs spec))
    (filter keyword? [(:datasource spec) (:input spec)])
    (keep :datasource (:sources spec)))))

(defn sources-with-consumers
  [store schema]
  (let [rid->name (sch/rid-name-index schema)
        txs       (vals (:transforms schema))
        target-of (fn [t] (let [o (:output t)]
                            (some->> (or (:object-type o) (:link-type o))
                                     (rid/resolve-rid->name rid->name))))
        srcs      (vals (:sources schema))
        agent-up? (if (some sources/agent-routed? srcs)
                    ((ports/foreign-datasources store :agent-reachable?))
                    true)]
    (vec
     (for [[_rid s] (:sources schema)
           :let [nm (:source-name s)]]
       (assoc (select-keys s [:source-name :type :access :egress
                              :markings :description :jdbc-url :host :path])
              :health (sources/runtime-health s agent-up?)
              :consumers
              (vec (for [t txs :when (contains? (tx-datasource-refs t) nm)]
                     {:transform (:transform-name t)
                      :target    (target-of t)})))))))
