(ns sutra.catalog.world-install
  (:require [clojure.string :as str]
            [sutra.catalog.function-contract :as contract]
            [sutra.catalog.world-columns :as world-columns]
            [sutra.world.db :as kdb]
            [sutra.world.view :as view]
            [sutra.types.attribute :as attribute]
            [sutra.types.column :as column]
            [sutra.types.derive-graph :as dg]
            [sutra.types.link-side :as link-side]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn fk-col
  [a]
  (str/lower-case (name (column/attr->col a))))

(defn- transform-facts
  [t]
  (concat
   (for [o (tio/outputs-of t)
         f [(some->> (:fk-attribute o) fk-col (vector :fk-col))
            (some->> (:dataset o) (vector :dataset-out))]
         :when f]
     f)
   (for [o (tio/outputs-of t)
         :when (and (= :link-type (some-> (:kind o) name keyword)) (:fk-attribute o))]
     [:fk-link (:link-type o)])))

(defn- object-facts
  [o]
  (concat (some->> (get-in o [:backing :dataset]) (vector :backed-by) vector)
          (into #{} (for [a (attribute/rows o)
                          :let [e (attribute/derived-expression a)]
                          :when e
                          h (dg/handles e)]
                      [:derive-handle (keyword (name h))]))))

(defn- link-facts
  [lt]
  (for [end (link-side/ends lt)]
    [:link-end end]))

(defn- source-facts
  [s]
  (let [wb (:writeback s)]
    (distinct (for [cfg (when (map? wb) (vals wb))
                    :let [ot (when (map? cfg) (:object-type cfg))]
                    :when (some? ot)]
                [:writeback-of ot]))))

(defn- fn-facts
  [e]
  (some->> (contract/call-key (:function-name e)) (vector :fn-named) vector))

(defn- call-facts [sec]
  (fn [k e] (for [c (contract/entry-calls sec k e)] [:calls (contract/call-key (:function c))])))

(def view
  (view/facts ::facts {:transforms   (fn [_ t] (concat (transform-facts t) (fn-facts t)))
                       :functions    (fn [_ f] (fn-facts f))
                       :object-types (fn [_ o] (object-facts o))
                       :link-types   (fn [_ lt] (link-facts lt))
                       :sources      (fn [_ s] (source-facts s))
                       :actions      (call-facts :actions)
                       :applications (call-facts :applications)}))

(def views
  [view world-columns/loop-state])

(defn loop-state
  [d]
  (or (kdb/view d :sutra.catalog.world-columns/loop-state)
      (throw (ex-info "kern db carries no loop state" {:view ::loop-state}))))

(defn nodes
  [d fact]
  (if-some [v (kdb/view d ::facts)]
    (get v fact #{})
    (throw (ex-info "kern db carries no install facts" {:view ::facts}))))
