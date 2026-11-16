(ns sutra.authoring.ports
  (:require [sutra.foundation.ops :as op]
            [sutra.kit.system :as sys]))

(set! *warn-on-reflection* true)

(defn code-compile-clojure-edit
  []
  (when (op/bound? :compute/check-edit-function)
    #(op/query {} :compute/check-edit-function {:code %})))

(defn products-offers [store] (sys/table (sys/of store) :products/offers))

(defn documents
  [store]
  (sys/provided (sys/of store) :documents/records))

(defn schema-plan [schema] (op/query {:schema schema} :builds/schema-plan {}))

(defn inputs-around
  [schema trunk nodes]
  (when (op/bound? :lineage/inputs-around)
    (op/query {:schema schema} :lineage/inputs-around {:trunk trunk :nodes (vec nodes)})))

(defn push-report
  [schema trunk around bases]
  (when (op/bound? :lineage/push-report)
    (op/query {:schema schema} :lineage/push-report
              (assoc bases :trunk trunk :around around))))
