(ns sutra.catalog.interfaces
  (:require [sutra.kit.tree :as tree]
            [sutra.types.schema :as sch]
            [sutra.ref.install :as ri]))

(set! *warn-on-reflection* true)

(defn closure
  [edges seeds]
  (into #{} (filter #(contains? edges %)) (tree/reach edges seeds)))

(defn iface-index
  [schema]
  (let [ancestors (ri/ancestors-of schema)]
    {:ancestors ancestors
     :known     (set (keys (:interfaces schema)))
     :effective (into {} (map (fn [k] [k (ri/effective-interface schema ancestors k)]))
                      (keys (:interfaces schema)))}))

(defn ifaces
  [schema]
  (sch/column schema ::ifaces iface-index))

(defn effective-implements
  [schema ot]
  (ri/effective-implements schema (:ancestors (ifaces schema)) ot))

(defn effective-interface
  [schema iface-kw]
  (or (get-in (ifaces schema) [:effective iface-kw])
      (ri/effective-interface schema {} iface-kw)))

(defn contribute-interface-props
  [schema impls attrs]
  (ri/contribute-props schema (:ancestors (ifaces schema)) impls attrs))

(defn satisfies-endpoint?
  [schema obj-kw endpoint-kw]
  (or (= obj-kw endpoint-kw)
      (and (contains? (:interfaces schema) endpoint-kw)
           (some #{endpoint-kw}
                 (effective-implements schema (get-in schema [:object-types obj-kw]))))))
