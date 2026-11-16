(ns sutra.catalog.nodes
  (:require [sutra.catalog.lookup :as lookup]
            [sutra.types.naming :as naming]
            [sutra.types.transform-io :as tio]))

(set! *warn-on-reflection* true)

(defn producer-of
  [sch ds]
  (some (fn [[rid t]] (when (some #(= ds (:dataset %)) (tio/outputs-of t)) rid))
        (:transforms sch)))

(defn- outputs-of-transform
  [sch rid]
  (into [] (comp (keep :dataset) (distinct)) (tio/outputs-of (get-in sch [:transforms rid]))))

(def node-kinds
  {:object-type {:table (fn [sch kw]
                          (some-> (lookup/rid-of sch :object-types kw) naming/obj-table))}
   :transform   {:table    (fn [_ kw] (naming/trans-table kw))
                 :datasets (fn [sch kw]
                             (some->> (lookup/rid-of sch :transforms kw)
                                      (outputs-of-transform sch) not-empty))
                 :dataset  (fn [sch kw]
                             (some-> (get-in sch [:transforms
                                                  (lookup/rid-of sch :transforms kw)])
                                     tio/outputs-of first :dataset))}
   :dataset     {:table    (fn [sch kw] (some-> (lookup/rid-of sch :datasets kw)
                                                naming/dataset-table))
                 :datasets (fn [sch kw]
                             (when-let [ds (lookup/rid-of sch :datasets kw)]
                               (if-let [t (producer-of sch ds)]
                                 (into [ds] (remove #{ds}) (outputs-of-transform sch t))
                                 [ds])))
                 :dataset  (fn [sch kw] (lookup/rid-of sch :datasets kw))}})

(defn- node [what sch node-kind node-key]
  (when-let [f (get-in node-kinds [(keyword node-kind) what])]
    (f sch (keyword node-key))))

(defn node->table [sch node-kind node-key] (node :table sch node-kind node-key))

(defn node->datasets
  [sch node-kind node-key]
  (node :datasets sch node-kind node-key))

(defn node->dataset [sch node-kind node-key] (node :dataset sch node-kind node-key))
