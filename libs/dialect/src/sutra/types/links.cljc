(ns sutra.types.links
  (:require [sutra.types.rid :as rid]))

(defn- object-type-names
  [otypes]
  (into {} (for [[r spec] otypes] [r (rid/->name (:object-type-name spec))])))

(defn links-of
  [schema otid]
  (let [idx (object-type-names (:object-types schema))
        of  (fn [r] (or (get idx r) (rid/->name r)))]
    (vec (for [[r l] (:link-types schema)
               :let [from (:from-object-type l) to (:to-object-type l)]
               :when (or (= from otid) (= to otid))]
           {:rid r
            :name (rid/->name (or (:link-type-name l) (:id l)))
            :from (of from) :to (of to)
            :cardinality (:cardinality l)
            :dir (if (= from otid) :out :in)}))))
