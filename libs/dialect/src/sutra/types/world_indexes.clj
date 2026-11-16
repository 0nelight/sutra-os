(ns sutra.types.world-indexes
  (:require [sutra.types.set-query :as sq]
            [sutra.kit.coerce :refer [->kw]]))

(set! *warn-on-reflection* true)

(def indexed-ops #{:= :not= :in :< :> :<= :>= :truthy :falsy})

(defn- clause-column [[prop op _]]
  (when (and (indexed-ops (->kw op)) (or (keyword? prop) (string? prop))
             (not= :* (->kw prop)))
    (->kw prop)))

(defn query-columns [{:keys [clauses order-by]}]
  (cond-> (set (keep clause-column clauses))
    order-by (conj (->kw order-by))))

(defn type-name
  [spec]
  (when-let [n (:object-type-name spec)] (->kw (name n))))

(defn- type-by-name
  [W]
  (into {} (for [[T spec] (sort-by (comp str key) (:object-types W))
                 :let [n (type-name spec)]
                 :when n]
             [n T])))

(defn stored [spec]
  (set (map :attribute-name (:attribute-list spec))))

(defn app-wants
  [a]
  (apply merge-with into {}
         (for [q (sq/queries (sq/layout-specs (:layout a)))]
           {(:root q) (query-columns q)})))

(defn type-columns
  [spec wanted]
  (let [cols (sort (filter (stored spec) wanted))]
    (when (seq cols) (vec cols))))

(defn columns [W]
  (let [by-name (type-by-name W)
        wanted  (apply merge-with into {}
                       (for [[_ a] (:applications W)
                             [root cols] (app-wants a)
                             :let  [T (get by-name root)]
                             :when T]
                         {T cols}))]
    (into {} (for [[T cols] wanted
                   :let [cols (type-columns (get-in W [:object-types T]) cols)]
                   :when cols]
               [T cols]))))
