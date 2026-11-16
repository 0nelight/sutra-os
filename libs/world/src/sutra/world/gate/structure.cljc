(ns sutra.world.gate.structure
  (:require [sutra.world.db :as db]
            [sutra.world.delta :as d]
            [sutra.world.gate.base :as b]
            [sutra.world.view :as view]
            [sutra.types.entities :as ents]))

(def fact-rows
  [{:id :function-targets :sections #{:functions}
    :facts (fn [_ e] (when-not (:library e) [(:target-type e)]))}])

(defn- all-nodes [db]
  (for [s ents/sections
        :let [es (get (db/world db) s)]
        :when (map? es)
        k (keys es)]
    [s k]))

(defn- entry-row [check]
  {:id    :structure
   :rule  :schema/structure
   :scope :local
   :keys  (fn [_ _ changes] (d/nodes changes))
   :all   all-nodes
   :check (fn [db [s k :as node]]
            (when-some [e (db/entry db node)] (check s k e)))})

(defn- ok-row [ok?]
  {:id     :structure-ok?
   :rule   :schema/structure
   :scope  :local
   :silent true
   :keys   (fn [db0 db1 changes]
             (into (d/nodes changes)
                   (b/both (fn [d t] (b/facts d :function-targets t))
                           db0 db1 (view/touched changes :object-types))))
   :all    all-nodes
   :check  (fn [db [s k :as node]]
             (when-some [e (db/entry db node)]
               (when-not (ok? s k e #(contains? (get (db/world db) :object-types) %))
                 [{:entity-class s :key k}])))})

(defn- frame-row [check]
  {:id    :structure-frame
   :rule  :schema/structure
   :scope :frame
   :frame (fn [db] (check (db/world db)))})

(defn rows
  [{:keys [structure structure-frame structure-ok?]}]
  (cond-> []
    structure-frame (conj (frame-row structure-frame))
    structure       (conj (entry-row structure))
    structure-ok?   (conj (ok-row structure-ok?))))
