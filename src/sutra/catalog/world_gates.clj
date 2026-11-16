(ns sutra.catalog.world-gates
  (:require [sutra.catalog.world-advice :as world-advice]
            [sutra.catalog.world-export :as world-export]
            [sutra.catalog.world-install :as world-install]
            [sutra.catalog.problems :as problems]
            [sutra.catalog.structure :as structure]
            [sutra.types.finding :as finding]
            [sutra.world.db :as db]
            [sutra.world.gate :as gate]
            [sutra.world.gate.lint :as kern-lint]
            [sutra.world.view :as view]
            [sutra.parser.lint :as lint]
            [sutra.parser.spec :as spec]
            [sutra.types.schema :as sch]
            [sutra.types.value-types :as vt]))

(defn- of-key
  [problems field sec name-field]
  (fn [W k]
    (let [n     (or (get-in W [sec k name-field]) k)
          owner (first (sort-by str (for [[k2 e] (get W sec)
                                          :when (= n (or (get e name-field) k2))]
                                      k2)))]
      (when (= k owner)
        (filterv #(= n (get % field)) (problems W))))))

(def ^:private vocab (delay (spec/vocabulary)))

(defn- lint-of
  [rule]
  (fn [W k]
    (let [[sec e] (world-advice/entry-of W k)]
      (filterv #(and (= rule (:rule %)) (world-advice/named-own? sec k e %))
               (lint/gate-violations W @vocab)))))

(def checks
  {:media?      vt/media?
   :structure   (fn [cls k e] (some-> (problems/entry-problem cls k e)
                                      problems/structural-finding vector))
   :structure-frame (fn [W] (mapv problems/structural-finding (problems/frame-problems W)))
   :structure-ok? structure/entry-ok?
   :link-types  (fn [W k]
                  (let [W (sch/indexed W)]
                    (when-let [r (problems/link-type-problem W k (get-in W [:link-types k]))]
                      [{:link k :reason r}])))
   :link-sides  (fn [W _] (problems/side-name-problems W))
   :actions     (of-key problems/explain-actions :action :actions :action-name)
   :object-sets (fn [W k]
                  (when-not (problems/object-set-ok? W (get-in W [:object-sets k]))
                    [{:object-set k}]))
   :transforms  (of-key problems/transform-problems :transform :transforms :transform-name)})

(def ^:private all-checks
  (into checks (for [r kern-lint/rule-ids] [[:lint r] (lint-of r)])))

(def rows (gate/rows-with all-checks))

(def views
  (-> view/rows (into (gate/views-with all-checks)) (into world-install/views)
      (into world-export/rows)))

(defn verdict
  [db0 db1]
  (let [W (db/world db1)]
    (if (gate/accepted-since rows db0 db1)
      {:ok W}
      (let [report (gate/report-since rows db0 db1)]
        {:schema   W
         :reasons  (problems/reasons report)
         :findings (finding/worst-first (mapv problems/word report))}))))
