(ns sutra.canvas.layouts
  (:require [sutra.foundation.ops :as op]
            [sutra.projects.api :as projects]
            [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(defn- moved
  [store {:keys [branch node-kind from to]}]
  (for [row (ctl/list store :graph-layouts
                      {:branch branch :node-kind node-kind :node-key (str from)})
        :when (nil? (ctl/get store :graph-layouts
                             (assoc (select-keys row [:branch :graph-id :node-kind])
                                    :node-key (str to))))]
    row))

(op/defop :canvas/move-layout-node [{:keys [store]} {:keys [to] :as in}]
  (let [rows (vec (moved store in))]
    {:tx  {:records (into []
                          (mapcat (fn [row]
                                    (concat
                                     (:records (ctl/deleting store :graph-layouts
                                                             (select-keys row [:branch :graph-id
                                                                               :node-kind
                                                                               :node-key])))
                                     [(ctl/entry store :graph-layouts
                                                 (assoc row :node-key (str to)))])))
                          rows)}
     :out (count rows)}))

(defn- side-data
  [store branch grid]
  (concat (:records (ctl/deleting store :graph-layouts {:branch branch :graph-id grid}))
          (:records (ctl/deleting store :color-schemes {:branch branch :graph-id grid}))
          (projects/placement-deletions store branch grid)))

(op/defop :canvas/sweep-dropped-graphs [{:keys [store]} {:keys [branch before after]}]
  (let [grids (for [[gkey spec] before
                    :when (not (contains? after gkey))]
                (or (:id spec) gkey))]
    {:tx  {:records (into [] (mapcat #(side-data store branch %)) grids)}
     :out (count grids)}))
