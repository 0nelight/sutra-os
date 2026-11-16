(ns sutra.viz.lineage-branch
  (:require [sutra.kit.remote :as rd]
            [sutra.types.entities :as ents]
            [sutra.viz.lineage-projection :as lp]))

(def trunk "main")

(defn selected
  [page-branch]
  (or (not-empty page-branch) trunk))

(defn keeps-view?
  [mode]
  (boolean (and mode (ents/authored-instance? :graphs {:mode (keyword mode)}))))

(defn switch-to
  [{:keys [saved current known]}]
  (when (and (seq saved)
             (not= saved (selected current))
             (or (= trunk saved) (contains? (set known) saved)))
    saved))

(def ^:private kind-of-section
  (into {} (map (juxt :scope :kind)) lp/node-kinds))

(defn modified-refs
  [tasks]
  (into []
        (comp (remove #(= "removed" (some-> (:status %) name)))
              (keep (fn [{:keys [section] nm :name}]
                      (when-let [kind (some-> section keyword kind-of-section)]
                        (when (seq (some-> nm name))
                          [kind (keyword (name nm))]))))
              (distinct))
        tasks))

(defn modified-node-ids
  [tasks]
  (mapv (fn [[kind k]] (lp/status-key kind k)) (modified-refs tasks)))

(defn missing-from-graph
  [refs scope hidden]
  (filterv (fn [[kind k]]
             (or (not (contains? (get scope (lp/scope-of kind)) k))
                 (contains? (set hidden) (lp/node-id kind k))))
           refs))

(defn viewed-branch
  [refs branch kind k]
  (let [own (:kind (lp/row kind) kind)]
    (if (some #{[own k]} refs) (selected branch) trunk)))

(defn changes-view
  [current changes]
  (if (= trunk (selected current))
    {:show :main}
    (rd/show changes
             {:absent (constantly {:show :main})
              :value  (fn [{:keys [branch] :as c}]
                        (if (= branch current)
                          (assoc (select-keys c [:tasks :target :proposal]) :show :changes)
                          {:show :loading}))})))

(defn view-modified-refs
  [view]
  (if (= :changes (:show view)) (modified-refs (:tasks view)) []))

(defn canvas-view
  [{:keys [mode current catalog graphs]}]
  (cond
    (and current (= mode (:mode current))) {:use :current}
    (not (rd/value? catalog))
    (rd/fold catalog {:loading (constantly {:wait :loading})
                      :absent  (constantly {:wait :loading})
                      :failed  (fn [r] {:wait :failed :reason r})})
    :else
    (if-let [g (->> graphs (filter #(= mode (:mode %))) (sort-by (comp str :name)) last)]
      {:open (str (:id g))}
      {:create true})))
