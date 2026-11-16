(ns sutra-ui.state.lineage-status
  (:require [sutra.viz.lineage-projection :as lp]
            [sutra-ui.api :as api]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]))

(defonce lineage-status
  (db/cursor [:lineage-status :lineage-status] {:nodes {} :last-build nil :last-logic-change nil}))

(def ^:private lineage-status-ttl-ms 30000)

(defn lineage-status-fresh?
  [{:keys [branch at]} b now]
  (and (= branch b) at (< (- now at) lineage-status-ttl-ms)))

(db/reg :sutra.lineage-status/refresh
        (fn [db [_ branch now]]
          (rq/request (assoc-in db [:lineage-status :read] {:branch branch :at now})
                      {:ask  [api/get! :lineage-status]
                       :then [[:sutra.lineage-status/loaded branch]]
                       :else [[:rf/set [:lineage-status :read] nil]]})))

(db/reg :sutra.lineage-status/loaded
        (fn [db [_ branch resp]]
          (when (= branch (get-in db [:lineage-status :read :branch]))
            {:db (-> db
                     (assoc-in [:lineage-status :lineage-status] (or resp {:nodes {}}))
                     (update-in [:catalog :schema-version] inc)
                     (update-in [:instances :instances-version] inc))})))

(defn refresh-lineage-status! []
  (db/send! [:sutra.lineage-status/refresh (api/effective-page-branch) (js/Date.now)]))

(defn ensure-lineage-status!
  []
  (when-not (lineage-status-fresh? (get-in @db/db [:lineage-status :read])
                                   (api/effective-page-branch) (js/Date.now))
    (refresh-lineage-status!)))

(defn node-status
  [kind kw]
  (get-in @lineage-status [:nodes (lp/status-key kind kw)]))
