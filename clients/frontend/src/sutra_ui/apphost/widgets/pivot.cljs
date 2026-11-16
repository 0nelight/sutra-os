(ns sutra-ui.apphost.widgets.pivot
  (:require [sutra-ui.ui.table :as table]
            [sutra-ui.apphost.events :refer [on-click-fn]]))

(defn pivot-table
  [{:keys [env on-select] :as cfg}]
  [table/pivot-table (-> (dissoc cfg :env)
                         (assoc :on-select (on-click-fn on-select (:ev-ctx env))))])
