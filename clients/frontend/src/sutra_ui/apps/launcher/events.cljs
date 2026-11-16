(ns sutra-ui.apps.launcher.events
  (:require [sutra-ui.api :as api]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.portal :as pstate]
            [sutra-ui.state.requests :as rq]))

(machine/reg-event :launcher/enter
                   (fn [{:keys [db]} [_ params]]
                     (update (rq/request db {:ask  [pstate/refresh-consumer-apps!]
                                             :then (when (contains? params :open-app-slug)
                                                     [(fn [_] [:rf/invoke routing/apply-open-app!
                                                               (:open-app-slug params)])])})
                             :fx #(into [[:invoke [pstate/set-consumer-app-version!
                                                   (:app-version params)]]] %))))

(machine/reg-event :launcher/tab
                   (fn [{:keys [db]} [_ k]]
                     (let [db (assoc db :tab k)]
                       (if (= :access k)
                         (rq/request db {:ask [api/get! :access] :path [:access] :report "access"})
                         {:db db}))))

(machine/reg-event :launcher/placement
                   (fn [{:keys [db]} [_ k]] {:db (assoc db :placement k)}))

(machine/reg-sub :launcher/at (fn [db [_ k]] (get db k)))
