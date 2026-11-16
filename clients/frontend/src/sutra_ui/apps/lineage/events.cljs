(ns sutra-ui.apps.lineage.events
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.apps.lineage.model :as lineage-model]
            [sutra-ui.state.graph :as graph]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.sync :as sync]))

(def blank-schedules {:items :loading :name "" :cron "0 2 * * *"})

(machine/reg-event :lineage/edit
                   (fn [{:keys [db]} [_ path f & args]]
                     {:db (if (seq path) (apply update-in db path f args) (apply f db args))}))

(machine/reg-event :lineage.group/rename
                   (fn [{:keys [db]} [_ g]]
                     (rq/request db {:ask  [overlay/prompt! {:title   (i18n/t :ui/group-rename)
                                                             :default (:name g)}]
                                     :then [[:lineage.group/renamed (:id g)]]})))

(machine/reg-event :lineage.group/renamed
                   (fn [_ [_ id nm]]
                     (when (seq (str nm))
                       {:fx [[:invoke [graph/update-group! id #(assoc % :name nm)]]]})))

(machine/reg-event :lineage.group/delete
                   (fn [_ [_ g]]
                     {:fx [[:confirm {:ask {:title   (i18n/t :ui/group-delete-confirm
                                                             {:name (:name g)})
                                            :danger? true}
                                      :ok  [:lineage.group/deleted (:id g)]}]]}))

(machine/reg-event :lineage.group/deleted
                   (fn [_ [_ id]] {:fx [[:invoke [lineage-model/delete-group! id]]]}))

(machine/reg-event :lineage.builds/enter
                   (fn [{:keys [db]} _] {:db (assoc db :builds {:strategy :selected})}))

(machine/reg-event :lineage.builds/leave
                   (fn [{:keys [db]} _] {:db (dissoc db :builds)}))

(machine/reg-event :lineage.builds/strategy
                   (fn [{:keys [db]} [_ k]]
                     {:db (update db :builds assoc :strategy k :preview nil)}))

(machine/reg-event :lineage.builds/cancel
                   (fn [{:keys [db]} _] {:db (assoc-in db [:builds :preview] nil)}))

(machine/reg-event :lineage.builds/estimate
                   (fn [{:keys [db]} [_ args]]
                     (rq/request (assoc-in db [:builds :preview] :loading)
                                 {:ask    [api/build-estimate args] :path [:builds :preview]
                                  :report #(i18n/t :ui/build-preview-failed {:error %})
                                  :else   [[:rf/set [:builds :preview] nil]]})))

(machine/reg-event :lineage.builds/run
                   (fn [{:keys [db]} [_ body]]
                     (rq/request db {:ask  (if body
                                             [api/post! :import-run body]
                                             [api/post! :import-run])
                                     :busy [:builds :busy?]
                                     :then [[:rf/set [:builds :preview] nil]]
                                     :fx   [[:invoke [sync/refresh-snapshot!]]]})))

(def ^:private reload-schedules [:lineage.schedules/load])

(def ^:private load-schedules
  {:ask [api/get! :schedules] :path [:schedules :items] :pick #(vec (:schedules %))})

(defn- schedule-request [db ask then fx]
  (rq/request db {:ask  ask :report #(i18n/t :ui/lin-schedule-failed {:error %})
                  :then then :fx fx}))

(machine/reg-event :lineage.schedules/enter
                   (fn [{:keys [db]} _]
                     (rq/request (assoc db :schedules blank-schedules) load-schedules)))

(machine/reg-event :lineage.schedules/leave
                   (fn [{:keys [db]} _] {:db (dissoc db :schedules)}))

(machine/reg-event :lineage.schedules/load
                   (fn [{:keys [db]} _] (rq/request db load-schedules)))

(machine/reg-event :lineage.schedules/field
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:schedules k] v)}))

(machine/reg-event :lineage.schedules/add
                   (fn [{:keys [db]} [_ target]]
                     (let [{nm :name cron :cron} (:schedules db)]
                       (schedule-request db [api/post! :schedules
                                             {:name nm :cron cron :target target}]
                                         [[:rf/set [:schedules :name] ""] reload-schedules]
                                         [[:invoke [lineage-status/refresh-lineage-status!]]]))))

(machine/reg-event :lineage.schedules/run
                   (fn [{:keys [db]} [_ id]]
                     (schedule-request db [api/post! :schedule-run {:id id} nil]
                                       [reload-schedules] [[:invoke [sync/refresh-snapshot!]]])))

(machine/reg-event :lineage.schedules/delete
                   (fn [_ [_ id nm]]
                     {:fx [[:confirm {:ask {:title   (i18n/t :ui/lin-schedule-delete-confirm
                                                             {:name nm})
                                            :danger? true}
                                      :ok  [:lineage.schedules/delete-confirmed id]}]]}))

(machine/reg-event :lineage.schedules/delete-confirmed
                   (fn [{:keys [db]} [_ id]]
                     (schedule-request db [api/delete! :schedule-of {:id id}]
                                       [reload-schedules] nil)))
