(ns sutra-ui.apps.branching.events
  (:require [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(defn all? [db] (= "all" (:status db "open")))

(defn policy-key [space] (if (= "*" space) "*" (str "space/" space)))

(defn- req
  [ask k what & {:as more}]
  (cond-> (merge {:ask ask :report what} more) k (assoc :path [k])))

(machine/reg-event :branching/set (fn [{:keys [db]} [_ k v]] {:db (assoc db k v)}))

(machine/reg-event :branching/reload
                   (fn [{:keys [db]} _]
                     (-> (rq/request db (req [api/get! :branches {:closed (all? db)}]
                                             :branches "branches"
                                             :pick #(vec (:branches %))))
                         (update :fx conj [:invoke [gov/refresh-proposals!
                                                    {:status (if (all? db) "all" "open")}]]))))

(machine/reg-event :branching/status
                   (fn [{:keys [db]} [_ v]]
                     {:db (assoc db :status v) :fx [[:dispatch [:branching/reload]]]}))

(machine/reg-event :branching/open
                   (fn [{:keys [db]} [_ nm]]
                     (rq/requests (-> db (assoc :open nm :owner-draft "") (dissoc :changes :roles))
                                  [(req [api/get! :branch-changes {:name nm}] :changes
                                        "branch changes")
                                   (req [api/get! :branch-roles {:name nm}] :roles "roles")])))

(machine/reg-event :branching/back (fn [{:keys [db]} _] {:db (dissoc db :open)}))

(machine/reg-event :branching/reactivate
                   (fn [{:keys [db]} [_ nm]]
                     (rq/request db (req [api/post! :branch-status {:name nm} {:inactive false}]
                                         nil "reactivate" :then [[:branching/reload]]))))

(machine/reg-event :branching/archive
                   (fn [_ [_ nm]]
                     {:fx [[:confirm {:ask {:body (i18n/t :ui/branch-close-confirm {:name nm})
                                            :danger? true
                                            :confirm-label (i18n/t :ui/branch-close)}
                                      :ok  [:branching/close nm]}]]}))

(machine/reg-event :branching/close
                   (fn [{:keys [db]} [_ nm]]
                     (rq/request db (req [api/post! :branch-status {:name nm} {:closed true}]
                                         nil "close"
                                         :then [[:branching/back] [:branching/reload]]))))

(machine/reg-event :branching/remove-task
                   (fn [{:keys [db]} [_ nm task]]
                     (rq/request db {:ask  [gov/remove-from-branch! nm [task]]
                                     :then [[:branching/open nm]]})))

(machine/reg-event :branching.role/set
                   (fn [{:keys [db]} [_ nm body]]
                     (rq/request db (req [api/post! :branch-roles {:name nm} body] :roles
                                         "roles"))))

(machine/reg-event :branching.role/add
                   (fn [{:keys [db]} [_ nm]]
                     {:db (assoc db :owner-draft "")
                      :fx [[:dispatch [:branching.role/set nm
                                       {:principal (str/trim (str (:owner-draft db)))}]]]}))

(machine/reg-event :branching.policy/enter
                   (fn [{:keys [db]} _]
                     {:db (assoc db :space "*" :policy nil)
                      :fx [[:dispatch [:branching.policy/load]]]}))

(machine/reg-event :branching.policy/space
                   (fn [{:keys [db]} [_ space]]
                     {:db (assoc db :space space) :fx [[:dispatch [:branching.policy/load]]]}))

(defn- policy [db verb & body]
  (rq/request db (req (into [verb :branch-policy {:name (policy-key (:space db "*"))}] body)
                      :policy "retention")))

(machine/reg-event :branching.policy/load (fn [{:keys [db]} _] (policy db api/get!)))

(machine/reg-event :branching.policy/field
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:policy k] v)}))

(machine/reg-event :branching.policy/save
                   (fn [{:keys [db]} _]
                     (policy db api/post!
                             (select-keys (:policy db) [:inactive-days :retention-days]))))

(machine/reg-sub :branching/at (fn [db [_ k]] (get db k)))
