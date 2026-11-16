(ns sutra-ui.apps.evals.events
  (:require [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.staging :as staging]))

(defn select-suite [db nm]
  (assoc db :sel nm :run nil :compare [] :debug nil :tab :results
         :experiment nil :finished nil))

(defn open-run [db run-id]
  (assoc db :run run-id :compare [] :debug nil :tab :results))

(defn- loading
  [ask path pick]
  {:ask ask :path path :pick pick :else [#(vector :rf/set path {:error %})]})

(defn- load-runs [suite-id then]
  (assoc (loading [api/get! :eval-results {:id suite-id}] [:runs suite-id] #(vec (:results %)))
         :then (when then [then])))

(defn- poll [db job-id]
  (rq/request db {:ask  [api/get! :eval-job {:id job-id}]
                  :then [[:eval-suites/job-polled job-id]] :else [[:eval-suites/run-failed]]}))

(machine/reg-event :eval-suites/load
                   (fn [{:keys [db]} [_ then-select]]
                     (rq/request db (assoc (loading [api/get! :eval-suites] [:suites]
                                                    #(vec (:suites %)))
                                           :then [[:eval-suites/loaded then-select]]))))

(machine/reg-event :eval-suites/loaded
                   (fn [{:keys [db]} [_ then-select]]
                     (let [suites (:suites db)
                           db     (cond-> db
                                    (and (nil? (:sel db)) (seq suites))
                                    (assoc :sel (:name (first suites))))]
                       {:db (cond-> db then-select (select-suite then-select))})))

(machine/reg-event :eval-suites/select (fn [{:keys [db]} [_ nm]] {:db (select-suite db nm)}))

(machine/reg-event :eval-suites/open-run (fn [{:keys [db]} [_ id]] {:db (open-run db id)}))

(machine/reg-event :eval-suites/runs
                   (fn [{:keys [db]} [_ suite-id]] (rq/request db (load-runs suite-id nil))))

(machine/reg-event :eval-suites/ensure-runs
                   (fn [{:keys [db]} [_ suite-id]]
                     (when (and suite-id (not (contains? (:runs db) suite-id)))
                       (rq/request (assoc-in db [:runs suite-id] :loading)
                                   (load-runs suite-id nil)))))

(machine/reg-event :eval-suites/ensure-cases
                   (fn [{:keys [db]} [_ run-id]]
                     (when (and run-id (not (contains? (:cases db) run-id)))
                       (rq/request (assoc-in db [:cases run-id] :loading)
                                   (loading [api/get! :eval-run-cases {:id run-id}]
                                            [:cases run-id] #(vec (:cases %)))))))

(machine/reg-event :eval-suites/register
                   (fn [{:keys [db]} [_ spec]]
                     (rq/request (assoc db :register nil)
                                 {:ask   [api/post! :eval-suites {:name (:name spec) :spec spec}]
                                  :busy  [:register :busy?] :error [:register :err]
                                  :then  [[:rf/set [:register-open?] false]
                                          [:eval-suites/load (name (:name spec))]]})))

(machine/reg-event :eval-suites/register-failed
                   (fn [{:keys [db]} [_ reason]]
                     {:db (assoc db :register {:err reason})}))

(machine/reg-event :eval-suites/draft-case
                   (fn [{:keys [db]} [_ c]]
                     {:db (assoc db :draft-case c :case-form nil :debug nil :tab :cases)}))

(machine/reg-event :eval-suites/drop-draft
                   (fn [{:keys [db]} _] {:db (dissoc db :draft-case)}))

(machine/reg-event :eval-suites/case-invalid
                   (fn [{:keys [db]} [_ reason]] {:db (assoc db :case-form {:err reason})}))

(machine/reg-event :eval-suites/add-case
                   (fn [{:keys [db]} [_ suite c]]
                     (rq/request (assoc db :case-form nil)
                                 {:ask  [api/post! :eval-suite-cases {:name (:name suite)}
                                         {:case c}]
                                  :busy [:case-form :busy?] :error [:case-form :err]
                                  :then [[:eval-suites/case-added (boolean (:declared suite))]]})))

(machine/reg-event :eval-suites/case-added
                   (fn [{:keys [db]} [_ declared?]]
                     {:db (-> db
                              (assoc :case-form
                                     {:done (i18n/t (if declared?
                                                      :ui/versions-staged
                                                      :ui/aev-case-added))})
                              (dissoc :draft-case))
                      :fx [(when declared? [:invoke [staging/refresh-staged!]])
                           [:dispatch [:eval-suites/load]]]}))

(machine/reg-event :eval-suites/preview
                   (fn [{:keys [db]} [_ nm body]]
                     (rq/request (assoc-in db [:run-form nm :err] nil)
                                 {:ask   [api/post! :eval-run (assoc body :preview true)]
                                  :path  [:run-form nm :preview] :error [:run-form nm :err]
                                  :else  [[:rf/set [:run-form nm :preview] nil]]})))

(machine/reg-event :eval-suites/start
                   (fn [{:keys [db]} [_ suite body]]
                     (rq/request (-> db
                                     (assoc :running {:suite suite :busy? true})
                                     (assoc-in [:run-form (:name suite) :err] nil))
                                 {:ask  [api/post! :eval-run body]
                                  :then [[:eval-suites/job-started]]
                                  :else [[:eval-suites/run-failed]]})))

(machine/reg-event :eval-suites/job-started (fn [{:keys [db]} [_ job]] (poll db (:id job))))

(machine/reg-event :eval-suites/job-poll (fn [{:keys [db]} [_ job-id]] (poll db job-id)))

(machine/reg-event :eval-suites/job-polled
                   (fn [{:keys [db]} [_ job-id j]]
                     (case (:status j)
                       "running" {:db (assoc-in db [:running :progress] (:progress j))
                                  :fx [[:after {:key :eval-job :ms 1000
                                                :event [:eval-suites/job-poll job-id]}]]}
                       "done"    {:fx [[:dispatch [:eval-suites/finished (:result j)]]]}
                       {:fx [[:dispatch [:eval-suites/run-failed (str (:error j))]]]})))

(machine/reg-event :eval-suites/run-failed
                   (fn [{:keys [db]} [_ reason]]
                     {:db (update db :running assoc :busy? false :err reason)}))

(machine/reg-event :eval-suites/finished
                   (fn [{:keys [db]} [_ res]]
                     (let [suite (get-in db [:running :suite])]
                       {:db (assoc db :running {:suite suite})
                        :fx [[:dispatch [:eval-suites/load]]
                             (rq/ask (load-runs (:id suite) [:eval-suites/show-result res]))]})))

(machine/reg-event :eval-suites/show-result
                   (fn [{:keys [db]} [_ res]]
                     (if-let [{:keys [id name]} (:experiment res)]
                       {:db (assoc db :run nil :tab :results :compare [] :experiment id
                                   :finished {:name name :runs (count (:runs res))})}
                       (when-let [id (:run-id res)] {:db (open-run db id)}))))

(def max-compare 4)

(machine/reg-sub :eval-suites/at (fn [db [_ k]] (get db k)))

(machine/reg-event :eval-suites/assoc (fn [{:keys [db]} [_ & kvs]] {:db (apply assoc db kvs)}))

(machine/reg-event :eval-suites/assoc-in
                   (fn [{:keys [db]} [_ path v]] {:db (assoc-in db path v)}))

(machine/reg-event :eval-suites/flip (fn [{:keys [db]} [_ k]] {:db (update db k not)}))

(machine/reg-event :eval-suites/toggle-in
                   (fn [{:keys [db]} [_ k x]]
                     {:db (update db k #(if (contains? % x) (disj % x) (conj (set %) x)))}))

(machine/reg-event :eval-suites/compare
                   (fn [{:keys [db]} [_ id]]
                     {:db (update db :compare
                                  (fn [ids]
                                    (cond (some #{id} ids) (vec (remove #{id} ids))
                                          (< (count ids) max-compare) (conj (vec ids) id)
                                          :else ids)))}))

(machine/reg-event :eval-suites/debug
                   (fn [{:keys [db]} [_ run-id case-id tab runs]]
                     {:db (assoc db :debug (when run-id [run-id case-id tab]) :debug-runs runs)}))
