(ns sutra-ui.apps.operations.events
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.kit.remote :as rd]
            [sutra-ui.api :as api]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(defn try-read-edn
  [s]
  (if (str/blank? s)
    {:ok nil}
    (try {:ok (reader/read-string s)}
         (catch :default e {:err (or (.-message e) (str e))}))))

(defn edit-value [s]
  (let [{:keys [ok err]} (try-read-edn s)]
    (if (or err (symbol? ok)) s ok)))

(def blank-automation-form {:name "" :on "" :action "" :when "" :args "" :every ""})

(def blank-health-form
  {:name "" :node-kind "object-type" :node-key "" :kind "row-count" :query "" :op ">="
   :value "1"})

(def proposal-page 50)

(defn- form-request
  [k f-and-args then]
  {:ask f-and-args :busy [k :busy] :error [k :err] :within [k] :then then})

(defn- saving-form [db k f-and-args then]
  (rq/request (assoc-in db [k :err] nil) (form-request k f-and-args then)))

(defn- reloading [f-and-args reload] (rq/ask {:ask f-and-args :then [[reload]]}))

(defn- load-automations [db]
  (rq/request db {:ask [api/get! :automations] :path [:automations] :rd? true
                  :pick #(vec (:automations %))}))

(defn- automation-body [{:keys [name on action when args every]}]
  (let [w (try-read-edn when) a (try-read-edn args)]
    (cond
      (:err w) {:err (str (i18n/t :ui/ops-condition-edn) ": " (:err w))}
      (:err a) {:err (str (i18n/t :ui/ops-args-edn) ": " (:err a))}
      :else    {:body (cond-> {:name name :on on
                               :effect {:action (keyword action) :args (or (:ok a) {})}
                               :paused false}
                        (:ok w)                 (assoc :when (:ok w))
                        (not (str/blank? every)) (assoc :every-ms (js/parseInt every 10)))})))

(defn- submit-automation [db]
  (let [{:keys [err body]} (automation-body (:automation-form db))]
    (if err
      {:db (assoc-in db [:automation-form :err] err)}
      (saving-form db :automation-form [api/post! :automations body]
                   [[:rf/set [:automation-form] nil] [:ops.automations/reload]]))))

(defn- toggle-pause
  [{:keys [id name on every-ms paused when-spec effect-spec]}]
  (let [body (cond-> {:id id :name name :on on :every-ms every-ms :paused (not paused)}
               (seq when-spec)   (assoc :when (reader/read-string when-spec))
               (seq effect-spec) (assoc :effect (reader/read-string effect-spec)))]
    {:fx [(reloading [api/post! :automations body] :ops.automations/reload)]}))

(defn- running [db id dry? f-and-args]
  (rq/request db {:ask f-and-args :busy [:rows id :busy] :path [:rows id :result]
                  :pick #(assoc % :dry? dry?)
                  :then (when-not dry? [[:ops.automations/reload]])}))

(defn- load-proposals [db aid more?]
  (let [{:keys [column items]} (get-in db [:proposals aid])
        offset (if (and more? (vector? items)) (count items) 0)]
    (rq/request db {:ask  [api/automation-proposals aid column
                           {:limit proposal-page :offset offset}]
                    :then [[:ops.proposals/loaded aid more?]]
                    :else [[:ops.proposals/failed aid]]})))

(defn- proposals-loaded [db aid more? {:keys [proposals counts]}]
  {:db (update-in db [:proposals aid]
                  (fn [{:keys [items] :as p}]
                    (assoc p :counts counts
                           :items (if (and more? (vector? items))
                                    (into items proposals)
                                    (vec proposals)))))})

(defn- edit-start [db aid pid params args]
  {:db (assoc-in db [:proposals aid :cards pid :editing]
                 (into {} (for [p params :let [v (get args p)]]
                            [p (cond (nil? v) "" (string? v) v :else (pr-str v))])))})

(defn- decide [db aid pid f-and-args]
  (let [card [:proposals aid :cards pid]]
    (rq/request (assoc-in db (conj card :err) nil)
                {:ask f-and-args :busy (conj card :busy) :error (conj card :err)
                 :then [[:ops.card/decided aid pid]]})))

(defn- accept [db aid pid args]
  (let [editing (get-in db [:proposals aid :cards pid :editing])
        changed (into {} (for [[p v] editing
                               :let [v (if (str/blank? v) nil (edit-value v))]
                               :when (not= v (get args p))]
                           [p v]))]
    (decide db aid pid [api/post! :automation-proposal-accept {:id aid :proposal pid}
                        (when (and editing (seq changed)) {:args changed})])))

(defn- decided [db aid pid r]
  {:db (update-in db [:proposals aid :cards pid]
                  (fn [c] (if (= "failed" (:status r))
                            (assoc c :err (:error r))
                            (assoc c :editing nil))))
   :fx [[:dispatch [:ops.proposals/load aid false]]]})

(defn- load-checks [db]
  (rq/request db {:ask [api/get! :health-checks] :path [:checks] :rd? true
                  :pick #(vec (:checks %))}))

(defn- health-body [{:keys [name node-kind node-key kind query op value]}]
  (cond-> {:name name :node-kind node-kind :node-key node-key :kind kind
           :expects (if (= op "nonzero")
                      {:op :nonzero}
                      {:op (keyword op) :value (js/parseFloat value)})
           :enabled true}
    (= kind "custom") (assoc :query query)))

(defn- confirm-delete [title ok]
  {:fx [[:confirm {:ask {:title title :danger? true} :ok ok}]]})

(defn- select [db id]
  (let [db (assoc db :focus (str id))]
    (if (= :health (:top-tab db))
      (rq/request db {:ask  [api/get! :health-check-results {:id id}]
                      :path [:check-rows id :results] :pick #(vec (:results %))})
      (let [a (rq/request (assoc-in db [:proposals id]
                                    {:column :open :items rd/loading :counts nil})
                          {:ask  [api/get! :automation-runs {:id id}]
                           :path [:rows id :runs] :pick #(vec (:runs %))})]
        (update (load-proposals (:db a) id false) :fx #(into (vec (:fx a)) %))))))

(machine/reg-event :ops/tab
                   (fn [{:keys [db]} [_ tab]] {:db (assoc db :top-tab tab :focus nil)}))

(machine/reg-event :ops/select (fn [{:keys [db]} [_ id]] (select db id)))

(machine/reg-event :ops/deselect (fn [{:keys [db]} _] {:db (assoc db :focus nil)}))

(machine/reg-event :ops/focus
                   (fn [{:keys [db]} [_ id]] (select (assoc db :top-tab :automate) id)))

(machine/reg-event :ops.automations/enter
                   (fn [{:keys [db]} _]
                     (load-automations (assoc db :automations rd/loading :rows {} :proposals {}
                                              :automation-form nil))))

(machine/reg-event :ops.automations/reload (fn [{:keys [db]} _] (load-automations db)))

(machine/reg-event :ops.automation-form/toggle
                   (fn [{:keys [db]} _]
                     {:db (assoc db :automation-form
                                 (when-not (:automation-form db) blank-automation-form))}))

(machine/reg-event :ops.automation-form/field
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:automation-form k] v)}))

(machine/reg-event :ops.automation-form/submit (fn [{:keys [db]} _] (submit-automation db)))

(machine/reg-event :ops.row/run
                   (fn [{:keys [db]} [_ id]]
                     (running db id false [api/post! :automation-run {:id id} nil])))

(machine/reg-event :ops.row/preview
                   (fn [{:keys [db]} [_ id]] (running db id true [api/preview-automation id])))

(machine/reg-event :ops.row/toggle-pause (fn [_ [_ a]] (toggle-pause a)))

(machine/reg-event :ops.row/mute
                   (fn [_ [_ id muted]]
                     {:fx [(reloading [api/post! :automation-mute {:id id} {:muted (not muted)}]
                                      :ops.automations/reload)]}))

(machine/reg-event :ops.row/subscribe
                   (fn [_ [_ id on?]]
                     {:fx [(reloading (if on?
                                        [api/delete! :automation-subscribe {:id id}]
                                        [api/post! :automation-subscribe {:id id} nil])
                                      :ops.automations/reload)]}))

(machine/reg-event :ops.row/delete
                   (fn [_ [_ id nm]]
                     (confirm-delete (i18n/t :ui/ops-automation-delete {:name nm})
                                     [:ops.row/delete-confirmed id])))

(machine/reg-event :ops.row/delete-confirmed
                   (fn [_ [_ id]]
                     {:fx [(reloading [api/delete! :automation-of {:id id}]
                                      :ops.automations/reload)]}))

(machine/reg-event :ops.proposals/column
                   (fn [{:keys [db]} [_ aid c]]
                     (load-proposals (update-in db [:proposals aid] assoc :column c
                                                :items rd/loading)
                                     aid false)))

(machine/reg-event :ops.proposals/load
                   (fn [{:keys [db]} [_ aid more?]] (load-proposals db aid more?)))

(machine/reg-event :ops.proposals/loaded
                   (fn [{:keys [db]} [_ aid more? r]] (proposals-loaded db aid more? r)))

(machine/reg-event :ops.proposals/failed
                   (fn [{:keys [db]} [_ aid reason]]
                     {:db (update-in db [:proposals aid :items] rd/after-failure reason)}))

(machine/reg-event :ops.card/toggle-log
                   (fn [{:keys [db]} [_ aid pid]]
                     {:db (update-in db [:proposals aid :cards pid :log?] not)}))

(machine/reg-event :ops.card/edit
                   (fn [{:keys [db]} [_ aid pid params args]]
                     (edit-start db aid pid params args)))

(machine/reg-event :ops.card/edit-field
                   (fn [{:keys [db]} [_ aid pid p v]]
                     {:db (assoc-in db [:proposals aid :cards pid :editing p] v)}))

(machine/reg-event :ops.card/close
                   (fn [{:keys [db]} [_ aid pid]]
                     {:db (assoc-in db [:proposals aid :cards pid :editing] nil)}))

(machine/reg-event :ops.card/accept
                   (fn [{:keys [db]} [_ aid pid args]] (accept db aid pid args)))

(machine/reg-event :ops.card/reject
                   (fn [{:keys [db]} [_ aid pid]]
                     (decide db aid pid [api/post! :automation-proposal-reject
                                         {:id aid :proposal pid} nil])))

(machine/reg-event :ops.card/decided
                   (fn [{:keys [db]} [_ aid pid r]] (decided db aid pid r)))

(machine/reg-event :ops.checks/enter
                   (fn [{:keys [db]} _]
                     (load-checks (assoc db :checks rd/loading :check-rows {} :health-form nil
                                         :run-all-busy false))))

(machine/reg-event :ops.checks/reload (fn [{:keys [db]} _] (load-checks db)))

(machine/reg-event :ops.checks/run-all
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask  [api/post! :health-run-all] :busy [:run-all-busy]
                                     :then [[:ops.checks/reload]]})))

(machine/reg-event :ops.health-form/toggle
                   (fn [{:keys [db]} _]
                     {:db (assoc db :health-form (when-not (:health-form db) blank-health-form))}))

(machine/reg-event :ops.health-form/field
                   (fn [{:keys [db]} [_ k v]]
                     {:db (update db :health-form
                                  #(cond-> (assoc % k v) (= k :node-kind) (assoc :node-key "")))}))

(machine/reg-event :ops.health-form/submit
                   (fn [{:keys [db]} _]
                     (saving-form db :health-form
                                  [api/post! :health-checks (health-body (:health-form db))]
                                  [[:ops.health-form/created]])))

(machine/reg-event :ops.health-form/created
                   (fn [_ [_ c]]
                     {:fx [(rq/ask (form-request :health-form
                                                [api/post! :health-check-run {:id (:id c)} nil]
                                                [[:rf/set [:health-form] nil]
                                                 [:ops.checks/reload]]))]}))

(machine/reg-event :ops.check/run
                   (fn [{:keys [db]} [_ id]]
                     (rq/request db {:ask  [api/post! :health-check-run {:id id} nil]
                                     :busy [:check-rows id :busy]
                                     :then [[:ops.checks/reload]]})))

(machine/reg-event :ops.check/delete
                   (fn [_ [_ id nm]]
                     (confirm-delete (i18n/t :ui/ops-check-delete {:name nm})
                                     [:ops.check/delete-confirmed id])))

(machine/reg-event :ops.check/delete-confirmed
                   (fn [_ [_ id]]
                     {:fx [(reloading [api/delete! :health-check-of {:id id}]
                                      :ops.checks/reload)]}))
