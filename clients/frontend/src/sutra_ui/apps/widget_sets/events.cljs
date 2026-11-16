(ns sutra-ui.apps.widget-sets.events
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(defn- req [ask & {:as more}] (merge {:ask ask :error [:problem]} more))

(def ^:private refresh-list [:invoke [ks/refresh! :widget-sets]])

(defn- detail [nm] (req [api/get! :widget-set {:name nm}] :path [:detail] :pick :widget-set))

(defn read-b64
  [file]
  (js/Promise.
   (fn [ok fail]
     (let [rd (js/FileReader.)]
       (set! (.-onload rd) #(ok (second (str/split (.-result rd) #"," 2))))
       (set! (.-onerror rd) #(fail (.-error rd)))
       (.readAsDataURL rd file)))))

(machine/reg-event :widget-sets/enter (fn [_ _] {:fx [refresh-list]}))

(machine/reg-event :widget-sets/set (fn [{:keys [db]} [_ k v]] {:db (assoc db k v)}))

(machine/reg-event :widget-sets/select
                   (fn [{:keys [db]} [_ nm]]
                     (let [db (assoc db :selected nm :detail nil :problem nil)]
                       (if nm (rq/request db (detail nm)) {:db db}))))

(machine/reg-event :widget-sets/toggle-new
                   (fn [{:keys [db]} _] {:db (-> db (update :adding? not) (assoc :draft {}))}))

(machine/reg-event :widget-sets/draft
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:draft k] v)}))

(machine/reg-event :widget-sets/create
                   (fn [{:keys [db]} _]
                     (let [{:keys [name project description]} (:draft db)
                           nm (str/trim (str name))]
                       {:fx [(rq/ask (req [api/post! :widget-sets {:name nm :project project
                                                                   :description description}]
                                          :then [[:widget-sets/created nm]]))]})))

(machine/reg-event :widget-sets/created
                   (fn [{:keys [db]} [_ nm]]
                     {:db (assoc db :adding? false :draft {})
                      :fx [refresh-list [:dispatch [:widget-sets/select nm]]]}))

(machine/reg-event :widget-sets/delete
                   (fn [_ [_ nm]]
                     {:fx [[:confirm {:ask {:title (i18n/t :ui/widget-sets-delete-q {:name nm})}
                                      :ok  [:widget-sets/remove nm]}]]}))

(machine/reg-event :widget-sets/remove
                   (fn [_ [_ nm]]
                     {:fx [(rq/ask (req [api/delete! :widget-set {:name nm}]
                                        :then [[:widget-sets/select nil]] :fx [refresh-list]))]}))

(machine/reg-event :widget-sets.release/upload
                   (fn [_ [_ nm file]]
                     {:fx [(rq/ask (req [read-b64 file] :then [[:widget-sets.release/read nm]]))]}))

(machine/reg-event :widget-sets.release/read
                   (fn [{:keys [db]} [_ nm b64]]
                     {:fx [(rq/ask (req [api/post! :widget-set-releases {:name nm}
                                         {:version (:upload-version db "") :zip-b64 b64}]
                                        :then [[:rf/set [:problem] nil]]
                                        :fx [refresh-list (rq/ask (detail nm))]))]}))

(machine/reg-event :widget-sets.playground/load
                   (fn [{:keys [db]} [_ nm version]]
                     (rq/request
                      (update db :playground #(-> % (assoc :for [nm version]) (dissoc :bundle)))
                      (req [api/get! :widget-set-bundle {:name nm :version version}]
                           :current [[:playground :for] [nm version]]
                           :then [[:widget-sets.playground/bundle]]))))

(machine/reg-event :widget-sets.playground/bundle
                   (fn [{:keys [db]} [_ r]]
                     (let [b (reader/read-string (:bundle-edn r))]
                       {:db (update db :playground assoc :bundle b
                                    :widget (first (keys (get-in b [:manifest :widgets]))))})))

(machine/reg-event :widget-sets.playground/widget
                   (fn [{:keys [db]} [_ id]] {:db (assoc-in db [:playground :widget] id)}))

(machine/reg-event :widget-sets.playground/value
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:playground :values k] v)}))

(machine/reg-event :widget-sets.playground/emitted
                   (fn [{:keys [db]} [_ event updates]]
                     {:db (update db :playground
                                  #(-> % (update :values merge updates)
                                       (update :log (fnil conj []) [event updates])))}))

(machine/reg-sub :widget-sets/at (fn [db [_ k]] (get db k)))
