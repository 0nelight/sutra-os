(ns sutra-ui.apps.developer-apps.events
  (:require [sutra-ui.ui.overlay :as overlay]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.kit.slug :refer [slug]]
            [sutra-ui.state.machine :as machine]))

(def blank-app
  {:display-name ""
   :status       :draft
   :project      nil
   :repository   nil
   :client       {:grant-types [:authorization-code] :redirect-uris [] :token-ttl 3600}
   :scope        {:object-types [] :actions [] :operations [:objects :object-types]}
   :sdk          {:namespace ""}
   :permissions  {:roles []}
   :hosting      {:subdomain "" :root "dist" :csp {:script-src ["'self'"]}}
   :metrics      true})

(defn app-keys [db declared]
  (sort (distinct (concat (keys declared) (keys (:drafts db))))))

(defn app-of [db declared k]
  (or (get-in db [:drafts k]) (get declared k)))

(defn restored
  [drafts stored declared]
  (if (map? stored)
    (merge (into {} (for [[k d] stored
                          :let [sec (get-in declared [k :client-secret])]]
                      [k (cond-> d sec (assoc :client-secret sec))]))
           drafts)
    drafts))

(defn- edited [db declared k path v]
  (update-in db [:drafts k]
             (fn [cur] (assoc-in (or cur (get declared k) blank-app) path v))))

(defn- toggled [db declared k path v]
  (let [cur (vec (get-in (app-of db declared k) path []))]
    (edited db declared k path (if (some #{v} cur) (vec (remove #{v} cur)) (conj cur v)))))

(defn fresh-key
  [nm taken]
  (let [s (slug nm)]
    (when (seq s)
      (loop [k (keyword s) i 2]
        (if (some #{k} taken) (recur (keyword (str s "-" i)) (inc i)) k)))))

(machine/reg-event :dev-apps/restore
                   (fn [{:keys [db]} [_ stored declared]]
                     (when-not (:restored? db)
                       {:db (-> db (assoc :restored? true)
                                (update :drafts restored stored declared))})))

(machine/reg-event :dev-apps/select
                   (fn [{:keys [db]} [_ k]] {:db (assoc db :selected k)}))

(machine/reg-event :dev-apps/open
                   (fn [{:keys [db]} [_ k]] {:db (assoc db :selected k :section :overview)}))

(machine/reg-event :dev-apps/section
                   (fn [{:keys [db]} [_ id]] {:db (assoc db :section id)}))

(machine/reg-event :dev-apps/edit
                   (fn [{:keys [db]} [_ declared k path v]]
                     {:db (edited db declared k path v)}))

(machine/reg-event :dev-apps/toggle
                   (fn [{:keys [db]} [_ declared k path v]]
                     {:db (toggled db declared k path v)}))

(machine/reg-event :dev-apps/discard
                   (fn [{:keys [db]} [_ k]] {:db (update db :drafts dissoc k)}))

(machine/reg-event :dev-apps/secret
                   (fn [{:keys [db]} _] {:db (update db :secret-shown? not)}))

(machine/reg-event :dev-apps/role-typed
                   (fn [{:keys [db]} [_ s]] {:db (assoc db :new-role s)}))

(machine/reg-event :dev-apps/role-add
                   (fn [{:keys [db]} [_ declared k]]
                     (let [r (:new-role db "")]
                       (when-not (str/blank? r)
                         {:db (-> (toggled db declared k [:permissions :roles] (keyword r))
                                  (assoc :new-role ""))}))))

(machine/reg-event :dev-apps/new
                   (fn [_ [_ declared]]
                     {:fx [[:call {:call [overlay/prompt!
                                          {:title (i18n/t :ui/devcon-new-app)
                                           :label (i18n/t :ui/devcon-display-name)
                                           :placeholder (i18n/t :ui/devcon-new-app-placeholder)
                                           :confirm-label (i18n/t :ui/create)}]
                                   :ok [:dev-apps/named declared]}]]}))

(machine/reg-event :dev-apps/named
                   (fn [{:keys [db]} [_ declared nm]]
                     (when-let [k (fresh-key nm (app-keys db declared))]
                       {:db (-> db
                                (assoc-in [:drafts k]
                                          (assoc blank-app :display-name (str/trim nm)))
                                (assoc :selected k :section :overview))})))

(machine/reg-sub :dev-apps/db (fn [db _] db))
