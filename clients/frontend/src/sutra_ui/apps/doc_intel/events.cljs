(ns sutra-ui.apps.doc-intel.events
  (:require [sutra-ui.api :as api]
            [sutra-ui.apps.doc-intel.model :as m]
            [sutra-ui.storage :as storage]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]))

(def stored-keys
  {:strategies "sutra.doc-intel.strategies" :source "sutra.doc-intel.source"})

(defn- remember
  [st st']
  (vec (for [k (keys stored-keys)
             :when (and (some? (k st')) (not= (k st) (k st')))]
         [:invoke [storage/set-json! (stored-keys k) (k st')]])))

(defn- to
  ([st st'] (to st st' nil))
  ([st st' fx] {:db st' :fx (into (remember st st') fx)}))

(defn- body [st s]
  {:source (:source st) :address (:address (:item st)) :page (:page st)
   :strategy (m/wire-strategy s)})

(defn- error-at [path] (fn [s] [:rf/set path {:error s}]))

(defn- load-sources []
  (rq/ask {:ask  [api/get! :doc-intel-sources] :then [[:doc-intel/sources]]
           :else [(error-at [:sources])]}))

(machine/reg-event :doc-intel/enter
                   (fn [{:keys [db]} [_ strategies source]]
                     (cond
                       (nil? (:sources db))
                       {:db (assoc (m/initial (some-> strategies vec)) :stored-source source)
                        :fx [(load-sources)]}
                       (= :loading (:sources db)) {:fx [(load-sources)]})))

(machine/reg-event :doc-intel/sources
                   (fn [{:keys [db]} [_ {:keys [sources models]}]]
                     (let [st' (-> db
                                   (assoc :sources (vec sources) :models (vec models))
                                   (update :judge #(or % (first models)))
                                   m/with-default-strategies)
                           s   (and (nil? (:source st'))
                                    (or (some #{(:stored-source db)} sources) (first sources)))]
                       (to db st' (when s [[:dispatch [:doc-intel/source s]]])))))

(machine/reg-event :doc-intel/source
                   (fn [{:keys [db]} [_ source]]
                     (let [k   (m/source-key source)
                           st' (assoc db :source source :q "")]
                       (if (sequential? (get-in db [:items k]))
                         (to db st')
                         (to db (assoc-in st' [:items k] :loading)
                             [(rq/ask {:ask  [api/get! :doc-intel-items source]
                                       :path [:items k] :pick #(vec (:items %))
                                       :else [(error-at [:items k])]})])))))

(machine/reg-event :doc-intel/open-item
                   (fn [{:keys [db]} [_ it]] {:db (m/open-item db it)}))

(machine/reg-event :doc-intel/query (fn [{:keys [db]} [_ q]] {:db (assoc db :q q)}))

(machine/reg-event :doc-intel/page (fn [{:keys [db]} [_ n]] {:db (m/turn-page db n)}))

(machine/reg-event :doc-intel/judge (fn [{:keys [db]} [_ j]] {:db (assoc db :judge j)}))

(machine/reg-event :doc-intel/focus
                   (fn [{:keys [db]} [_ id]]
                     (when-not (= id (:focus db)) {:db (assoc db :focus id)})))

(machine/reg-event :doc-intel/toggle-raw
                   (fn [{:keys [db]} [_ id]]
                     {:db (update db :raw (fn [r] ((if (contains? r id) disj conj) r id)))}))

(machine/reg-event :doc-intel/toggle-editing
                   (fn [{:keys [db]} [_ id]]
                     {:db (update db :editing (fn [e] (when-not (= e id) id)))}))

(machine/reg-event :doc-intel/add-strategy
                   (fn [{:keys [db]} [_ kind]] (to db (m/add-strategy db kind))))

(machine/reg-event :doc-intel/remove-strategy
                   (fn [{:keys [db]} [_ id]] (to db (m/remove-strategy db id))))

(machine/reg-event :doc-intel/edit-strategy
                   (fn [{:keys [db]} [_ id k v]] (to db (m/edit-strategy db id k v))))

(defn- run [db k c]
  (rq/request db {:ask  c :busy [k m/running] :path k :pick m/finished
                  :else [(fn [s] [:rf/set k (m/failed s)])]}))

(machine/reg-event :doc-intel/extract
                   (fn [{:keys [db]} [_ id]]
                     (let [s (m/strategy-of db id) k [:results (m/result-key db s)]]
                       (when (and s (:item db) (not= :running (:status (get-in db k))))
                         (run db k [api/post! :doc-intel-extract (body db s)])))))

(machine/reg-event :doc-intel/run-all
                   (fn [{:keys [db]} _]
                     {:fx (mapv #(vector :dispatch [:doc-intel/extract (:id %)])
                                (:strategies db))}))

(machine/reg-event :doc-intel/evaluate
                   (fn [{:keys [db]} [_ id]]
                     (let [s     (m/strategy-of db id)
                           judge (:judge db)
                           md    (get-in (m/result db s) [:row :markdown])]
                       (when (and judge md)
                         (run db [:evals (m/eval-key db s judge)]
                              [api/post! :doc-intel-evaluate
                               (assoc (dissoc (body db s) :strategy) :markdown md
                                      :model judge)])))))

(machine/reg-event :doc-intel/evaluate-all
                   (fn [{:keys [db]} _]
                     {:fx (mapv #(vector :dispatch [:doc-intel/evaluate (:id %)])
                                (:strategies db))}))

(machine/reg-event :doc-intel/deployment
                   (fn [{:keys [db]} [_ id k v]]
                     {:db (assoc-in db [:deployments id]
                                    (assoc (m/deployment db id) k v :result nil))}))

(machine/reg-event :doc-intel/deploy
                   (fn [{:keys [db]} [_ id]]
                     (let [s (m/strategy-of db id)
                           {:keys [name repository]} (m/deployment db id)]
                       (run db [:deployments id :result]
                            [api/post! :doc-intel-deploy
                             {:source (:source db) :name name :repository repository
                              :strategy (m/wire-strategy s)}]))))
