(ns sutra-ui.apps.explorer.events
  (:require [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra.types.rid :as rid]
            [sutra-ui.api :as api]
            [sutra-ui.apps.explorer.address :as address]
            [sutra-ui.apps.explorer.model :as explorer-model]
            [sutra-ui.apps.explorer.object-set :as oset]
            [sutra-ui.routing :as routing]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]))

(defn- fail [sentence] [:invoke [shell/fail! sentence]])

(defn- set-of [db ot] (or (get-in db [:set ot]) address/blank-set))

(defn- query-of [db ot] (oset/server-query ot (set-of db ot)))

(defn- load-set
  [db ot]
  {:db (update db :reads dissoc ot)
   :fx [[:invoke [panel/refresh-panel-instances! ot
                  {:limit oset/page-size
                   :query (oset/server-query ot (or (get-in db [:set ot]) {}))}]]]})

(defn change-set
  [db ot f args]
  (let [db' (update-in db [:set ot] #(apply f (or % address/blank-set) args))]
    {:db db'
     :fx [(when (not= (query-of db ot) (query-of db' ot))
            [:after {:key [:reload ot] :ms 300 :event [:explorer.set/load ot]}])]}))

(defn- with-fx [effects & fx] (update effects :fx into fx))

(defn- requesting
  [{:keys [db fx]} req]
  (update (rq/request db req) :fx #(into (vec fx) %)))

(machine/reg-event :explorer.set/load (fn [{:keys [db]} [_ ot]] (load-set db ot)))

(machine/reg-event :explorer.set/change
                   (fn [{:keys [db]} [_ ot f & args]] (change-set db ot f args)))

(machine/reg-event :explorer.restrict/clear
                   (fn [{:keys [db]} [_ ot]]
                     (change-set (dissoc db :object-set) ot dissoc [:restrict])))

(machine/reg-event :explorer/drawer
                   (fn [{:keys [db]} [_ open?]]
                     {:db (update db :drawer? #(if (some? open?) open? (not %)))}))

(machine/reg-event :explorer/home
                   (fn [{:keys [db]} _] {:db (assoc db :object-type nil)}))

(machine/reg-event :explorer.read/need
                   (fn [{:keys [db]} [_ ot q]]
                     (when-not (contains? (get-in db [:reads ot]) q)
                       (rq/request db {:ask   [api/post! :ontology-query q] :path [:reads ot q]
                                       :error [:reads ot q :error] :dedup [::read ot q]}))))

(def ^:private no-saved-sets {:object-sets [] :temporary []})

(machine/reg-event :explorer.saved/ensure
                   (fn [{:keys [db]} _]
                     (when-not (:saved-sets db)
                       (rq/request (assoc db :saved-sets no-saved-sets)
                                   {:ask  [api/get! :object-sets] :path [:saved-sets]
                                    :pick (fn [r] {:object-sets (vec (:object-sets r))
                                                   :temporary   (vec (:temporary r))})}))))

(defn known-set
  [db entry]
  (let [{:keys [object-sets temporary]} (:saved-sets db)]
    (merge (some #(when (= (:rid entry) (:rid %)) %) (concat object-sets temporary)) entry)))

(defn- restrict-to-saved
  [db entry rows ot]
  (load-set (-> db
                (assoc :object-type ot :object-set (:rid entry))
                (assoc-in [:set ot :filters] [])
                (assoc-in [:set ot :search] "")
                (assoc-in [:set ot :restrict]
                          {:ids   (set (map #(str (:id %)) rows))
                           :label (i18n/t :ui/ox-saved-restrict
                                          {:name (explorer-model/saved-label entry)})}))
            ot))

(defn- file-ref [file] (select-keys file [:rid :kind :name]))

(defn- open-failed [nm] #(i18n/t :ui/ox-saved-failed {:name nm :error %}))

(machine/reg-event :explorer/open-saved
                   (fn [{:keys [db]} [_ entry]]
                     (let [rid (:rid entry)]
                       (rq/request (assoc db :drawer? false)
                                   (if (= :exploration (rid/rid-kind rid))
                                     {:ask    [api/get! :exploration {:id rid}]
                                      :then   [[:explorer.file/loaded]]
                                      :report (open-failed rid)}
                                     {:ask    [api/post! :ontology-query {:object-set rid}]
                                      :then   [[:explorer.set-file/loaded entry]]
                                      :report (open-failed (explorer-model/saved-label
                                                            (known-set db entry)))})))))

(machine/reg-event :explorer.file/loaded
                   (fn [{:keys [db]} [_ {:keys [rid object-type state] :as file}]]
                     (let [ot (keyword (name object-type))]
                       (if (= "list" (name (:kind file)))
                         (rq/request db {:ask  [api/post! :ontology-query {:object-set rid}]
                                         :then [[:explorer.list/opened file]]})
                         (load-set (-> db
                                       (assoc :object-type ot :object-set rid)
                                       (assoc-in [:set ot]
                                                 (merge address/blank-set state
                                                        {:file (file-ref file)})))
                                   ot)))))

(machine/reg-event :explorer.list/opened
                   (fn [{:keys [db]} [_ file resp]]
                     (let [ot (keyword (name (:object-type file)))]
                       (restrict-to-saved
                        (update-in db [:set ot] merge
                                   (select-keys (:state file) [:filters :sort :viz :columns :chart])
                                   {:file (file-ref file)})
                        file (:rows resp) ot))))

(machine/reg-event :explorer.set-file/loaded
                   (fn [{:keys [db]} [_ entry resp]]
                     (let [entry (known-set db entry)
                           rows  (:rows resp)
                           ot    (or (some-> (:object-type resp) catalog/name-of)
                                     (some-> (first rows) :object-type catalog/name-of))]
                       (if (nil? ot)
                         {:fx [(fail (i18n/t :ui/ox-saved-no-type
                                             {:name (explorer-model/saved-label entry)}))]}
                         (restrict-to-saved db entry rows ot)))))

(machine/reg-event :explorer/select-type
                   (fn [{:keys [db]} [_ ot]]
                     {:db (-> db (assoc :object-type ot :drawer? false) (dissoc :object-set))
                      :fx [[:invoke [explorer-model/ensure-explorer-rows! ot]]]}))

(machine/reg-event :explorer/open-with-keywords
                   (fn [{:keys [db]} [_ ot q]]
                     (with-fx (load-set (-> db
                                            (dissoc :object-set)
                                            (assoc-in [:set ot :search] q)
                                            (assoc-in [:set ot :filters] [])
                                            (update-in [:set ot] dissoc :restrict)
                                            (assoc :object-type ot :drawer? false))
                                        ot)
                       [:invoke [explorer-model/ensure-explorer-rows! ot]])))

(defn- search-groups
  [resp]
  (vec (for [{:keys [type count rows]} (:groups resp)]
         {:t (catalog/name-of type) :n count :rows rows})))

(machine/reg-event :explorer.home/type
                   (fn [{:keys [db]} [_ v]]
                     {:db (update db :home assoc :q v :hits nil)
                      :fx [(if (>= (count (str/trim v)) 2)
                             [:after {:key :home-search :ms 200 :event [:explorer.home/search v]}]
                             [:cancel :home-search])]}))

(machine/reg-event :explorer.home/hand
                   (fn [{:keys [db]} [_ kw]]
                     (when (not= kw (get-in db [:home :handed]))
                       {:db (assoc-in db [:home :handed] kw)
                        :fx [[:dispatch [:explorer.home/type kw]]]})))

(machine/reg-event :explorer.home/search
                   (fn [{:keys [db]} [_ v]]
                     (rq/request db {:ask     [api/get! :instances-search {:q v :type nil}]
                                     :path    [:home :hits] :pick search-groups
                                     :current [[:home :q] v]
                                     :else    [[:rf/set [:home :hits] []]]})))

(machine/reg-event :explorer.home/leave
                   (fn [{:keys [db]} _] {:db (dissoc db :home) :fx [[:cancel :home-search]]}))

(machine/reg-event :explorer/pivot
                   (fn [{:keys [db]} [_ from-type {:keys [side label peer-type]} chip]]
                     (let [view-state (set-of db from-type)
                           defn-q     (oset/wire-query from-type view-state)
                           from-q     (oset/query-base from-type view-state)
                           q          (update (or defn-q from-q) :around (fnil conj []) side)]
                       (requesting
                        {:db (-> db
                                 (assoc :object-type peer-type)
                                 (assoc-in [:set peer-type :restrict]
                                           (cond-> {:ids #{} :pending? true :label chip :side side}
                                             defn-q (assoc :query q)))
                                 (assoc-in [:set peer-type :filters] [])
                                 (assoc-in [:set peer-type :search] ""))
                         :fx [[:dispatch [:explorer.set/load peer-type]]]}
                        {:ask    [api/post! :ontology-query q]
                         :then   [[:explorer.pivot/loaded peer-type]]
                         :else   [(fn [_] [:explorer.set/change peer-type dissoc :restrict])]
                         :report #(i18n/t :ui/ox-pivot-failed {:link label :error %})}))))

(defn- arrived [ids]
  (fn [restrict] (when restrict (-> restrict (assoc :ids ids) (dissoc :pending?)))))

(machine/reg-event :explorer.pivot/loaded
                   (fn [{:keys [db]} [_ ot resp]]
                     (change-set db ot update
                                 [:restrict (arrived (set (map (comp str :id) (:rows resp))))])))

(defn- saving [db ot c kind after]
  (requesting (change-set db ot assoc [:saved nil])
              {:ask  c :busy [:set ot :saving?] :error [:set ot :saved :error]
               :then [[:explorer.save/saved ot kind after]]}))

(machine/reg-event :explorer.save/file
                   (fn [{:keys [db]} [_ ot {:keys [kind name description private? project
                                                    folder]}]]
                     (saving db ot
                             [api/post! :explorations
                              (cond-> (assoc (oset/file-body ot (set-of db ot))
                                             :object-type ot :kind kind :name name
                                             :description description)
                                (not private?) (assoc :project project))]
                             kind
                             (when (and (not private?) (seq folder)) {:folder folder}))))

(machine/reg-event :explorer.save/changes
                   (fn [{:keys [db]} [_ ot {:keys [rid kind]}]]
                     (saving db ot
                             [api/put! :exploration {:id rid} (oset/file-body ot (set-of db ot))]
                             kind
                             (when (= "list" (name kind)) {:list rid}))))

(machine/reg-event :explorer.save/saved
                   (fn [{:keys [db]} [_ ot kind {:keys [folder list]} resp]]
                     (with-fx (change-set db ot assoc
                                          [:save-dialog nil
                                           :saved {:rid (str (:rid resp)) :kind kind}
                                           :file (file-ref resp)])
                       [:invoke [ks/refresh! :explorations]]
                       (when folder
                         (rq/ask {:ask    [ks/kompass-place! (str "exploration/" (:rid resp))
                                           folder nil]
                                  :report true}))
                       (when list
                         (rq/ask {:ask  [api/post! :ontology-query {:object-set list}]
                                  :then [[:explorer.list/reloaded ot]]})))))

(machine/reg-event :explorer.list/reloaded
                   (fn [{:keys [db]} [_ ot resp]]
                     (with-fx (change-set db ot assoc-in
                                          [[:restrict :ids]
                                           (set (map #(str (:id %)) (:rows resp)))])
                       [:dispatch [:explorer.set/load ot]])))

(machine/reg-event :explorer.compare/snapshot
                   (fn [{:keys [db]} [_ ot label]]
                     (rq/request db {:ask    [api/post! :ontology-query
                                              (merge (oset/query-base ot (set-of db ot))
                                                     (oset/id-projection ot))]
                                     :then   [[:explorer.compare/taken ot label]]
                                     :report true})))

(machine/reg-event :explorer.compare/taken
                   (fn [{:keys [db]} [_ ot label resp]]
                     (let [rows (:rows resp)]
                       (change-set db ot assoc [:compare-a {:ids   (set (map #(str (:id %)) rows))
                                                            :label label
                                                            :count (count rows)}]))))

(machine/reg-event :explorer/export
                   (fn [{:keys [db]} [_ ot f]]
                     (rq/request db {:ask    [api/post! :ontology-query
                                              (oset/query-base ot (set-of db ot))]
                                     :then   [[:explorer/exported ot f]]
                                     :report true})))

(machine/reg-event :explorer/exported
                   (fn [_ [_ ot f resp]] {:fx [[:invoke [f ot (:rows resp)]]]}))

(machine/reg-event :explorer.seed/type
                   (fn [{:keys [db]} [_ v]]
                     (if (< (count (str/trim v)) 2)
                       {:db (update db :seed assoc :q v :results nil) :fx [[:cancel :seed-search]]}
                       {:db (assoc-in db [:seed :q] v)
                        :fx [[:after {:key   :seed-search :ms 220
                                      :event [:explorer.seed/search v]}]]})))

(machine/reg-event :explorer.seed/now
                   (fn [{:keys [db]} _]
                     {:fx [[:cancel :seed-search]
                           [:dispatch [:explorer.seed/search (get-in db [:seed :q] "")]]]}))

(machine/reg-event :explorer.seed/search
                   (fn [{:keys [db]} [_ q]]
                     (when-not (str/blank? q)
                       (rq/request db {:ask  [api/get! :instances-search {:q q :type nil}]
                                       :busy [:seed :busy?] :path [:seed :results]
                                       :pick (fn [resp]
                                               (vec (for [{t :type rows :rows} (:groups resp)
                                                          row rows]
                                                      {:type-ref t :row row})))
                                       :else [[:explorer.seed/failed]]}))))

(machine/reg-event :explorer.seed/failed
                   (fn [_ [_ reason]]
                     {:fx [[:invoke [js/console.error "[seed-search]" reason]]]}))

(machine/reg-event :explorer.seed/done
                   (fn [{:keys [db]} [_ clear-q?]]
                     {:db (update db :seed #(cond-> (assoc % :results nil)
                                              clear-q? (assoc :q "")))}))

(defn temp-set-href
  [{:keys [object-type query]}]
  (-> (api/post! :object-sets-temp (assoc query :find object-type))
      (.then (fn [{set-rid :rid}] (routing/href :object-workspace {:object-set set-rid})))))
