(ns sutra-ui.apps.admin-settings.events
  (:require [clojure.string :as str]
            [sutra.kit.remote :as rd]
            [sutra.types.feedback :as b]
            [sutra-ui.api :as api]
            [sutra-ui.platform :as platform]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.staging :as staging]))

(defn- saving
  [k req]
  (assoc req :busy [k :busy] :error [k :error]))

(defn- loading
  [db k ask]
  (rq/request (update-in db [k :data] #(or % rd/loading)) {:ask ask :path [k :data] :rd? true}))

(machine/reg-event :admin/tab
                   (fn [{:keys [db]} [_ k]] {:db (-> db (dissoc :sitzung) (assoc :tab k))}))

(machine/reg-event :admin/close (fn [{:keys [db]} [_ & ks]] {:db (apply dissoc db ks)}))

(machine/reg-event :admin/all-sessions (fn [{:keys [db]} _] {:db (dissoc db :sitzung)}))

(machine/reg-event :admin.platform/enter
                   (fn [{:keys [db]} _]
                     {:db (assoc db :platform {:draft {}})
                      :fx [[:invoke [platform/refresh!]]
                           [:invoke [principals/refresh-principals!]]]}))

(machine/reg-event :admin.platform/title
                   (fn [{:keys [db]} [_ v]] {:db (assoc-in db [:platform :draft :title] v)}))

(machine/reg-event :admin.platform/logo
                   (fn [{:keys [db]} [_ size data]]
                     {:db (assoc-in db [:platform :draft :logo size] data)}))

(machine/reg-event :admin.platform/discard
                   (fn [{:keys [db]} _] {:db (assoc-in db [:platform :draft] {})}))

(machine/reg-event :admin.platform/save
                   (fn [{:keys [db]} _]
                     (let [d (get-in db [:platform :draft])]
                       (rq/request (update db :platform assoc :error nil :saved false)
                                   (saving :platform
                                           {:ask  [platform/save!
                                                   (cond-> d (:title d) (update :title str/trim))]
                                            :then [[:rf/set [:platform :saved] true]
                                                   [:rf/set [:platform :draft] {}]]})))))

(defn- move [v i d]
  (let [j (+ i d)]
    (if (< -1 j (count v)) (assoc v i (v j) j (v i)) v)))

(defn edit-overrides
  [ovs [op i x]]
  (let [ovs (vec ovs)]
    (case op
      :url    (assoc-in ovs [i :url] x)
      :groups (assoc-in ovs [i :groups] x)
      :up     (move ovs i -1)
      :down   (move ovs i 1)
      :remove (vec (concat (subvec ovs 0 i) (subvec ovs (inc i))))
      :add    (conj ovs {:groups [] :url ""}))))

(defn- home [r] {:loaded r :draft (:home-page r)})

(machine/reg-event :admin.home/enter
                   (fn [{:keys [db]} _]
                     (rq/request (assoc db :home {})
                                 {:ask   [api/get! :home-page] :path [:home] :pick home
                                  :error [:home :error]})))

(machine/reg-event :admin.home/default
                   (fn [{:keys [db]} [_ v]] {:db (assoc-in db [:home :draft :default] v)}))

(machine/reg-event :admin.home/organization
                   (fn [{:keys [db]} [_ id v]]
                     {:db (update-in db [:home :draft :organizations]
                                     #(-> % (dissoc (keyword id)) (assoc id v)))}))

(machine/reg-event :admin.home/override
                   (fn [{:keys [db]} [_ & op]]
                     {:db (update-in db [:home :draft :overrides] edit-overrides op)}))

(machine/reg-event :admin.home/discard
                   (fn [{:keys [db]} _]
                     {:db (assoc-in db [:home :draft] (get-in db [:home :loaded :home-page]))}))

(machine/reg-event :admin.home/save
                   (fn [{:keys [db]} _]
                     (rq/request (update db :home assoc :error nil :saved false)
                                 (saving :home {:ask  [api/post! :home-page
                                                       (get-in db [:home :draft])]
                                                :path [:home]
                                                :pick #(assoc (home %) :saved true)}))))

(machine/reg-event :admin.egress/load
                   (fn [{:keys [db]} _] (loading db :egress [api/get! :egress-policies])))

(machine/reg-event :admin.egress/decide
                   (fn [{:keys [db]} [_ id decision]]
                     (rq/request (assoc-in db [:egress :error] nil)
                                 {:ask   [api/post! :egress-policy-decide
                                          {:id id :decision decision}]
                                  :error [:egress :error] :then [[:admin.egress/load]]})))

(machine/reg-event :admin.peering/load
                   (fn [{:keys [db]} _] (loading db :peering [api/get! :peering-connections])))

(machine/reg-event :admin.peering/jobs
                   (fn [{:keys [db]} [_ id]]
                     (rq/request db {:ask  [api/get! :peering-jobs {:connection id}]
                                     :path [:peering-jobs id]
                                     :error [:peering :error]})))

(machine/reg-event :admin.peering/act
                   (fn [{:keys [db]} [_ id endpoint body]]
                     (rq/request (assoc-in db [:peering :error] nil)
                                 {:ask   [api/post! endpoint {:connection id} body]
                                  :error [:peering :error] :then [[:admin.peering/load]]})))

(defn num-of [s]
  (let [n (js/parseFloat (str/replace (str s) "," "."))] (when-not (js/isNaN n) n)))

(machine/reg-event :admin.budget/load
                   (fn [{:keys [db]} _] (loading db :budget [api/get! :llm-budget])))

(machine/reg-event :admin.budget/draft
                   (fn [{:keys [db]} [_ path v]]
                     {:db (assoc-in db (into [:budget :draft] path) v)}))

(machine/reg-event :admin.budget/discard-limits
                   (fn [{:keys [db]} _]
                     {:db (update-in db [:budget :draft] dissoc :eur :reserve-pct)}))

(machine/reg-event :admin.budget/save
                   (fn [{:keys [db]} [_ body]]
                     (rq/request (assoc-in db [:budget :error] nil)
                                 (saving :budget {:ask  [api/post! :llm-budget body]
                                                  :path [:budget :data] :pick rd/of-answer
                                                  :then [[:rf/set [:budget :draft] {}]]}))))

(machine/reg-event :admin.budget/save-limits
                   (fn [{:keys [db]} _]
                     (let [d (get-in db [:budget :draft])]
                       {:fx [[:dispatch
                              [:admin.budget/save
                               (cond-> {}
                                 (contains? d :eur) (assoc :eur (num-of (:eur d)))
                                 (contains? d :reserve-pct)
                                 (assoc :reserve-pct (num-of (:reserve-pct d))))]]]})))

(machine/reg-event :admin.budget/save-quota
                   (fn [{:keys [db]} [_ kind nm]]
                     (let [k (if (= :project kind) :quotas :sources)]
                       {:fx [[:dispatch [:admin.budget/save
                                         {kind nm
                                          :quota (num-of (get-in db [:budget :draft k nm]))}]]]})))

(defn- meldungen
  [verlauf]
  (into []
        (mapcat #(cond->> (:meldungen %) (or (b/meldung? %) (b/fehler? %)) (cons %)))
        verlauf))

(defn- meldungen-je-sitzung [sitzungen]
  (into {}
        (map (fn [{:keys [id verlauf]}]
               [id (meldungen (map (fn [e] (update e :art keyword)) verlauf))]))
        sitzungen))

(machine/reg-event :admin.feedback/load
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask   [api/get! :feedback {:verlauf true}]
                                     :error [:feedback :error] :then [[:admin.feedback/loaded]]})))

(machine/reg-event :admin.feedback/loaded
                   (fn [{:keys [db]} [_ {:keys [sitzungen]}]]
                     {:db (update db :feedback assoc
                                  :sitzungen (mapv #(dissoc % :verlauf) sitzungen)
                                  :meldungen (meldungen-je-sitzung sitzungen)
                                  :error nil)}))

(machine/reg-event :admin.feedback/filter
                   (fn [{:keys [db]} [_ f]] {:db (assoc-in db [:feedback :filter] f)}))

(machine/reg-event :admin.feedback/check
                   (fn [{:keys [db]} [_ id erledigt?]]
                     (rq/request db {:ask   [api/post! :feedback-erledigt
                                             {:sitzung id :erledigt erledigt?}]
                                     :error [:feedback :error]
                                     :then  [[:admin.feedback/checked id]]})))

(machine/reg-event :admin.feedback/checked
                   (fn [{:keys [db]} [_ id s]]
                     {:db (update-in db [:feedback :sitzungen]
                                     (fn [ss] (mapv #(if (= id (:id %)) (merge % s) %) ss)))}))

(machine/reg-event :admin.orgs/load
                   (fn [{:keys [db]} _]
                     (rq/requests
                      (-> db
                          (update-in [:orgs :data] #(or % rd/loading))
                          (update-in [:spaces :data] #(or % rd/loading)))
                      [{:ask [api/get! :organizations] :path [:orgs :data] :rd? true}
                       {:ask [api/get! :spaces] :path [:spaces :data] :rd? true}
                       {:ask  [api/get! :schema-sections {:sections "organizations,spaces"}]
                        :path [:security-raw]}])))

(machine/reg-event :admin.orgs/open
                   (fn [{:keys [db]} [_ nm]]
                     (rq/request (assoc db :org-open nm :org-detail nil)
                                 {:ask   [api/get! :organization {:name nm}]
                                  :path  [:org-detail]
                                  :error [:orgs :error]})))

(machine/reg-event :admin.spaces/open
                   (fn [{:keys [db]} [_ nm]]
                     (rq/request (assoc db :space-open nm :space-detail nil)
                                 {:ask   [api/get! :space {:name nm}]
                                  :path  [:space-detail]
                                  :error [:spaces :error]})))

(machine/reg-event :admin.security/saved
                   (fn [_ _]
                     {:fx [[:invoke [staging/refresh-staged!]]
                           [:dispatch [:admin.orgs/load]]]}))

(machine/reg-event :admin.security/stage
                   (fn [{:keys [db]} [_ k action args]]
                     (rq/request (assoc-in db [k :error] nil)
                                 (saving k {:ask  [api/mutate action args {:stage? true}]
                                            :then [[:admin.security/saved]]}))))

(machine/reg-event :admin.markings/load
                   (fn [{:keys [db]} _] (loading db :markings [api/get! :markings])))

(machine/reg-event :admin.markings/open
                   (fn [{:keys [db]} [_ m]]
                     (rq/request (assoc db :marking-open m :marking-detail nil)
                                 {:ask   [api/get! :marking {:marking m}]
                                  :path  [:marking-detail]
                                  :error [:markings :error]})))

(machine/reg-event :admin.markings/why
                   (fn [{:keys [db]} [_ user resource]]
                     (rq/request (assoc db :why nil)
                                 {:ask   [api/get! :access-blocked
                                          {:user user :resource resource}]
                                  :path  [:why]
                                  :error [:markings :why-error]})))

(machine/reg-sub :admin/db (fn [db _] db))
