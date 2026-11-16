(ns sutra-ui.apps.object-view.events
  (:require [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.object.page :as object-page]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.object-view :as object-view]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]
            [sutra.capps.variables :as vars]
            [sutra.types.link-side :as ls]))

(defn mutate!
  [action-name args opts]
  (shell/with-loading (api/mutate (keyword action-name) args opts)))

(defn save-layout!
  [object-type form-factor layout]
  (let [existing (catalog/object-view-app object-type form-factor)
        nm       (or (:application-name existing)
                     (keyword (str (name object-type) "-view-" (name form-factor))))
        project  (get-in @catalog/canonical-schema [:object-types object-type :project])]
    (ontology/mutate! (if existing :update-application :create-application)
                      (cond-> {:name nm :object-type object-type :form-factor form-factor
                               :layout layout}
                        (not existing) (assoc :auto-publish true)
                        project        (assoc :project project)))))

(defn- refresh-if-shown!
  [object-type id]
  (when (contains? @object-view/object-view-data [object-type id])
    (object-view/refresh-object-view-data! object-type id)))

(defn supplied-params
  [params]
  (into {} (remove (fn [[_ v]] (or (nil? v) (and (string? v) (str/blank? v))))) params))

(defn- current-object? [pspec]
  (= "current-object" (some-> (:prefill pspec) name)))

(defn- bind-target [cands ids]
  (let [pool   (if (= 1 (count ids)) cands (filter (comp :multiple val) cands))
        marked (filter (comp current-object? val) pool)
        pick   (if (seq marked) marked pool)]
    (when (= 1 (count pick))
      (let [[p pspec] (first pick)]
        [p (if (:multiple pspec) ids (first ids))]))))

(defn- bound
  [schema spec bindings]
  (keep (fn [[ty v]]
          (let [ids (mapv str (if (sequential? v) v [v]))]
            (when (seq ids)
              (when-let [[p x] (bind-target (filter #(ls/satisfies-type? schema ty (:type (val %)))
                                                    (:params spec))
                                            ids)]
                [ty p x]))))
        bindings))

(defn seed-values
  [schema spec bindings]
  (into {} (map (fn [[_ p x]] [p x])) (bound schema spec bindings)))

(defn property-defaults
  [schema spec bindings rows]
  (let [row-of  (into {} (keep (fn [[ty p x]]
                                 (when-let [row (and (string? x) (get rows ty))]
                                   [(name p) row])))
                      (bound schema spec bindings))
        current (some (fn [[q s]] (when (current-object? s) q)) (:params spec))]
    (into {}
          (keep (fn [[q s]]
                  (let [{:keys [from property]} (when (map? (:default s)) (:default s))
                        from (or from current)
                        row  (when (and from property) (row-of (name from)))
                        x    (when row (vars/item-field row (keyword (name property))))]
                    (when (some? x) [q x]))))
          (:params spec))))

(defn- swap-at [v i j] (assoc v i (get v j) j (get v i)))

(machine/reg-event :object-view/by-key
                   (fn [{:keys [db]} [_ object-type [k v]]]
                     (rq/request db {:ask    [api/post! :ontology-query
                                              {:find object-type :where {k {:= v}} :limit 2}]
                                     :then   [[:object-view/by-key-found object-type k v]]
                                     :report (name object-type)})))

(machine/reg-event :object-view/by-key-found
                   (fn [_ [_ object-type k v {:keys [rows]}]]
                     {:fx [[:invoke
                            (if (= 1 (count rows))
                              [shell/enter! :object-view
                               {:object-type object-type
                                :instance-id (str (:id (first rows)))}]
                              [shell/fail! (str (name object-type) ": "
                                                (if (seq rows) "mehrere Objekte" "kein Objekt")
                                                " mit " (name k) " = " v)])]]}))

(machine/reg-event :object-view.action/open
                   (fn [{:keys [db]} [_ {:keys [spec bindings] :as opened}]]
                     {:db (assoc db :action-launcher
                                 (assoc opened
                                        :bound-types (set (keys bindings))
                                        :values (seed-values @catalog/schema spec bindings)
                                        :defaults (property-defaults @catalog/schema spec
                                                                     bindings (:rows opened))
                                        :preview nil :error nil :busy? false))}))

(machine/reg-event :object-view.action/close
                   (fn [{:keys [db]} _] {:db (dissoc db :action-launcher)}))

(machine/reg-event :object-view.action/submit
                   (fn [{:keys [db]} [_ action params reply]]
                     (let [on-applied (get-in db [:action-launcher :on-applied])]
                       (rq/request db {:ask     [mutate! action (supplied-params params)
                                                 {:publish? true}]
                                       :then    [[:object-view.action/applied on-applied]]
                                       :reject? true}
                                   reply))))

(machine/reg-event :object-view.action/applied
                   (fn [{:keys [db]} [_ on-applied]]
                     {:db (dissoc db :action-launcher)
                      :fx [[:invoke [sync/refresh-snapshot!]]
                           [:invoke [instances/refresh-instance-state!]]
                           [:invoke [on-applied]]]}))

(machine/reg-event :object-view.action/evaluate
                   (fn [{:keys [db]} [_ action values search reply]]
                     (rq/request db {:ask [object-page/evaluate-action-params!
                                           action (supplied-params values) search]}
                                 reply)))

(machine/reg-event :object-view.action/preview
                   (fn [{:keys [db]} [_ action values]]
                     (rq/request (update db :action-launcher assoc :error nil :preview nil)
                                 {:ask    [mutate! action (supplied-params values) {:dry-run? true}]
                                  :path   [:action-launcher :preview] :pick #(vec (:plan %))
                                  :busy   [:action-launcher :busy?]
                                  :error  [:action-launcher :error]
                                  :within [:action-launcher]})))

(machine/reg-event :object-view.layout/configure
                   (fn [{:keys [db]} [_ object-type view items]]
                     {:db (assoc db :configure-dashboard
                                 {:object-type object-type :view view :draft (vec items)})}))

(machine/reg-event :object-view.layout/close
                   (fn [{:keys [db]} [_ which]] {:db (dissoc db which)}))

(machine/reg-event :object-view.layout/move
                   (fn [{:keys [db]} [_ i j]]
                     {:db (update-in db [:configure-dashboard :draft] swap-at i j)}))

(machine/reg-event :object-view.layout/toggle-hidden
                   (fn [{:keys [db]} [_ i]]
                     {:db (update-in db [:configure-dashboard :draft i]
                                     #(if (:hidden? %)
                                        (dissoc % :hidden?)
                                        (assoc % :hidden? true)))}))

(machine/reg-event :object-view.layout/remove
                   (fn [_ [_ i label]]
                     {:fx [[:confirm {:ask {:title   (i18n/t :ui/ov-layout-remove-confirm
                                                             {:name label})
                                            :body    (i18n/t :ui/ov-layout-remove-body)
                                            :danger? true
                                            :confirm-label (i18n/t :ui/ov-layout-remove)}
                                      :ok  [:object-view.layout/drop i]}]]}))

(machine/reg-event :object-view.layout/drop
                   (fn [{:keys [db]} [_ i]]
                     {:db (update-in db [:configure-dashboard :draft]
                                     (fn [d] (vec (concat (subvec d 0 i) (subvec d (inc i))))))}))

(machine/reg-event :object-view.layout/save
                   (fn [{:keys [db]} [_ object-type form-factor layout which]]
                     (let [id (some-> (get-in db [:object-view :instance-id]) str)]
                       (rq/request (dissoc db which)
                                   {:ask [save-layout! object-type form-factor layout]
                                    :fx  (when id
                                           [[:invoke [refresh-if-shown! object-type id]]])}))))

(machine/reg-event :object-view.new-view/open
                   (fn [{:keys [db]} [_ object-type view view-type config]]
                     {:db (assoc db :new-view-modal
                                 {:object-type object-type :view view :type object-type
                                  :view-type view-type :config config :name ""})}))

(machine/reg-event :object-view.new-view/type
                   (fn [{:keys [db]} [_ kw]]
                     {:db (update db :new-view-modal
                                  #(-> % (assoc :type kw) (assoc-in [:config :owner-type] kw)))}))

(machine/reg-event :object-view.new-view/kind
                   (fn [{:keys [db]} [_ view-type config]]
                     {:db (update db :new-view-modal assoc :view-type view-type
                                  :config config)}))

(machine/reg-event :object-view.new-view/set
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:new-view-modal k] v)}))

(machine/reg-event :object-view.documents/load
                   (fn [{:keys [db]} [_ object-type id]]
                     (let [k [object-type (str id)]]
                       (rq/request (update db :linked-documents assoc k nil)
                                   {:ask  [api/get! :documents-linked
                                           {:object-type (name object-type) :id (str id)}]
                                    :path [:linked-documents k] :pick #(vec (:documents %))
                                    :else [[:rf/set [:linked-documents k] []]]}))))

(machine/reg-event :object-view.floorplan/place
                   (fn [_ [_ floorplan-id values [link target]]]
                     {:fx [[:call {:call [ontology/mutate! :create-placement {:values values}]
                                   :ok   [:object-view.floorplan/placed floorplan-id link
                                          target]}]]}))

(machine/reg-event :object-view.floorplan/placed
                   (fn [_ [_ floorplan-id link target resp]]
                     (when-let [pid (:id resp)]
                       {:fx [[:invoke [ontology/mutate! :connect-on-floorplan
                                       {:from-id pid :to-id floorplan-id}]]
                             [:invoke [ontology/mutate! link {:from-id pid :to-id target}]]]})))

(machine/reg-event :object-view.floorplan/geometry
                   (fn [_ [_ pid m]]
                     {:fx [[:invoke [ontology/mutate! :update-placement {:id pid :values m}]]]}))

(machine/reg-event :object-view.floorplan/remove
                   (fn [_ [_ pid]]
                     {:fx [[:invoke [ontology/mutate! :delete-placement {:id pid}]]]}))
