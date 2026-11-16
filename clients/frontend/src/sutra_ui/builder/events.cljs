(ns sutra-ui.builder.events
  (:require [sutra-ui.ui.overlay :as overlay]
            [sutra-ui.ui.core :as ui]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.feature :as feature]
            [sutra-ui.object.card :as oc]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.canvas :as canvas-state]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.state.graph :as graph-state]
            [sutra-ui.state.atlas :as kompass]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]))

(def search-ms 200)

(defn- invoke [f & args] [:invoke (into [f] args)])

(defn- asking [db ask then] (rq/request db {:ask ask :then [then]}))

(defn flash-toast! [msg]
  (ui/show! (ui/default-toaster)
            {:message msg :timeout 2600 :is-close-button-shown false}))

(defn- copy-to-clipboard! [text]
  (if-let [clip (some-> js/navigator .-clipboard)]
    (.writeText clip text)
    (js/Promise.resolve nil)))

(defn object-hits
  [resp]
  (vec (for [{t :type rows :rows} (:groups resp)
             row      (take 5 rows)
             :let [tn (catalog/name-of t)
                   row (assoc row :object-type tn)]]
         {:kind   (i18n/t :ui/kind-object-of {:type (oc/type-label tn)})
          :group  (i18n/t :ui/objects)
          :glyph  "▢"
          :label  (catalog/instance-display-name row)
          :object [tn (:id row) row]
          :nav    #(shell/open-object! tn (:id row))})))

(defn- show-hits! [resp] (shell/set-cmdk-objects! (object-hits resp)))

(defn- checkout-if-listed!
  [nm]
  (when (some (comp #{nm} :name) (branches/branch-list))
    (sync/checkout-branch! nm)))

(machine/reg-event :builder/toast (fn [_ [_ msg]] {:fx [(invoke flash-toast! msg)]}))

(machine/reg-event :builder.rail/fold
                   (fn [{:keys [db]} [_ folded?]] {:db (assoc db :rail-folded? folded?)}))

(machine/reg-event :builder.cmdk/query
                   (fn [_ [_ q]]
                     {:fx (into [(invoke shell/set-cmdk-query! q)]
                                (if (< (count (str/trim (str q))) 2)
                                  [[:cancel :cmdk-search] (invoke shell/set-cmdk-objects! [])]
                                  [[:after {:key :cmdk-search :ms search-ms
                                            :event [:builder.cmdk/search q]}]]))}))

(machine/reg-event :builder.cmdk/search
                   (fn [{:keys [db]} [_ q]]
                     (rq/request db {:ask  [api/get! :instances-search {:q q :type nil}]
                                     :then [[:rf/invoke show-hits!]]
                                     :else [(fn [_] [:rf/invoke shell/set-cmdk-objects! []])]})))

(machine/reg-event :builder.menu/actions
                   (fn [{:keys [db]} [_ open?]] {:db (assoc db :actions-menu? open?)}))

(machine/reg-event :builder.save-as/open
                   (fn [{:keys [db]} _]
                     {:db (assoc db :save-as {:name (i18n/t :ui/untitled) :folder-rid nil})
                      :fx [(invoke kompass/refresh-kompass-nodes!)]}))

(machine/reg-event :builder.save-as/field
                   (fn [{:keys [db]} [_ k v]] {:db (assoc-in db [:save-as k] v)}))

(machine/reg-event :builder.save-as/close
                   (fn [{:keys [db]} _] {:db (assoc db :save-as nil)}))

(machine/reg-event :builder.save-as/submit
                   (fn [{:keys [db]} [_ area]]
                     (let [{:keys [name folder-rid]} (:save-as db)
                           nm (str/trim (or name ""))]
                       (when (seq nm)
                         {:db (assoc db :save-as nil)
                          :fx [(invoke graph-state/save-as-view!
                                       {:name nm :area area :folder-rid folder-rid})
                               (invoke flash-toast! (i18n/t :ui/saved-as {:name nm}))]}))))

(machine/reg-event :builder.save/in-place
                   (fn [{:keys [db]} [_ vid]]
                     (if vid
                       (asking db [canvas-state/save-layout-to-view! vid]
                               [:builder/toast (i18n/t :ui/saved)])
                       {:fx [[:dispatch [:builder.save-as/open]]]})))

(machine/reg-event :builder.share/link
                   (fn [{:keys [db]} [_ url]]
                     (rq/request db {:ask  [copy-to-clipboard! url]
                                     :then [[:builder/toast (i18n/t :ui/link-copied)]]
                                     :else [[:builder/toast (i18n/t :ui/link-copy-failed)]]})))

(machine/reg-event :builder.open-graph/open
                   (fn [{:keys [db]} _] {:db (assoc db :open-graph? true)}))

(machine/reg-event :builder.open-graph/rid
                   (fn [{:keys [db]} [_ rid]] {:db (assoc db :open-graph-rid rid)}))

(machine/reg-event :builder.open-graph/close
                   (fn [{:keys [db]} _] {:db (assoc db :open-graph? false :open-graph-rid "")}))

(machine/reg-event :builder.open-graph/load
                   (fn [{:keys [db]} [_ vid]]
                     (let [vid (str/trim (str vid))]
                       (when (seq vid)
                         {:db (assoc db :open-graph? false :open-graph-rid "")
                          :fx [(invoke graph-state/load-view! vid)]}))))

(machine/reg-event :builder.scheme/delete
                   (fn [_ [_ {:keys [id name]}]]
                     {:fx [[:confirm {:ask {:body    (i18n/t :ui/scheme-delete-confirm
                                                             {:name name})
                                            :danger? true}
                                      :ok [:builder.scheme/drop id]}]]}))

(machine/reg-event :builder.scheme/drop
                   (fn [_ [_ id]] {:fx [(invoke graph-state/delete-color-scheme! id)]}))

(machine/reg-event :builder.scheme/freeze
                   (fn [{:keys [db]} _]
                     (asking db [overlay/prompt! {:title   (i18n/t :ui/scheme-name-title)
                                                  :label   (i18n/t :ui/scheme-name-prompt)
                                                  :default "by-type"}]
                             [:builder.scheme/named])))

(machine/reg-event :builder.scheme/named
                   (fn [_ [_ nm]]
                     (let [nm (str/trim (str nm))]
                       (when (seq nm)
                         {:fx [(invoke graph-state/snapshot-by-type-scheme! nm)
                               (invoke shell/set-color-popover! false)]}))))

(machine/reg-event :builder.view/delete
                   (fn [_ [_ view-name]]
                     {:fx [[:confirm {:ask {:body (i18n/t :ui/view-delete-confirm) :danger? true}
                                      :ok [:builder.view/drop view-name]}]]}))

(machine/reg-event :builder.view/drop
                   (fn [_ [_ view-name]] {:fx [(invoke graph-state/delete-view! view-name)]}))

(machine/reg-event :builder.branch/new
                   (fn [{:keys [db]} [_ cur]]
                     (asking db [overlay/prompt! {:title (i18n/t :ui/branch-new-title)
                                                  :body  (i18n/t :ui/branch-new-prompt)
                                                  :label (i18n/t :ui/branch-new-name)
                                                  :confirm-label (i18n/t :ui/fork)}]
                             [:builder.branch/named cur])))

(machine/reg-event :builder.branch/named
                   (fn [{:keys [db]} [_ cur nm]]
                     (let [nm (str/trim (str nm))]
                       (when (seq nm)
                         (rq/request db {:ask [branches/fork-branch! {:parent cur :name nm}]
                                         :fx  [(invoke checkout-if-listed! nm)]})))))

(machine/reg-event :builder.branch/remove
                   (fn [_ [_ cur task nm]]
                     {:fx [[:confirm {:ask {:body (i18n/t :ui/branch-remove-confirm {:name nm})
                                            :danger? true
                                            :confirm-label (i18n/t :ui/branch-remove-resource)}
                                      :ok [:builder.branch/removed cur task]}]]}))

(machine/reg-event :builder.branch/removed
                   (fn [_ [_ cur task]] {:fx [(invoke gov/remove-from-branch! cur [task])]}))

(machine/reg-event :builder.branch/drop
                   (fn [_ [_ cur]]
                     {:fx [[:confirm {:ask {:body (i18n/t :ui/branch-drop-confirm {:name cur})
                                            :danger? true
                                            :confirm-label (i18n/t :ui/drop)}
                                      :ok [:builder.branch/dropping cur]}]]}))

(machine/reg-event :builder.branch/dropping
                   (fn [{:keys [db]} [_ cur]]
                     (rq/request db {:ask [branches/drop-branch! cur]
                                     :fx  [(invoke sync/checkout-branch! "main")]})))

(machine/reg-event :builder.taskbar/set
                   (fn [{:keys [db]} [_ m]] {:db (update db :taskbar merge m)}))

(defn- build-of [v] (case v "modified" :modified "affected" :affected nil))

(machine/reg-event :builder.merge/go
                   (fn [{:keys [db]} [_ id]]
                     (asking (assoc-in db [:taskbar :merging?] false)
                             [gov/merge-branch-proposal! id
                              (build-of (get-in db [:taskbar :build] "none"))]
                             [:builder.merge/done])))

(machine/reg-event :builder.merge/done
                   (fn [_ [_ r]] (when r {:fx [(invoke sync/checkout-branch! "main")]})))

(machine/reg-event :builder.propose/go
                   (fn [{:keys [db]} [_ cur target title view-id]]
                     (asking (assoc-in db [:taskbar :naming?] false)
                             [gov/propose-branch! {:source cur :target target :title title}]
                             [:builder.propose/done view-id])))

(machine/reg-event :builder.propose/done
                   (fn [_ [_ view-id p]]
                     (cond (:auto-landed p) {:fx [(invoke sync/checkout-branch! "main")]}
                           (:id p)          {:fx [(invoke feature/command! :open-proposal
                                                          view-id)]})))

(machine/reg-event :builder.staging/enter
                   (fn [{:keys [db]} _]
                     {:db (assoc db :staging {:proposal? false :pname ""})
                      :fx [(invoke data/refresh-schema-problems!)]}))

(machine/reg-event :builder.staging/set
                   (fn [{:keys [db]} [_ m]] {:db (update db :staging merge m)}))

(defn proposal-title [pname title]
  (if (str/blank? (str title)) (str "Changes from " pname) title))

(machine/reg-event :builder.proposal/make
                   (fn [{:keys [db]} [_ parent pname title actions]]
                     (asking db [branches/fork-branch! {:parent parent :name pname}]
                             [:builder.proposal/forked {:parent parent :pname pname
                                                        :title title :actions actions}])))

(machine/reg-event :builder.proposal/forked
                   (fn [{:keys [db]} [_ {:keys [pname] :as p}]]
                     (asking db [sync/checkout-branch! pname] [:builder.proposal/replay p])))

(machine/reg-event :builder.proposal/replay
                   (fn [{:keys [db]} [_ {:keys [actions] :as p}]]
                     (if-let [{:keys [action args]} (first actions)]
                       (asking db [ontology/mutate! action args]
                               [:builder.proposal/replay (update p :actions rest)])
                       (asking db [sync/commit-staged!] [:builder.proposal/committed p]))))

(machine/reg-event :builder.proposal/committed
                   (fn [{:keys [db]} [_ {:keys [parent pname title]}]]
                     (asking db [gov/propose-change! {:source pname :target parent
                                                      :title (proposal-title pname title)}]
                             [:builder.proposal/made])))

(machine/reg-event :builder.proposal/made
                   (fn [_ [_ p]]
                     (when (:id p)
                       {:fx [(invoke principals/refresh-principals!)
                             (invoke feature/command! :open-proposal (:id p))]})))
