(ns sutra-ui.apps.repository.events
  (:require [sutra-ui.ui.overlay :as overlay]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.state.branches :as branches]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.lineage-status :as lineage-status]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]
            [sutra-ui.state.sync :as sync]))

(defn- asking
  [db c then]
  (rq/request db {:ask c :then [then]}))

(defn- invoke [f & args] [:invoke (into [f] args)])

(defn- reviewed
  [db id c prefix & fx]
  (rq/request db {:ask c :fx (into [(invoke gov/load-proposal! id)] fx) :report prefix}))

(defn- toggle [s x] (let [s (or s #{})] (if (contains? s x) (disj s x) (conj s x))))

(defn- verdict-body [task note] (cond-> {} note (assoc :note note) task (assoc :task task)))

(defn- rebase!
  [branch resolutions]
  (shell/with-loading
    (api/post! :branch-rebase {:name branch}
               {:resolutions (vec (for [[table side] resolutions]
                                    {:table table :side (name side)}))})))

(defn reviewers
  [draft]
  (vec (remove str/blank? (map str/trim (str/split (str draft) #",")))))

(defn update-transform-args
  [spec field-k body]
  (merge {:name        (:transform-name spec)
          :language    (:language spec)
          :inputs      (:inputs spec)
          :primary-key (:primary-key spec)
          :column-map  (:column-map spec)
          :output      (:output spec)}
         (when (:outputs spec) {:outputs (:outputs spec)})
         {field-k body}))

(machine/reg-event :repository/enter
                   (fn [_ _]
                     {:fx [(invoke data/refresh-datasets!) (invoke branches/refresh-branches!)
                           (invoke staging/refresh-staged!) (invoke data/refresh-schema-problems!)
                           (invoke gov/refresh-proposals! {:status "open"})]}))

(machine/reg-event :repository/tab
                   (fn [{:keys [db]} [_ tab]] {:db (assoc db :tab tab)}))

(machine/reg-event :repository/helper
                   (fn [{:keys [db]} [_ tab]] {:db (assoc db :helper tab)}))

(machine/reg-event :repository/select
                   (fn [{:keys [db]} [_ id]] {:db (assoc db :selected id)}))

(machine/reg-event :repository/recheck
                   (fn [_ _] {:fx [(invoke data/refresh-schema-problems!)]}))

(machine/reg-event :repository/commit
                   (fn [{:keys [db]} _] (asking db [sync/commit-staged!] [:repository/recheck])))

(machine/reg-event :repository.staging/reset
                   (fn [_ [_ n]]
                     {:fx [[:confirm {:ask {:title (i18n/t :ui/repo-reset-title {:n n})
                                            :body (i18n/t :ui/repo-reset-body)
                                            :danger? true
                                            :confirm-label (i18n/t :ui/repo-reset)}
                                      :ok  [:repository.staging/clear]}]]}))

(machine/reg-event :repository.staging/clear
                   (fn [{:keys [db]} _] (asking db [sync/clear-staged!] [:repository/recheck])))

(machine/reg-event :repository.staging/discard
                   (fn [_ [_ seq-no]] {:fx [(invoke sync/discard-staged! seq-no)]}))

(machine/reg-event :repository.draft/edit
                   (fn [{:keys [db]} [_ id text]] {:db (assoc-in db [:drafts id] text)}))

(machine/reg-event :repository.file/save
                   (fn [{:keys [db]} [_ spec field-k body]]
                     (asking db [ontology/mutate! :update-transform
                                 (update-transform-args spec field-k body)]
                             [:repository/recheck])))

(machine/reg-event :repository.file/preview
                   (fn [_ [_ rid]] (when rid {:fx [(invoke data/load-dataset-preview! rid)]})))

(machine/reg-event :repository/build
                   (fn [{:keys [db]} [_ names]]
                     (rq/request db {:ask    (if (seq names)
                                               [api/post! :import-run
                                                {:only (mapv (fn [n] (keyword (name n))) names)}]
                                               [api/post! :import-run])
                                     :fx     [(invoke sync/refresh-snapshot!)
                                              (invoke lineage-status/refresh-lineage-status!)
                                              (invoke data/refresh-import-status!)]
                                     :report "build"})))

(machine/reg-event :repository.branch/new
                   (fn [{:keys [db]} [_ current]]
                     (asking db [overlay/prompt! {:title (i18n/t :ui/repo-new-branch-title)
                                                 :label (i18n/t :ui/repo-new-branch-label
                                                                {:parent current})
                                                 :confirm-label
                                                 (i18n/t :ui/repo-new-branch-create)}]
                             [:repository.branch/named current])))

(machine/reg-event :repository.branch/named
                   (fn [_ [_ current nm]]
                     (when-not (str/blank? nm)
                       {:fx [(invoke branches/fork-branch!
                                     {:parent current :name (str/trim nm)})]})))

(machine/reg-event :repository.branch/checkout
                   (fn [{:keys [db]} [_ nm code?]]
                     (cond-> {:fx [(invoke sync/checkout-branch! nm)]}
                       code? (assoc :db (assoc db :tab :code)))))

(machine/reg-event :repository.proposals/enter
                   (fn [_ _]
                     {:fx [(invoke gov/refresh-proposals! {:status "open"})
                           (invoke principals/refresh-principals!)]}))

(machine/reg-event :repository.proposals/refresh
                   (fn [_ [_ status]] {:fx [(invoke gov/refresh-proposals! {:status status})]}))

(machine/reg-event :repository.proposal/open
                   (fn [{:keys [db]} [_ id]]
                     {:db (dissoc db :review)
                      :fx [(invoke shell/enter! :repository {:tab :pull-requests})
                           (invoke gov/load-proposal! id)]}))

(machine/reg-event :repository.proposal/show
                   (fn [{:keys [db]} [_ id]]
                     {:db (dissoc db :review) :fx [(invoke gov/load-proposal! id)]}))

(machine/reg-event :repository.proposal/close
                   (fn [{:keys [db]} _]
                     {:db (dissoc db :review) :fx [(invoke reset! gov/open-proposal nil)]}))

(machine/reg-event :repository.proposal/merge
                   (fn [_ [_ id]] {:fx [(invoke gov/merge-proposal! id)]}))

(machine/reg-event :repository.proposal/withdraw
                   (fn [{:keys [db]} [_ id]]
                     (reviewed db id [api/delete! :proposal {:id id}] "withdraw"
                               (invoke gov/refresh-proposals! {:status "all"}))))

(machine/reg-event :repository.review/reviewers
                   (fn [{:keys [db]} [_ id text]]
                     {:db (assoc-in db [:review id :reviewers] text)}))

(machine/reg-event :repository.review/ask
                   (fn [{:keys [db]} [_ id]]
                     (reviewed db id [api/ask-review id
                                      (reviewers (get-in db [:review id :reviewers]))]
                               "ask for review")))

(machine/reg-event :repository.review/approve
                   (fn [{:keys [db]} [_ id task note]]
                     (reviewed db id [api/post! :proposal-approve {:id id} (verdict-body task note)]
                               "approve")))

(machine/reg-event :repository.review/reject
                   (fn [{:keys [db]} [_ id task note]]
                     (reviewed db id [api/post! :proposal-reject {:id id} (verdict-body task note)]
                               "reject")))

(machine/reg-event :repository.review/expand
                   (fn [{:keys [db]} [_ id task]]
                     {:db (update-in db [:review id :expanded] toggle task)}))

(machine/reg-event :repository.comment/draft
                   (fn [{:keys [db]} [_ id task text]]
                     {:db (assoc-in db [:review id :drafts task] text)}))

(machine/reg-event :repository.comment/add
                   (fn [{:keys [db]} [_ id task]]
                     (let [body (get-in db [:review id :drafts task])]
                       (reviewed (assoc-in db [:review id :drafts task] "") id
                                 [api/post! :proposal-comments {:id id}
                                  (cond-> {:body body} task (assoc :task (name task)))]
                                 "comment"))))

(machine/reg-event :repository.comment/delete
                   (fn [{:keys [db]} [_ id comment-id]]
                     (reviewed db id [api/delete! :proposal-comment {:id id :comment comment-id}]
                               "delete comment")))

(machine/reg-event :repository.conflict/toggle
                   (fn [{:keys [db]} [_ id table]]
                     {:db (update-in db [:review id :open-conflicts] toggle table)}))

(machine/reg-event :repository.conflict/choose
                   (fn [{:keys [db]} [_ id table side]]
                     {:db (assoc-in db [:review id :choices table] side)}))

(machine/reg-event :repository.conflict/rebase
                   (fn [{:keys [db]} [_ id branch]]
                     (rq/request db {:ask    [rebase! branch (get-in db [:review id :choices])]
                                     :then   [[:repository.conflict/rebased id]]
                                     :report "rebase"})))

(machine/reg-event :repository.conflict/rebased
                   (fn [{:keys [db]} [_ id r]]
                     {:db (cond-> db r (update-in [:review id] dissoc :choices))
                      :fx [(when id (invoke gov/load-proposal! id))
                           (invoke sync/chain-refreshes!
                                   [sync/refresh-snapshot! branches/refresh-branch-changes!
                                    instances/refresh-instance-state!])]}))
