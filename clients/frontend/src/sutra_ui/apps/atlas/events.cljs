(ns sutra-ui.apps.atlas.events
  (:require [clojure.string :as str]
            [sutra-ui.api :as api]
            [sutra-ui.state.governance :as gov]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.sync :as sync]))

(def blank-annotation
  {:tags #{} :trashed false :curated false :doc nil :pinned false :promoted false})

(def ^:private refreshes
  {:nodes     [ks/refresh-kompass-nodes!]
   :index     [ks/refresh-kompass-index!]
   :snapshot  [sync/refresh-snapshot!]
   :documents [ks/refresh! :documents]
   :files     [ks/refresh! :files]
   :explorations [ks/refresh! :explorations]})

(defn- refresh
  [& ks]
  (mapv #(vector :invoke (refreshes %)) ks))

(defn- then-refresh
  [c what]
  {:fx [(rq/ask {:ask c :fx (refresh what) :report true})]})

(defn- normalize-annotations [resp]
  (into {} (for [[ref a] (:annotations resp)]
             [(if (keyword? ref) (name ref) (str ref))
              (update a :tags #(into #{} (map str) %))])))

(defn- annotate
  [db ref f]
  (let [a (f (get-in db [:annotations ref] blank-annotation))]
    (rq/request (assoc-in db [:annotations ref] a)
                {:ask    [api/post! :kompass-annotations (assoc a :ref ref :tags (vec (:tags a)))]
                 :report "annotation"})))

(defn- toggle-tag [ts tag]
  (let [ts (or ts #{})] (if (contains? ts tag) (disj ts tag) (conj ts tag))))

(def ^:private family->delete-action
  {:object-types      :delete-object-type
   :datasets          :delete-dataset
   :link-types        :delete-link-type
   :interfaces        :delete-interface
   :functions         :delete-function
   :transforms        :delete-transform
   :shared-properties :delete-shared-property
   :sources           :delete-source})

(defn- delete-forever
  [family nm]
  (case family
    :documents    (then-refresh [api/delete! :document-of {:id (str nm)}] :documents)
    :explorations (then-refresh [api/delete! :exploration {:id (str nm)}] :explorations)
    :files        (then-refresh [api/delete! :kompass-file {:id (str nm)}] :files)
    :folders      (then-refresh [api/delete! :kompass-node-of {:rid (str nm)}] :nodes)
    (when-let [action (family->delete-action family)]
      {:fx [(rq/ask {:ask [ontology/mutate! action {:name (name nm)}]})]})))

(defn- share-call
  [db ref c]
  (rq/request db {:ask  c :path [:share ref] :pick #(hash-map :view %)
                  :else [(fn [s] [:rf/set [:share ref] {:refused s}])]}))

(defn- as-b64
  [file]
  (js/Promise.
   (fn [ok fail]
     (let [rd (js/FileReader.)]
       (set! (.-onload rd) #(ok (second (str/split (.-result rd) #"," 2))))
       (set! (.-onerror rd) #(fail (.-error rd)))
       (.readAsDataURL rd file)))))

(defn- place-after! [ref folder]
  (when folder (ks/kompass-place! ref folder nil)))

(defn- upload-raw! [pkey folder file]
  (-> (as-b64 file)
      (.then #(api/post! :kompass-files {:name (.-name file) :project (name pkey)
                                         :mime (.-type file) :bytes-b64 %}))
      (.then (fn [r]
               (ks/refresh! :files)
               (place-after! (str "file/" (:id r)) folder)))))

(defn- upload-document! [pkey folder file]
  (-> (.text file)
      (.then #(api/post! :documents {:name (str/replace (.-name file) #"\.[^.]+$" "")
                                     :format :markdown :body % :project (name pkey)}))
      (.then (fn [r]
               (ks/refresh! :documents)
               (place-after! (str "document/" (get-in r [:document :id])) folder)))))

(defn- upload-dataset!
  [pkey file nm]
  (-> (as-b64 file)
      (.then #(api/post! :kompass-tabular {:filename (.-name file) :bytes-b64 %}))
      (.then (fn [{:keys [columns rows]}]
               (-> (api/mutate :create-dataset
                               {:name (keyword nm) :project (name pkey)
                                :columns (mapv (fn [{col :name ty :type}]
                                                 {:attribute-name (keyword col)
                                                  :attribute-type (keyword ty)})
                                               columns)}
                               {:publish? true})
                   (.then #(sync/refresh-snapshot!))
                   (.then #(api/post! :dataset-write {:name nm}
                                      {:rows (mapv (fn [row] {:values row}) rows)
                                       :mode :snapshot
                                       :message (str "Upload " (.-name file))})))))))

(defn upload-all!
  [pkey folder choices]
  (js/Promise.all
   (to-array
    (for [{:keys [file kind dataset]} choices]
      (case kind
        :dataset  (upload-dataset! pkey file dataset)
        :document (upload-document! pkey folder file)
        (upload-raw! pkey folder file))))))

(machine/reg-event :kompass/confirm
                   (fn [_ [_ ask events]]
                     {:fx [[:confirm {:ask ask :ok [:kompass/all events]}]]}))

(machine/reg-event :kompass/all
                   (fn [_ [_ events]] {:fx (mapv #(vector :dispatch %) events)}))

(machine/reg-event :kompass.annotations/refresh
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask  [api/get! :kompass-annotations] :path [:annotations]
                                     :pick normalize-annotations})))

(machine/reg-event :kompass.annotation/set
                   (fn [{:keys [db]} [_ ref k v]] (annotate db ref #(assoc % k v))))

(machine/reg-event :kompass.annotation/toggle
                   (fn [{:keys [db]} [_ ref k]] (annotate db ref #(update % k not))))

(machine/reg-event :kompass.annotation/tag
                   (fn [{:keys [db]} [_ ref tag]]
                     (annotate db ref #(update % :tags toggle-tag tag))))

(machine/reg-event :kompass.access/refresh
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask  [api/get! :access-requests {:status "open"}]
                                     :path [:access-requests] :pick #(vec (:requests %))})))

(machine/reg-event :kompass.access/request
                   (fn [_ [_ pkey role reason]]
                     {:fx [(rq/ask {:ask    [api/post! :access-requests
                                             {:kind :project-role :project (name pkey)
                                              :role role :justification reason}]
                                    :then   [[:kompass.access/refresh]]
                                    :report true})]}))

(machine/reg-event :kompass.project/enter
                   (fn [{:keys [db]} [_ pkey folder]]
                     {:db (assoc db :project (when (not= pkey ks/default-project) (name pkey))
                                 :folder folder :dashboard-tab :files)
                      :fx [[:invoke [ks/select-project! pkey]]]}))

(machine/reg-event :kompass.project/leave
                   (fn [{:keys [db]} _]
                     {:db (dissoc db :project :folder)
                      :fx [[:invoke [ks/select-project! nil]]]}))

(machine/reg-event :kompass.project/create
                   (fn [_ [_ pname description]]
                     {:fx [(rq/ask {:ask  [ontology/mutate! :create-project
                                           (cond-> {:name pname}
                                             (seq description) (assoc :description description))]
                                    :then [[:kompass.project/enter (keyword pname) nil]]})]}))

(machine/reg-event :kompass.project/sharing
                   (fn [_ [_ pkey on?]]
                     (then-refresh [api/post! :kompass-project-sharing {:name (name pkey)}
                                    {:on on?}]
                                   :index)))

(machine/reg-event :kompass.project/imports
                   (fn [_ [_ pkey names]]
                     (then-refresh [api/post! :kompass-project-imports {:name (name pkey)}
                                    {:imports (mapv name names)}]
                                   :snapshot)))

(machine/reg-event :kompass.folder/create
                   (fn [_ [_ nm parent-rid project]]
                     (then-refresh [api/post! :kompass-folders
                                    (cond-> {:name nm :parent-rid parent-rid}
                                      project (assoc :project (name project)))]
                                   :nodes)))

(defn- node-change [ask] {:fx [(rq/ask {:ask ask :fx (refresh :nodes)})]})

(machine/reg-event :kompass.node/rename
                   (fn [_ [_ rid to]]
                     (node-change [api/post! :kompass-node-rename {:rid rid} {:to to}])))

(machine/reg-event :kompass.node/move
                   (fn [_ [_ rid parent-rid]]
                     (node-change [api/post! :kompass-node-move {:rid rid}
                                   {:parent-rid parent-rid}])))

(machine/reg-event :kompass.node/delete
                   (fn [_ [_ rid]] (node-change [api/delete! :kompass-node-of {:rid rid}])))

(machine/reg-event :kompass.document/create
                   (fn [_ [_ project]]
                     {:fx [(rq/ask {:ask    [api/post! :documents
                                             {:name    "Unbenanntes Dokument"
                                              :format  :markdown :body ""
                                              :project (some-> project name)}]
                                    :then   [[:kompass.document/created]]
                                    :report true})]}))

(machine/reg-event :kompass.document/created
                   (fn [_ [_ r]]
                     {:fx (conj (refresh :documents)
                                [:invoke [shell/enter! :notebook
                                          {:doc        (str (get-in r [:document :id]))
                                           :read-only? false}]])}))

(machine/reg-event :kompass.portfolio/create
                   (fn [_ [_ pname description projects]]
                     (then-refresh [api/post! :kompass-portfolios
                                    {:name        pname
                                     :description description
                                     :projects    (mapv name projects)}]
                                   :index)))

(machine/reg-event :kompass.portfolio/change
                   (fn [_ [_ rid changes]]
                     (then-refresh [api/put! :kompass-portfolio {:rid rid} changes] :index)))

(machine/reg-event :kompass.portfolio/delete
                   (fn [_ [_ rid]]
                     (then-refresh [api/delete! :kompass-portfolio {:rid rid}] :index)))

(machine/reg-event :kompass.collection/create
                   (fn [_ [_ cname description refs]]
                     (then-refresh [api/post! :kompass-collections
                                    {:name cname :description description :refs (vec refs)}]
                                   :index)))

(machine/reg-event :kompass.collection/change
                   (fn [_ [_ rid changes]]
                     (then-refresh [api/put! :kompass-collection {:rid rid} changes] :index)))

(machine/reg-event :kompass.collection/delete
                   (fn [_ [_ rid]]
                     (then-refresh [api/delete! :kompass-collection {:rid rid}] :index)))

(machine/reg-event :kompass.share/redeem
                   (fn [_ [_ token]]
                     {:fx [(rq/ask {:ask    [api/post! :kompass-share-redeem {:token token} {}]
                                    :then   [[:rf/set [:area] :shared]]
                                    :fx     (refresh :index :documents :explorations :files)
                                    :report true})]}))

(machine/reg-event :kompass/delete-forever
                   (fn [_ [_ family nm]] (delete-forever family nm)))

(machine/reg-event :kompass.share/load
                   (fn [{:keys [db]} [_ ref]]
                     (share-call (assoc-in db [:share ref] nil) ref
                                 [api/get! :kompass-file-grants {:ref ref}])))

(machine/reg-event :kompass.share/grant
                   (fn [{:keys [db]} [_ ref role principal]]
                     (share-call db ref [api/post! :kompass-file-grants
                                         {:ref ref :role role :principal principal}])))

(machine/reg-event :kompass.share/revoke
                   (fn [{:keys [db]} [_ ref principal]]
                     (share-call db ref [api/post! :kompass-file-grants
                                         {:ref ref :principal principal :remove true}])))

(machine/reg-event :kompass.share/link
                   (fn [{:keys [db]} [_ ref on?]]
                     (share-call db ref [api/post! :kompass-share-link
                                         (cond-> {:ref ref :on on?} on? (assoc :role :viewer))])))

(machine/reg-event :kompass.dialog/open
                   (fn [{:keys [db]} [_ kind opts]]
                     {:db (assoc db :dialog (assoc opts :kind kind))}))

(machine/reg-event :kompass.dialog/close
                   (fn [{:keys [db]} _] {:db (assoc db :dialog nil :upload nil)}))

(machine/reg-event :kompass.upload/start
                   (fn [{:keys [db]} [_ pkey folder choices]]
                     (rq/request (assoc db :upload {:problem nil})
                                 {:ask  [upload-all! pkey folder choices]
                                  :busy [:upload :busy?] :error [:upload :problem]
                                  :then [[:kompass.dialog/close]]})))

(machine/reg-event :kompass.policy/load
                   (fn [_ [_ pname]]
                     {:fx [[:dispatch [:kompass.policy/fetch pname]]
                           [:invoke [gov/refresh-protections!]]]}))

(machine/reg-event :kompass.policy/fetch
                   (fn [{:keys [db]} [_ pname]]
                     (rq/request db {:ask  [api/get! :project-policy {:name pname}]
                                     :path [:policy]})))

(machine/reg-event :kompass.policy/save
                   (fn [_ [_ pname body]]
                     {:fx [(rq/ask {:ask    [api/post! :project-policy {:name pname} body]
                                    :then   [[:kompass.policy/fetch pname]]
                                    :report "project policy"})]}))

(machine/reg-event :kompass.deps/load
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask [api/get! :project-deps] :path [:deps] :report true})))

(machine/reg-event :kompass.packages/load
                   (fn [{:keys [db]} _]
                     (rq/request db {:ask [api/get! :packages] :path [:packages] :pick :packages})))

(def blank-filters
  {:q "" :types #{} :promoted? false :favorites? false :portfolios #{} :projects #{} :tags #{}
   :quick nil :grouped? false})

(machine/reg-sub :kompass/at (fn [db [_ k]] (get db k)))

(defn- filters [db scope] (merge blank-filters (get-in db [:filters scope])))

(machine/reg-sub :kompass/filters (fn [db [_ scope]] (filters db scope)))

(defn- put [k] (fn [{:keys [db]} [_ v]] {:db (assoc db k v)}))

(machine/reg-event :kompass/area (put :area))
(machine/reg-event :kompass/portfolio (put :portfolio))
(machine/reg-event :kompass/collection (put :collection))
(machine/reg-event :kompass/catalog-tab (put :catalog-tab))
(machine/reg-event :kompass/details-tab (put :details-tab))
(machine/reg-event :kompass/folder (put :folder))

(machine/reg-event :kompass/dashboard
                   (fn [{:keys [db]} [_ tab]] {:db (assoc db :dashboard-tab tab :folder nil)}))

(machine/reg-event :kompass/quick-hidden
                   (fn [{:keys [db]} _] {:db (update db :quick-hidden? not)}))

(defn- set-filters [db scope f] {:db (assoc-in db [:filters scope] f)})

(machine/reg-event :kompass.filter/set
                   (fn [{:keys [db]} [_ scope k v]]
                     (set-filters db scope (assoc (filters db scope) k v))))

(machine/reg-event :kompass.filter/flip
                   (fn [{:keys [db]} [_ scope k]]
                     (set-filters db scope (update (filters db scope) k not))))

(machine/reg-event :kompass.filter/toggle-in
                   (fn [{:keys [db]} [_ scope k v]]
                     (let [f (filters db scope)]
                       (set-filters db scope
                                    (update f k (if (contains? (get f k) v) disj conj) v)))))

(machine/reg-event :kompass.filter/clear
                   (fn [{:keys [db]} [_ scope]]
                     (set-filters db scope
                                  (merge (filters db scope)
                                         (select-keys blank-filters
                                                      [:types :promoted? :favorites?
                                                       :portfolios :projects :tags])))))
