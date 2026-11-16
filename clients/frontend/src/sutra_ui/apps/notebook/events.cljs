(ns sutra-ui.apps.notebook.events
  (:require [sutra-ui.ui.overlay :as overlay]
            [clojure.string :as str]
            [sutra.i18n.core :as i18n]
            [sutra-ui.api :as api]
            [sutra-ui.apps.notebook.doc :as doc]
            [sutra-ui.apps.notebook.live :as live]
            [sutra-ui.apps.notebook.state :as st :refer [at autosave-ms]]
            [sutra-ui.download :as dl]
            [sutra-ui.state.loading :as loading]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.principals :as principals]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]
            [sutra-ui.state.staging :as staging]))

(defn- req
  [ask & {:as more}]
  (merge {:ask ask :error [:err]} more))

(defn- busy
  [ask & more]
  (apply req ask :busy [:busy] more))

(defn- dispatches [events] (mapv #(vector :dispatch %) events))

(defn- save-then
  [& events]
  [[:dispatch [:notebook/save nil (vec events)]]])

(defn- id-of [db] (some-> (:id (:open-doc db)) str))

(defn dirty? [db]
  (not= (doc/persistable (:open-doc db)) (doc/persistable (:saved-doc db))))

(defn- fence
  [db]
  (if-let [peer (when (:session db) (:peer (doc/live-state (:session db))))]
    {:live peer}
    {:expect-version (:version (:open-doc db))}))

(defn- put-doc!
  [id body]
  (-> (api/put! :document-of {:id id} body)
      (.catch (fn [e]
                (if (= 409 (:status (ex-data e)))
                  {:conflict (loading/problem e)}
                  (throw e))))))

(defn- rethrow [e] (throw e))

(defn- with-route-doc [db id]
  (if (= id (:doc db)) db (-> db (assoc :doc id) (dissoc :read-only?))))

(defn- link-rows
  [db docs]
  (let [known (at db :link-rows)
        want  (->> docs
                   (mapcat :links)
                   (keep (fn [{:keys [object-type id]}]
                           (when-let [t (doc/type-kw object-type)] [t (str id)])))
                   (remove #(contains? known %))
                   distinct)
        by-t  (group-by first want)]
    [(update db :link-rows #(into (or % {}) (map (fn [k] [k nil])) want))
     (vec (for [[t ids] by-t :let [ids (mapv second ids)]]
            (rq/ask {:ask  [api/post! :ontology-query {:find t :ids ids :limit (count ids)}]
                     :then [[:notebook.links/loaded t]]})))]))

(defn- with-link-rows [{:keys [db fx]} docs]
  (let [[db' more] (link-rows db docs)] {:db db' :fx (into (vec fx) more)}))

(defn- show
  [db d]
  (with-link-rows
    {:db (-> db
             (assoc :open-doc d :saved-doc d :save-state :saved :v-open nil :versions nil
                    :rendered nil :layout-mode :view)
             (update :reload inc)
             (with-route-doc (:id d)))
     :fx (cond-> [[:invoke [reset! st/frame-outline []]]
                  [:invoke [reset! st/sel-widget nil]]]
           (doc/layout-doc? d)
           (conj [:dispatch [:notebook/render]])
           (= :markdown (keyword (:format d)))
           (conj (rq/ask (req [api/post! :document-export {:id (str (:id d))}
                               {:targets ["html"]}]
                              :then [[:notebook.doc/rich-text (:id d)]])))
           true (conj [:dispatch [:notebook.live/start]])
           (= :versions (at db :left-tab)) (conj [:dispatch [:notebook.versions/refresh]]))}
    [d]))

(defn- end-live [db]
  (let [s (:session db)]
    {:db (assoc db :session nil :seed-pending false)
     :fx [(when (:state s) [:notebook.live/close s])]}))

(defn- start-live
  [db]
  (let [d (:open-doc db)
        s (:session db)]
    (cond
      (:read-only? db) (end-live db)
      (and s (= (str (:id d)) (str (:id s)))) nil
      :else
      (let [{db' :db fx :fx} (end-live db)]
        (if (doc/live-doc? d)
          {:db (assoc db' :session {:id (:id d)})
           :fx (conj fx [:notebook.live/open (:id d)])}
          {:db db' :fx fx})))))

(defn- save-step
  [db]
  (if-let [{:keys [extra then]} (first (at db :save-queue))]
    (let [db (update db :save-queue (comp vec rest))
          d  (:open-doc db)
          s  (:session db)]
      (if (and (:id d) (or extra (and (dirty? db) (doc/saves-here? s)))
               (or (nil? s) (= :live (:status (doc/live-state s)))))
        (let [sent (doc/persistable d)]
          (rq/request (assoc db :save-state :saving)
                      {:ask    [put-doc! (str (:id d)) (merge sent extra (fence db))]
                       :busy   [:saving?] :error [:err]
                       :then   [[:notebook.save/done sent]]
                       :always (-> [[:notebook.save/ended]] (into then)
                                   (conj [:notebook.save/next]))}))
        {:db db :fx (conj (dispatches then) [:dispatch [:notebook.save/next]])}))
    {:db (assoc db :saving? false)}))

(defn- saved [db sent r]
  (let [srv (:document r)
        db  (-> db
                (assoc :saved-doc (merge srv sent))
                (update :open-doc merge (dissoc srv :body :name :page :format))
                (update :docs (fn [ds] (mapv #(if (= (:id %) (:id srv)) (dissoc srv :body) %)
                                             ds))))]
    (with-link-rows {:db (assoc db :save-state (if (dirty? db) :dirty :saved))} [srv])))

(machine/reg-event :notebook/save
                   (fn [{:keys [db]} [_ extra then]]
                     {:db (update db :save-queue (fnil conj []) {:extra extra :then then})
                      :fx [[:cancel :notebook/autosave]
                           (when-not (:saving? db) [:dispatch [:notebook.save/next]])]}))

(machine/reg-event :notebook.save/next
                   (fn [{:keys [db]} _] (when-not (:saving? db) (save-step db))))

(machine/reg-event :notebook.save/done
                   (fn [{:keys [db]} [_ sent r]]
                     (if-let [c (:conflict r)]
                       (if (:session db)
                         {:db (assoc db :save-state :dirty)}
                         {:db (cond-> (assoc db :save-state :error) c (assoc :err c))})
                       (saved db sent r))))

(machine/reg-event :notebook.save/ended
                   (fn [{:keys [db]} _]
                     (when (= :saving (:save-state db)) {:db (assoc db :save-state :error)})))

(machine/reg-event :notebook/flush
                   (fn [{:keys [db]} _] (when (dirty? db) {:fx (save-then)})))

(machine/reg-event :notebook/edit
                   (fn [{:keys [db]} [_ changes]]
                     (let [old (:open-doc db)
                           new (merge old changes)
                           s   (:session db)]
                       {:db (assoc db :open-doc new :save-state :dirty)
                        :fx (conj (vec (for [k [:name :page]
                                             :when (and (:state s) (not= (k old) (k new)))]
                                         [:invoke [live/set-meta! s k (k new)]]))
                                  [:after {:key :notebook/autosave :ms autosave-ms
                                           :event [:notebook/save]}])})))

(machine/reg-event :notebook/set (fn [{:keys [db]} [_ k v]] {:db (assoc db k v)}))

(machine/reg-event :notebook/open-project
                   (fn [_ [_ pk]]
                     {:fx [[:invoke [ks/select-project! pk]]
                           [:invoke [shell/enter! :kompass {:area :files
                                                             :dashboard-tab :files}]]]}))

(defn- ensure-principals! []
  (when (empty? @principals/principal-names) (principals/refresh-principals!)))

(machine/reg-event :notebook/enter
                   (fn [{:keys [db]} _]
                     (let [id     (:doc db)
                           d      (:open-doc db)
                           other? (and id (not= id (:id d)))
                           back?  (and d (not other?))]
                       {:db (cond-> db back? (with-route-doc (:id d)))
                        :fx (cond-> [[:dispatch [:notebook.docs/refresh]]
                                     [:dispatch [:notebook.templates/refresh]]
                                     [:invoke [ensure-principals!]]]
                              other? (conj [:dispatch [:notebook/open id]])
                              back?  (conj [:dispatch [:notebook.live/start]]))})))

(machine/reg-event :notebook/leave (fn [_ _] {:fx (save-then [:notebook.live/end])}))

(machine/reg-event :notebook.docs/refresh
                   (fn [_ _] {:fx [(rq/ask (req [api/documents] :then [[:notebook.docs/loaded]]))]}))

(machine/reg-event :notebook.docs/loaded
                   (fn [{:keys [db]} [_ r]]
                     (let [ds (vec (:documents r))]
                       (with-link-rows {:db (assoc db :docs ds)} ds))))

(machine/reg-event :notebook.templates/refresh
                   (fn [_ _]
                     {:fx [(rq/ask (req [api/get! :templates] :path [:templates]
                                        :pick #(vec (:templates %))))]}))

(machine/reg-event :notebook.versions/refresh
                   (fn [{:keys [db]} _]
                     (when-let [id (id-of db)]
                       {:fx [(rq/ask (req [api/get! :document-versions {:id id}]
                                          :path [:versions] :pick #(vec (:versions %))))]})))

(machine/reg-event :notebook.links/loaded
                   (fn [{:keys [db]} [_ t r]]
                     {:db (update db :link-rows into
                                  (map (fn [row] [[t (str (:id row))] row]))
                                  (:rows r))}))

(machine/reg-event :notebook.diagram/rows
                   (fn [{:keys [db]} [_ t]]
                     (when-not (contains? (at db :diagram-rows) t)
                       {:db (assoc-in db [:diagram-rows t] :loading)
                        :fx [(rq/ask {:ask  [api/post! :ontology-query
                                             {:find (keyword t) :limit 2000}]
                                      :path [:diagram-rows t]
                                      :pick #(vec (:rows %))
                                      :else [[:rf/set [:diagram-rows t] []]]})]})))

(machine/reg-event :notebook.diagram/upload
                   (fn [_ [_ body on-done on-fail]]
                     {:fx [(rq/ask {:ask  [api/post! :kompass-files body]
                                    :then [[:rf/invoke on-done]]
                                    :else [[:rf/invoke on-fail]]})]}))

(machine/reg-event :notebook/render
                   (fn [{:keys [db]} _]
                     (when-let [id (id-of db)]
                       {:fx [(rq/ask (req [api/post! :document-export {:id id}
                                           {:targets ["html"]}]
                                          :path [:rendered] :pick #(doc/wrap-html (:html %))))]})))

(machine/reg-event :notebook/tab
                   (fn [{:keys [db]} [_ k]]
                     {:db (assoc db :left-tab k)
                      :fx [(when (= k :versions) [:dispatch [:notebook.versions/refresh]])]}))

(machine/reg-event :notebook/layout-mode
                   (fn [{:keys [db]} [_ m]]
                     {:db (assoc db :layout-mode m)
                      :fx (when (= :view m) (save-then [:notebook/render]))}))

(machine/reg-event :notebook/open
                   (fn [_ [_ id]] {:fx (save-then [:notebook.doc/fetch id])}))

(machine/reg-event :notebook.doc/fetch
                   (fn [_ [_ id]]
                     {:fx [(rq/ask (req [api/get! :document-of {:id (str id)}]
                                        :then [[:notebook.doc/show]]))]}))

(machine/reg-event :notebook.doc/show (fn [{:keys [db]} [_ d]] (show db d)))

(machine/reg-event :notebook.doc/rich-text
                   (fn [{:keys [db]} [_ id r]]
                     (when (= id (:id (:open-doc db)))
                       (let [body (doc/html-body (:html r))
                             db   (-> db
                                      (update :open-doc assoc :body body :format :html)
                                      (update :saved-doc assoc :body body :format :html)
                                      (update :reload inc))]
                         {:db db :fx [[:dispatch [:notebook.live/start]]]}))))

(machine/reg-event :notebook/close
                   (fn [_ _] {:fx (save-then [:notebook.doc/closed])}))

(machine/reg-event :notebook.doc/closed
                   (fn [{:keys [db]} _]
                     (let [{db :db fx :fx} (end-live db)]
                       {:db (-> db (assoc :open-doc nil :saved-doc nil) (with-route-doc nil))
                        :fx (conj fx [:dispatch [:notebook.docs/refresh]])})))

(machine/reg-event :notebook/create
                   (fn [{:keys [db]} [_ d project]]
                     {:db (assoc db :busy true)
                      :fx (save-then [:notebook.doc/post
                                      (merge {:format :html :body ""
                                              :name "Unbenanntes Dokument" :project project}
                                             d)])}))

(machine/reg-event :notebook.doc/post
                   (fn [_ [_ body]]
                     {:fx [(rq/ask (busy [api/post! :documents body]
                                         :then [[:notebook.doc/created]]))]}))

(machine/reg-event :notebook.doc/created
                   (fn [{:keys [db]} [_ r]]
                     (update (show db (:document r)) :fx
                             conj [:dispatch [:notebook.docs/refresh]])))

(machine/reg-event :notebook/regenerate
                   (fn [{:keys [db]} _]
                     {:db (assoc db :busy true :err nil)
                      :fx (save-then [:notebook.doc/regenerate (id-of db)])}))

(machine/reg-event :notebook.doc/regenerate
                   (fn [_ [_ id]]
                     {:fx [(rq/ask (busy [api/post! :document-regenerate {:id id} {}]
                                         :then [[:notebook.doc/created]]))]}))

(machine/reg-event :notebook/delete
                   (fn [{:keys [db]} _]
                     (let [d (:open-doc db)]
                       {:fx [[:confirm {:ask {:title   (i18n/t :ui/notebook-delete-title
                                                              {:name (:name d)})
                                              :body    (i18n/t :ui/notebook-delete-body)
                                              :danger? true}
                                        :ok  [:notebook.doc/delete (str (:id d))]}]]})))

(machine/reg-event :notebook.doc/delete
                   (fn [_ [_ id]]
                     {:fx [[:cancel :notebook/autosave]
                           (rq/ask (req [api/delete! :document-of {:id id}]
                                        :then [[:notebook.doc/deleted]]))]}))

(machine/reg-event :notebook.doc/deleted
                   (fn [{:keys [db]} _]
                     {:db (-> db (assoc :open-doc nil :saved-doc nil) (with-route-doc nil))
                      :fx [[:dispatch [:notebook.docs/refresh]]]}))

(machine/reg-event :notebook/move
                   (fn [{:keys [db]} [_ pk]]
                     {:db (assoc db :dialog nil)
                      :fx (save-then [:notebook.doc/move (id-of db) pk])}))

(machine/reg-event :notebook.doc/move
                   (fn [_ [_ id pk]]
                     {:fx [(rq/ask (req [ks/move-file-to-project! :documents id pk rethrow]
                                        :then [[:notebook.doc/moved id]]))]}))

(machine/reg-event :notebook.doc/moved
                   (fn [_ [_ id r]]
                     (when r
                       {:fx [[:dispatch [:notebook.doc/fetch id]]
                             [:dispatch [:notebook.docs/refresh]]]})))

(machine/reg-event :notebook/edit-instead
                   (fn [{:keys [db]} _]
                     {:db (-> db (assoc :read-only? false) (update :reload inc))
                      :fx [[:dispatch [:notebook.live/start]]]}))

(machine/reg-event :notebook.version/open
                   (fn [_ [_ v diff?]] {:fx (save-then [:notebook.version/fetch (:seq v) diff?])}))

(machine/reg-event :notebook.version/fetch
                   (fn [{:keys [db]} [_ seq-no diff?]]
                     {:fx [(rq/ask (req [api/get! :document-version
                                         {:id (id-of db) :seq (str seq-no)}]
                                        :path [:v-open] :pick #(assoc % :diff? diff?)))]}))

(machine/reg-event :notebook.version/confirm-restore
                   (fn [_ [_ v]]
                     {:fx [[:confirm {:ask {:title         (i18n/t :ui/notebook-restore-title)
                                            :body          (i18n/t :ui/notebook-restore-body
                                                                   {:name (doc/version-title v)})
                                            :confirm-label (i18n/t :ui/notebook-restore)}
                                      :ok  [:notebook.version/restore (:seq v)]}]]}))

(machine/reg-event :notebook.version/restore
                   (fn [{:keys [db]} [_ seq-no]]
                     {:db (assoc db :busy true)
                      :fx (save-then [:notebook.version/post seq-no])}))

(machine/reg-event :notebook.version/post
                   (fn [{:keys [db]} [_ seq-no]]
                     {:fx [(rq/ask (busy [api/post! :document-restore {:id (id-of db)}
                                          {:seq seq-no}]
                                         :then [[:notebook.version/restored]]))]}))

(machine/reg-event :notebook.version/restored
                   (fn [{:keys [db]} [_ r]]
                     (update (show db (:document r)) :fx
                             conj [:dispatch [:notebook.versions/refresh]])))

(machine/reg-event :notebook.version/save
                   (fn [{:keys [db]} [_ label endorsed]]
                     {:db (assoc db :busy true)
                      :fx [[:dispatch [:notebook/save {:label (str/trim label)
                                                      :endorsed (boolean endorsed)}
                                       [[:notebook.version/saved]]]]]}))

(machine/reg-event :notebook.version/saved
                   (fn [{:keys [db]} _]
                     {:db (assoc db :busy false :dialog nil :v-filter :named)
                      :fx [[:dispatch [:notebook.versions/refresh]]]}))

(def ^:private office "application/vnd.openxmlformats-officedocument.")

(defn- download! [fmt d r]
  (case fmt
    :pdf  (dl/b64! (:pdf_b64 r) "application/pdf" (str (:name d) ".pdf"))
    :xlsx (dl/b64! (:xlsx_b64 r) (str office "spreadsheetml.sheet")
                         (str (:name d) ".xlsx"))
    :csv  (dl/text! (:csv r) "text/csv" (str (:name d) ".csv"))
    :docx (dl/b64! (:docx_b64 r) (str office "wordprocessingml.document")
                         (str (:name d) ".docx"))
    (dl/text! (doc/wrap-html (:html r) (:page d)) "text/html" (str (:name d) ".html"))))

(machine/reg-event :notebook/export
                   (fn [{:keys [db]} [_ fmt]]
                     {:db (assoc db :busy true :err nil)
                      :fx (save-then [:notebook.export/fetch fmt (:open-doc db)])}))

(machine/reg-event :notebook.export/fetch
                   (fn [_ [_ fmt d]]
                     {:fx [(rq/ask (busy [api/post! :document-export {:id (str (:id d))}
                                          {:targets [(name fmt)]}]
                                         :then [[:rf/invoke download! fmt d]]))]}))

(defn print-html
  [d html]
  (let [[w h] (doc/page-dims (:page d))]
    (str/replace-first
     (doc/wrap-html html (:page d)) "</style>"
     (str "@media screen{html{background:#404854}"
          "body{background:#ffffff;box-sizing:border-box;width:" w "mm;"
          "min-height:" h "mm;margin:24px auto;padding:"
          (or (not-empty (:margin (:page d))) "18mm 16mm")
          ";box-shadow:0 2px 12px rgba(17,20,24,.4)}}</style>"))))

(machine/reg-event :notebook/print
                   (fn [{:keys [db]} _]
                     {:db (assoc db :busy true)
                      :fx (save-then [:notebook.print/fetch (:open-doc db)])}))

(machine/reg-event :notebook.print/fetch
                   (fn [_ [_ d]]
                     {:fx [(rq/ask (busy [api/post! :document-export {:id (str (:id d))}
                                          {:targets ["html"]}]
                                         :path [:print-view] :pick #(print-html d (:html %))))]}))

(machine/reg-fx :notebook.live/open
                (fn [{f :frame} id]
                  (let [reply #(machine/dispatch % {:frame f})
                        s     (atom nil)]
                    (reset! s (live/open!
                               id (or (:username @auth/auth) "")
                               {:on-sync  (fn [seed?]
                                            (reply [:notebook.live/synced seed?
                                                    (when-not seed? (live/meta-of @s))]))
                                :on-meta  (fn [m] (reply [:notebook/edit m]))
                                :on-reset (fn [] (reply [:notebook.live/reset]))}))
                    (reply [:notebook.live/opened @s]))))

(machine/reg-fx :notebook.live/close (fn [_ s] (live/close! s)))

(machine/reg-event :notebook.live/start (fn [{:keys [db]} _] (start-live db)))

(machine/reg-event :notebook.live/end (fn [{:keys [db]} _] (end-live db)))

(machine/reg-event :notebook.live/opened
                   (fn [{:keys [db]} [_ s]]
                     (let [cur (:session db)]
                       (if (and cur (not (:state cur)) (= (str (:id cur)) (str (:id s))))
                         {:db (assoc db :session s)}
                         {:fx [[:notebook.live/close s]]}))))

(machine/reg-event :notebook.live/synced
                   (fn [{:keys [db]} [_ seed? m]]
                     (cond
                       seed? {:db (assoc db :seed-pending true)}
                       m     {:db (-> db (update :open-doc merge m) (update :saved-doc merge m))})))

(machine/reg-event :notebook.live/seeded
                   (fn [{:keys [db]} _] {:db (assoc db :seed-pending false)}))

(machine/reg-event :notebook.live/reset
                   (fn [{:keys [db]} _]
                     (when-let [id (id-of db)]
                       (let [{db :db fx :fx} (end-live db)]
                         {:db db
                          :fx (into [[:cancel :notebook/autosave]]
                                    (conj fx [:dispatch [:notebook.doc/fetch id]]))}))))

(defn- dialog-open? [db k] (some? (get-in db [:dialog k])))

(machine/reg-event :notebook.dialog/set
                   (fn [{:keys [db]} [_ ks v]] {:db (assoc-in db (into [:dialog] ks) v)}))

(machine/reg-event :notebook.dialog/close-menu
                   (fn [{:keys [db]} [_ k]]
                     (when (= k (get-in db [:dialog :menu])) {:db (assoc db :dialog nil)})))

(machine/reg-event :notebook.dialog/close
                   (fn [{:keys [db]} _] {:db (assoc db :dialog nil :err nil)}))

(machine/reg-event :notebook.template/pick
                   (fn [{:keys [db]} [_ t {:keys [values version name]}]]
                     {:db (assoc db :dialog {:template t :values (or values {}) :version version
                                             :name name})
                      :fx [(rq/ask (req [api/get! :template-of
                                         {:name (clojure.core/name (:template-name t))}]
                                        :then [[:notebook.template/full]]))]}))

(machine/reg-event :notebook.template/full
                   (fn [{:keys [db]} [_ full]]
                     (when (dialog-open? db :template)
                       {:db (update-in db [:dialog :template] merge full)})))

(machine/reg-event :notebook.template/from-doc
                   (fn [{:keys [db]} _]
                     (let [d (:open-doc db)]
                       (if-let [t (doc/template-in (at db :templates) (:template d))]
                         {:fx [[:dispatch [:notebook.template/pick t
                                           {:values  (:parameters d)
                                            :version (:template-version d)}]]]}
                         {:db (assoc db :err (i18n/t :ui/notebook-template-missing
                                                     {:name (doc/template-title
                                                             (:template d))}))}))))

(machine/reg-event :notebook.template/preview
                   (fn [{:keys [db]} [_ tname body]]
                     (rq/request (assoc db :err nil)
                                 (busy [api/post! :template-render {:name tname} body]
                                       :then [[:notebook.template/previewed]]))))

(machine/reg-event :notebook.template/previewed
                   (fn [{:keys [db]} [_ r]]
                     {:db (cond-> db
                            (dialog-open? db :template)
                            (assoc-in [:dialog :preview] (doc/wrap-html (:html r))))}))

(machine/reg-event :notebook.template/generate
                   (fn [{:keys [db]} [_ tname body]]
                     {:db (assoc db :busy true :err nil)
                      :fx (save-then [:notebook.template/post tname body])}))

(machine/reg-event :notebook.template/post
                   (fn [_ [_ tname body]]
                     {:fx [(rq/ask (busy [api/post! :template-generate {:name tname} body]
                                         :then [[:notebook.template/generated]]))]}))

(machine/reg-event :notebook.template/generated
                   (fn [{:keys [db]} [_ r]]
                     (update (show (assoc db :dialog nil) (:document r)) :fx
                             conj [:dispatch [:notebook.docs/refresh]])))

(defn- project-name [p] (name (:name p)))

(machine/reg-event :notebook.templatize/load
                   (fn [_ _]
                     {:fx [(rq/ask {:ask  [api/get! :projects]
                                    :then [[:notebook.templatize/projects]]
                                    :else [[:notebook.templatize/projects nil]]})]}))

(machine/reg-event :notebook.templatize/projects
                   (fn [{:keys [db]} [_ r]]
                     (when (dialog-open? db :templatize)
                       (let [ps (vec (sort-by #(str/lower-case (project-name %)) (:projects r)))]
                         {:db (update db :dialog assoc :projects ps
                                      :project (some-> (first ps) project-name))}))))

(machine/reg-event :notebook.templatize/save
                   (fn [{:keys [db]} [_ id nm project]]
                     {:db (assoc db :busy true :err nil)
                      :fx (save-then [:notebook.templatize/post id nm project])}))

(machine/reg-event :notebook.templatize/post
                   (fn [_ [_ id nm project]]
                     {:fx [(rq/ask (busy [api/post! :document-templatize {:id id}
                                          {:name nm :project project}]
                                         :then [[:notebook.templatize/saved nm]]))]}))

(machine/reg-event :notebook.templatize/saved
                   (fn [{:keys [db]} [_ nm]]
                     {:db (cond-> db
                            (dialog-open? db :templatize) (assoc-in [:dialog :saved] nm))
                      :fx [[:invoke [staging/refresh-staged!]]]}))

(machine/reg-event :notebook.widget/open
                   (fn [{:keys [db]} [_ kind attrs pos]]
                     {:db (assoc db :wdialog {:kind kind :attrs (merge {:kind kind} attrs)
                                              :pos pos})}))

(machine/reg-event :notebook.widget/close (fn [{:keys [db]} _] {:db (assoc db :wdialog nil)}))

(machine/reg-event :notebook.widget/attrs
                   (fn [{:keys [db]} [_ m]]
                     (when (:wdialog db) {:db (update-in db [:wdialog :attrs] merge m)})))

(machine/reg-event :notebook.widget/query
                   (fn [{:keys [db]} [_ q]]
                     (when (:wdialog db) {:db (assoc-in db [:wdialog :q] q)})))

(machine/reg-event :notebook.widget/rows
                   (fn [{:keys [db]} [_ t]]
                     (when (and (seq t) (not (contains? (at db :pick-rows) t)))
                       (rq/request (assoc-in db [:pick-rows t] :loading)
                                   {:ask  [api/post! :ontology-query {:find (keyword t) :limit 200}]
                                    :path [:pick-rows t] :pick #(vec (:rows %))
                                    :else [[:rf/set [:pick-rows t] []]]}))))

(defn- lineage-graphs [r] (filterv #(= "lineage" (some-> (:mode %) name)) (:graphs r)))

(machine/reg-event :notebook.widget/graphs
                   (fn [{:keys [db]} _]
                     (when (nil? (:graphs db))
                       (rq/request (assoc db :graphs [])
                                   {:ask [api/get! :graphs] :path [:graphs]
                                    :pick lineage-graphs}))))

(machine/reg-event :notebook.editor/confirm
                   (fn [_ [_ ask f]] {:fx [[:confirm {:ask ask :ok [:rf/invoke f]}]]}))

(machine/reg-event :notebook.editor/prompt
                   (fn [_ [_ ask f]]
                     {:fx [(rq/ask {:ask [overlay/prompt! ask] :then [[:rf/invoke f]]})]}))

(machine/reg-event :notebook.editor/call
                   (fn [_ [_ c f fail]]
                     {:fx [(rq/ask {:ask  c :then [[:rf/invoke f]]
                                    :else (when fail [[:rf/invoke fail]])})]}))
