(ns sutra-ui.state.panel
  (:require [sutra.kit.paging :as pg]
            [sutra.types.rid :as rid]
            [sutra.types.transform-io :as tio]
            [sutra-ui.api :as api]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.data :as data]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]))

(declare panel-instances)

(defn rows-of
  [type-kw]
  (get-in @panel-instances [type-kw :rows] []))

(defonce panel-instances (db/cursor [:panel :panel-instances] {}))

(defonce panel-selected-run (db/cursor [:panel :panel-selected-run] {}))

(defonce preview-instances (db/cursor [:panel :preview-instances] {}))

(defonce complete-instances (db/cursor [:panel :complete-instances] {}))

(defonce panel-selected-output (db/cursor [:panel :panel-selected-output] {}))

(defonce
  panels (db/cursor [:panel :panels] {}))

(defn panel [art & args] (get @panels (into [art] args)))

(defn- next-req
  [db]
  (let [db (update-in db [:panel :req] (fnil inc 0))]
    [db (get-in db [:panel :req])]))

(defn- tagged [type-kw rows] (mapv #(assoc % :object-type type-kw) rows))

(defn fetch-panel-page
  [type-kw {:keys [limit offset query]}]
  (if (seq query)
    (let [offset (or offset 0)]
      (-> (api/post! :ontology-query
                     (merge query {:find type-kw :limit limit :offset offset :count? true}))
          (.then (fn [{:keys [rows count]}]
                   (pg/page {:rows rows :offset offset :limit limit :total count})))))
    (api/list-objects type-kw {:limit limit :offset offset})))

(db/reg :sutra.panel/failed
        (fn [db [_ p m sentence]]
          {:db (assoc-in db p (merge {:loading? false :error sentence} m))}))

(defn- cached [type-kw extra tag? resp]
  (let [page? (and (map? resp) (contains? resp :rows))
        rows  (vec (if page? (:rows resp) (or resp [])))]
    (merge {:loading? false :rows (if tag? (tagged type-kw rows) rows)}
           (when page? (select-keys resp [:offset :limit :total :next-offset]))
           extra)))

(db/reg :sutra.panel/fill-cache
        (fn [db [_ cache type-kw ask extra tag?]]
          (let [[db req] (next-req db)
                p        [:panel cache type-kw]]
            (rq/request (update-in db p assoc :loading? true :req req)
                        {:ask     ask :path p :pick #(cached type-kw extra tag? %)
                         :current [(conj p :req) req]
                         :else    [[:sutra.panel/failed p (merge {:rows []} extra)]]}))))

(defn refresh-panel-instances!
  ([type-kw] (refresh-panel-instances! type-kw nil))
  ([type-kw opts]
   (when type-kw
     (db/send! [:sutra.panel/fill-cache :panel-instances type-kw
                [fetch-panel-page type-kw opts] {:query (:query opts)} true]))))

(defn refresh-preview-instances!
  [type-kw]
  (when type-kw
    (db/send! [:sutra.panel/fill-cache :preview-instances type-kw
               [api/list-objects type-kw {:limit 200}] nil false])))

(defn- complete-req [type-kw req]
  (let [p [:panel :complete-instances type-kw]]
    (assoc req :busy (conj p :loading?) :error (conj p :error))))

(db/reg :sutra.panel/complete
        (fn [db [_ type-kw]]
          (rq/request db (complete-req type-kw
                                       {:ask  [api/post! :ontology-query {:find type-kw}]
                                        :then [[:sutra.panel/complete-rows type-kw]]}))))

(db/reg :sutra.panel/complete-rows
        (fn [db [_ type-kw {:keys [rows]}]]
          (let [rows (tagged type-kw rows)
                ids  (mapv (comp str :id) rows)
                db   (assoc-in db [:panel :complete-instances type-kw :rows] rows)]
            (if (empty? ids)
              {:db (assoc-in db [:panel :complete-instances type-kw :links] [])}
              (rq/request db (complete-req type-kw
                                           {:ask  [api/post! :instances-around
                                                   {:ids ids :types [type-kw] :links-only? true}]
                                            :path [:panel :complete-instances type-kw :links]
                                            :pick (comp vec :links)}))))))

(defn load-complete-type! [type-kw] (db/send! [:sutra.panel/complete type-kw]))

(db/reg :sutra.panel/reload-complete
        (fn [db _]
          {:fx (mapv (fn [t] [:dispatch [:sutra.panel/complete t]])
                     (keys (get-in db [:panel :complete-instances])))}))

(defn reload-complete-types! [] (db/send! [:sutra.panel/reload-complete]))

(defn lake-snapshot-ref?
  [x]
  (boolean (re-matches #"\d+" (str x))))

(def ^:private unaddressable-at
  {:loading? false :unaddressable? true :rows [] :spec nil})

(defn- at-lake [commit _] (when (lake-snapshot-ref? commit) unaddressable-at))

(defn- dataset-rid [d-kw] (get-in @catalog/canonical-schema [:datasets d-kw :id]))

(defn- picked-rows [r] {:rows (or (:rows r) [])})

(defn- picked-spec [r] {:spec (:spec r)})

(defn- picked-tables [r] {:tables (or (:tables r) [])})

(def ^:private panel-arts
  {:transform-rows     {:endpoint :transform-rows :params (fn [out] {:name out})
                        :pick picked-rows :failed {:rows []}}
   :transform-spec     {:endpoint :transform-spec :params (fn [t] {:name t}) :pick picked-spec}
   :transform-data-at  {:endpoint :transform-rows
                        :params (fn [commit out] {:name out :as-of commit})
                        :pick picked-rows :failed {:rows []}}
   :transform-spec-at  {:endpoint :transform-spec
                        :params (fn [commit t] {:name t :as-of commit}) :pick picked-spec}
   :dataset-data-at    {:endpoint :dataset-preview
                        :params (fn [commit d] {:id (dataset-rid d) :as-of commit})
                        :pick (fn [r] {:rows (vec (:rows r)) :columns (vec (:columns r))})
                        :failed {:rows []}
                        :refuse (fn [_ d]
                                  (when-not (dataset-rid d)
                                    {:loading? false :rows []
                                     :error "dataset not in the current schema snapshot"}))}
   :datasource-data-at {:endpoint :datasource-preview
                        :params (fn [commit ds] {:name ds :as-of commit})
                        :pick picked-tables :failed {:tables []}}
   :datasource-spec-at {:endpoint :datasource-spec
                        :params (fn [commit ds] {:name ds :as-of commit}) :pick picked-spec}
   :datasource-preview {:endpoint :datasource-preview :params (fn [ds] {:name ds})
                        :pick picked-tables :failed {:tables []}}
   :data-at            {:endpoint :at-objects
                        :params (fn [commit t] {:commit commit :type t})
                        :pick picked-rows :failed {:rows []} :refuse at-lake}
   :schema-at          {:endpoint :at-object-type
                        :params (fn [commit t] {:commit commit :type t})
                        :pick picked-spec :refuse at-lake}
   :transactions       {:endpoint :catalog-transactions
                        :params (fn [node] {:dataset (name node) :limit 100})
                        :pick (fn [r] {:transactions (vec (:transactions r)) :via (:via r)})
                        :failed {:transactions []}}
   :run-jobs           {:endpoint :run-jobs :params (fn [id] {:id id})
                        :pick (fn [r] {:jobs (vec (:jobs r))}) :failed {:jobs []}}
   :related            {:endpoint :lineage-related
                        :params (fn [kind node] {:node node :kind kind})
                        :pick (fn [r] {:items (vec (:items r))}) :failed {:items []}}})

(db/reg :sutra.panel/fill
        (fn [db [_ slot ask pick {:keys [extra] :as opts}]]
          (let [[db req] (next-req db)
                p        [:panel :panels slot]]
            (rq/request (assoc-in db p (merge {:loading? true :req req} extra))
                        {:ask     ask :path p :pick #(merge {:loading? false} (pick %) extra)
                         :current [(conj p :req) req]
                         :else    [[:sutra.panel/failed p (merge (:failed opts) extra)]]}))))

(db/reg :sutra.panel/hold
        (fn [db [_ slot held]] {:db (assoc-in db [:panel :panels slot] held)}))

(defn load-panel!
  [art & args]
  (when (every? some? args)
    (let [{:keys [endpoint params pick refuse] :as spec} (panel-arts art)
          slot (into [art] args)]
      (db/send! (if-let [held (and refuse (apply refuse args))]
                  [:sutra.panel/hold slot held]
                  [:sutra.panel/fill slot [api/get! endpoint (apply params args)] pick
                   (select-keys spec [:failed])])))))

(def ^:private branch-panel-arts
  #{:transform-rows :transform-spec :datasource-preview :transactions :related})

(defn reload-branch-panels! []
  (doseq [[art & args] (keys @panels) :when (branch-panel-arts art)]
    (apply load-panel! art args)))

(defn- previewed [r]
  {:rows (or (:rows r) []) :input-caps (:input-caps r) :error (:error r)})

(defn preview-panel-transform!
  [t-kw output-name-kw]
  (when (and t-kw output-name-kw)
    (db/send! [:sutra.panel/fill [:transform-rows output-name-kw]
               [api/preview-transform (name t-kw)] previewed
               {:failed {:rows []} :extra {:preview? true}}])))

(db/reg :sutra.panel/output
        (fn [s [_ t-kw out]] {:db (assoc-in s [:panel :panel-selected-output t-kw] out)}))

(db/reg :sutra.panel/run
        (fn [s [_ type-kw run-id]] {:db (assoc-in s [:panel :panel-selected-run type-kw] run-id)}))

(defn select-panel-output! [t-kw output-name-kw]
  (db/send! [:sutra.panel/output t-kw output-name-kw])
  (load-panel! :transform-rows output-name-kw))

(def transaction-chain-kinds
  #{:object-type :transform :dataset})

(defn select-panel-run!
  ([type-kw run]
   (let [kind (some-> (:detail-modal @shell/ui-state) :kind)]
     (select-panel-run! (or kind :object-type) type-kw run)))
  ([kind type-kw run]
   (let [run-id (:id run)
         commit (:snapshot-id run)]
     (db/send! [:sutra.panel/run type-kw run-id])
     (load-panel! :run-jobs run-id)
     (when commit
       (case kind
         :object-type
         (do (load-panel! :data-at commit type-kw)
             (load-panel! :schema-at commit type-kw))

         :transform
         (do (when-let [out (get @panel-selected-output type-kw)]
               (load-panel! :transform-data-at commit out))
             (load-panel! :transform-spec-at commit type-kw))

         :datasource
         (do (load-panel! :datasource-data-at commit type-kw)
             (load-panel! :datasource-spec-at commit type-kw))

         :dataset
         (load-panel! :dataset-data-at commit type-kw)

         nil)))))

(def schema-panel-kinds
  #{:object-type :transform :datasource :dataset})

(shell/reg ::open
           (fn [db kind id]
             (assoc db
                    :detail-modal {:kind kind :id id}
                    :bottom-panel (merge {:tab :preview :collapsed? false}
                                         (:bottom-panel db)))))

(defn open-bottom-panel!
  [kind id]
  (shell/send! [::open kind id])
  (catalog/hydrate-body! (rid/tag->kinds-key kind) id)
  (when (transaction-chain-kinds kind)
    (load-panel! :transactions id))
  (load-panel! :related kind id)
  (cond
    (= kind :object-type)
    (do (refresh-preview-instances! id)
        (data/refresh-runs!))

    (= kind :transform)
    (do (load-panel! :transform-spec id)
        (let [spec     (get-in @catalog/canonical-schema [:transforms id])
              outs     (tio/outputs-of spec)
              first-out (some-> outs first :name)]
          (when first-out
            (db/send! [:sutra.panel/output id first-out])
            (load-panel! :transform-rows first-out)))
        (data/refresh-runs!))

    (= kind :datasource)
    (do (load-panel! :datasource-preview id)
        (data/refresh-runs!))

    (= kind :dataset)
    (do (when-let [rid (get-in @catalog/canonical-schema [:datasets id :id])]
          (data/load-dataset-preview! rid))
        (data/refresh-runs!))))

(shell/reg ::close-bottom-panel (fn [db] (assoc db :detail-modal nil :bottom-panel nil)))
(defn close-bottom-panel! [] (shell/send! [::close-bottom-panel]))

(shell/reg ::set-panel-tab (fn [db tab] (assoc-in db [:bottom-panel :tab] tab)))
(defn set-panel-tab! [tab] (shell/send! [::set-panel-tab tab]))

(shell/reg ::toggle-panel-collapsed (fn [db] (update-in db [:bottom-panel :collapsed?] not)))
(defn toggle-panel-collapsed! [] (shell/send! [::toggle-panel-collapsed]))

(shell/reg ::set-panel-editing (fn [db v] (assoc-in db [:bottom-panel :editing?] (boolean v))))
(defn set-panel-editing! [v] (shell/send! [::set-panel-editing v]))

(shell/reg ::set-panel-column-query (fn [db k q] (assoc-in db [:bottom-panel :col-query k] q)))
(defn set-panel-column-query! [k q] (shell/send! [::set-panel-column-query k q]))

(shell/reg ::select-panel-txn (fn [db txn] (assoc-in db [:bottom-panel :selected-txn] txn)))
(defn select-panel-txn! [txn] (shell/send! [::select-panel-txn txn]))

(shell/reg ::set-panel-history-sub (fn [db sub] (assoc-in db [:bottom-panel :history-sub] sub)))
(defn set-panel-history-sub! [sub] (shell/send! [::set-panel-history-sub sub]))

(db/reg :sutra.panel/more
        (fn [db [_ type-kw]]
          (let [p (get-in db [:panel :panel-instances type-kw])
                {:keys [limit query req]} p]
            (when-let [o (pg/next-offset p)]
              (rq/request db {:ask     [fetch-panel-page type-kw
                                        {:limit limit :offset o :query query}]
                              :dedup   [:sutra.panel/more type-kw o]
                              :busy    [:panel :panel-instances type-kw :loading?]
                              :current [[:panel :panel-instances type-kw :req] req]
                              :then    [[:sutra.panel/more-rows type-kw]]})))))

(db/reg :sutra.panel/more-rows
        (fn [db [_ type-kw resp]]
          {:db (update-in db [:panel :panel-instances type-kw]
                          #(pg/append % (update resp :rows (partial tagged type-kw))))}))

(defn load-more-instances!
  [type-kw]
  (db/send! [:sutra.panel/more type-kw]))
