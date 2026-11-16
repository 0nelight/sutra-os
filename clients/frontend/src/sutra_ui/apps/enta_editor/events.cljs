(ns sutra-ui.apps.enta-editor.events
  (:require [sutra-ui.ui.overlay :as overlay]
            [clojure.string :as str]
            [sutra.capps.catalog :as cat]
            [sutra.capps.editing :as ed]
            [sutra-ui.api :as api]
            [sutra-ui.storage :as storage]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.staging :as staging]))

(def blank-draft
  (merge ed/blank
         {:app nil :descriptor nil :data {} :saved nil :version 0
          :loading? false :error nil :staged nil}))

(def preview-ms 300)

(def ui-defaults {:pane :preview :left :struktur :data-open #{:object-types}})

(defn draft-of [db] (or (:draft db) blank-draft))

(defn fresh [m] (assoc m :past [] :future [] :mode :idle :pending nil))

(defn app-key
  [d]
  (or (:app d) (some-> (:name (:file d)) name keyword) :neue-app))

(defn enta-file
  [d]
  (assoc (or (:file d) {:name (app-key d)}) :name (app-key d) :layout (:tree d)))

(defn preview-key
  [d]
  [(:app d) (some-> (:file d) (dissoc :layout)) (:tree d)])

(defn intended
  [d id args types]
  (let [n (ed/run d id args)]
    (cond-> n
      (not= (:tree n) (:tree d))
      (-> (update :file #(cat/bind-dependencies % (:tree n) types))
          (update :version inc)))))

(def ^:private recent-key "sutra.builder.recent")

(defn recent-apps
  []
  (mapv keyword (storage/get-json recent-key {:keywordize? false})))

(defn- remember-recent! [app-kw]
  (let [ids (->> (recent-apps) (remove #{app-kw}) (cons app-kw) (take 6) (map name))]
    (storage/set-json! recent-key ids)))

(defn size-key [app] (str "sutra.builder.size." (name app)))

(defn pin-key [app] (str "sutra.builder.pin." (name app)))

(defn fill
  [row]
  (reduce-kv (fn [acc k v]
               (cond
                 (#{:id :object-type} k)                    acc
                 (or (nil? v) (and (string? v) (str/blank? v))
                     (and (coll? v) (empty? v)))            acc
                 (string? v)                                (+ acc 1 (/ (count v) 200))
                 :else                                      (inc acc)))
             0 row))

(defn- candidates [resp] {:rows (vec (sort-by fill (:rows resp))) :total (:total resp)})

(defn- invoke [f & args] [:invoke (into [f] args)])

(defn- update-draft [db f & args] (assoc db :draft (apply f (draft-of db) args)))

(defn- refresh-due
  [db rid]
  (let [d (draft-of db)]
    (when (and (:preview-shown? db) (:file d) (not= (:read-for db) (preview-key d)))
      [:after {:key :preview :ms preview-ms :event [:enta.preview/refresh rid]}])))

(defn- previewing [db n req] (rq/request db (assoc req :current [[:preview-seq] n])))

(machine/reg-event :enta.preview/shown
                   (fn [{:keys [db]} [_ shown? rid]]
                     (let [db (assoc db :preview-shown? shown?)]
                       {:db db :fx [(when shown? (refresh-due db rid))]})))

(machine/reg-event :enta.preview/refresh
                   (fn [{:keys [db]} [_ rid]]
                     (let [d (draft-of db)
                           n (inc (:preview-seq db 0))]
                       (when (:file d)
                         (previewing (-> db
                                         (assoc :preview-seq n :read-for (preview-key d))
                                         (update-draft assoc :preview-source :loading))
                                     n
                                     {:ask  [api/post! :application-preview
                                             {:name (name (app-key d))} {:file (enta-file d)}]
                                      :then [[:enta.preview/staged n rid]]
                                      :else [[:enta.preview/refused n rid]]})))))

(machine/reg-event :enta.preview/staged
                   (fn [{:keys [db]} [_ n rid {:keys [id]}]]
                     (previewing db n {:ask  [api/get! :my-application {:rid id}]
                                       :then [[:enta.preview/read :draft nil]]
                                       :else [[:enta.preview/refused n rid]]})))

(machine/reg-event :enta.preview/read
                   (fn [{:keys [db]} [_ source msg r]]
                     {:db (update-draft db assoc :data (:data r) :consumer (:application r)
                                        :preview-source source :preview-error msg)}))

(defn- no-preview [db msg]
  (update-draft db assoc :data {} :consumer nil :preview-source :none :preview-error msg))

(machine/reg-event :enta.preview/refused
                   (fn [{:keys [db]} [_ n rid msg]]
                     (if rid
                       (previewing db n {:ask  [api/get! :my-application {:rid (str rid)}]
                                         :then [[:enta.preview/read :published msg]]
                                         :else [[:enta.preview/refused n nil msg]]})
                       {:db (no-preview db msg)})))

(defn- intending
  [db id args {:keys [types rid]}]
  (let [d  (draft-of db)
        d' (intended d id args types)
        db (assoc db :draft d')]
    {:db db
     :fx [(when (not= (select-keys d [:tree :file]) (select-keys d' [:tree :file]))
            (refresh-due db rid))]}))

(machine/reg-event :enta/intend
                   (fn [{:keys [db]} [_ id args env]] (intending db id args env)))

(machine/reg-event :enta/mode
                   (fn [{:keys [db]} [_ event payload]]
                     {:db (update-draft db ed/transition event payload)}))

(machine/reg-event :enta/load
                   (fn [{:keys [db]} [_ app-kw]]
                     (update (rq/request (update-draft db assoc :app app-kw :error nil
                                                       :cursor [] :staged nil)
                                         {:ask  [api/get! :application-file {:name (name app-kw)}]
                                          :busy [:draft :loading?]
                                          :then [[:enta/loaded]] :else [[:enta/load-failed]]})
                             :fx conj (invoke remember-recent! app-kw))))

(machine/reg-event :enta/loaded
                   (fn [{:keys [db]} [_ {:keys [file]}]]
                     {:db (update-draft db #(-> %
                                                (assoc :file file :descriptor file
                                                       :tree (:layout file)
                                                       :saved [file (:layout file)] :cursor [])
                                                fresh))}))

(machine/reg-event :enta/load-failed
                   (fn [{:keys [db]} [_ msg]]
                     {:db (update-draft db assoc :error (str "Laden fehlgeschlagen: " msg))}))

(machine/reg-event :enta/restore
                   (fn [{:keys [db]} [_ file]]
                     {:db (update-draft db #(-> %
                                                (assoc :file file :descriptor file
                                                       :tree (:layout file) :cursor []
                                                       :staged nil)
                                                fresh))}))

(defn new-file
  [template-id {:keys [display-name type action] :as opts}]
  (let [tree (cat/template (or template-id :leer) opts)]
    (cond-> {:name (if display-name (cat/var-slug display-name []) :neue-app)
             :layout tree}
      display-name (assoc :display-name display-name)
      type         (assoc :object-types [(keyword type)])
      action       (assoc :actions [{:action (keyword action)}]))))

(machine/reg-event :enta/new
                   (fn [{:keys [db]} [_ template-id opts]]
                     (let [file (new-file template-id opts)]
                       {:db (-> db
                                (update-draft #(-> %
                                                   (assoc :app nil :file file :descriptor file
                                                          :data {} :consumer nil :error nil
                                                          :loading? false :cursor [] :staged nil
                                                          :saved nil :tree (:layout file))
                                                   fresh))
                                (assoc :pane :preview))})))

(machine/reg-event :enta/stage
                   (fn [{:keys [db]} _]
                     (let [d (draft-of db)
                           k (app-key d)]
                       (rq/request (update-draft db assoc :error nil)
                                   {:ask   [api/post! :application-file {:name (name k)}
                                            {:file (enta-file d)}]
                                    :busy  [:draft :loading?] :error [:draft :error]
                                    :then  [[:enta/staged]]}))))

(machine/reg-event :enta/staged
                   (fn [{:keys [db]} [_ tally]]
                     {:db (update-draft db #(assoc % :staged tally :saved [(:file %) (:tree %)]))
                      :fx [(invoke staging/refresh-staged!)]}))

(machine/reg-event :enta.ui/set (fn [{:keys [db]} [_ m]] {:db (merge db m)}))

(machine/reg-event :enta.ui/flip
                   (fn [{:keys [db]} [_ k x]]
                     (let [s (get db k (get ui-defaults k #{}))]
                       {:db (assoc db k (if (contains? s x) (disj s x) (conj s x)))})))

(machine/reg-event :enta.data/focus-derived
                   (fn [{:keys [db]} [_ t nm]]
                     {:db (-> db
                              (assoc :data-focus (keyword t) :derived-editing [(keyword t) nm]
                                     :left :daten :pane :preview)
                              (update :data-open #(conj (or % (:data-open ui-defaults))
                                                        :derived)))}))

(machine/reg-event :enta.types/unbind
                   (fn [{:keys [db]} [_ users ask remaining env]]
                     (let [args {:key :object-types :value (vec remaining)}]
                       (if (empty? users)
                         (intending db :put-file-key args env)
                         {:fx [[:confirm {:ask ask
                                          :ok [:enta/intend :put-file-key args env]}]]}))))

(machine/reg-event :enta.var/add
                   (fn [{:keys [db]} [_ path ask env]]
                     (rq/request db {:ask  [overlay/prompt! ask]
                                     :then [[:enta.var/named path env]]})))

(machine/reg-event :enta.var/named
                   (fn [{:keys [db]} [_ path env words]]
                     (when-not (str/blank? words)
                       (let [nm (cat/var-slug words (cat/declared-vars (:tree (draft-of db))))]
                         (update (intending db :put-var
                                            {:path path :name nm :spec (cat/blank-var :state)}
                                            env)
                                 :db assoc :open-var [path nm])))))

(machine/reg-event :enta.new/start
                   (fn [_ [_ id]]
                     (let [tpl (some #(when (= (keyword id) (:id %)) %) cat/templates)]
                       (if (:needs-type? tpl)
                         {:fx [[:dispatch [:enta.ui/set {:asking {:id (:id tpl)}}]]]}
                         {:fx [[:dispatch [:enta/new (:id tpl) nil]]]}))))

(machine/reg-event :enta.new/create
                   (fn [{:keys [db]} [_ opts]]
                     {:db (assoc db :asking nil)
                      :fx [[:dispatch [:enta/new (get-in db [:asking :id]) opts]]]}))

(machine/reg-event :enta.objects/need
                   (fn [{:keys [db]} [_ ot]]
                     (when-not (get-in db [:candidates ot])
                       (rq/request db {:ask   [api/list-objects ot {:limit 200 :offset 0}]
                                       :path  [:candidates ot] :pick candidates
                                       :busy  [:candidates ot :loading?]
                                       :error [:candidates ot :error]}))))

(machine/reg-event :enta.preview/pin
                   (fn [{:keys [db]} [_ app id]]
                     {:db (assoc-in db [:pins app] id)
                      :fx [(invoke storage/set-json! (pin-key app) {:id id})]}))

(machine/reg-event :enta.preview/size
                   (fn [{:keys [db]} [_ app s]]
                     {:db (assoc-in db [:sizes app] s)
                      :fx [(invoke storage/set-json! (size-key app) s)]}))
