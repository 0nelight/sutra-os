(ns sutra.documents.http.documents
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as ctx]
            [clojure.string :as str]
            [sutra.foundation.encoding :as enc]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.types.rid :as rid]
            [sutra.types.schema :as sch]
            [sutra.types.shape :as tshape]
            [sutra.parser.ent :as ent]
            [sutra.catalog.functions :as functions]
            [sutra.catalog.lookup :as lookup]
            [sutra.blobs.api :as blobs]
            [sutra.parser.project-emit :as pemit]
            [sutra.documents.records :as docs]
            [sutra.documents.widgets :as widgets]
            [sutra.documents.use-case :as use-case]
            [sutra.documents.sheets :as sheets]
            [sutra.notifications.api :as notifications]
            [sutra.catalog.project-scope :as scope]
            [sutra.compute.api :as compute]
            [sutra.authoring.http.oms :as oms]
            [sutra.http.endpoint :as ep]
            [sutra.http.responses :refer [ok json-response negotiated]]
            [sutra.apps.http.actions :as app-actions]
            [sutra.apps.http.scope :as app-scope]
            [sutra.apps.http.session :as app-session]
            [sutra.documents.http.live :as live]))

(set! *warn-on-reflection* true)

(defn- save-doc!
  [store auth doc]
  (if (and (:id doc) (docs/ends-live? doc))
    (live/resetting store (:id doc) #(docs/save! store auth doc))
    (docs/save! store auth doc)))

(defn- template-summary
  [spec]
  (-> (select-keys spec [:template-name :title :description :icon :format :engine
                         :status :version :targets :page :limits])
      (assoc :parameters (:parameters spec))
      (assoc :versions (vec (sort (keys (:releases spec)))))))

(defn- template-spec
  [schema nm version]
  (when-let [rid (lookup/rid-of schema :templates (->kw nm))]
    (let [spec (get-in schema [:templates rid])]
      (if version
        (when-let [snap (get (functions/releases spec) version)]
          (merge spec snap {:version version}))
        spec))))

(defn- draft-template-spec
  [schema nm draft]
  (let [kw      (->kw nm)
        parsed  (try (-> (ent/parse-entt (assoc draft :name kw) (str "draft " (name kw)))
                         (get-in [:schema :templates kw]))
                     (catch clojure.lang.ExceptionInfo e
                       (refusal/refuse! :refusal.documents/passed-ex-message-e (ex-data e) {:message (ex-message e)} e)))
        idx     (rid/name->rid-index schema)
        spec    (rid/rewrite-refs :templates
                                  (tshape/->canonical :templates parsed {:template-name kw})
                                  (fn [targets v] (or (rid/resolve-name->rid idx targets v) v)))
        missing (for [[p {:keys [object-type]}] (:parameters spec)
                      :let  [k :object-type v object-type]
                      :when (and v (not (rid/rid? v)))]
                  (str (name p) " " k " " (pr-str v)))]
    (when (seq missing)
      (refusal/refuse! :refusal.documents/draft-names-what-the-branch {} {:kw (name kw) :list (str/join ", " missing)}))
    spec))

(defn- keywordize-values
  [m]
  (into {} (map (fn [[k v]] [(->kw k) v])) (or m {})))

(defn- save-rendered!
  [store auth user spec body out m]
  (let [tn (name (:template-name spec))]
    (save-doc! store auth
               (merge {:name             (or (:name body) (:title spec) tn)
                       :format           :html
                       :body             (:html out)
                       :template         tn
                       :template-version (:version spec)
                       :page             (:page spec)
                       :author           user}
                      m))))

(defn- render-targets
  [body]
  (let [ts (or (:targets body) ["html"])]
    (mapv ->kw (if (sequential? ts) ts [ts]))))

(def list-templates
  (ep/handler :templates
              {:call (fn [{:keys [schema]}]
                       {:templates (mapv template-summary (vals (:templates schema)))})}))

(defn- template-param-spec*
  [pspec]
  (cond-> pspec
    (and (nil? (:options-from pspec))
         (#{:object :object-set} (->kw (:type pspec)))
         (:object-type pspec))
    (assoc :options-from {:object-type (->kw (:object-type pspec))})))

(defn- template-options
  [store branch spec]
  (let [params (update-vals (or (:parameters spec) {}) template-param-spec*)]
    (when (some :options-from (vals params))
      (ep/branch-read (ctx/at store branch)
                      #(app-actions/resolve-param-options %1 %2 params (vec (keys params)))))))

(def get-template
  (ep/handler :template-of
              {:respond (fn [{:keys [store schema branch name req]}]
                          (if-let [spec (template-spec schema name nil)]
                            (ok (assoc (template-summary spec)
                                       :source (:source spec)
                                       :parameters (update-vals (or (:parameters spec) {})
                                                                template-param-spec*)
                                       :options (template-options store branch spec)))
                            (negotiated req 404 {:error (str "unknown template " name)})))}))

(defn- render-template
  [store schema req nm body]
  (if-let [spec (if (:draft body)
                  (draft-template-spec schema nm (:draft body))
                  (template-spec schema nm (:version body)))]
    (negotiated req (cond-> (compute/generate spec
                                             {:store       store
                                              :schema   schema
                                              :values   (keywordize-values (:values body))
                                              :targets  (render-targets body)
                                              :preview? (boolean (:preview body))
                                              :layout   (some-> (:layout body) ->kw)})
                      (:draft body) (assoc :draft true)))
    (negotiated req 404
                {:error (str "unknown template " nm)
                 :hint  "`bin/sutra templates` lists what is installed; `render-template --file <x.entt>` renders one that is not"})))

(def post-template-render
  (ep/handler :template-render
              {:respond (fn [{:keys [store schema name body req]}]
                          (render-template store schema req name body))}))

(def post-template-generate
  (ep/handler :template-generate
              {:respond
               (fn [{:keys [store auth schema name body req user]}]
                 (if-let [spec (template-spec schema name (:version body))]
                   (let [values  (keywordize-values (:values body))
                         project (scope/resolve-project schema (:project body))
                         _       (scope/require-unplaced! auth :update project "Das Dokument")
                         out
                         (compute/generate spec
                                          {:store store :schema schema :values values
                                           :targets [:html]})]
                     (negotiated req
                                 {:document (save-rendered! store auth user spec body out
                                                            {:project    project
                                                             :parameters values
                                                             :links
                                                             (vec
                                                              (distinct
                                                               (concat (:links out)
                                                                       (:links body))))
                                                             :label      "erzeugt"})}))
                   (negotiated req 404 {:error (str "unknown template " name)})))}))

(def list-documents
  (ep/handler :documents
              {:call (fn [{:keys [store auth template]}]
                       {:documents
                        (docs/list-documents store auth
                                             {:template (some-> template clojure.core/name)})})}))

(defn- with-embeds
  [prior body]
  (let [kept (remove #(= "embed" (:via %)) prior)]
    (vec (distinct (concat kept (widgets/embed-links body))))))

(defn- notify-mentions!
  [store user before d]
  (let [me  user
        new (remove (into #{me} (widgets/mentions before)) (widgets/mentions (:body d)))]
    (when (seq new)
      (notifications/send! store (:branch store) (vec new)
                   {:source  :mention
                    :heading (str "Erwähnt in „" (:name d) "“")
                    :message (str me " hat dich im Notebook-Dokument „" (:name d)
                                  "“ erwähnt.")}))))

(def save-document
  (ep/handler :documents
              {:respond (fn [{:keys [store auth schema body req user]}]
                          (let [d (save-doc! store auth
                                             (assoc body
                                                    :links (with-embeds (:links body)
                                                             (:body body))
                                                    :project
                                                    (scope/resolve-project
                                                     schema (:project body))
                                                    :author
                                                    user))]
                            (notify-mentions! store user "" d)
                            (negotiated req {:document d})))}))

(def get-document
  (ep/handler :document-of
              {:respond (fn [{:keys [store auth id req]}]
                          (if-let [d (docs/document store auth id :read)]
                            (negotiated req d)
                            (negotiated req 404 {:error "kein solches Dokument" :id id})))}))

(def put-document
  (ep/handler :document-of
              {:respond
               (fn [{:keys [store auth id body req user]}]
                 (let [old  (docs/document store auth id :update)
                       peer (when (string? (:live body)) (:live body))
                       doc  (cond-> (assoc (dissoc body :project :expect-version) :id id
                                           :author user)
                              (not peer)            (assoc :expect-version (:expect-version body))
                              (contains? body :body) (assoc :links (with-embeds (:links old)
                                                                     (:body body))))]
                   (if (and (not peer) (not (int? (:expect-version body))))
                     (refusal/refused :refusal.documents/ein-speichern-nennt-die-geladene {:id id})
                     (let [d (if peer
                               (live/as-peer store id peer #(docs/save! store auth doc))
                               (save-doc! store auth doc))]
                       (notify-mentions! store user (:body old) d)
                       (negotiated req {:document d})))))}))

(def delete-document
  (ep/handler :document-of
              {:respond (fn [{:keys [store auth id req]}]
                          (negotiated req
                                      (live/resetting store id
                                                      #(docs/delete! store auth id))))}))

(defn post-document-templatize
  [{{:keys [id]} :path-params :keys [body-params] :as req}]
  (let [store      (:sutra/store req)
        auth    (:sutra/auth req)
        nm      (some-> (:name body-params) name str/trim)
        project (some-> (:project body-params) name)
        schema  (:sutra/schema req)
        d       (docs/document store auth id :read)]
    (cond
      (nil? d)
      (negotiated req 404 {:error "kein solches Dokument" :id id})

      (not (and nm (re-matches #"[a-z][a-z0-9-]*" nm)))
      (refusal/refused :refusal.documents/der-vorlagenname-ist-ein-api {:name nm})

      (lookup/rid-of schema :templates (keyword nm))
      (negotiated req 409 {:error (str "eine Vorlage " nm " gibt es schon") :name nm})

      (nil? project)
      (refusal/refused :refusal.documents/eine-vorlage-gehoert-einem-project {:name nm})

      (not (:edit (app-scope/workspace-rights auth schema {:project (keyword project)})))
      (refusal/refused :refusal.documents/keine-bearbeitungsrechte-im-project {} {:project project})

      :else
      (let [src    (when-let [t (:template d)]
                     (when-let [rid (lookup/rid-of schema :templates (->kw t))]
                       (-> (pemit/names-resolved (sch/rid-name-index schema) :templates
                                              {rid (get-in schema [:templates rid])})
                           (get rid))))
            spec   (pemit/document-template-spec d src {:template nm :project project})
            parsed (try {:ok (ent/parse-entt (pemit/template-decl spec {:inline? true})
                                             (str nm ".entt"))}
                        (catch clojure.lang.ExceptionInfo e {:msg (ex-message e)}))]
        (if-let [msg (:msg parsed)]
          (refusal/refused :refusal.documents/passed-msg {:name nm} {:message msg})
          (oms/install-preset (assoc req :body-params {:schema (:schema (:ok parsed))})))))))

(def get-document-versions
  (ep/handler :document-versions
              {:call (fn [{:keys [store auth id]}] {:versions (docs/versions store auth id)})}))

(def post-document-restore
  (ep/handler :document-restore
              {:respond (fn [{:keys [store auth id body req user]}]
                          (if-let [d (live/resetting
                                      store id
                                      #(docs/restore! store auth id (enc/safe-long (:seq body))
                                                      {:author user}))]
                            (negotiated req {:document d})
                            (negotiated req 404 {:error "keine solche Version" :id id})))}))

(def get-document-version
  (ep/handler :document-version
              {:respond (fn [{:keys [store auth id req] seq-no :seq}]
                          (if-let [v (docs/version store auth id (enc/safe-long seq-no))]
                            (negotiated req v)
                            (negotiated req 404
                                        {:error "keine solche Version" :id id :seq seq-no})))}))

(def post-document-regenerate
  (ep/handler :document-regenerate
              {:respond
               (fn [{:keys [store auth schema id body req user]}]
                 (let [d    (docs/document store auth id :update)
                       spec (when (:template d)
                              (template-spec schema (:template d)
                                             (if (:latest body) nil (:template-version d))))]
                   (cond
                     (nil? d)
                     (negotiated req 404 {:error "kein solches Dokument" :id id})

                     (nil? (:template d))
                     (negotiated req 400
                                 {:error "ein frei geschriebenes Dokument hat keine Vorlage, aus der es neu erzeugt werden könnte"})

                     (nil? spec)
                     (negotiated req 404
                                 {:error (str "Vorlage " (:template d) " v" (:template-version d)
                                              " ist nicht mehr installiert")})

                     :else
                     (let [values (keywordize-values (:parameters d))
                           out
                           (compute/generate spec
                                            {:store store :schema schema :values values
                                             :targets [:html]})]
                       (negotiated req
                                   {:document
                                    (save-doc! store auth
                                               {:id               id
                                                :name             (:name d)
                                                :format           :html
                                                :body             (:html out)
                                                :template-version (:version spec)
                                                :links            (:links out)
                                                :author
                                                user
                                                :label            (str "Daten aktualisiert"
                                                                       (when (:version spec)
                                                                         (str " (v" (:version spec)
                                                                              ")")))})})))))}))

(def post-document-sheet
  (ep/handler :document-sheet
              {:respond
               (fn [{:keys [store auth id body req]}]
                 (let [d (docs/document store auth id :read)]
                   (cond
                     (nil? d) (negotiated req 404 {:error "kein solches Dokument" :id id})
                     (not (sheets/sheet? d))
                     (refusal/refused :refusal.documents/dieses-dokument-ist-keine-tabelle {:id id})
                     :else
                     (let [loc (:locale body)
                           w   (if (sequential? (:sheets body))
                                 (sheets/from-sheets (:sheets body) loc)
                                 (sheets/from-body (if (string? (:body body)) (:body body)
                                                       (:body d))
                                                   loc))]
                       (negotiated req {:sheets (sheets/view w) :body (sheets/body w)})))))}))

(defn- sheet-export
  [d body]
  (let [w       (sheets/from-body (:body d) (:locale body))
        sep     (when (= "rfc4180" (some-> (:csv-separator body) name)) :rfc4180)
        targets (set (render-targets body))
        office  (cond-> {}
                  (:xlsx targets) (assoc :xlsx_b64 (sheets/xlsx-b64 w))
                  (:csv targets)  (assoc :csv (sheets/csv-text w (:sheet body) {:sep sep})))
        printed (disj targets :xlsx :csv)]
    (merge office
           (when (seq printed)
             (compute/generate {:source (sheets/html w) :format :html :page (:page d)
                               :template-name (:name d)}
                              {:targets (vec printed) :literal? true})))))

(def post-document-export
  (ep/handler :document-export
              {:respond
               (fn [{:keys [store auth schema id body req]}]
                 (if-let [d (docs/document store auth id :read)]
                   (if (sheets/sheet? d)
                     (negotiated req (sheet-export d (merge {:targets ["xlsx"]} body)))
                     (negotiated req (compute/generate
                                      {:source (widgets/resolve-body store schema (:body d) auth)
                                       :format (or (:format d) :markdown)
                                       :page   (merge (:page d) (:page body))
                                       :template-name (:name d)}
                                      {:targets  (render-targets
                                                  (merge {:targets ["pdf"]} body))
                                       :literal? true})))
                   (negotiated req 404 {:error "kein solches Dokument" :id id})))}))

(def post-document-widget
  (ep/handler :document-widget
              {:call (fn [{:keys [store schema body auth]}]
                       (let [attrs (into {} (map (fn [[k v]] [(keyword (name k)) (str v)]))
                                         (:attrs body))]
                         {:html (widgets/render-widget store schema
                                                       (dissoc attrs :locked :snapshot)
                                                       auth)}))}))

(def post-use-case-plan
  (ep/handler :use-case-plan
              {:call (fn [{:keys [id body req] :as a}]
                       (use-case/plan (assoc a :branch (ep/request-branch req)) id
                                      {:preview? (boolean (:preview? body))}))}))

(def post-use-case-apply
  (ep/handler :use-case-apply
              {:call (fn [{:keys [id req] :as a}]
                       (use-case/apply! (assoc a :branch (ep/request-branch req)) id))}))

(def get-documents-linked
  (ep/handler :documents-linked
              {:call (fn [{:keys [store auth object-type id]}]
                       {:documents (docs/linking store auth (->kw object-type) id)})}))

(defn my-application-document
  [req]
  (app-session/with-app req {:json? true :branch? true}
    (fn [{:keys [store auth schema a body]}]
      (let [allowed (into #{} (map ->kw) (get-in a [:layout :documents]))
            nm      (some-> (:template body) ->kw)]
        (cond
          (empty? allowed)
          (json-response 400 {:error "diese App erzeugt keine Belege (:documents fehlt)"})

          (not (contains? allowed nm))
          (json-response 403 {:error (str "Vorlage " (pr-str nm) " ist für diese App nicht"
                                          " freigegeben — erlaubt sind "
                                          (str/join ", " (sort (map name allowed))))})

          :else
          (if-let [spec (template-spec schema nm (:version body))]
            (let [values (keywordize-values (:values body))
                  links  (when-let [l (:link body)]
                           (when (and (:object-type l) (:id l))
                             [{:object-type (name (->kw (:object-type l)))
                               :id          (str (:id l))}]))
                  out    (compute/generate spec
                                          {:store store :schema schema :values values
                                           :targets [:html :pdf]})
                  pdf    (:pdf_b64 out)]
              (if (str/blank? pdf)
                (json-response 500 {:error (str "die Vorlage " (name (:template-name spec))
                                                " lieferte kein PDF")})
                (let [media (when (:media body)
                              (:hash (blobs/put! (ctx/blobs)
                                                 (.decode (java.util.Base64/getDecoder)
                                                          ^String pdf))))
                      doc (when (:save body)
                            (save-rendered! store auth (ep/signed-in-user req)
                                            spec body out
                                            {:parameters values
                                             :links
                                             (vec (distinct (concat links (:links out))))
                                             :label      "aus App erzeugt"}))]
                  (json-response 200
                                 {:ok true
                                  :document (some-> doc (dissoc :body))
                                  :media    media
                                  :filename
                                  (let [f (str (or (:name body) (name (:template-name spec))))]
                                    (if (str/ends-with? (str/lower-case f) ".pdf") f
                                        (str f ".pdf")))
                                  :file_b64 pdf}))))
            (json-response 404 {:error (str "unbekannte Vorlage " (pr-str nm))})))))))

(def module
  {:name        :documents
   :provides    {:documents/records {:list    docs/list-documents
                                     :linking docs/linking
                                     :get     docs/document
                                     :save!   docs/save!
                                     :delete! docs/delete!}}
   :contributes {:kompass/movers {:document docs/move!}
                 :kompass/file-rows {:document (fn [store branch id]
                                                 (docs/file-row store branch id))}
                 :http/routes
                 {:document-export         {:post post-document-export}
                  :document-sheet          {:post post-document-sheet}
                  :document-widget         {:post post-document-widget}
                  :document-of             {:get get-document :put put-document
                                            :delete delete-document}
                  :document-regenerate     {:post post-document-regenerate}
                  :document-restore        {:post post-document-restore}
                  :document-templatize     {:post post-document-templatize}
                  :document-version        {:get get-document-version}
                  :document-versions       {:get get-document-versions}
                  :documents               {:get list-documents :post save-document}
                  :documents-linked        {:get get-documents-linked}
                  :my-application-document {:post my-application-document}
                  :template-generate       {:post post-template-generate}
                  :use-case-plan           {:post post-use-case-plan}
                  :use-case-apply          {:post post-use-case-apply}
                  :template-of             {:get get-template}
                  :template-render         {:post post-template-render}
                  :templates               {:get list-templates}}}})
