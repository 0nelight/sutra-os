(ns sutra.documents.records
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.projects.api :as projects]
            [sutra.catalog.project-scope :as scope]))

(set! *warn-on-reflection* true)

(def ^:private doc-keys
  [:id :name :format :body :template :template-version :parameters :page :links
   :author :created-at :updated-at :project :version])

(def ^:private what "Dieses Dokument")

(defn- row->doc [r] (when r (select-keys r doc-keys)))

(def ^:private version-cols [:seq :document-id :label :endorsed :author :created-at])

(defn- version->map [r] (select-keys r version-cols))

(defn- file-ref [id] (str "document/" id))

(defn- readable?
  [shares auth r]
  (or (scope/may-unplaced? auth :read (:project r))
      (projects/file-role-allows? shares (file-ref (:id r)) :read)))

(defn file-row
  [store branch id]
  (ctl/get (ctx/at store branch) :documents {:branch branch :id id}))

(defn list-documents
  [store auth {:keys [template]}]
  (into []
        (comp (filter (let [shares (projects/file-roles-held store auth)]
                        #(readable? shares auth %)))
              (map #(dissoc (row->doc %) :body)))
        (ctl/list store :documents
                  (cond-> {:branch (:branch store)} template (assoc :template template)))))

(defn- read-document
  [store id]
  (row->doc (ctl/get store :documents {:branch (:branch store) :id (str id)})))

(defn document
  [store auth id op]
  (when-let [d (read-document store id)]
    (when-not (projects/file-role-may? store auth (file-ref id) op)
      (scope/require-unplaced! auth op (:project d) what))
    d))

(def autosave-every-ms
  (* 5 60 1000))

(defn- autosave? [v] (and v (nil? (:label v)) (not (:endorsed v))))

(defn- coalesces?
  [v author now-ms]
  (let [at (:created-at v)]
    (boolean (and (autosave? v) (= author (:author v)) (inst? at)
                  (< (- now-ms (inst-ms at)) autosave-every-ms)))))

(defn- latest-version [store id]
  (first (ctl/list store :document-versions {:branch (:branch store) :document-id (str id)}
                   {:limit 1})))

(defn- versioning
  [id body {:keys [label endorsed author]}]
  (fn [store _]
    (let [latest (when-not (or label endorsed) (latest-version store id))
          entry  (ctl/entry store :document-versions
                            (cond-> {:branch (:branch store) :document-id (str id)
                                     :body (or body "") :endorsed (boolean endorsed)}
                              label  (assoc :label label)
                              author (assoc :author author)))]
      (if (coalesces? latest author (inst-ms (ctl/clock store)))
        {:records (conj (:records (ctl/deleting store :document-versions
                                                {:branch (:branch store) :document-id (str id)
                                                 :seq (:seq latest)}))
                        (assoc-in entry [:value :created-at] (:created-at latest)))}
        {:records [entry]}))))

(defn- writing
  [id row expect-version must-exist?]
  (fn [store t]
    (ctl/swapping store :documents {:branch (:branch store) :id (str id)}
                  (fn [old]
                    (cond
                      (and must-exist? (nil? old))
                      (refusal/refuse! :refusal.documents/document-gone
                                       {:error :deleted :id (str id)})

                      (and (some? expect-version)
                           (not= (long expect-version) (long (or (:version old) 0))))
                      (refusal/refuse! :refusal.documents/document-changed
                                       {:error :conflict :id (str id)
                                        :version (:version old)}
                                       {:loaded expect-version})

                      :else
                      (merge old row
                             {:version (inc (long (or (:version old) 0)))})))
                  t)))

(defn- forgetting-live
  [id]
  (fn [store _]
    (ctl/deleting store :document-live {:branch (:branch store) :document-id (str id)})))

(defn live-log
  [store id]
  (or (:log (ctl/get store :document-live {:branch (:branch store) :document-id (str id)})) []))

(defn save-live-log!
  [store id log]
  (ctl/swap! store :document-live {:branch (:branch store) :document-id (str id)}
             (fn [row] {:log (into [] (distinct) (concat (:log row) log))})))

(defn ends-live?
  [{:keys [body live]}]
  (and (some? body) (not live)))

(defn save!
  [store auth {:keys [id name format body template template-version parameters page links
                      author label endorsed project expect-version] :as doc}]
  (let [existing (when id (read-document store id))
        _   (scope/require-unplaced! auth :update
                                     (if existing (:project existing) project) what)
        id  (str (or id (java.util.UUID/randomUUID)))
        row (cond-> {:id id :branch (:branch store)
                     :name (or name "Unbenanntes Dokument")
                     :format (or format :markdown)
                     :body (or body "")}
              template         (assoc :template template)
              template-version (assoc :template-version template-version)
              parameters       (assoc :parameters parameters)
              page             (assoc :page page)
              links            (assoc :links links)
              author           (assoc :author author)
              (and project (nil? existing)) (assoc :project project))]
    (ctl/commit! store auth
                 (cond-> [(writing id row expect-version false)
                          (versioning id (:body row)
                                      {:label label :endorsed endorsed :author author})]
                   (ends-live? doc) (conj (forgetting-live id))))
    (read-document store id)))

(defn move!
  [store auth id project]
  (when-let [d (document store auth id :read)]
    (scope/require-move! auth (:project d) project what)
    (ctl/update-as! store auth :documents {:branch (:branch store) :id (str id)}
                    {:project project})
    (read-document store id)))

(defn delete!
  [store auth id]
  (document store auth id :update)
  (ctl/commit! store auth
               [(fn [store _]
                  (ctl/deleting store :document-versions
                                {:branch (:branch store) :document-id (str id)}))
                (fn [store _]
                  (ctl/deleting store :documents {:branch (:branch store) :id (str id)}))
                (forgetting-live id)])
  {:deleted (str id)})

(defn versions
  [store auth id]
  (document store auth id :read)
  (mapv version->map
        (ctl/list store :document-versions
                  {:branch (:branch store) :document-id (str id)}
                  {:cols version-cols})))

(defn version
  [store auth id seq-no]
  (document store auth id :read)
  (when-let [r (ctl/get store :document-versions
                        {:branch (:branch store) :document-id (str id) :seq seq-no})]
    (assoc (version->map r) :body (:body r))))

(defn version-body
  [store id seq-no]
  (some-> (ctl/get store :document-versions
                   {:branch (:branch store) :document-id (str id) :seq seq-no})
          :body))

(defn restore!
  [store auth id seq-no {:keys [author]}]
  (let [doc  (document store auth id :update)
        body (version-body store id seq-no)]
    (when (and doc body)
      (ctl/commit! store auth [(writing id {:body body} nil true)
                               (versioning id body
                                           {:label  (str "zurückgesetzt auf #" seq-no)
                                            :author author})
                               (forgetting-live id)])
      (read-document store id))))

(defn linking
  [store auth object-type id]
  (into []
        (comp (filter (let [shares (projects/file-roles-held store auth)]
                        #(readable? shares auth %)))
              (filter (fn [d] (some (fn [l] (and (:object-type l)
                                                 (= (name object-type) (name (:object-type l)))
                                                 (= (str id) (str (:id l)))))
                                    (:links d))))
              (map #(dissoc % :body :links)))
        (mapv row->doc (ctl/list store :documents {:branch (:branch store)}))))
