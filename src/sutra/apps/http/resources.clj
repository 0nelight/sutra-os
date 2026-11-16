(ns sutra.apps.http.resources
  (:require [sutra.kit.coll :as coll]
            [sutra.http.endpoint :as ep]
            [clojure.string :as str]
            [sutra.apps.http.scope :refer [in-scope not-in-app resource-access]]
            [sutra.apps.http.session :refer [with-app]]
            [sutra.apps.ports :as ports]
            [sutra.http.responses :refer [json-response]]
            [sutra.kit.coerce :refer [->kw]]
            [sutra.kit.tree :as tree]
            [sutra.projects.api :as kompass]
            [sutra.store.access.scope :as ctx]
            [sutra.types.rid :as rid]))

(set! *warn-on-reflection* true)

(defn- list-documents [store auth] ((:list (ports/documents store)) store auth {}))
(defn- linking [store auth t id] ((:linking (ports/documents store)) store auth t id))
(defn- document [store auth id op] ((:get (ports/documents store)) store auth id op))
(defn- save-document! [store auth d] ((:save! (ports/documents store)) store auth d))

(def ^:private listed-families
  [:object-types :applications :datasets :transforms :functions :sources :workspaces])

(defn- declares?
  [a widget]
  (some #(= widget (->kw (:widget %))) (tree/nodes (:layout a))))

(defn- project-names
  [schema]
  (into {} (map (fn [[prid p]] [prid (:project-name p)])) (:projects schema)))

(defn- resource-entry
  [schema family [r spec] ann-of]
  (let [nf   (rid/name-field-of family)
        nm   (nf spec)
        ann  (or (ann-of (str r)) (ann-of (str (some-> family name) "/" (some-> nm name))))]
    {:family      family
     :name        nm
     :rid         r
     :label       (or (not-empty (:display-name spec)) (some-> nm name))
     :description (:description spec)
     :icon        (:icon spec)
     :project     (get (project-names schema) (:project spec))
     :tags        (vec (:tags ann))
     :prominent   (boolean (:curated ann))}))

(defn- document-entry
  [schema d]
  {:family      :documents
   :name        (:id d)
   :rid         (:id d)
   :label       (:name d)
   :project     (get (project-names schema) (:project d))})

(defn- folder-contents
  [store]
  (let [nodes (kompass/read-kompass-nodes store)
        idx   (coll/index-by :rid nodes)]
    (reduce (fn [m n]
              (if-let [folder (and (:ref n) (:name (get idx (:parent-rid n))))]
                (update m (str folder) (fnil conj []) (str (:ref n)))
                m))
            {} nodes)))

(defn my-application-resources
  [req]
  (with-app req {:json? true :branch? true}
    (fn [{:keys [schema branch a store auth]}]
      (if-not (declares? a :resource-list)
        (json-response 400 {:error "diese App hat keine Resource List"})
        (let [store  (ctx/at store branch)
              ann-of (coll/index-by (comp str :ref) (kompass/read-kompass-annotations store))
              specs  (for [family listed-families
                           entry  (get schema family)
                           :when  (= :open (resource-access auth schema (val entry)))]
                       (resource-entry schema family entry ann-of))]
          (json-response
           200
           {:resources (into (vec specs)
                             (map #(document-entry schema %))
                             (list-documents store auth))
            :folders   (folder-contents store)}))))))

(defn- linked-documents
  [store auth objects]
  (into []
        (comp (mapcat (fn [{:keys [object-type id]}]
                        (linking store auth (->kw object-type) id)))
              (distinct))
        objects))

(defn- text-upload?
  [{:keys [mime name]}]
  (or (str/starts-with? (str mime) "text/")
      (re-find #"(?i)\.(md|markdown|txt|csv)$" (str name))))

(defn- link-of
  [{:keys [object-type id]}]
  {:object-type (name (->kw object-type)) :id (str id)})

(defn- add-links!
  [store auth doc-id objects username]
  (let [d (document store auth doc-id :update)]
    (save-document! store auth {:id             doc-id
                                :name           (:name d)
                                :format         (:format d)
                                :body           (:body d)
                                :links          (vec (distinct (concat (:links d)
                                                                       (map link-of objects))))
                                :author         username
                                :expect-version (:version d)})))

(defn- upload!
  [store auth a {:keys [name text]} objects username]
  (save-document! store auth {:name    name
                              :format  :markdown
                              :body    text
                              :links   (mapv link-of objects)
                              :project (:project a)
                              :author  username}))

(defn my-application-linked-resources
  [req]
  (with-app req {:json? true :branch? true}
    (fn [{:keys [schema branch a body store auth scope]}]
      (let [store    (ctx/at store branch)
            objects  (:objects body)
            username (ep/signed-in-user req)
            stray    (some #(when-not (in-scope auth schema scope :object-type (:object-type %))
                              (:object-type %))
                           objects)
            read-all #(json-response 200 {:documents (linked-documents store auth objects)})]
        (cond
          (not (declares? a :linked-resources))
          (json-response 400 {:error "diese App hat keine Linked Resources"})

          stray
          (json-response 404 (not-in-app :object-type stray))

          :else
          (case (->kw (:op body))
            :candidates (json-response 200 {:documents (list-documents store auth)})
            :link       (do (add-links! store auth (:document body) objects username)
                            (read-all))
            :upload     (if (text-upload? (:file body))
                          (do (upload! store auth a (:file body) objects username)
                              (read-all))
                          (json-response
                           415
                           {:error (str "Ein Dokument haelt Text; diese Datei ist binaer und "
                                        "hat hier keinen Ablageort.")}))
            (read-all)))))))
