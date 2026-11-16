(ns sutra.projects.http
  (:require [clojure.data.csv :as csv]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [sutra.types.rid :as rid]
            [sutra.blobs.api :as blobs]
            [sutra.calx.io.csv :as calx-csv]
            [sutra.calx.io.xlsx :as xlsx]
            [sutra.calx.core :as calx]
            [sutra.projects.file-grants :as file-grants]
            [sutra.catalog.grants :as grants]
            [sutra.types.media :as media-types]
            [sutra.foundation.settings :as settings]
            [sutra.store.world :as world]
            [sutra.store.access.scope :as ctx]
            [sutra.parser.project-emit :as pemit]
            [sutra.projects.kompass :as cp]
            [sutra.catalog.project-scope :as scope]
            [sutra.types.project-graph :as pgraph]
            [sutra.projects.ports :as ports]
            [sutra.http.endpoint :as ep]
            [sutra.kit.refusal :as refusal]
            [sutra.http.responses :refer [ok]])
  (:import (java.io ByteArrayOutputStream)))

(set! *warn-on-reflection* true)

(def list-kompass-nodes
  (ep/handler :kompass-nodes
              {:call (fn [{:keys [store auth branch]}]
                       {:nodes (scope/visible-nodes auth
                                                    (cp/read-kompass-nodes
                                                     (ctx/at store branch)))})}))

(def list-kompass-annotations
  (ep/handler :kompass-annotations
              {:call (fn [{:keys [store branch]}]
                       {:annotations (cp/read-kompass-annotations (ctx/at store branch))})}))

(def upsert-kompass-annotation
  (ep/handler :kompass-annotations
              {:respond
               (fn [{:keys [store branch body]}]
                 (let [{:keys [ref tags trashed curated doc pinned promoted]} body]
                   (if-not (seq (str ref))
                     (refusal/refuse! :refusal.projects/annotation-ref body)
                     (let [empty? (and (empty? tags) (not trashed) (not curated) (not pinned)
                                       (not promoted)
                                       (not (seq (str (or doc "")))))]
                       (let [store (ctx/at store branch)]
                         (if empty?
                           (cp/delete-kompass-annotation! store ref)
                           (cp/write-kompass-annotation! store {:ref ref :tags tags :trashed trashed
                                                                :curated curated :doc doc
                                                                :pinned pinned
                                                                :promoted promoted})))
                       (ok {:ref ref :cleared? empty?})))))}))

(def ^:private folder-what "Dieser Ordner")

(defn- kompass-move-folder!
  [store auth nodes node parent-rid to]
  (let [from (:project node)]
    (if (= from to)
      (scope/require-unplaced! auth :update from folder-what)
      (scope/require-move! auth from to folder-what))
    (cp/move-kompass-node! store auth (:rid node) parent-rid)
    (when (not= from to)
      (cp/set-kompass-project! store auth (scope/subtree-folders nodes (:rid node)) to))))

(def create-kompass-folder
  (ep/handler :kompass-folders
              {:respond
               (fn [{:keys [store auth branch schema body]}]
                 (let [{:keys [name parent-rid position]} body]
                   (if-not (seq name)
                     (refusal/refuse! :refusal.projects/folder-name body)
                     (let [folder-rid (rid/new-rid :folder (random-uuid))]
                       (let [store      (ctx/at store branch)
                             parent  (when parent-rid (cp/kompass-node store parent-rid))
                             project (if parent
                                       (:project parent)
                                       (scope/resolve-project schema (:project body)))]
                         (scope/require-unplaced! auth :update project folder-what)
                         (cp/write-kompass-node! store auth {:rid folder-rid :node-kind :folder
                                                             :parent-rid parent-rid :name name
                                                             :ref nil :position (or position 0)
                                                             :project project}))
                       (ok {:rid folder-rid})))))}))

(defn- require-node-edit!
  [auth nodes rid]
  (let [idx (scope/by-rid nodes)]
    (when-let [n (get idx rid)]
      (scope/require-unplaced! auth :update (scope/node-home idx n) folder-what)
      n)))

(def rename-kompass-folder
  (ep/handler :kompass-node-rename
              {:call (fn [{:keys [store auth branch rid body]}]
                       (let [store (ctx/at store branch)]
                         (require-node-edit! auth (cp/read-kompass-nodes store) rid)
                         (cp/rename-kompass-node! store auth rid (:to body)))
                       {:rid rid :name (:to body)})}))

(defn- kompass-move!
  [store auth rid parent-rid]
  (let [nodes (cp/read-kompass-nodes store)
        idx   (scope/by-rid nodes)
        node  (get idx rid)
        parent (get idx parent-rid)]
    (cond
      (nil? node) nil

      (= :folder (keyword (:node-kind node)))
      (kompass-move-folder! store auth nodes node parent-rid
                            (if parent (:project parent) (:project node)))

      (and (str/starts-with? (str (:ref node)) "exploration/")
           (not= (:project parent) (scope/node-home idx node)))
      (refusal/refuse! :refusal.projects/exploration-via-place {:rid rid})

      :else
      (do (scope/require-unplaced! auth :update (scope/node-home idx node) folder-what)
          (scope/require-unplaced! auth :update (:project parent) folder-what)
          (cp/move-kompass-node! store auth rid parent-rid)))))

(def move-kompass-node
  (ep/handler :kompass-node-move
              {:call (fn [{:keys [store auth branch rid body]}]
                       (let [store (ctx/at store branch)]
                         (kompass-move! store auth rid (:parent-rid body)))
                       {:rid rid :parent-rid (:parent-rid body)})}))

(def delete-kompass-node
  (ep/handler :kompass-node-of
              {:call (fn [{:keys [store auth branch rid]}]
                       (let [store       (ctx/at store branch)
                             nodes    (cp/read-kompass-nodes store)
                             node     (require-node-edit! auth nodes rid)
                             children (filter #(= rid (:parent-rid %)) nodes)]
                         (doseq [ch children]
                           (cp/move-kompass-node! store auth (:rid ch) (:parent-rid node)))
                         (cp/delete-kompass-node! store auth rid))
                       {:deleted rid})}))

(defn- move-to-project!
  [store auth ref project]
  (scope/require-home! project)
  (let [nodes (cp/read-kompass-nodes store)
        node  (get (scope/by-rid nodes) ref)
        doc   (second (re-matches #"document/(.+)" (str ref)))
        ws    (second (re-matches #"widget-set/(.+)" (str ref)))
        ex    (second (re-matches #"exploration/(.+)" (str ref)))
        file  (second (re-matches #"file/(.+)" (str ref)))]
    (cond
      file
      (if-let [f (cp/file store file)]
        (do (scope/require-move! auth (:project f) project "Diese Datei")
            (cp/move-file! store file project)
            (file-grants/moved! store ref project)
            (ok {:ref ref :project project}))
        (refusal/refuse! :refusal.projects/no-file {:ref ref}))

      ex
      (if-let [f ((get (ports/movers store) :exploration) store auth ex project)]
        (do (some->> nodes (filter #(= ref (:ref %))) first :rid
                     (cp/delete-kompass-node! store auth))
            (file-grants/moved! store ref project)
            (ok {:ref ref :project (:project f)}))
        (refusal/refuse! :refusal.projects/no-exploration {:ref ref}))

      ws
      (ok {:ref ref
           :project (:project ((get (ports/movers store) :widget-set) store auth ws project))})

      doc
      (if-let [d ((get (ports/movers store) :document) store auth doc project)]
        (do (file-grants/moved! store ref project)
            (ok {:ref ref :project (:project d)}))
        (refusal/refuse! :refusal.projects/no-document {:ref ref}))

      (and node (= :folder (keyword (:node-kind node))))
      (let [parent (get (scope/by-rid nodes) (:parent-rid node))]
        (kompass-move-folder! store auth nodes node
                              (when (= project (:project parent)) (:parent-rid node))
                              project)
        (ok {:ref ref :project project}))

      :else
      (refusal/refuse! :refusal.projects/move-kinds {:ref ref}))))

(defn- place-exploration!
  [store auth ref parent-rid]
  (let [nodes  (cp/read-kompass-nodes store)
        parent (get (scope/by-rid nodes) parent-rid)
        ex     (subs (str ref) (count "exploration/"))]
    (cond
      (not= :folder (some-> parent :node-kind keyword))
      (refusal/refuse! :refusal.projects/exploration-into-folder {:ref ref})

      (nil? (:project parent))
      (refusal/refuse! :refusal.projects/folder-without-project {:ref ref})

      :else
      (if ((get (ports/movers store) :exploration) store auth ex (:project parent))
        (if-let [existing (first (filter #(= ref (:ref %)) nodes))]
          (do (cp/move-kompass-node! store auth (:rid existing) parent-rid)
              (ok {:rid (:rid existing) :moved true :project (:project parent)}))
          (let [place-rid (rid/new-rid :placement (random-uuid))]
            (cp/write-kompass-node! store auth {:rid place-rid :node-kind :placement
                                                :parent-rid parent-rid :name ref :ref ref
                                                :position 0})
            (ok {:rid place-rid :project (:project parent)})))
        (refusal/refuse! :refusal.projects/no-exploration {:ref ref})))))

(def place-resource
  (ep/handler :kompass-place
              {:respond
               (fn [{:keys [store auth branch schema body]}]
                 (let [{:keys [ref parent-rid name position]} body]
                   (cond
                     (not (seq ref))
                     (refusal/refuse! :refusal.projects/placement-ref body)

                     (contains? body :project)
                     (let [store (ctx/at store branch)]
                       (move-to-project! store auth ref
                                         (scope/resolve-project schema (:project body))))

                     (str/starts-with? (str ref) "exploration/")
                     (place-exploration! (ctx/at store branch) auth ref parent-rid)

                     :else
                     (let [store (ctx/at store branch)]
                       (if-let [existing (->> (cp/read-kompass-nodes store)
                                              (filter #(= ref (:ref %))) first)]
                         (do (kompass-move! store auth (:rid existing) parent-rid)
                             (ok {:rid (:rid existing) :moved true}))
                         (let [place-rid (rid/new-rid :placement (random-uuid))]
                           (scope/require-unplaced! auth :update
                                                    (some->> parent-rid (cp/kompass-node store)
                                                             :project)
                                                    folder-what)
                           (cp/write-kompass-node! store auth
                                                   {:rid place-rid :node-kind :placement
                                                    :parent-rid parent-rid :name (or name ref)
                                                    :ref ref :position (or position 0)})
                           (ok {:rid place-rid})))))))}))

(defn- pprint-ent
  [ent {:keys [project skipped]}]
  (let [banner (str ";; " (if project (str "Project export: " (name project)) "Ontology export")
                    " — generated by Sutra-OS. A text changes through `projection pull`/`push`;"
                    " git refuses it, and the CLI installs no `.ent` from disk.\n"
                    (when (pos? (long (or (:credentials skipped) 0)))
                      (str ";; NOTE: " (:credentials skipped)
                           " :source(s) are declared here WITHOUT"
                           " their credential (a sealed secret never leaves the instance)."
                           " Installing this file leaves a live source's credential untouched;"
                           " a source created BY it needs `source secret <name>` once.\n"))
                    (when (pos? (long (or (:transforms skipped) 0)))
                      (str ";; NOTE: " (:transforms skipped) " generated FK producer(s) are"
                           " not listed as transforms — each rides as `:fk` sugar on its own"
                           " link-type, so this file restores them.\n")))]
    (str banner (with-out-str (pprint/pprint ent)))))

(defn- ent->stream [^String s]
  (java.io.ByteArrayInputStream. (.getBytes s "UTF-8")))

(defn- zip-bytes
  [ent-name ent-str files]
  (let [baos (ByteArrayOutputStream.)]
    (with-open [zos (java.util.zip.ZipOutputStream. baos)]
      (let [put (fn [^String path ^bytes data]
                  (.putNextEntry zos (java.util.zip.ZipEntry. path))
                  (.write zos data)
                  (.closeEntry zos))]
        (put ent-name (.getBytes ^String ent-str "UTF-8"))
        (doseq [{:keys [path content bytes]} files]
          (put path (or bytes (let [^String text (or content "")]
                                (.getBytes text "UTF-8")))))))
    (.toByteArray baos)))

(defn kompass-export
  [{:keys [query-params] schema :sutra/schema}]
  (let [project (some-> (or (get query-params "project") (get query-params :project))
                        not-empty)
        repository (some-> (or (get query-params "repository") (get query-params :repository))
                           not-empty)
        fmt     (or (some-> (or (get query-params "format") (get query-params :format))
                            not-empty)
                    "ent")
        base    (or repository project "ontology")
        {:keys [ent files skipped]}
        (pemit/export schema
                   {:project (some-> project keyword)
                    :repository (some-> repository keyword)
                    :inline? (not= "zip" fmt)})
        ent-str (pprint-ent ent {:project (some-> project keyword) :skipped skipped})]
    (if (= "zip" fmt)
      {:status  200
       :headers {"Content-Type"        "application/zip"
                 "Content-Disposition" (str "attachment; filename=\"" base ".zip\"")}
       :body    (java.io.ByteArrayInputStream. (zip-bytes (str base ".ent") ent-str files))}
      {:status  200
       :headers {"Content-Type"        "application/edn; charset=utf-8"
                 "Content-Disposition" (str "attachment; filename=\"" base ".ent\"")}
       :body    (ent->stream ent-str)})))

(defn- members-of
  [schema kinds ref-key owner-rid]
  (into {}
        (for [k kinds
              :let [hits (vec (for [[_ spec] (get schema k)
                                    :when (= (get spec ref-key) owner-rid)]
                                (get spec (rid/name-field-of k))))]
              :when (seq hits)]
          [k hits])))

(def ^:private project-member-kinds
  [:object-types :link-types :interfaces :transforms :functions :sources :shared-properties])

(defn- projects-with-members
  [schema]
  (vec (for [[proj-rid spec] (:projects schema)]
         {:id          proj-rid
          :name        (:project-name spec)
          :description (:description spec)
          :members     (members-of schema project-member-kinds :project proj-rid)})))

(def list-projects
  (ep/handler :projects {:call (fn [{:keys [schema]}] {:projects (projects-with-members schema)})}))

(def project-deps
  (ep/handler :project-deps {:call (fn [{:keys [schema]}] (pgraph/graph schema))}))

(defn- repositories-with-members
  [schema]
  (let [pname (fn [prid] (some-> (get-in schema [:projects prid :project-name]) name))]
    (vec (for [[repo-rid spec] (:repositories schema)]
           {:id          repo-rid
            :name        (:repository-name spec)
            :description (:description spec)
            :type        (:type spec)
            :project     (pname (:project spec))
            :members     (members-of schema [:transforms :functions] :repository repo-rid)}))))

(def list-repositories
  (ep/handler :repositories
              {:call (fn [{:keys [schema]}] {:repositories (repositories-with-members schema)})}))

(defonce ^:private changed-cache (atom {}))

(defonce ^:private changed-runs (atom {}))

(defn- changes-since
  [at branch tick cached]
  (let [{:keys [changed floor-at]} (world/last-changed at (:rev cached))
        entry {:tick tick
               :rev (reduce max (or (:rev cached) 0) (keep :rev (vals changed)))
               :changed (merge (:changed cached) changed)
               :floor-at (or floor-at (:floor-at cached))}]
    (swap! changed-cache #(assoc (if (> (count %) 16) {} %) branch entry))
    entry))

(defn- schema-changes
  [store branch]
  (let [at     (world/at store branch)
        tick   (world/schema-tick at)
        cached (get @changed-cache branch)]
    (if (= tick (:tick cached))
      cached
      (let [run (get (swap! changed-runs
                            (fn [runs]
                              (if (= tick (get-in runs [branch :tick]))
                                runs
                                (assoc runs branch
                                       {:tick tick
                                        :run  (delay (future (changes-since at branch tick
                                                                            cached)))}))))
                     branch)
            v   (deref @(:run run) 5000 nil)]
        (or v (assoc cached :computing true))))))

(defn- modified-times
  [store branch schema]
  (let [{:keys [changed floor-at computing]} (schema-changes store branch)]
    {:floor-at (some-> floor-at str)
     :computing (boolean computing)
     :times    (into {}
                     (keep (fn [[[section k] {:keys [at]}]]
                             (when (and at (contains? (get schema section) k))
                               [(str (name section) "/" k) (str at)])))
                     changed)}))

(defn- project-rights
  [auth schema]
  (into {}
        (for [[prid {:keys [project-name]}] (:projects schema)]
          [(name project-name)
           {:read?   (scope/may? auth :read prid)
            :update? (scope/may? auth :update prid)
            :manage? (scope/may? auth :manage-permissions prid)}])))

(def kompass-index
  (ep/handler :kompass-index
              {:call (fn [{:keys [store auth branch schema]}]
                       {:modified     (modified-times store branch schema)
                        :rights       (project-rights auth schema)
                        :shared       (file-grants/held store auth)
                        :sharing      (file-grants/projects-sharing store)
                        :collections  (cp/collections store)
                        :portfolios   (cp/portfolios store)
                        :favorites    (cp/favorites store (grants/current-user-id auth))
                        :home-folders (settings/value :home-folders)
                        :me           (grants/current-user-id auth)})}))

(def set-favorite
  (ep/handler :kompass-favorites
              {:respond (fn [{:keys [store auth body]}]
                          (if-not (seq (str (:ref body)))
                            (refusal/refuse! :refusal.projects/favorite-ref body)
                            (do (cp/set-favorite! store (grants/current-user-id auth)
                                                  (str (:ref body)) (boolean (:on body)))
                                (ok {:ref (:ref body) :on (boolean (:on body))}))))}))

(defn- project-rids
  [schema names]
  (mapv #(scope/resolve-project schema %) names))

(def create-portfolio
  (ep/handler :kompass-portfolios
              {:respond (fn [{:keys [store auth schema body]}]
                          (let [nm (some-> (:name body) str/trim not-empty)]
                            (if-not nm
                              (refusal/refuse! :refusal.projects/portfolio-name body)
                              (let [prid (rid/new-rid :portfolio (random-uuid))]
                                (cp/create-portfolio!
                                 store {:rid prid :name nm :description (:description body)
                                        :projects (project-rids schema (:projects body))
                                        :created-by (grants/current-user-id auth)})
                                (ok {:rid prid})))))}))

(def change-portfolio
  (ep/handler :kompass-portfolio
              {:call (fn [{:keys [store schema rid body]}]
                       (cp/update-portfolio!
                        store rid (cond-> (select-keys body [:name :description])
                                    (contains? body :projects)
                                    (assoc :projects (project-rids schema (:projects body)))))
                       {:rid rid})}))

(def delete-portfolio
  (ep/handler :kompass-portfolio
              {:call (fn [{:keys [store rid]}]
                       (cp/delete-portfolio! store rid)
                       {:deleted rid})}))

(defn- new-collection
  [store auth body]
  (let [nm (some-> (:name body) str/trim not-empty)]
    (if-not nm
      (refusal/refuse! :refusal.projects/collection-name body)
      (let [c-rid (rid/new-rid :collection (random-uuid))]
        (cp/create-collection! store {:rid c-rid :name nm :description (:description body)
                                      :refs (vec (:refs body))
                                      :created-by (grants/current-user-id auth)})
        (ok {:rid c-rid})))))

(def create-collection
  (ep/handler :kompass-collections
              {:respond (fn [{:keys [store auth body]}] (new-collection store auth body))}))

(def change-collection
  (ep/handler :kompass-collection
              {:call (fn [{:keys [store rid body]}]
                       (cp/update-collection! store rid body)
                       {:rid rid})}))

(def delete-collection
  (ep/handler :kompass-collection
              {:call (fn [{:keys [store rid]}]
                       (cp/delete-collection! store rid)
                       {:deleted rid})}))

(defn- file-allows?
  [store auth {:keys [id project]} op]
  (or (scope/may? auth op project)
      (file-grants/may? store auth (str "file/" id) op)))

(defn- require-file!
  [store auth id op]
  (when-let [f (cp/file store id)]
    (when-not (file-allows? store auth f op)
      (refusal/refuse! :refusal.projects/file-denied {:sutra/acl-denied true :operation op}))
    f))

(defn- decoded [^String b64]
  (when-let [^bytes bs (try (some->> b64 (.decode (java.util.Base64/getDecoder)))
                            (catch IllegalArgumentException _ nil))]
    (when (pos? (alength bs)) bs)))

(def list-files
  (ep/handler :kompass-files
              {:call (fn [{:keys [store auth]}]
                       {:files (filterv #(file-allows? store auth % :read) (cp/files store))})}))

(def upload-file
  (ep/handler :kompass-files
              {:respond
               (fn [{:keys [store auth schema body]}]
                 (let [nm      (some-> (:name body) str/trim not-empty)
                       project (scope/resolve-project schema (:project body))
                       bytes   (decoded (:bytes-b64 body))]
                   (scope/require-home! project)
                   (scope/require! auth :create project "Diese Datei")
                   (cond
                     (not (and nm bytes))
                     (refusal/refuse! :refusal.projects/file-fields {:name nm})

                     (> (alength ^bytes bytes) media-types/max-upload-bytes)
                     (refusal/refuse! :refusal.projects/file-too-large
                                      {:max media-types/max-upload-bytes})

                     :else
                     (let [{:keys [hash size]} (blobs/put! (ctx/blobs)
                                                           (java.io.ByteArrayInputStream. bytes))]
                       (ok (cp/create-file! store {:id (str (random-uuid)) :name nm
                                                   :project project :address hash
                                                   :size (or size (alength ^bytes bytes))
                                                   :mime (:mime body)
                                                   :author (grants/current-user-id auth)}))))))}))

(def file-content
  (ep/handler :kompass-file
              {:respond
               (fn [{:keys [store auth id]}]
                 (if-let [f (require-file! store auth id :read)]
                   {:status  200
                    :headers {"Content-Type" "application/octet-stream"
                              "X-Content-Type-Options" "nosniff"
                              "Cache-Control" "private, no-store"
                              "Content-Disposition"
                              (str "attachment; filename=\""
                                   (str/replace (:name f) #"[\"\\\r\n]" "_") "\"")}
                    :body    (blobs/open (ctx/blobs) (:address f))}
                   (refusal/refuse! :refusal.projects/no-file {:id id})))}))

(def delete-file
  (ep/handler :kompass-file
              {:respond (fn [{:keys [store auth id]}]
                          (if (require-file! store auth id :delete)
                            (do (cp/delete-file! store id) (ok {:deleted id}))
                            (refusal/refuse! :refusal.projects/no-file {:id id})))}))

(defn- cell-text [v] (str/trim (str v)))

(defn- column-type
  [values]
  (let [vs (remove str/blank? values)]
    (cond
      (empty? vs)                               :string
      (every? #(re-matches #"-?\d{1,18}" %) vs)   :long
      (every? #(re-matches #"-?\d+[.,]\d+" %) vs) :double
      :else                                     :string)))

(defn- column-name
  [i s]
  (let [n (-> (str/lower-case s) (str/replace #"[^a-z0-9]+" "_") (str/replace #"^_+|_+$" ""))]
    (if (re-matches #"[a-z].*" n) n (str "spalte_" (inc i)))))

(defn- parse-separated
  [text sep]
  (filterv #(not-every? str/blank? %) (csv/read-csv text :separator sep)))

(defn- text-of
  [filename ^bytes bytes]
  (if (re-find #"(?i)\.xlsx$" (str filename))
    (let [w (xlsx/read-xlsx (java.io.ByteArrayInputStream. bytes))]
      [(calx-csv/export-csv w (first (calx/sheet-names w))) \,])
    (let [text (String. bytes "UTF-8")
          head (first (str/split-lines text))]
      [text (cond
              (re-find #"(?i)\.tsv$" (str filename)) \tab
              (> (count (re-seq #";" (str head))) (count (re-seq #"," (str head)))) \;
              :else \,)])))

(defn- tabular
  [filename bytes]
  (let [[text sep]     (text-of filename bytes)
        [head & lines] (parse-separated text sep)
        names          (vec (map-indexed #(column-name %1 (cell-text %2)) head))
        cells          (fn [line i] (cell-text (get line i "")))
        types          (vec (for [i (range (count names))]
                              (column-type (map #(cells % i) lines))))
        value          (fn [t s]
                         (when-not (str/blank? s)
                           (case t
                             :long   (parse-long s)
                             :double (parse-double (str/replace s "," "."))
                             s)))]
    {:columns (mapv (fn [n t] {:name n :type t}) names types)
     :rows    (mapv (fn [line]
                      (into {} (map-indexed (fn [i n]
                                              [(keyword n) (value (types i) (cells line i))]))
                            names))
                    lines)}))

(def read-tabular
  (ep/handler :kompass-tabular
              {:respond
               (fn [{:keys [body]}]
                 (if-let [bytes (decoded (:bytes-b64 body))]
                   (if (> (alength ^bytes bytes) media-types/max-upload-bytes)
                     (refusal/refuse! :refusal.projects/file-too-large
                                      {:max media-types/max-upload-bytes})
                     (ok (tabular (:filename body) bytes)))
                   (refusal/refuse! :refusal.projects/empty-upload {})))}))

(defn- ref-file
  [store auth branch ref]
  (let [[_ kind id] (re-matches #"(document|exploration|file)/(.+)" (str ref))
        row (case kind
              ("document" "exploration")
              (when-let [read-row (get (ports/file-rows store) (keyword kind))]
                (read-row store branch id))
              "file"        (cp/file store id)
              nil)]
    (when row {:ref ref :kind kind :project (:project row) :home (:home row)
               :owner (or (:created-by row) (:author row))})))

(defn- may-share?
  [store auth {:keys [ref project home]}]
  (or (file-grants/may? store auth ref :manage-permissions)
      (if home
        (= home (grants/current-user-id auth))
        (scope/may? auth :manage-permissions project))))

(defn- on-file
  [store auth branch ref f]
  (let [file (ref-file store auth branch ref)]
    (cond
      (nil? file)
      (refusal/refuse! :refusal.projects/file-roles-kinds {:ref ref})

      (not (may-share? store auth file))
      (refusal/refuse! :refusal.projects/no-file {:ref ref})

      (not (or (:home file) (file-grants/sharing-on? store (:project file))))
      (refusal/refuse! :refusal.projects/file-roles-off {:project (:project file)})

      :else (f file))))

(defn- grants-view [store ref]
  {:grants (mapv #(select-keys % [:principal :role :via]) (file-grants/grants-on store ref))
   :link   (some-> (file-grants/link-of store ref) (select-keys [:token :role]))})

(def list-file-grants
  (ep/handler :kompass-file-grants
              {:respond (fn [{:keys [store auth branch ref]}]
                          (on-file store auth branch ref
                                   (fn [_] (ok (grants-view store ref)))))}))

(defn- role-of [body]
  (let [role (some-> (:role body) name keyword)]
    (or (file-grants/roles role)
        (refusal/refuse! :refusal.projects/file-role {:role role}))))

(def add-file-grant
  (ep/handler :kompass-file-grants
              {:respond
               (fn [{:keys [store auth branch body]}]
                 (on-file store auth branch (:ref body)
                          (fn [{:keys [project]}]
                            (cond
                              (not (seq (str (:principal body))))
                              (refusal/refuse! :refusal.projects/grant-principal body)

                              (:remove body)
                              (do (file-grants/revoke! store (:ref body) (:principal body))
                                  (ok (grants-view store (:ref body))))

                              :else
                              (do (file-grants/grant! store {:ref (:ref body)
                                                             :principal (:principal body)
                                                             :role (role-of body)
                                                             :by (grants/current-user-id auth)
                                                             :project project})
                                  (ok (grants-view store (:ref body))))))))}))

(defn- new-token []
  (str/replace (str (random-uuid) (random-uuid)) "-" ""))

(def set-share-link
  (ep/handler :kompass-share-link
              {:respond
               (fn [{:keys [store auth branch body]}]
                 (on-file store auth branch (:ref body)
                          (fn [{:keys [project]}]
                            (file-grants/relink!
                             store (:ref body)
                             (when (:on body)
                               {:token (new-token) :role (role-of body)
                                :by (grants/current-user-id auth) :project project}))
                            (ok (grants-view store (:ref body))))))}))

(def redeem-share-link
  (ep/handler :kompass-share-redeem
              {:respond
               (fn [{:keys [store auth token]}]
                 (if-let [{:keys [ref role project]} (file-grants/link-by-token store token)]
                   (do (file-grants/grant! store {:ref ref :role role :via :link :project project
                                                  :by (:created-by (file-grants/link-by-token
                                                                    store token))
                                                  :principal (grants/current-user-id auth)})
                       (ok {:ref ref :role role}))
                   (refusal/refuse! :refusal.projects/link-gone {})))}))

(def set-project-imports
  (ep/handler :kompass-project-imports
              {:call (fn [{:keys [store branch schema edge name body]}]
                       (let [project (scope/resolve-project schema name)
                             imports (project-rids schema (:imports body))]
                         (ports/install-schema! (ctx/at store branch)
                                                (if (seq imports)
                                                  (assoc-in schema [:projects project :imports]
                                                            imports)
                                                  (update-in schema [:projects project]
                                                             dissoc :imports))
                                                {:edge edge})
                         {:project name :imports (mapv str imports)}))}))

(def set-project-sharing
  (ep/handler :kompass-project-sharing
              {:call (fn [{:keys [store schema name body]}]
                       (let [project (scope/resolve-project schema name)]
                         (file-grants/set-sharing! store project (:on body))
                         {:project name :on (boolean (:on body))}))}))

(def module
  {:name        :kompass
   :contributes {:blobs/roots {:kompass-files cp/file-addresses}
                 :http/routes
                 {:kompass-annotations {:get list-kompass-annotations
                                        :post upsert-kompass-annotation}
                  :kompass-export      {:get kompass-export}
                  :kompass-folders     {:post create-kompass-folder}
                  :kompass-node-move   {:post move-kompass-node}
                  :kompass-node-of     {:delete delete-kompass-node}
                  :kompass-node-rename {:post rename-kompass-folder}
                  :kompass-nodes       {:get list-kompass-nodes}
                  :kompass-collections {:post create-collection}
                  :kompass-collection  {:put change-collection :delete delete-collection}
                  :kompass-files       {:get list-files :post upload-file}
                  :kompass-file        {:get file-content :delete delete-file}
                  :kompass-tabular     {:post read-tabular}
                  :kompass-file-grants {:get list-file-grants :post add-file-grant}
                  :kompass-share-link  {:post set-share-link}
                  :kompass-share-redeem {:post redeem-share-link}
                  :kompass-project-sharing {:post set-project-sharing}
                  :kompass-project-imports {:post set-project-imports}
                  :kompass-index       {:get kompass-index}
                  :kompass-favorites   {:post set-favorite}
                  :kompass-portfolios  {:post create-portfolio}
                  :kompass-portfolio   {:put change-portfolio :delete delete-portfolio}
                  :kompass-place       {:post place-resource}
                  :project-deps      {:get project-deps}
                  :projects          {:get list-projects}
                  :repositories      {:get list-repositories}}}})
