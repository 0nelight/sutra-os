(ns sutra.objects.explorer
  (:require [sutra.kit.refusal :as refusal]
            [sutra.store.access.scope :as ctx]
            [sutra.store.control :as ctl]
            [sutra.projects.api :as projects]
            [sutra.catalog.grants :as grants]
            [sutra.foundation.settings :as settings]
            [sutra.catalog.project-scope :as scope]
            [sutra.types.rid :as rid]
            [sutra.catalog.lookup :as lookup]
            [sutra.types.ctx :as tctx]))

(set! *warn-on-reflection* true)

(defn- live [store] (ctx/at store tctx/default-branch))

(def ^:private what "Diese Exploration")
(def ^:private kinds #{:exploration :list})

(defn- layout-row->map [r]
  (select-keys r [:object-type :layout :updated-at]))

(defn readable-type!
  [store schema ot]
  (let [rid (lookup/rid-of schema :object-types (keyword ot))]
    (when-not rid
      (refusal/refuse! :refusal.objects/explorer-unknown-type {:object-type ot} {:name (name ot)}))
    (grants/authorize-read! (grants/read-rights store) (get-in schema [:object-types rid]) rid)))

(defn exploration-rid? [x] (= :exploration (rid/rid-kind x)))

(defn- me [store] (grants/current-user-id (grants/read-rights store)))

(defn- ->file
  [store {:keys [id home created-by] :as row}]
  (-> (select-keys row [:id :kind :object-type :name :description :state :project :created-by
                        :created-at :updated-at])
      (assoc :rid id :private (some? home) :mine (= created-by (me store)))))

(defn- row-of [store id] (ctl/get store :explorations {:id (str id)}))

(defn- file-ref [id] (str "exploration/" id))

(defn- share-allows?
  [store id op]
  (projects/file-role-may? store (grants/read-rights store) (file-ref id) op))

(defn- visible?
  [store shares {:keys [id home project]}]
  (or (projects/file-role-allows? shares (file-ref id) :read)
      (if home
        (= home (me store))
        (scope/may? (grants/read-rights store) :read project))))

(defn- readable-row
  [store id]
  (when-let [row (row-of store id)]
    (cond
      (share-allows? store id :read) row
      (:home row) (when (= (:home row) (me store)) row)
      :else (do (scope/require! (grants/read-rights store) :read (:project row) what)
                row))))

(defn- change-row!
  ([store id] (change-row! store id :update))
  ([store id op]
   (when-let [row (readable-row store id)]
     (when-not (share-allows? store id op)
       (if (:home row)
         (when (not= (:home row) (me store))
           (refusal/refuse! :refusal.objects/exploration-not-owner {:id id}))
         (scope/require! (grants/read-rights store) :update (:project row) what)))
     row)))

(defn- require-home-folders!
  []
  (when-not (settings/value :home-folders)
    (refusal/refuse! :refusal.objects/no-home-files {})))

(defn- drop-placement!
  [store id]
  (let [live (live store)]
    (projects/unplace! live (grants/read-rights store) (str "exploration/" id))))

(defn row
  [store id]
  (ctl/get store :explorations {:id id}))

(defn list-files
  [store {:keys [object-type]}]
  (into []
        (comp (filter (let [shares (projects/file-roles-held store (grants/read-rights store))]
                        #(visible? store shares %)))
              (map #(->file store %)))
        (ctl/list store :explorations (cond-> {} object-type (assoc :object-type object-type)))))

(defn read-file [store id] (some->> (readable-row store id) (->file store)))

(defn definition
  [store id]
  (:definition (readable-row store id)))

(defn create!
  [store {:keys [kind object-type name description state definition project]}]
  (when-not (kinds kind)
    (refusal/refuse! :refusal.objects/exploration-kind {:kind kind}))
  (if project
    (scope/require! (grants/read-rights store) :update project what)
    (require-home-folders!))
  (let [id (rid/new-rid :exploration (random-uuid))]
    (ctl/commit! store [[:insert :explorations
                         [{:id id :kind kind :object-type object-type :name name
                           :description description :state (or state {}) :definition definition
                           :project project :home (when-not project (me store))
                           :created-by (me store)}]]])
    (read-file store id)))

(defn update!
  [store id changes]
  (when (change-row! store id)
    (ctl/commit! store [[:update :explorations
                         {:id (str id)}
                         (select-keys changes [:name :description :state :definition])]])
    (read-file store id)))

(defn make-private!
  [store id]
  (when (change-row! store id)
    (require-home-folders!)
    (drop-placement! store id)
    (ctl/commit! store [[:update :explorations
                         {:id (str id)}
                         {:project nil :home (me store)}]])
    (read-file store id)))

(defn move!
  [store auth id project]
  (when-let [row (readable-row store id)]
    (if (:home row)
      (scope/require! auth :update project what)
      (scope/require-move! auth (:project row) project what))
    (ctl/commit! store [[:update :explorations
                         {:id (str id)}
                         {:project project :home nil}]])
    (read-file store id)))

(defn delete!
  [store id]
  (when (change-row! store id :delete)
    (drop-placement! store id)
    (ctl/commit! store [[:delete :explorations {:id (str id)}]])
    {:deleted (str id)}))

(defn read-layout [store object-type]
  (let [store (live store)]
    (some-> (ctl/get store :explorer-dashboards {:branch (:branch store) :object-type object-type})
            layout-row->map)))

(defn upsert-layout! [store {:keys [object-type layout]}]
  (let [store (live store)]
    (ctl/commit! store [[:upsert :explorer-dashboards
                         [{:branch (:branch store) :object-type object-type :layout layout}]]])
    (read-layout store object-type)))

(def module {:name :explorer})
