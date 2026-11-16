(ns sutra.wire.api
  (:require [clojure.string :as str]
            [sutra.types.paths :as paths]
            [sutra.types.payloads :as pl]
            [sutra.types.table :as tbl]
            [sutra.wire.transport :as tx]))

(defn get!    ([id] (get! id nil))    ([id m] (tx/send! :get (paths/address id m))))
(defn delete! ([id] (delete! id nil)) ([id m] (tx/send! :delete (paths/address id m))))
(defn post!   ([id] (post! id nil nil)) ([id body] (post! id nil body))
  ([id m body] (tx/send! :post (paths/address id m) body)))
(defn put!    ([id body] (put! id nil body))
  ([id m body] (tx/send! :put (paths/address id m) body)))

(defn negotiate-url [] (paths/url :negotiate))
(defn current-schema []        (get! :schema))
(defn list-objects
  ([type-kw] (list-objects type-kw nil))
  ([type-kw {:keys [limit offset]}]
   (get! :objects-of {:type type-kw :limit limit :offset offset})))

(defn list-links
  ([] (list-links {}))
  ([{:keys [type from to limit]}]
   (get! :links {:type type :from from :to to :limit limit})))

(defn install-package
  ([package] (install-package package nil))
  ([package {:keys [preview?]}]
   (post! :package-install {:package (name package) :preview? (boolean preview?)})))

(defn documents      [& [tpl]] (get! :documents (cond-> {} tpl (assoc :template (name tpl)))))
(defn delete-object-type
  ([nm] (delete-object-type nm false))
  ([nm cascade?] (delete! :object-type-by {:name nm :cascade (when cascade? true)})))
(defn delete-conn-type
  ([nm] (delete-conn-type nm false))
  ([nm cascade?] (delete! :conn-type-by {:name nm :cascade (when cascade? true)})))

(defn delete-function [nm]      (delete! :function-by {:name nm}))

(defn mutate
  ([action args] (mutate action args nil))
  ([action args {:keys [publish? stage? dry-run? return-edits?]}]
   (post! :actions
          (cond-> (cond dry-run? {:dry-run true}
                        publish? {:publish true}
                        stage?   {:stage true})
            return-edits? (assoc :return-edits true))
          (pl/action-call action args))))

(defn mutate-batch
  ([action arg-maps] (mutate-batch action arg-maps nil))
  ([action arg-maps {:keys [publish? stage? return-edits?]}]
   (post! :actions-batch
          (cond-> (cond publish? {:publish true}
                        stage?   {:stage true})
            return-edits? (assoc :return-edits true))
          {:action action :requests (vec arg-maps)})))

(defn list-graphs
  ([] (list-graphs {}))
  ([{:keys [area mode project]}]
   (get! :graphs {:area area :mode mode :project project})))

(defn list-branches
  ([] (list-branches nil))
  ([{:keys [closed? machine?]}]
   (get! :branches {:closed (when closed? true) :machine (when machine? true)})))
(defn list-proposals
  ([] (list-proposals nil))
  ([{:keys [status target source]}]
   (get! :proposals {:status status :target target :source source})))
(defn ask-review
  ([id] (ask-review id nil))
  ([id reviewers] (post! :proposal-ask-review {:id id}
                         (cond-> {} (seq reviewers) (assoc :reviewers (vec reviewers))))))

(defn add-comment
  ([id body]      (add-comment id body nil))
  ([id body task] (post! :proposal-comments {:id id}
                         (cond-> {:body body} task (assoc :task (name task))))))
(def rename-paths
  {:project          :project-rename
   :repository       :repository-rename
   :object-type      :object-type-rename
   :object-type-group :object-type-group-rename
   :link-type        :conn-type-rename
   :interface        :interface-rename
   :function         :function-rename
   :dataset          :dataset-rename
   :transform        :transform-rename
   :application      :application-rename})

(defn rename
  [kind old to]
  (if-let [path-key (rename-paths (keyword kind))]
    (post! path-key {:name (name old)} {:to (name to)})
    {:status 400 :body (tbl/unknown :unknown-kind "rename-able kind" rename-paths (keyword kind))}))

(defn object-counts       []     (get! :object-counts))

(defn my-workspace-search
  [rid {:keys [q type set limit offset]}]
  (get! :my-workspace-search (cond-> {:rid rid}
                               q      (assoc :q q)
                               type   (assoc :type type)
                               set    (assoc :set set)
                               limit  (assoc :limit limit)
                               offset (assoc :offset offset))))
(defn test-datasource     [body] (post! :datasource-test body))
(defn delete-dataset
  ([nm] (delete-dataset nm false))
  ([nm cascade?] (delete! :dataset-by {:name nm :cascade (when cascade? true)})))
#?(:clj
   (defn put-blob
     [bytes] (tx/post-bytes! (paths/url :blob-upload) bytes)))

(defn list-runs
  ([] (get! :runs))
  ([kind-or-params]
   (let [params (if (map? kind-or-params) kind-or-params {:kind kind-or-params})]
     (get! :runs (into {} (remove (comp nil? val))
                       (select-keys params [:kind :hours :limit :brief]))))))
(defn active-builds  []   (get! :builds-active))
(defn preview-transform
  ([name]              (preview-transform name nil nil nil))
  ([name limit]        (preview-transform name limit nil nil))
  ([name limit sample] (preview-transform name limit sample nil))
  ([name limit sample params] (preview-transform name limit sample params nil))
  ([name limit sample params output]
   (get! :preview-transform
         (cond-> {:name name :limit limit :sample sample
                  :param (mapv (fn [[k v]] (str (clojure.core/name k) "=" v)) params)}
           output (assoc :output output)))))

(defn test-transform [name] (get! :test-transform {:name name}))

(defn build-estimate
  ([] (build-estimate {}))
  ([{:keys [only strategy from days calls-per-row]}]
   (let [csv #(when (seq %) (str/join "," (map name %)))]
     (get! :build-estimate {:only (csv only) :strategy strategy :from (csv from)
                            :days days :calls-per-row calls-per-row}))))

(defn stream-records
  ([nm] (stream-records nm nil))
  ([nm {:keys [partition start-offset limit]}]
   (get! :stream-records {:name (name nm) :partition partition
                          :start-offset start-offset :limit limit})))
(defn stream-subscribe-url [nm] (paths/address :stream-subscribe {:name (name nm)}))

(defn upsert-schedule [body] (post! :schedules body))
(defn upsert-automation [body] (post! :automations body))
(defn preview-automation
  ([id] (preview-automation id nil))
  ([id {:keys [object-id with-model?]}]
   (post! :automation-preview {:id id}
          (when (or object-id with-model?)
            (cond-> {} object-id (assoc :object-id object-id)
              with-model? (assoc :with-model? true))))))
(defn automation-events
  ([id] (automation-events id nil nil))
  ([id status {:keys [limit offset]}]
   (get! :automation-events (cond-> {:id id}
                              status (assoc :status (name status))
                              limit  (assoc :limit limit)
                              offset (assoc :offset offset)))))
(defn automation-proposals
  ([id] (automation-proposals id nil))
  ([id status] (automation-proposals id status nil))
  ([id status {:keys [limit offset]}]
   (get! :automation-proposals (cond-> {:id id}
                                 status (assoc :status (name status))
                                 limit  (assoc :limit limit)
                                 offset (assoc :offset offset)))))
(defn accept-proposal
  ([id proposal] (accept-proposal id proposal nil))
  ([id proposal edits]
   (post! :automation-proposal-accept {:id id :proposal proposal}
          (when (seq edits) {:args edits}))))
(defn upsert-health-check [body] (post! :health-checks body))
(defn reindex-object-type
  ([type] (reindex-object-type type nil))
  ([type {:keys [force?]}]
   (post! :object-type-reindex {:type type :force (when force? true)} nil)))
(defn data-freshness [items] (post! :data-freshness {:items items}))
(defn edit-history [type & [opts]]
  (get! :object-type-edit-history
        (into {:type type}
              (filter (comp some? val))
              (select-keys opts [:limit :pk :token :sort :kinds :editors :props :timeline]))))
(defn delete-transform  [t] (delete! :transform-by {:name t}))
(defn delete-action-type [a] (delete! :action-type-by {:name a}))
