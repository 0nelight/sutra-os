(ns sutra-ui.apps.atlas.model
  (:require [sutra.types.rid :as rid]
            [sutra-ui.apps.atlas.events]
            [sutra-ui.state.atlas :as ks]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.machine :as machine]
            [sutra-ui.state.ontology :as ontology]
            [sutra-ui.state.shell :as shell]))

(defonce ui (shell/slice :home))

(defonce kompass (machine/frame ui {:name :kompass}))

(defn send! [event] (machine/dispatch event {:frame kompass}))

(defn at
  [k]
  @(machine/subscribe [:kompass/at k] {:frame kompass}))

(defn filters-of [scope] @(machine/subscribe [:kompass/filters scope] {:frame kompass}))

(defn kompass-create-folder!
  ([name parent-rid] (kompass-create-folder! name parent-rid nil))
  ([name parent-rid project] (send! [:kompass.folder/create name parent-rid project])))

(defn create-document! [project] (send! [:kompass.document/create project]))

(defn kompass-rename-node! [rid to] (send! [:kompass.node/rename rid to]))

(defn kompass-move-node! [rid parent-rid] (send! [:kompass.node/move rid parent-rid]))

(defn kompass-delete-node! [rid] (send! [:kompass.node/delete rid]))

(defn create-project! [pname description] (send! [:kompass.project/create pname description]))

(defn update-project!
  [pname description grants]
  (ontology/mutate! :update-project
              (cond-> {:name (clojure.core/name pname)}
                (some? description) (assoc :description description)
                (some? grants)      (assoc :grants (vec grants)))))

(def ^:private family->update-action
  {:object-types     :update-object-type
   :datasets    :update-dataset
   :link-types :update-link-type
   :interfaces  :update-interface
   :functions        :update-function
   :transforms  :update-transform
   :shared-properties :update-shared-property
   :sources          :update-source})

(def ^:private canonical->arg-key
  {:object-type-name :name :link-type-name :name :interface-name :name
   :function-name :name :transform-name :name :shared-property-name :name
   :source-name :name :attribute-list :attributes
   :from-object-type :from :to-object-type :to})

(defn- spec->update-args
  [spec]
  (-> (reduce-kv (fn [m k v] (assoc m (get canonical->arg-key k k) v)) {} spec)
      (dissoc :id)))

(defn move-resource-to-project!
  [family nm project-kw]
  (when-let [action (family->update-action family)]
    (when-let [spec (get-in @catalog/canonical-schema [family nm])]
      (ontology/mutate! action (assoc (spec->update-args spec) :project project-kw)))))

(defonce ^:private name->rid-cache (atom nil))

(defn- name->rid*
  []
  (let [sch @catalog/canonical-schema]
    (if (identical? sch (first @name->rid-cache))
      (second @name->rid-cache)
      (second (reset! name->rid-cache [sch (rid/name->rid-index sch)])))))

(defn- handle->ref
  [handle]
  (let [idx  (name->rid*)
        ref* (fn [family nm]
               (if-let [rid (get-in idx [family nm])]
                 (rid/resource-ref family rid)
                 (if (= :datasources family)
                   (rid/resource-ref family nm)
                   (str (clojure.core/name family) "/" (clojure.core/name nm)))))]
    (cond
      (vector? handle)  (ref* (first handle) (second handle))
      (keyword? handle) (ref* :projects handle)
      :else             (str handle))))

(defn- annotation-of [handle] (get-in @ui [:annotations (handle->ref handle)]))

(defn refresh-kompass-annotations! [] (send! [:kompass.annotations/refresh]))

(defn- annotate! [handle k v] (send! [:kompass.annotation/set (handle->ref handle) k v]))

(defn- toggle! [handle k] (send! [:kompass.annotation/toggle (handle->ref handle) k]))

(defn trashed? [handle] (boolean (:trashed (annotation-of handle))))

(defn trash-resource! [handle]   (annotate! handle :trashed true))

(defn restore-resource! [handle] (annotate! handle :trashed false))

(defn curated? [handle] (boolean (:curated (annotation-of handle))))

(defn toggle-curated! [handle] (toggle! handle :curated))

(defn project-doc [pkey] (:doc (annotation-of pkey)))

(defn set-project-doc! [pkey md] (annotate! pkey :doc md))

(defn annotation-doc [handle] (:doc (annotation-of handle)))

(defn set-annotation-doc! [handle md] (annotate! handle :doc md))

(defn pinned? [handle] (boolean (:pinned (annotation-of handle))))

(defn toggle-pinned! [handle] (toggle! handle :pinned))

(defn promoted? [handle] (boolean (:promoted (annotation-of handle))))

(defn set-promoted! [handle on?] (annotate! handle :promoted on?))

(defn- project-ref [pkey]
  (str "project/" (get-in (catalog/project-list) [pkey :id])))

(defn project-favorite? [pkey] (ks/file-favorite? (project-ref pkey)))

(defn toggle-project-favorite! [pkey] (ks/toggle-file-favorite! (project-ref pkey)))

(defn resource-tags [handle] (or (:tags (annotation-of handle)) #{}))

(defn all-tags [] (into (sorted-set) (mapcat :tags) (vals (:annotations @ui))))

(defn toggle-resource-tag! [handle tag] (send! [:kompass.annotation/tag (handle->ref handle) tag]))

(defn refresh-access-requests! [] (send! [:kompass.access/refresh]))

(defn request-project-access! [pkey role reason] (send! [:kompass.access/request pkey role reason]))

(defn pending-access-request
  [pkey]
  (let [me (:me @ks/kompass-index)]
    (some #(when (and (= (clojure.core/name pkey) (str (:project %))) (= me (:author %))) %)
          (:access-requests @ui))))

(defn create-portfolio! [pname description projects]
  (send! [:kompass.portfolio/create pname description projects]))

(defn change-portfolio! [rid changes] (send! [:kompass.portfolio/change rid changes]))

(defn delete-portfolio! [rid] (send! [:kompass.portfolio/delete rid]))

(defn create-collection! [cname description refs]
  (send! [:kompass.collection/create cname description refs]))

(defn change-collection! [rid changes] (send! [:kompass.collection/change rid changes]))

(defn delete-collection! [rid] (send! [:kompass.collection/delete rid]))

(defn set-project-sharing! [pkey on?] (send! [:kompass.project/sharing pkey on?]))

(defn set-project-imports!
  [pkey names]
  (send! [:kompass.project/imports pkey names]))

(defn redeem-share-link-of-address!
  []
  (when-let [token (.get (js/URLSearchParams. js/window.location.search) "share")]
    (.replaceState js/history nil "" js/window.location.pathname)
    (send! [:kompass.share/redeem token])))

(defn confirm!
  [ask & events]
  (send! [:kompass/confirm ask (vec events)]))

(defn delete-file-forever-confirmed!
  [ask family nm handle]
  (confirm! ask [:kompass/delete-forever family nm]
            [:kompass.annotation/set (handle->ref handle) :trashed false]))
