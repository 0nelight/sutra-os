(ns sutra.apps.http.home-page
  (:require [sutra.kit.refusal :as refusal]
            [sutra.types.versions :as vers]
            [sutra.access.api :as access]
            [sutra.apps.http.scope :refer [resource-access]]
            [sutra.apps.http.workspaces :refer [app-label workspace-label]]
            [sutra.http.responses :refer [ok]]
            [sutra.store.storage.instance-config :as ic]
            [sutra.types.ontologies :as ontologies]))

(set! *warn-on-reflection* true)

(defn home-url
  [{:keys [default organizations overrides]} groups orgs]
  (let [groups (set groups) orgs (set orgs)]
    (or (some (fn [{gs :groups url :url}] (when (some groups gs) url)) overrides)
        (some (fn [[o url]] (when (contains? orgs (str o)) url)) (sort-by key organizations))
        default)))

(defn default-workspace
  [defaults visible groups]
  (let [groups (set groups)
        hits   (distinct (for [[rid w] visible
                               :when (and (:promoted w)
                                          (some groups (get defaults (str rid))))]
                           rid))]
    (when (= 1 (count hits)) (first hits))))

(defn- slug [x] (if (keyword? x) (name x) (str x)))

(defn- ws-card
  [rid w]
  {:id rid :name (:workspace-name w) :label (workspace-label w)
   :promoted (boolean (:promoted w)) :path (str "/workspace/" (slug (:workspace-name w)))})

(defn my-home
  [{auth :sutra/auth schema :sutra/schema store :sutra/store}]
  (let [groups  (get-in auth [:subject :groups])
        orgs    (get-in auth [:subject :organizations])
        visible (into {} (filter (fn [[_ w]] (resource-access auth schema w)))
                      (:workspaces schema))
        ws      (default-workspace (ic/default-workspaces store) visible groups)]
    (ok {:url               (home-url (ic/home-page store) groups orgs)
         :default-workspace (when ws (ws-card ws (get visible ws)))})))

(defn- hidden-organizations
  [auth schema]
  (if (= "admin" (:username auth))
    #{}
    (ontologies/hidden-organizations schema (get-in auth [:subject :organizations]) :name)))

(defn- choices
  [store schema hidden]
  {:groups        (vec (sort-by :name (for [[id n] (access/group-names store)]
                                        {:id id :name n})))
   :organizations (vec (sort-by :name (for [[rid o] (:organizations schema)
                                             :when (not (contains? hidden rid))]
                                        {:id   (str rid)
                                         :name (or (some-> (:organization-name o) slug)
                                                   (slug rid))})))
   :workspaces    (vec (sort-by :label (for [[rid w] (:workspaces schema)] (ws-card rid w))))
   :apps          (vec (sort-by :label (for [[_ a] (:applications schema)
                                             :when (vers/published? a)
                                             :when (not (:form-factor a))
                                             :let [nm (slug (:application-name a))]]
                                         {:name nm :label (app-label a)
                                          :path (str "/apps/" nm)})))})

(defn- admin-view
  [store schema hidden]
  (merge (choices store schema hidden)
         {:home-page          (ic/home-page store)
          :default-workspaces (ic/default-workspaces store)}))

(defn home-page
  [{store :sutra/store schema :sutra/schema auth :sutra/auth}]
  (ok (admin-view store schema (hidden-organizations auth schema))))

(defn set-home-page
  [{:keys [body-params] store :sutra/store schema :sutra/schema auth :sutra/auth}]
  (ic/set-home-page! store body-params)
  (ok (admin-view store schema (hidden-organizations auth schema))))

(defn set-default-workspace
  [{{:keys [workspace groups]} :body-params store :sutra/store schema :sutra/schema
    auth :sutra/auth}]
  (let [[rid w] (some (fn [[rid w :as e]]
                        (when (or (= (str rid) (str workspace))
                                  (= (slug (:workspace-name w)) (str workspace)))
                          e))
                      (:workspaces schema))]
    (cond
      (nil? rid)
      (refusal/refused :refusal.apps/no-workspace {:workspace workspace} {:workspace workspace})

      (and (seq groups) (not (:promoted w)))
      (refusal/refused :refusal.apps/a-default-workspace-must-be {:workspace workspace})

      :else
      (do (ic/set-default-workspace! store rid groups)
          (ok (admin-view store schema (hidden-organizations auth schema)))))))
