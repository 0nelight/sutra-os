(ns sutra.cli.verbs.apps
  (:require [sutra.cli.dispatch :refer [step!]]
            [sutra.cli.errors :refer [req!]]
            [sutra.cli.live :refer [type-name-index]]
            [sutra.cli.out :refer [emit! emit-rows!]]
            [sutra.cli.view :refer [objects->rows]]
            [sutra.wire.api :as api]))

(defn- workspace-rows []
  (:workspaces (req! "workspaces" (api/get! :my-workspaces))))

(defmethod step! :workspaces [ctx [_ _]]
  (let [rows (workspace-rows)]
    (emit! ctx {:workspaces rows}))
  nil)

(defmethod step! :workspace [ctx [_ {:keys [name]}]]
  (let [rows (workspace-rows)
        w    (or (first (filter #(= (str (:name %)) (str name)) rows))
                 (throw (ex-info (str "no workspace named " (pr-str name)
                                      " — run `workspaces` to list them")
                                 {:desc "workspace" :name name})))]
    (emit! ctx w)
    nil))

(defn- app-by-name
  [nm]
  (let [appmap (:applications (req! "app" (api/get! :applications)))]
    (or (get appmap (keyword nm))
        (get appmap nm)
        (throw (ex-info (str "no application named " (pr-str nm)
                             " — run `apps` to list them")
                        {:desc "app" :name nm})))))

(defmethod step! :app [ctx [_ {:keys [name]}]]
  (let [a     (app-by-name name)
        lay   (:layout a)
        ->n   (fn [x] (cond (keyword? x) (clojure.core/name x) (some? x) (str x)))
        binds (->> (or (:object-types lay) (some-> (:object-type a) vector))
                   (keep ->n) distinct vec)
        lay*  (cond-> lay
                (get-in lay [:export :template-b64])
                (-> (update :export dissoc :template-b64)
                    (assoc-in [:export :has-template] true)))]
    (emit! ctx {:name name :published (->n (:published a)) :project (->n (:project a))
                :binds binds :layout lay* :has-source (some? (:source a))})
    nil))

(defmethod step! :app-data [ctx [_ {:keys [name type limit scenario vars]}]]
  (let [a    (app-by-name name)
        body (req! (str "app-data " name (clojure.core/when scenario (str " @" scenario))
                        (clojure.core/when vars (str " vars " vars)))
                   (api/get! :my-application (cond-> {:rid (:id a)}
                                               scenario (assoc :scenario scenario)
                                               vars     (assoc :vars vars))))
        data (:data body)
        data (if type (select-keys data [type (keyword type)]) data)
        answered (not-empty (into (sorted-map) (remove (comp coll? val)) data))
        data (into {} (filter (comp coll? val)) data)
        data (if limit (into {} (map (fn [[t rs]] [t (vec (take limit rs))])) data) data)
        rows (objects->rows data (type-name-index))]
    (clojure.core/when answered (emit! ctx {:answered answered}))
    (emit-rows! ctx rows (str "app " (pr-str name) " serves no data"
                              (clojure.core/when type
                                (str " for type " (clojure.core/name type)))))))

(defmethod step! :scenarios [ctx [_ {:keys [name]}]]
  (let [a  (app-by-name name)
        sc (:scenarios (req! (str "scenarios " name)
                             (api/get! :my-application-scenarios {:rid (:id a)})))]
    (emit! ctx {:scenarios sc})
    nil))

(defmethod step! :scenario-new [ctx [_ {:keys [name scenario-name]}]]
  (let [a   (app-by-name name)
        res (req! (str "scenario-new " name)
                  (api/post! :my-application-scenarios {:rid (:id a)} {:name scenario-name}))]
    (emit! ctx res)
    nil))

(defmethod step! :scenario-drop [ctx [_ {:keys [name scenario]}]]
  (let [a (app-by-name name)]
    (req! (str "scenario-drop " name)
          (api/delete! :my-application-scenario {:rid (:id a) :uuid scenario}))
    (emit! ctx {:ok true :scenario scenario})
    nil))

(defmethod step! :scenario-actions [ctx [_ {:keys [name scenario]}]]
  (let [a   (app-by-name name)
        res (req! (str "scenario-actions " name)
                  (api/get! :my-application-scenario-actions {:rid (:id a) :uuid scenario}))]
    (emit! ctx res)
    nil))

(defmethod step! :scenario-seal [ctx [_ {:keys [name scenario]}]]
  (let [a (app-by-name name)]
    (req! (str "scenario-seal " name)
          (api/post! :my-application-scenario-seal {:rid (:id a) :uuid scenario} {}))
    (emit! ctx {:ok true :scenario scenario :sealed? true})
    nil))

(defmethod step! :scenario-rebuild [ctx [_ {:keys [name scenario force]}]]
  (let [a   (app-by-name name)
        res (req! (str "scenario-rebuild " name)
                  (api/post! :my-application-scenario-rebuild {:rid (:id a) :uuid scenario}
                             {:force (boolean force)}))]
    (emit! ctx res)
    nil))

(defmethod step! :scenario-apply [ctx [_ {:keys [name scenario again]}]]
  (let [a   (app-by-name name)
        res (req! (str "scenario-apply " name)
                  (api/post! :my-application-scenario-apply {:rid (:id a) :uuid scenario}
                             {:again (boolean again)}))]
    (emit! ctx res)
    nil))

(defmethod step! :scenario-duplicate [ctx [_ {:keys [name scenario scenario-name]}]]
  (let [a   (app-by-name name)
        res (req! (str "scenario-duplicate " name)
                  (api/post! :my-application-scenario-duplicate {:rid (:id a) :uuid scenario}
                             {:name scenario-name}))]
    (emit! ctx res)
    nil))
