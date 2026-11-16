(ns sutra-ui.state.object-view
  (:require [goog.object :as gobj]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.requests :as rq]
            [sutra.capps.variables :as vars]
            [sutra-ui.api :as api]
            [sutra-ui.feature :as feature]
            [sutra-ui.state.auth :as auth]
            [sutra-ui.state.instances :as instances]
            [sutra-ui.state.panel :as panel]
            [sutra-ui.ui.size :as groesse]))

(defn builder-session?
  []
  (and (nil? @auth/current-workspace) (not= "consumer" (:role @auth/auth))))

(defn object-view-surface?
  []
  (and (some? (feature/command :open-object)) (builder-session?)))

(defonce object-view-data (db/cursor [:object-view :object-view-data] {}))

(def ^:private FRESH-MS 5000)

(defn- form [] (when (groesse/kompakt?) "mobile"))

(defn object-view-fresh?
  [object-type id]
  (when-let [at (:read-at (get @object-view-data [object-type (str id)]))]
    (< (- (.now js/Date) at) FRESH-MS)))

(defn- early-object-view [object-type id]
  (when-let [e (and (exists? js/window) (gobj/get js/window "__sutraEarlyObject"))]
    (js-delete js/window "__sutraEarlyObject")
    (when (and (nil? (api/effective-page-branch))
               (= (or (gobj/get e "form") nil) (form))
               (= (gobj/get e "type") (name object-type))
               (= (gobj/get e "id") (str id)))
      e)))

(defn read-object-view
  ([object-type id] (read-object-view object-type id false))
  ([object-type id whole?]
   (let [ask #(api/post! :object-view-standard (cond-> {:type (name object-type) :id (str id)}
                                                 whole? (assoc :standard true)
                                                 (form) (assoc :form (form))))]
     (if-let [e (when-not whole? (early-object-view object-type id))]
       (-> (gobj/get e "response")
           (.then (fn [^js r]
                    (if (.-ok r)
                      (api/adopt! :post "/api/object-view/standard" (js/Promise.resolve r))
                      (ask))))
           (.catch (fn [_] (ask))))
       (ask)))))

(defn- connection-cut? [e] (instance? js/TypeError e))

(defn- once-visible []
  (js/Promise.
   (fn [resolve]
     (if-not (.-hidden js/document)
       (resolve nil)
       (letfn [(seen [_]
                 (when-not (.-hidden js/document)
                   (.removeEventListener js/document "visibilitychange" seen)
                   (resolve nil)))]
         (.addEventListener js/document "visibilitychange" seen))))))

(defn- read-object-view-across-cuts
  [object-type id whole? attempts]
  (-> (read-object-view object-type id whole?)
      (.catch (fn [e]
                (if (and (pos? attempts) (connection-cut? e))
                  (-> (once-visible)
                      (.then #(js/Promise. (fn [resolve] (js/setTimeout resolve 1000))))
                      (.then #(read-object-view-across-cuts object-type id whole? (dec attempts))))
                  (throw e))))))

(defn- object-view-payload
  [object-type res bound]
  (vars/object-view-payload (-> res
                                (update :objects instances/objects-by-name)
                                (assoc :object bound :object-type object-type))
                            bound))

(defn ask-object-view!
  [object-type id vs]
  (let [k     [object-type (str id)]
        e     (get @object-view-data k)
        bound (:instance e)]
    (-> (api/post! :object-view-standard (cond-> {:type (name object-type) :id (str id)
                                                  :vars vs}
                                           (:whole? e) (assoc :standard true)
                                           (form)      (assoc :form (form))))
        (.then (fn [res]
                 (object-view-payload object-type res
                                      (or (some-> (:object res)
                                                  (assoc :object-type object-type))
                                          bound)))))))

(defn refresh-object-view-data!
  [object-type id & [{:keys [whole?]}]]
  (let [id    (str id)
        known (or (some #(when (= (str (:id %)) id) (assoc % :object-type object-type))
                        (panel/rows-of object-type))
                  (instances/object-by-id id))]
    (db/promise! [:sutra.object-view/refresh object-type id whole? known])))

(defn- read-entry
  [object-type id whole?]
  (-> (read-object-view-across-cuts object-type id whole? 3)
      (.then (fn [res] {:res res :at (.now js/Date)}))
      (.catch (fn [e] (if (connection-cut? e) {:cut (.-message e)} (throw e))))))

(db/reg :sutra.object-view/refresh
        (fn [db [_ object-type id whole? known reply]]
          (let [k      [object-type id]
                whole? (or whole? (:whole? (get-in db [:object-view :object-view-data k])))]
            (rq/request (update-in db [:object-view :object-view-data k]
                                   (fn [e] (assoc (or e {}) :loading? true :whole? whole?
                                                  :instance (or (:instance e) known))))
                        {:ask  [read-entry object-type id whole?]
                         :then [[:sutra.object-view/read k whole? known]]
                         :else [[:sutra.object-view/failed k]]}
                        reply))))

(def ^:private fact-keys
  [:object-page :standard-view :property-display :property-types :object-titles
   :object-names :object-rows :object-keys :object-views :standard-views :inputs])

(db/reg :sutra.object-view/read
        (fn [db [_ [object-type :as k] whole? known {:keys [res at cut]}]]
          (let [path [:object-view :object-view-data k]
                bound (or (some-> (:object res) (assoc :object-type object-type)) known)]
            {:db (cond
                   (and cut (:data (get-in db path))) (assoc-in db (conj path :loading?) false)
                   cut (assoc-in db path {:loading? false :error cut})
                   :else (assoc-in db path
                                   {:loading? false :whole? whole? :read-at at :instance bound
                                    :data (object-view-payload object-type res bound)
                                    :facts (select-keys res fact-keys)}))})))

(db/reg :sutra.object-view/failed
        (fn [db [_ k sentence]]
          {:db (assoc-in db [:object-view :object-view-data k] {:loading? false :error sentence})}))
