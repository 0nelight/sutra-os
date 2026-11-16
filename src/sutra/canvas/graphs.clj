(ns sutra.canvas.graphs
  (:require [sutra.store.control :as ctl]))

(set! *warn-on-reflection* true)

(defn- layout-row->map [r]
  (select-keys r [:graph-id :node-kind :node-key :x :y]))

(defn- default-scheme
  [store graph-id rows]
  (or (:default-scheme (ctl/get store :graph-defaults {:branch (:branch store) :graph-id graph-id}))
      (:id (first (filter :is-default rows)))))

(defn- color-schemes [store graph-id]
  (let [rows (ctl/list store :color-schemes {:branch (:branch store) :graph-id graph-id})
        d    (default-scheme store graph-id rows)]
    (mapv (fn [r] (-> (select-keys r [:id :graph-id :name :rules :created-at :updated-at])
                      (assoc :is-default (= d (:id r)))))
          rows)))

(defn read-graph-side-data [store graph-rid]
  (let [b (:branch store)]
    {:layout        (mapv layout-row->map
                          (ctl/list store :graph-layouts {:branch b :graph-id graph-rid}))
     :color-schemes (color-schemes store graph-rid)}))

(defn upsert-layout! [store entry]
  (ctl/upsert! store :graph-layouts
                (assoc (select-keys entry [:graph-id :node-kind :node-key :x :y])
                       :branch (:branch store)))
  {:ok true})

(defn query-color-schemes [store graph-id]
  (color-schemes store graph-id))

(defn- read-color-scheme [store graph-id scheme-id]
  (some #(when (= scheme-id (:id %)) %) (color-schemes store graph-id)))

(defn- clear-default! [store graph-id scheme-id]
  (when (= scheme-id
           (default-scheme store graph-id (ctl/list store :color-schemes {:branch (:branch store)
                                                                    :graph-id graph-id})))
    (ctl/delete! store :graph-defaults {:branch (:branch store) :graph-id graph-id})
    (doseq [o (ctl/list store :color-schemes
                        {:branch (:branch store) :graph-id graph-id :is-default true})]
      (ctl/update! store :color-schemes {:branch (:branch store) :id (:id o)}
                   {:is-default false}))))

(defn upsert-color-scheme! [store {:keys [id graph-id name rules is-default]}]
  (let [b (:branch store)
        graph-id (or graph-id (:graph-id (ctl/get store :color-schemes {:branch b :id id})))
        stored-id
        (if id
          (do (ctl/update! store :color-schemes {:branch b :id id}
                            {:name name :rules rules})
              id)
          (:id (ctl/upsert! store :color-schemes {:id (str (java.util.UUID/randomUUID)) :branch b
                                                :graph-id graph-id :name name :rules rules
                                                :is-default false})))]
    (if is-default
      (ctl/upsert! store :graph-defaults {:branch b :graph-id graph-id :default-scheme stored-id})
      (clear-default! store graph-id stored-id))
    (read-color-scheme store graph-id stored-id)))

(defn delete-color-scheme! [store scheme-id]
  (when-let [{:keys [graph-id]} (ctl/get store :color-schemes
                                         {:branch (:branch store) :id scheme-id})]
    (clear-default! store graph-id scheme-id))
  (ctl/delete! store :color-schemes {:branch (:branch store) :id scheme-id})
  {:deleted scheme-id})
