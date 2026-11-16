(ns sutra.catalog.changes)

(set! *warn-on-reflection* true)

(def group-order
  [[:objects :create] [:objects :update] [:objects :delete] [:links :create]])

(def ^:private groups (set group-order))

(defn create-object
  ([id obj-type values] (create-object id obj-type values nil))
  ([id obj-type values stamps]
   {:op :publish :kind :objects :verb :create :type obj-type
    :row (cond-> {:id id :values values} stamps (assoc :stamps stamps))}))

(defn update-object
  ([id obj-type values] (update-object id obj-type values nil))
  ([id obj-type values stamps]
   {:op :publish :kind :objects :verb :update :type obj-type
    :row (cond-> {:id id :values values} stamps (assoc :stamps stamps))}))

(defn delete-object
  [id obj-type]
  {:op :publish :kind :objects :verb :delete :type obj-type
   :row {:id id}})

(defn connect
  [conn-type from-id to-id]
  {:op :publish :kind :links :verb :create :type conn-type
   :row {:from-id from-id :to-id to-id}})

(defn publish-group
  [d]
  (when (and (= :publish (:op d))
             (contains? groups [(:kind d) (:verb d)])
             (some? (:type d))
             (map? (:row d)))
    {:kind (:kind d) :verb (:verb d) :type (:type d)}))
