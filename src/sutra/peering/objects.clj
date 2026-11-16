(ns sutra.peering.objects
  (:require [sutra.catalog.lookup :as lookup]
            [sutra.catalog.shape :as shape]
            [sutra.catalog.streaming :as streaming]
            [sutra.kit.datalog :as dl]
            [sutra.peering.ports :as ports]
            [sutra.store.api :as api]
            [sutra.store.machine :as machine]
            [sutra.store.seat :as seat]
            [sutra.store.world :as world]
            [sutra.types.authority :as policy]
            [sutra.types.peering :as peering])
  (:import (java.time Instant)
           (java.util Date)))

(set! *warn-on-reflection* true)

(def branch
  "main")

(defn home
  [_connection-id]
  branch)

(defn schema [store] (seat/branch-schema (world/at store branch)))

(defn peer-action
  [connection-id]
  (str "peer/" connection-id))

(defn type-rid
  [schema t]
  (if (contains? (:object-types schema) t)
    t
    (lookup/rid-of schema :object-types (keyword (name t)))))

(defn datasource-kind
  [schema rid]
  (if-let [{:keys [direct?]} (streaming/settings schema rid)]
    (if direct? :direct :stream)
    (peering/datasource-kind (get-in schema [:object-types rid :backing]) nil)))

(defn source-origins
  [store rid]
  (let [f (ports/direct-writes)]
    (cond-> [:index]
      f (into (keep :origin) [(f (world/at store branch) rid)]))))

(defn type-name [schema rid] (some-> (get-in schema [:object-types rid :object-type-name]) name))

(defn attr-types
  [schema rid]
  (when-let [as (lookup/attrs schema rid)]
    (into {} (map (fn [{:keys [attribute-name attribute-type]}]
                    [(keyword (name attribute-name)) attribute-type]))
          as)))

(defn project-of
  [schema rid]
  (let [p (get-in schema [:object-types rid :project])]
    (some-> (or (get-in schema [:projects p :project-name]) p) name)))

(defn type-markings
  [schema rid]
  (let [db (policy/schema-database (policy/schema-facts schema))]
    (into #{} (map #(keyword (name ('?m %)))) (dl/query db [:marked rid '?m]))))

(defn- millis [at]
  (cond (instance? Date at) (.getTime ^Date at)
        (instance? Instant at) (.toEpochMilli ^Instant at)
        (number? at) (long at)
        (string? at) (try (.toEpochMilli (Instant/parse at)) (catch Exception _ 0))
        :else 0))

(defn- read-at [store q]
  (world/with-snapshot (world/at store branch) (fn [s] (api/read s api/unrestricted q))))

(defn clock [store rid] (read-at store [:objects/clock rid]))

(defn objects
  ([store rid] (objects store rid nil))
  ([store rid ids]
   (into {} (for [o (read-at store (cond-> [:objects rid] ids (conj (vec ids))))]
              [(str (:id o)) {:values (or (:values o) {}) :markings (vec (:markings o))}]))))

(defn- compacted?
  [store rid origin after es top]
  (and (empty? es) (some? top) (not= top after)
       (let [other (read-at store [:objects/changes rid
                                   {:after after :origin (if (= :index origin) :edit :index)}])]
         (not= top (:rev (last other))))))

(defn changes
  [store rid origin after]
  (let [es  (when after
              (read-at store [:objects/changes rid {:after after :origin origin}]))
        top (clock store rid)]
    (if (and after (not (compacted? store rid origin after es top)))
      {:rev (or (:rev (last es)) after)
       :entries (mapv #(-> (select-keys % [:id :op :values :actor])
                           (assoc :at (millis (:at %))))
                      es)}
      {:rev top :entries (mapv (fn [id] {:id id :op :create-object :at 0})
                               (keys (objects store rid)))})))

(defn links-from
  [store link-rids id]
  (into {} (for [l link-rids]
             [l (mapv (comp str :to-object-id) (read-at store [:links l {:from [(str id)]}]))])))

(defn last-edit
  [store rid id]
  (when-let [e (last (read-at store [:objects/changes rid {:ids [(str id)] :origin :edit}]))]
    {:at  (millis (:at e))
     :via (some->> (get-in e [:actor :action]) (re-matches #"peer/(.+)") second)}))

(defn- actor [connection-id]
  {:principal (str "peer:" connection-id) :action (peer-action connection-id)})

(defn write-source!
  [store connection-id rid items]
  (let [ops (vec (for [{:keys [id op values markings]} items]
                   (if (= :delete-object op)
                     {:op :delete-object :type rid :payload {:id id}}
                     {:op :create-object :type rid
                      :payload (cond-> {:id id :values values}
                                 (seq markings) (assoc :markings (vec markings)))})))]
    (when (seq ops)
      (world/index! (world/at store (home connection-id)) ops {:actor (actor connection-id)}))
    (count ops)))

(defn- command!
  [store br connection-id rid decide]
  (machine/command! (:db store) {:branch br :fences (when rid [[:type rid]])}
                    (fn [snap]
                      (let [ops (vec (decide snap))]
                        (if (seq ops)
                          {:tx {:ops ops :origin :edit :actor (actor connection-id)
                                :message (str "peered edits of " (peer-action connection-id))}
                           :result (count ops)}
                          {:result 0})))))

(defn write-edits!
  [store connection-id rid items]
  (command! store (home connection-id) connection-id rid
            (fn [snap]
              (let [have (into #{} (map (comp str :id))
                               (api/read snap api/unrestricted [:objects rid (mapv :id items)]))]
                (for [{:keys [id op values markings]} items
                      :let [o (cond
                                (= :delete-object op)
                                (when (have id) {:op :delete-object :type rid :payload {:id id}})
                                (have id)
                                {:op :update-object :type rid :payload {:id id :values values}}
                                :else
                                {:op :create-object :type rid
                                 :payload (cond-> {:id id :values values}
                                            (seq markings) (assoc :markings (vec markings)))})]
                      :when o]
                  o)))))

(defn- link-target [schema l]
  (let [t (get-in schema [:link-types l :to-object-type])] (or (type-rid schema t) t)))

(defn- present
  [snap t ids]
  (cond
    (empty? ids) #{}
    (nil? t)     (set (map str ids))
    :else        (into #{} (map (comp str :id))
                       (api/read snap api/unrestricted [:objects t (vec ids)]))))

(defn write-links!
  [store schema connection-id desired]
  (let [held (volatile! [])
        n    (command! store (home connection-id) connection-id nil
                       (fn [snap]
                         (vreset! held [])
                         (vec (for [[from by-link] desired
                                    [l to-ids] by-link
                                    :let [idf  (shape/link-id-fn schema l)
                                          have (into #{} (map (comp str :to-object-id))
                                                     (api/read snap api/unrestricted
                                                               [:links l {:from [(str from)]}]))
                                          want (set to-ids)
                                          new  (remove have want)
                                          here (present snap (link-target schema l) new)]
                                    op (concat
                                        (for [t new]
                                          (if (here t)
                                            {:op :create-link :type l
                                             :payload {:id (idf from t) :from-id from :to-id t}}
                                            (do (vswap! held conj {:link-type l :from-id from
                                                                   :to-id t})
                                                nil)))
                                        (for [t (remove want have)]
                                          {:op :delete-link :type l
                                           :payload {:id (idf from t)}}))
                                    :when op]
                                op))))]
    {:written (:result n 0) :held @held}))

(defn create-links!
  [store schema connection-id edges]
  (let [settled (volatile! [])]
    (command! store (home connection-id) connection-id nil
              (fn [snap]
                (vreset! settled [])
                (vec (for [[l es] (group-by :link-type edges)
                           :let [idf  (shape/link-id-fn schema l)
                                 here (present snap (link-target schema l) (map :to-id es))]
                           {:keys [from-id to-id] :as e} es
                           :when (here (str to-id))
                           :let [_   (vswap! settled conj e)
                                 had (seq (api/read snap api/unrestricted
                                                    [:links l {:from [(str from-id)]}]))]
                           :when (not-any? #(= (str to-id) (str (:to-object-id %))) had)]
                       {:op :create-link :type l
                        :payload {:id (idf from-id to-id) :from-id from-id :to-id to-id}}))))
    @settled))

(defn marking-type [schema] (type-rid schema :marking))

(defn create-markings!
  [store schema connection-id names]
  (if-let [mt (marking-type schema)]
    (do (when (seq names)
          (command! store (home connection-id) "markings" mt
                    (fn [snap]
                      (let [have (into #{} (map (comp str :id))
                                       (api/read snap api/unrestricted
                                                 [:objects mt (vec names)]))]
                        (for [n names :when (not (have n))]
                          {:op :create-object :type mt
                           :payload {:id n :values {:name n}}})))))
        true)
    false))
