(ns sutra-ui.state.instances
  (:require [sutra-ui.api :as api]
            [sutra-ui.state.catalog :as catalog]
            [sutra-ui.state.db :as db]
            [sutra-ui.state.panel]
            [sutra-ui.state.requests :as rq]
            [sutra-ui.state.shell :as shell]))

(defonce instances (db/cursor [:instances :instances] {:objects {} :links []}))

(defonce instances-version (db/cursor [:instances :instances-version] 0))

(defonce instance-visible-ids (db/cursor [:instances :instance-visible-ids] nil))

(defn clear-instance-filter! []
  (db/put! instance-visible-ids nil)
  (db/edit! instances-version inc))

(defonce ^:private object-index-cache (atom [nil {}]))

(defn object-by-id
  [id]
  (let [objects (:objects @instances)
        [seen idx] @object-index-cache]
    (get (if (identical? seen objects)
           idx
           (let [idx (persistent!
                      (reduce-kv (fn [acc type-kw rows]
                                   (reduce (fn [acc row]
                                             (let [k (str (:id row))]
                                               (if (contains? acc k)
                                                 acc
                                                 (assoc! acc k
                                                         (assoc row :object-type type-kw)))))
                                           acc rows))
                                 (transient {}) objects))]
             (reset! object-index-cache [objects idx])
             idx))
         (str id))))

(defn link-by-id
  [id]
  (let [id-str (str id)]
    (some #(when (= (str (:id %)) id-str) %) (:links @instances))))

(defn edges-touching
  [id]
  (let [id-str (str id)]
    (filterv #(or (= id-str (str (:from-id %)))
                  (= id-str (str (:to-id %))))
             (:links @instances))))

(defonce staged-types
  (db/cursor [:instances :staged-types] {:object-types #{} :link-types #{} :interfaces #{}}))

(defn objects-by-name
  [objects]
  (persistent!
   (reduce-kv (fn [m rid rows] (assoc! m (or (catalog/name-of rid) rid) rows))
              (transient {}) objects)))

(defn type-of
  [db id-str]
  (some (fn [[t rows]] (when (some #(= id-str (str (:id %))) rows) t))
        (get-in db [:instances :instances :objects])))

(defonce rf-instance (atom nil))

(defn focus-nodes!
  [ids]
  (when-let [^js inst @rf-instance]
    (let [targets (clj->js (mapv (fn [id] {:id (str id)}) ids))]
      (js/setTimeout
       (fn [] (.fitView inst #js {:nodes targets :duration 500
                                  :padding 0.3 :maxZoom 1.4}))
       80))))

(defn- merge-rows
  [a b]
  (->> (concat a b)
       (reduce (fn [m r] (assoc m (str (:id r)) r)) {})
       vals vec))

(defn merge-instance-graph
  [base add]
  {:objects     (reduce-kv (fn [m t rows] (update m t merge-rows rows))
                           (:objects base) (:objects add))
   :links (->> (concat (:links base) (:links add))
               (reduce (fn [m c] (assoc m (str (:id c)) c)) {})
               vals vec)})

(defn ring-positions
  [center ids rx ry]
  (let [n (max 1 (count ids))]
    (into {}
          (map-indexed
           (fn [i id]
             (let [theta (* 2 Math/PI (/ i n))]
               [id {:x (+ (:x center) (* rx (Math/cos theta)))
                    :y (+ (:y center) (* ry (Math/sin theta)))}]))
           ids))))

(defn- ask-canvas
  [ids-by-type all-ids]
  (-> (js/Promise.all
       (clj->js (concat
                 (for [[t ids] ids-by-type]
                   (-> (api/post! :ontology-query {:find t :ids ids})
                       (.then (fn [resp] [t (vec (:rows resp))]))))
                 [(api/post! :instances-around
                             {:ids (vec all-ids) :types (vec (keys ids-by-type))
                              :links-only? true :among? true})])))
      (.then (fn [results]
               (let [results (vec (array-seq results))]
                 {:objects (into {} (butlast results))
                  :links   (vec (:links (last results)))})))))

(defn- grow [i] (update i :instances-version inc))

(db/reg :sutra.instances/canvas
        (fn [db [_ reply]]
          (let [ids-by-type (into {} (for [[t rows] (get-in db [:instances :instances :objects])
                                           :let [ids (mapv #(str (:id %)) rows)]
                                           :when (seq ids)]
                                       [t ids]))
                all-ids     (into #{} (mapcat val) ids-by-type)]
            (if (empty? all-ids)
              {:fx [(rq/settle reply nil)]}
              (rq/request db {:ask  [ask-canvas ids-by-type all-ids]
                              :then [[:sutra.instances/canvas-read]] :report "canvas refresh"}
                          reply)))))

(db/reg :sutra.instances/canvas-read
        (fn [db [_ graph]] {:db (update db :instances #(grow (assoc % :instances graph)))}))

(defn refresh-canvas!
  []
  (db/promise! [:sutra.instances/canvas]))

(defn- projection-query
  [db]
  (let [v (get-in db [:instances :projection-version])]
    (cond-> {}
      (and v (seq (get-in db [:catalog :canonical-schema :object-types])))
      (assoc :since v))))

(defn- adopt-projection
  [db resp]
  (-> db
      (update :catalog catalog/adopt (:schema resp) (:schema-derived resp))
      (assoc-in [:instances :staged-types]
                (or (:staged-types resp) {:object-types #{} :link-types #{} :interfaces #{}}))))

(db/reg :sutra.instances/refresh
        (fn [db [_ live? reply]]
          (update (rq/request (update db :instances assoc :links-asked #{}
                                      :object-links-failed {})
                              {:ask       [api/get! :instances (projection-query db)]
                               :handover? true :report "projection"
                               :then      [(fn [resp reply]
                                             [:sutra.instances/projected live? reply resp])]
                               :else      [[:sutra.instances/projection-failed]]}
                              reply)
                  :fx #(into [[:dispatch [:sutra.panel/reload-complete]]] %))))

(db/reg :sutra.instances/projected
        (fn [db [_ live? reply resp]]
          {:db (cond-> (assoc-in db [:instances :projection-version] (:version resp))
                 (not (:unchanged resp)) (adopt-projection resp))
           :fx [(if live?
                  [:dispatch [:sutra.instances/canvas reply]]
                  (rq/settle reply resp))]}))

(db/reg :sutra.instances/projection-failed
        (fn [db [_ sentence]] {:db (update db :catalog catalog/failed sentence)}))

(defn refresh-instance-state!
  []
  (db/promise! [:sutra.instances/refresh (shell/instance-graph-live?)]))

(defonce object-links-failed (db/cursor [:instances :object-links-failed] {}))

(db/reg :sutra.instances/links
        (fn [db [_ id-str type]]
          (when-not (contains? (get-in db [:instances :links-asked]) id-str)
            {:db (update-in db [:instances :links-asked] (fnil conj #{}) id-str)
             :fx [[:after {:key [:sutra.instances/links id-str] :ms 0
                           :event [:sutra.instances/ask-links id-str type]}]]})))

(db/reg :sutra.instances/ask-links
        (fn [db [_ id-str type]]
          (rq/request db {:ask   [api/post! :instances-around
                                  {:ids [id-str] :types [(or type (type-of db id-str))]}]
                          :then  [[:sutra.instances/links-read]]
                          :error [:instances :object-links-failed id-str]})))

(db/reg :sutra.instances/links-read
        (fn [db [_ resp]]
          (let [add {:objects (objects-by-name (:objects resp)) :links (vec (:links resp))}]
            {:db (update db :instances #(grow (update % :instances merge-instance-graph add)))})))

(defn ensure-object-links!
  [id object-type]
  (db/send! [:sutra.instances/links (str id) object-type])
  nil)

(defn canvas-node-ids
  []
  (if-let [^js inst @rf-instance]
    (into #{} (map (fn [^js n] (.-id n))) (array-seq (.getNodes inst)))
    #{}))

(defn bump-instances-version!
  []
  (db/edit! instances-version inc))

(defn add-instances!
  [add]
  (db/edit! instances merge-instance-graph add))

(defn clear-instances!
  []
  (db/put! instances {:objects {} :links []})
  (bump-instances-version!))

(defn set-instance-filter!
  [ids]
  (db/put! instance-visible-ids (set (map str ids)))
  (bump-instances-version!))

(defn set-rf-instance! [inst] (reset! rf-instance inst))
