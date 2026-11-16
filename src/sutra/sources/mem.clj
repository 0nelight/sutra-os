(ns sutra.sources.mem
  (:require [sutra.kit.refusal :as refusal]
            [clojure.string :as str]
            [sutra.sources.protocols :as api]
            [sutra.sources.dialect :as dialect]
            [sutra.sources.policy :as policy]
            [sutra.types.source :as source]))

(set! *warn-on-reflection* true)

(defn- row-out [row]
  (update-keys row #(keyword (str/lower-case (name %)))))

(defn- table-of [db spec table]
  (or (get-in db [:tables table])
      (refusal/refuse! :refusal.foreign/mem-no-table {:table table} {:source (:source-name spec)})))

(defn- scan-rows [db spec table]
  (mapv row-out (:rows (table-of db spec table))))

(def ^:private scan-sql
  #"(?i)^\s*select\s+\*\s+from\s+[\"`\[]?([A-Za-z_][A-Za-z0-9_]*)[\"`\]]?\s*;?\s*$")

(defn- select-rows [db spec sql]
  (cond
    (re-matches #"(?i)^\s*select\s+1\s*;?\s*$" sql) [{:1 1}]
    :else (if-let [[_ t] (re-matches scan-sql sql)]
            (scan-rows db spec t)
            (refusal/refuse! :refusal.foreign/mem-select {:unsupported :select :sql sql}))))

(defn- same? [a b]
  (if (or (nil? a) (nil? b)) (and (nil? a) (nil? b)) (= (str a) (str b))))

(defn- key-match [{:keys [pk pk-columns pk-value]}]
  (let [kv (if (seq pk-columns)
             (zipmap pk-columns (policy/pk-parts pk-columns pk-value))
             {pk pk-value})]
    (fn [row] (every? (fn [[c v]] (same? (get row c) v)) kv))))

(defn- guarded [rows {:keys [table pk-value expect] :as plan} verb]
  (let [hit? (key-match plan)
        ok?  (fn [row] (and (hit? row) (every? (fn [[c v]] (same? (get row c) v)) expect)))
        n    (count (filter ok? rows))]
    (when (zero? n)
      (refusal/refuse! ({"edit"
                           :refusal.foreign/mem-edit-conflict}
                        verb :refusal.foreign/mem-delete-conflict)
                       {:conflict true :table table :pk-value pk-value :expect expect}))
    (when (> n 1)
      (refusal/refuse! :refusal.foreign/key-not-unique {:table table :pk-value pk-value :rows n}))
    ok?))

(defn- append-log [db {:keys [table rows]}]
  (update-in db [:tables table :rows] (fnil into []) rows))

(defn- logged [res log rows]
  (cond-> (assoc res :logged (count rows)) log (assoc :log-table (:table log))))

(defn- apply-update [db {:keys [table log] sets :set :as plan}]
  (let [ok? (guarded (get-in db [:tables table :rows]) plan "edit")]
    [(cond-> (update-in db [:tables table :rows] #(mapv (fn [r] (if (ok? r) (merge r sets) r)) %))
       log (append-log log))
     (logged {:update-count 1} log (:rows log))]))

(defn- apply-delete [db {:keys [table log] :as plan}]
  (let [ok? (guarded (get-in db [:tables table :rows]) plan "delete")]
    [(cond-> (update-in db [:tables table :rows] #(vec (remove ok? %)))
       log (append-log log))
     (logged {:delete-count 1} log (:rows log))]))

(defn- next-identity [rows col]
  (inc (reduce max 0 (keep #(some-> (get % col) str parse-long) rows))))

(defn- apply-insert [db {:keys [table insert returning autonumber sequence log] :as plan}]
  (when (or autonumber sequence)
    (refusal/refuse! :refusal.foreign/mem-no-numbers {:unsupported :write :table table}))
  (let [rows (get-in db [:tables table :rows])
        row  (update-keys insert name)
        id   (when returning (or (get row returning) (next-identity rows returning)))
        row  (cond-> row returning (assoc returning id))
        lrs  (when log (policy/insert-log-rows log table id (apply dissoc row (:stamped plan))))]
    [(cond-> (update-in db [:tables table :rows] (fnil conj []) row)
       log (append-log {:table (:table log) :rows lrs}))
     (logged {:insert-count 1 :identity id} log lrs)]))

(def profile
  {:capabilities #{:catalog :scan :sql :land :write :invoke} :route #{:direct} :needs []
   :scan source/local-scan})

(defn- operations [spec]
  (merge (:webhooks spec) (get-in spec [:rest :operations])
         (get-in spec [:config :rest :operations])))

(defn- invoke [state spec webhook-id params]
  (let [wh (get (operations spec) webhook-id)]
    (when-not wh
      (refusal/refuse! :refusal.foreign/webhook-not-allowed
                       {:webhook-id webhook-id :allowed (vec (keys (operations spec)))}))
    (when-not (= (set (:params wh)) (set (keys params)))
      (refusal/refuse! :refusal.foreign/webhook-params
                       {:webhook-id webhook-id :declared (set (:params wh))
                        :supplied (set (keys params))}))
    (swap! state update :calls (fnil conj []) {:webhook-id webhook-id :params params})
    {:status 200 :body {:webhook-id webhook-id}}))

(defrecord MemSource [spec state clock]
  api/Source
  (-profile [_] profile)
  (-probe [_] nil)
  (-close [_] nil)
  api/Catalog
  (-tables [_] (vec (sort (keys (:tables @state)))))
  (-columns [_ table]
    (mapv (fn [{:keys [name type]}]
            {:name name :type type :ontology-type (dialect/sql-type->ontology-type type)})
          (:columns (table-of @state spec table))))
  (-relations [_] (vec (:relations @state)))
  api/Scan
  (-scan-profile [_ _] (:scan profile))
  (-scan [_ table pushed] (api/run-local (scan-rows @state spec table) pushed))
  api/Sql
  (-select [_ sql _params limit]
    (cond->> (select-rows @state spec sql) limit (take limit) true vec))
  api/Land
  (-reach! [_] (or (:landed @state) (api/unsupported :reach! spec)))
  api/Invoke
  (-invoke! [_ webhook-id params _] (invoke state spec webhook-id params))
  api/Write
  (-write! [_ write-spec]
    (let [plan (policy/write-plan (clock) spec write-spec)
          step (case (:op plan) :insert apply-insert :delete apply-delete apply-update)
          res  (volatile! nil)]
      (swap! state (fn [db] (let [[db' r] (step db plan)] (vreset! res r) db')))
      @res)))

(defn connector
  ([state] (connector state {}))
  ([state {:keys [clock] :or {clock #(java.time.LocalDateTime/now)}}]
   (fn [spec] (->MemSource spec state clock))))
