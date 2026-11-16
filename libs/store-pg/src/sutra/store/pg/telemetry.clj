(ns sutra.store.pg.telemetry
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.pg.ns :as pgns]))

(set! *warn-on-reflection* true)

(def ^:private samples-table "control_node_samples")

(def ^:private observations-table "control_node_observations")

(def ^:private recordings-table "control_node_recordings")

(def ^:private keep-ms
  (* 60 60 1000))

(defn- ds
  [table pools installed]
  (let [d (pools nil)]
    (when-not (identical? d @installed)
      (jdbc/execute! d [(str "CREATE TABLE IF NOT EXISTS " table
                             " (node VARCHAR(255) PRIMARY KEY, role VARCHAR(32),"
                             "  at TIMESTAMPTZ NOT NULL, sample TEXT NOT NULL)")])
      (reset! installed d))
    d))

(def ^:private lower {:builder-fn rs/as-unqualified-lower-maps})

(defn- publish!
  [table d {:keys [node role] :as sample}]
  (jdbc/execute! d [(str "INSERT INTO " table " (node, role, at, sample) VALUES (?, ?, now(), ?)"
                         " ON CONFLICT (node) DO UPDATE SET role = EXCLUDED.role,"
                         "  at = EXCLUDED.at, sample = EXCLUDED.sample")
                    (str node) (some-> role name) (pr-str sample)])
  (jdbc/execute! d [(str "DELETE FROM " table " WHERE at < now() - (? * interval '1 millisecond')")
                    keep-ms])
  nil)

(defn- recent
  [table d]
  (vec (for [{:keys [sample age_ms]}
             (jdbc/execute! d [(str "SELECT sample, (extract(epoch FROM now() - at) * 1000)::bigint"
                                    " AS age_ms FROM " table " ORDER BY node")]
                            lower)]
         (assoc (edn/read-string sample) :age-ms age_ms))))

(defn- node-rows
  [table pools]
  (let [installed (atom nil)]
    {:publish! #(publish! table (ds table pools installed) %)
     :recent   #(recent table (ds table pools installed))}))

(def ^:private size-settings
  ["shared_buffers" "work_mem" "maintenance_work_mem" "effective_cache_size"])

(defn- settings
  [c]
  (let [rows (jdbc/execute! c ["SELECT name, setting, unit FROM pg_settings WHERE name = ANY(?)"
                               (into-array String (conj size-settings "max_connections"))]
                            lower)
        unit {"8kB" 8192 "kB" 1024 "MB" 1048576 "B" 1}]
    (into (sorted-map)
          (for [{:keys [name setting] u :unit} rows]
            [(keyword (str/replace name "_" "-"))
             (* (parse-long setting) (long (get unit u 1)))]))))

(defn- connections
  [c]
  (vec (jdbc/execute! c [(str "SELECT coalesce(nullif(application_name, ''), backend_type) AS app,"
                              " coalesce(state, '') AS state, count(*) AS n"
                              " FROM pg_stat_activity WHERE datname = current_database()"
                              " GROUP BY 1, 2 ORDER BY 3 DESC")]
                      lower)))

(def ^:private relkinds
  {"r" :tables "p" :partitioned "i" :indexes "I" :partitioned-indexes "t" :toast
   "v" :views "m" :materialized "S" :sequences "c" :composite "f" :foreign})

(defn- catalog
  [c]
  (into (sorted-map)
        (for [{:keys [relkind n]} (jdbc/execute! c [(str "SELECT relkind::text, count(*) AS n"
                                                         " FROM pg_class GROUP BY 1")]
                                                 lower)]
          [(get relkinds relkind (keyword relkind)) n])))

(defn- schemas
  [c]
  (vec (jdbc/execute! c [(str "SELECT n.nspname AS schema,"
                              " count(c.oid) FILTER (WHERE c.relkind IN ('r','p')) AS tables,"
                              " count(c.oid) FILTER (WHERE c.relkind = 'r'"
                              "   AND coalesce(s.n_live_tup, 0) = 0) AS empty,"
                              " (coalesce(sum(c.relpages::bigint + coalesce(t.relpages, 0)), 0)"
                              "   * current_setting('block_size')::bigint)::bigint AS bytes"
                              " FROM pg_namespace n"
                              " LEFT JOIN pg_class c ON c.relnamespace = n.oid"
                              " LEFT JOIN pg_class t ON t.oid = c.reltoastrelid"
                              " LEFT JOIN pg_stat_user_tables s ON s.relid = c.oid"
                              " WHERE n.nspname = 'main' OR n.nspname LIKE 'bo\\_%'"
                              " GROUP BY 1")]
                      lower)))

(defn- by-branch
  [rows branches]
  (let [owner (into {} (for [{:keys [name] :as b} branches] [(pgns/own-schema name) b]))]
    (mapv #(assoc % :branch (get owner (:schema %))) rows)))

(defn- footprint
  [d {:keys [branches]}]
  (jdbc/with-transaction [c d {:read-only true}]
    (jdbc/execute! c ["SET LOCAL statement_timeout = '15s'"])
    {:database-bytes (:n (jdbc/execute-one! c ["SELECT pg_database_size(current_database()) AS n"]
                                            lower))
     :settings       (settings c)
     :connections    (connections c)
     :catalog        (catalog c)
     :schemas        (by-branch (schemas c) branches)}))

(def module
  {:name     :pg-telemetry
   :requires [:pg/ds]
   :opens    {:telemetry/samples (fn [_ {pools :pg/ds}] (node-rows samples-table pools))
              :observe/reports   (fn [_ {pools :pg/ds}] (node-rows observations-table pools))
              :observe/recordings (fn [_ {pools :pg/ds}] (node-rows recordings-table pools))
              :telemetry/store   (fn [_ {pools :pg/ds}] #(footprint (pools nil) %))}})
