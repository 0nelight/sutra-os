(ns sutra.store.pg.control-tables
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.sql :as kit-sql]
            [sutra.store.records :as records]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(set! *warn-on-reflection* true)

(defn- index-spec [{:keys [cols] :as s}]
  (assoc s
         :codec (into {} (keep (fn [[c _ & flags]]
                                 (when-let [f (some #{:edn :keyword :bool} flags)] [c f])))
                      cols)
         :touch (into [] (keep (fn [[c _ & flags]] (when (some #{:touch} flags) c))) cols)))

(defn specs
  [decls]
  (into {} (map (fn [s] [(:name s) (index-spec s)])) decls))

(defn- spec [specs t]
  (or (clojure.core/get specs t)
      (throw (ex-info (str "unknown control table " (pr-str t)) {:table t :known (keys specs)}))))

(defn- q [k] (kit-sql/dq (str/replace (name k) "-" "_")))

(defn- constraints
  [{:keys [unique enums]}]
  (concat (for [[nm & cs] unique]
            (str "CONSTRAINT " (kit-sql/dq nm) " UNIQUE (" (str/join ", " (map q cs)) ")"))
          (for [[nm c vs] enums]
            (str "CONSTRAINT " (kit-sql/dq nm) " CHECK (" (q c) " IN ("
                 (str/join "," (map (comp kit-sql/sq name) (sort-by name vs))) "))"))))

(defn- create-sql [{:keys [table cols key] :as s}]
  (str "CREATE TABLE IF NOT EXISTS " table " ("
       (str/join ", " (concat (map (fn [[c t]] (str (q c) " " t)) cols)
                              [(str "PRIMARY KEY (" (str/join ", " (map q key)) ")")]
                              (constraints s)))
       ")"))

(defn- add-columns-sql [{:keys [table cols]}]
  (str "ALTER TABLE " table " "
       (str/join ", " (map (fn [[c t]] (str "ADD COLUMN IF NOT EXISTS " (q c) " " t)) cols))))

(defn- index-sql [table [nm cols where]]
  (str "CREATE INDEX IF NOT EXISTS " nm " ON " table " (" cols ")"
       (when where (str " WHERE " where))))

(defn- add-unique-sql
  [{:keys [table unique]}]
  (let [bare (str/replace table "\"" "")]
    (for [[nm & cs] unique]
      (str "DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_indexes WHERE schemaname = current_schema()
                  AND tablename = " (kit-sql/sq bare) " AND indexname = " (kit-sql/sq nm) ") THEN
    ALTER TABLE " table " ADD CONSTRAINT " (kit-sql/dq nm)
           " UNIQUE (" (str/join ", " (map q cs)) ");
  END IF;
END $$"))))

(defn- moved-col-sql
  [table [from to]]
  (let [bare (str/replace table "\"" "")]
    (str "DO $$ BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = current_schema()
              AND table_name = " (kit-sql/sq bare)
         " AND column_name = " (kit-sql/sq (str/replace (name from) "-" "_")) ") THEN
    UPDATE " table " SET " (q to) " = " (q from)
         " WHERE " (q to) " IS NULL AND " (q from) " IS NOT NULL;
    ALTER TABLE " table " DROP COLUMN " (q from) ";
  END IF;
END $$")))

(defn- table-ddl
  [{:keys [table indexes moved-cols dropped-cols] :as s}]
  (concat [(create-sql s) (add-columns-sql s)] (map #(index-sql table %) indexes) (add-unique-sql s)
          (map #(moved-col-sql table %) moved-cols)
          (map #(str "ALTER TABLE " table " DROP COLUMN IF EXISTS " (q %)) dropped-cols)))

(defn- rename-table-sql
  [{:keys [from to renames if-column]}]
  (str "DO $$ BEGIN
  IF to_regclass('" from "') IS NOT NULL"
       (when if-column
         (str " AND EXISTS (SELECT 1 FROM information_schema.columns "
              "WHERE table_schema = current_schema()
              AND table_name = " (kit-sql/sq from)
              " AND column_name = " (kit-sql/sq if-column) ")"))
       " THEN
    IF to_regclass('" to "') IS NOT NULL THEN
      IF EXISTS (SELECT 1 FROM " to ") THEN
        RAISE EXCEPTION 'both % and % hold rows; merge them by hand', '" from "', '" to "';
      END IF;
      DROP TABLE " to ";
    END IF;
    ALTER TABLE " from " RENAME TO " to ";"
       (apply str
              (for [[o n] renames]
                (str "
    IF EXISTS (SELECT 1 FROM pg_constraint WHERE conname = '" o
                "' AND conrelid = '" to "'::regclass) THEN
      ALTER TABLE " to " RENAME CONSTRAINT " o " TO " n ";
    ELSIF to_regclass('" o "') IS NOT NULL THEN
      ALTER INDEX " o " RENAME TO " n ";
    END IF;")))
       "
  END IF;
END $$"))

(defn ddl
  [decls renames]
  (into [] (concat (map rename-table-sql renames)
                   (mapcat table-ddl decls))))

(defn widenings
  [decls existing]
  (for [{:keys [table cols key]} decls
        :let [bare (str/replace table "\"" "")
              keyed (set key)]
        [c t] cols
        :let [have (clojure.core/get existing [bare (str/replace (name c) "-" "_")])]
        :when have
        stmt [(when (and (re-find #"(?i)^text\b" t) (not= "text" (:data_type have)))
                (str "ALTER TABLE " table " ALTER COLUMN " (q c) " TYPE TEXT"))
              (when (and (not (keyed c)) (not (re-find #"(?i)not null|generated" t))
                         (= "NO" (:is_nullable have)))
                (str "ALTER TABLE " table " ALTER COLUMN " (q c) " DROP NOT NULL"))]
        :when stmt]
    stmt))

(defn- col-name [c] (str/replace (name c) "-" "_"))

(defn- pending-table-ddl
  [{:keys [table cols indexes moved-cols dropped-cols] :as s} {:keys [columns tables index-names]}]
  (let [bare (str/replace table "\"" "")
        has? #(contains? columns [bare (col-name %)])]
    (if-not (contains? tables bare)
      (table-ddl s)
      (concat (when-let [missing (seq (remove (comp has? first) cols))]
                [(add-columns-sql (assoc s :cols missing))])
              (for [[nm :as ix] indexes :when (not (index-names (str/lower-case nm)))]
                (index-sql table ix))
              (add-unique-sql s)
              (for [[from :as mv] moved-cols :when (has? from)] (moved-col-sql table mv))
              (for [c dropped-cols :when (has? c)]
                (str "ALTER TABLE " table " DROP COLUMN IF EXISTS " (q c)))))))

(defn pending-ddl
  [decls catalog]
  (for [s decls stmt (pending-table-ddl s catalog)] [(:table s) stmt]))

(defn- columns [c]
  (into {} (map (juxt (juxt :table_name :column_name) identity))
        (jdbc/execute! c ["SELECT table_name, column_name, data_type, is_nullable
                           FROM information_schema.columns WHERE table_schema = current_schema()"]
                       {:builder-fn rs/as-unqualified-maps})))

(defn- catalog [c]
  (let [cols (columns c)]
    {:columns     cols
     :tables      (into #{} (map first) (keys cols))
     :index-names (into #{} (map :indexname)
                        (jdbc/execute! c
                                       ["SELECT indexname FROM pg_indexes WHERE schemaname = current_schema()"]
                                       {:builder-fn rs/as-unqualified-maps}))}))

(def ^:private lock-wait-ms 5000)
(def ^:private lock-attempts 12)

(defn- holders
  [c table]
  (jdbc/execute! c
                 [(str "SELECT a.pid, a.state, l.mode, date_trunc('second', now() - "
                       "a.xact_start)::text AS open_for,"
                       " left(regexp_replace(a.query, '\\s+', ' ', 'g'), 120) AS query"
                       " FROM pg_locks l JOIN pg_stat_activity a ON a.pid = l.pid"
                       " WHERE l.relation = to_regclass(?) AND l.granted AND a.pid <> "
                       "pg_backend_pid()")
                  table]
                 {:builder-fn rs/as-unqualified-maps}))

(defn- exec-waiting!
  [c [table stmt]]
  (loop [n 1]
    (let [r (try (jdbc/execute-one! c [stmt]) ::done
                 (catch java.sql.SQLException e
                   (if (= "55P03" (.getSQLState e)) ::locked (throw e))))]
      (when (= ::locked r)
        (let [hs (when table (holders c table))]
          (when (>= n lock-attempts)
            (anomaly/raise! :unavailable
                            (str "control-tables: no lock on " table " within "
                                 (* lock-attempts lock-wait-ms) " ms; held by " (pr-str hs))
                            {:table table :statement stmt :holders hs}))
          (binding [*out* *err*]
            (println (str "control-tables: " table " is locked (attempt " n "/" lock-attempts
                          "), held by " (pr-str hs))))
          (recur (inc n)))))))

(defn install! [ds decls renames]
  (with-open [c (jdbc/get-connection ds)]
    (jdbc/execute-one! c [(str "SET lock_timeout = '" lock-wait-ms "ms'")])
    (try
      (doseq [r renames] (exec-waiting! c [(:from r) (rename-table-sql r)]))
      (doseq [ddl (pending-ddl decls (catalog c))] (exec-waiting! c ddl))
      (doseq [stmt (widenings decls (columns c))] (exec-waiting! c [nil stmt]))
      (finally (jdbc/execute-one! c ["RESET lock_timeout"]))))
  nil)

(def now ::now)

(defn- encode [codec v]
  (case codec
    :bool (boolean v)
    (when (some? v)
      (case codec :edn (pr-str v) :keyword (name v) v))))

(defn- decode [codec v]
  (case codec
    :bool (boolean v)
    :edn (some-> v edn/read-string)
    :keyword (some-> v keyword)
    v))

(defn- decode-row [s row]
  (reduce-kv (fn [m k v] (assoc m k (decode (clojure.core/get (:codec s) k) v))) {} row))

(defn- scoped [s m]
  (cond-> m
    (and (not (:branch-verbatim? s))
         (or (contains? m :branch) (= :branch (:scope s))))
    (update :branch #(or % records/trunk))))

(defn- value-sql [s c v]
  (let [codec (clojure.core/get (:codec s) c)]
    (cond
      (= v now) ["CURRENT_TIMESTAMP" []]
      :else ["?" [(encode codec v)]])))

(defn- kebab-rows [ds sql params]
  (jdbc/execute! ds (into [sql] params) {:builder-fn rs/as-unqualified-kebab-maps}))

(defn- values-sql [s cols row]
  (let [parts (map (fn [c] (value-sql s c (clojure.core/get row c))) cols)]
    [(str "(" (str/join ", " (map first parts)) ")") (into [] (mapcat second) parts)]))

(defn- insert-sql [s cols vs tail]
  (str "INSERT INTO " (:table s) " (" (str/join ", " (map q cols)) ") VALUES " vs tail))

(defn- write-one! [ds s row tail]
  (let [row (scoped s row)
        cols (vec (keys row))
        [vs params] (values-sql s cols row)]
    (some->> (first (kebab-rows ds (insert-sql s cols vs (str tail " RETURNING *")) params))
             (decode-row s))))

(defn insert! [ds decls t row]
  (write-one! ds (spec (specs decls) t) row ""))
