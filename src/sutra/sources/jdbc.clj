(ns sutra.sources.jdbc
  (:require [sutra.foundation.settings :as settings]
            [sutra.kit.refusal :as refusal]
            [sutra.kit.redact :as redact]
            [clojure.string :as str]
            [sutra.foundation.sql-trace :as sqltrace]
            [sutra.sources.protocols :as api]
            [sutra.sources.dialect :as dialect]
            [sutra.sources.local-files :as local-files]
            [sutra.kit.read-only-sql :as read-only]
            [sutra.sources.policy :as policy]
            [sutra.store.scan :as scan]
            [sutra.types.source :as source]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import (java.sql DriverManager Connection)))

(set! *warn-on-reflection* true)

(def ^:private driver-class
  {:sqlite   "org.sqlite.JDBC"
   :postgres "org.postgresql.Driver"
   :mysql    "com.mysql.cj.jdbc.Driver"
   :mssql    "com.microsoft.sqlserver.jdbc.SQLServerDriver"})

(defn- load-driver! [type-kw]
  (when-let [cls (driver-class type-kw)]
    (try (Class/forName cls)
         (catch ClassNotFoundException _
           (throw (ex-info (str "JDBC driver not on classpath for " type-kw
                                " (expected " cls "). Add it to deps.edn.")
                           {:type type-kw :class cls}))))))

(defn- trace-label
  [{:keys [type source-name] :as spec}]
  (let [[host port] (policy/spec-endpoint spec)]
    (str (name (or type :jdbc))
         ":" (or host source-name "?")
         (when port (str ":" port)))))

(defn- assert-url!
  [{:keys [type jdbc-url] :as spec}]
  (when-not jdbc-url
    (refusal/refuse! :refusal.foreign/jdbc-url-missing {:spec (dissoc spec :password)}))
  (policy/assert-jdbc-url! spec)
  (when (= :sqlite type)
    (some-> (re-find #"^jdbc:sqlite:(/.*)" jdbc-url) second local-files/path))
  (policy/assert-egress-allowed! spec))

(defn open!
  ^Connection [{:keys [type jdbc-url user password] :as spec}]
  (assert-url! spec)
  (load-driver! type)
  (let [conn (DriverManager/getConnection jdbc-url user password)]
    (try (.setReadOnly conn true)
         (catch java.sql.SQLException _ nil))
    (sqltrace/wrap-connection conn (trace-label spec))))

(defn close! [^Connection conn]
  (when conn (.close conn)))

(defmacro with-open-conn
  [[conn-sym spec] & body]
  `(let [c# (open! ~spec)]
     (try (let [~conn-sym c#] ~@body)
          (finally (close! c#)))))

(defn- source-query-seconds
  []
  (settings/value :source-query-seconds))

(defn execute!
  ([conn type sql] (execute! conn type sql nil))
  ([conn type sql params]
   (read-only/assert-read-only-sql! sql type)
   (let [secs (source-query-seconds)]
     (jdbc/execute! conn (into [sql] params)
                    (cond-> {:builder-fn rs/as-unqualified-lower-maps}
                      (and secs (pos? secs)) (assoc :timeout secs))))))

(defn open-writable!
  ^Connection [{:keys [type jdbc-url user password access] :as spec}]
  (when-not jdbc-url
    (refusal/refuse! :refusal.foreign/jdbc-url-missing {:spec (redact/redact spec)}))
  (when-not (= :read-write access)
    (throw (ex-info "open-writable! on a non-:read-write source" {:access access})))
  (assert-url! spec)
  (load-driver! type)
  (sqltrace/wrap-connection
   (DriverManager/getConnection jdbc-url user password)
   (trace-label spec)))

(defmacro with-writable-conn
  [[conn-sym spec] & body]
  `(let [c# (open-writable! ~spec)]
     (try (let [~conn-sym c#] ~@body)
          (finally (close! c#)))))

(def ^:private as-maps {:builder-fn rs/as-unqualified-lower-maps})

(defn- cast-columns
  [conn type table expect]
  (let [stmt (dialect/cast-columns-stmt type table)]
    (if (or (nil? stmt) (empty? expect))
      #{}
      (->> (jdbc/execute! conn stmt as-maps)
           (map (comp str/upper-case :c))
           set))))

(defn- run-counted!
  [conn type stmt]
  (if (dialect/output? type)
    (long (:n (jdbc/execute-one! conn stmt as-maps)))
    (:next.jdbc/update-count (jdbc/execute-one! conn stmt))))

(defn execute-update!
  [conn type {:keys [table expect] :as plan}]
  (run-counted! conn type (dialect/update-stmt type plan (cast-columns conn type table expect))))

(defn execute-delete!
  [conn type {:keys [table expect] :as plan}]
  (run-counted! conn type (dialect/delete-stmt type plan (cast-columns conn type table expect))))

(defn execute-log!
  [conn type log]
  (reduce (fn [n stmt]
            (+ n (or (:next.jdbc/update-count (jdbc/execute-one! conn stmt)) 0)))
          0 (dialect/log-stmts type log)))

(defn execute-next-in-sequence!
  [conn type sequence scope-value]
  (long (or (:n (jdbc/execute-one! conn (dialect/next-in-sequence-stmt type sequence scope-value)
                                   as-maps))
            1)))

(defn execute-allocate-number!
  [conn type allocation]
  (dialect/allocated-number allocation
                            (jdbc/execute! conn (dialect/allocate-number-stmt type allocation)
                                           as-maps)))

(defn execute-insert!
  [conn type plan]
  (let [{:keys [stmt read-back]} (dialect/insert-stmt type plan)]
    (case read-back
      (:output :returning)
      {:insert-count 1 :identity (:id (jdbc/execute-one! conn stmt as-maps))}
      :generated-keys
      {:insert-count 1
       :identity (some-> (jdbc/execute-one! conn stmt {:return-keys true}) vals first)}
      (do (jdbc/execute-one! conn stmt)
          {:insert-count 1 :identity nil}))))

(defn execute-insert-rows!
  [conn type {:keys [table columns rows]}]
  (let [stmt (dialect/insert-rows-stmt type table columns rows)]
    (or (:next.jdbc/update-count (jdbc/execute-one! conn stmt)) (count rows))))

(defn execute-truncate!
  [conn type {:keys [table]}]
  (jdbc/execute-one! conn (dialect/truncate-stmt type table))
  nil)

(def ^:private system-schemas
  #{"pg_catalog" "information_schema" "INFORMATION_SCHEMA"
    "performance_schema" "mysql" "sys"
    "sqlite_temp_master" "sqlite_master"})

(defn- list-tables
  [^Connection conn]
  (let [md (.getMetaData conn)
        rs (.getTables md nil nil "%" (into-array String ["TABLE" "VIEW"]))]
    (try
      (loop [out (transient [])]
        (if (.next rs)
          (let [schema (.getString rs "TABLE_SCHEM")
                tname  (.getString rs "TABLE_NAME")]
            (recur (if (or (contains? system-schemas schema)
                           (and tname (.startsWith tname "sqlite_")))
                     out
                     (conj! out tname))))
          (vec (sort (distinct (persistent! out))))))
      (finally (.close rs)))))

(defn- list-columns
  [^Connection conn ^String table]
  (let [md (.getMetaData conn)
        rs (.getColumns md nil nil table "%")]
    (try
      (loop [out (transient [])]
        (if (.next rs)
          (let [type-name (.getString rs "TYPE_NAME")]
            (recur (conj! out {:name          (.getString rs "COLUMN_NAME")
                               :type          type-name
                               :ontology-type (dialect/sql-type->ontology-type type-name)})))
          (persistent! out)))
      (finally (.close rs)))))

(defn table-exists?
  [^Connection conn ^String table]
  (let [md (.getMetaData conn)
        rs (.getTables md nil nil table (into-array String ["TABLE" "VIEW"]))]
    (try (.next rs)
         (finally (.close rs)))))

(defn create-table!
  [^Connection conn table columns primary-key]
  (let [tname (policy/safe-ident table)]
    (if (table-exists? conn tname)
      {:created? false :table tname}
      (do (try (.setReadOnly conn false) (catch java.sql.SQLException _ nil))
          (jdbc/execute! conn [(dialect/create-table-sql tname columns primary-key)])
          {:created? true :table tname}))))

(defn- list-relations
  [^Connection conn]
  (let [md (.getMetaData conn)]
    (vec (for [t (list-tables conn)
               r (let [rs (.getImportedKeys md nil nil t)]
                   (try
                     (loop [out (transient [])]
                       (if (.next rs)
                         (recur (conj! out {:from-table (.getString rs "FKTABLE_NAME")
                                            :from-col   (.getString rs "FKCOLUMN_NAME")
                                            :to-table   (.getString rs "PKTABLE_NAME")
                                            :to-col     (.getString rs "PKCOLUMN_NAME")
                                            :constraint (.getString rs "FK_NAME")}))
                         (persistent! out)))
                     (finally (.close rs))))]
           r))))

(defn- in-tx
  [conn in-session? f]
  (if in-session? (f conn) (jdbc/with-transaction [tx conn] (f tx))))

(defn- check-logged!
  [plan log logged expected what]
  (when (not= logged expected)
    (throw (ex-info (str "change-log incomplete: wrote " logged " of " expected " rows into "
                         (:table log) " — rolling the " what " back")
                    {:table (:table plan) :log-table (:table log)
                     :logged logged :expected expected}))))

(defn- check-one-row!
  [plan n none many]
  (when (zero? n)
    (refusal/refuse! none {:conflict true :table (:table plan) :pk-value (:pk-value plan)
                           :expect (:expect plan)}))
  (when (> n 1)
    (refusal/refuse! many {:table (:table plan) :pk-value (:pk-value plan) :rows n})))

(defn- perform-update!
  [conn in-session? type {:keys [log] :as plan}]
  (in-tx conn in-session?
         (fn [tx]
           (let [n (execute-update! tx type plan)]
             (check-one-row! plan n :refusal.foreign/edit-conflict
                             :refusal.foreign/key-not-unique)
             (if-not log
               {:update-count n :logged 0}
               (let [logged (execute-log! tx type log)]
                 (check-logged! plan log logged (:expect-count log) "writeback")
                 {:update-count n :logged logged :log-table (:table log)}))))))

(defn- perform-delete!
  [conn in-session? type {:keys [log] :as plan}]
  (in-tx conn in-session?
         (fn [tx]
           (let [logged (when log (execute-log! tx type log))
                 _      (when log (check-logged! plan log logged (:expect-count log) "delete"))
                 n      (execute-delete! tx type plan)]
             (check-one-row! plan n :refusal.foreign/delete-conflict
                             :refusal.foreign/delete-key-not-unique)
             (cond-> {:delete-count n :logged (or logged 0)}
               log (assoc :log-table (:table log)))))))

(defn- autonumbers [autonumber]
  (when autonumber (if (sequential? autonumber) (vec autonumber) [autonumber])))

(defn- zero-padded [n width]
  (let [s (str n)] (str (apply str (repeat (max 0 (- width (count s))) \0)) s)))

(defn- drawn-insert
  [{:keys [sequence] :as plan} drawn lauf]
  (cond-> plan
    (seq drawn) (update :insert merge (into {} drawn))
    lauf        (update :insert merge
                        (into {(:column sequence) lauf}
                              (map (fn [c] [c (zero-padded lauf (or (:pad sequence) 0))]))
                              (:also sequence)))))

(defn- insert-answer
  [res {:keys [sequence] :as plan} drawn lauf]
  (cond-> res
    (seq drawn)                (assoc :number (second (first drawn)))
    (and (empty? drawn) lauf)  (assoc :number lauf)
    lauf        (assoc :drawn (select-keys (:insert plan)
                                           (cons (:column sequence) (:also sequence))))))

(defn- perform-insert!
  [conn in-session? type {:keys [log autonumber sequence] :as plan}]
  (in-tx conn in-session?
         (fn [tx]
           (let [drawn (mapv (fn [a] [(:into a) (execute-allocate-number! tx type a)])
                             (autonumbers autonumber))
                 lauf  (when sequence
                         (execute-next-in-sequence! tx type (assoc sequence :table (:table plan))
                                                    (get (:insert plan) (:scope sequence))))
                 plan  (drawn-insert plan drawn lauf)
                 res   (insert-answer (execute-insert! tx type plan) plan drawn lauf)]
             (if-not log
               (assoc res :logged 0)
               (let [rows   (policy/insert-log-rows log (:table plan) (:identity res)
                                                    (apply dissoc (:insert plan) (:stamped plan)))
                     logged (execute-log! tx type {:table (:table log) :rows rows})]
                 (check-logged! plan log logged (count rows) "insert")
                 (assoc res :logged logged :log-table (:table log))))))))

(defn apply-plan!
  [conn in-session? type plan]
  (when (= :mssql type)
    (jdbc/execute! conn ["SET XACT_ABORT ON"]))
  (case (:op plan)
    :insert      (try (perform-insert! conn in-session? type plan)
                      (catch java.sql.SQLException e
                        (throw (ex-info (str (ex-message e) " — " (:table plan) " "
                                             (pr-str (update-vals
                                                      (:insert plan)
                                                      #(some-> % class .getSimpleName))))
                                        {:table (:table plan)} e))))
    :delete      (perform-delete! conn in-session? type plan)
    :insert-rows (in-tx conn in-session?
                        (fn [tx] {:insert-count (execute-insert-rows! tx type plan)}))
    :truncate    (in-tx conn in-session?
                        (fn [tx] (execute-truncate! tx type plan) {:truncated true}))
    (perform-update! conn in-session? type plan)))

(defn- columns-of
  [conn cache table]
  (or (get @cache table)
      (get (swap! cache assoc table (list-columns @conn table)) table)))

(defn- capped-select [conn type sql params n]
  (try (vec (take n (execute! conn type (dialect/cap-wrap type sql n) params)))
       (catch Exception e (throw (or (dialect/capped-refusal e) e)))))

(defn sql-scan
  [type table columns run key {:keys [where project order] :as pushed}]
  (let [cols (when (or (seq where) project (seq order))
               (into {} (map (juxt #(keyword (str/lower-case (:name %))) identity)) (columns)))
        stmt (dialect/scan-stmt type table cols #(scan/holds? [nil {}] %) key pushed)]
    (cond->> (vec (run stmt))
      project (mapv #(select-keys % project)))))

(defn table-scan-profile
  [type columns key]
  (let [per-op (apply merge-with merge
                      {}
                      (for [{:keys [name type]} columns
                            :let [attr (keyword (str/lower-case name))]
                            [op by-class] (dialect/comparison-judgements type)]
                        {op (into {} (for [[class j] by-class] [[attr class] j]))}))]
    (-> (:scan (source/profile type))
        (update :preds merge per-op)
        (assoc :not? false)
        (assoc :order? (dialect/orderable-attrs columns key)))))

(defrecord JdbcSource [spec conn clock cols]
  api/Source
  (-profile [_] (source/profile (:type spec)))
  (-probe [_] (execute! @conn (:type spec) "SELECT 1") nil)
  (-close [_] (when (realized? conn) (close! @conn)))
  api/Catalog
  (-tables [_] (list-tables @conn))
  (-columns [_ table] (columns-of conn cols table))
  (-relations [_] (list-relations @conn))
  api/Scan
  (-scan-profile [_ table]
    (table-scan-profile (:type spec) (columns-of conn cols table) (policy/scan-key spec table)))
  (-scan [_ table pushed]
    (sql-scan (:type spec) table #(columns-of conn cols table)
              (fn [[sql & params]] (execute! @conn (:type spec) sql params))
              (policy/scan-key spec table) pushed))
  api/Sql
  (-select [_ sql params limit]
    (if limit
      (capped-select @conn (:type spec) sql params limit)
      (vec (execute! @conn (:type spec) sql params))))
  api/Write
  (-write! [_ write-spec]
    (let [plan (policy/write-plan (clock) spec write-spec)]
      (with-writable-conn [c spec]
        (apply-plan! c false (:type spec) plan)))))

(defn source
  ([spec] (source spec {}))
  ([spec {:keys [clock] :or {clock #(java.time.LocalDateTime/now)}}]
   (->JdbcSource spec (delay (open! spec)) clock (atom {}))))
