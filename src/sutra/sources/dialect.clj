(ns sutra.sources.dialect
  (:require [sutra.kit.refusal :as refusal]
            [sutra.kit.sql :as kit-sql]
            [clojure.string :as str]
            [sutra.types.coltype :as ctype]
            [sutra.types.value-types :as vt]
            [sutra.sources.policy :as policy]))

(set! *warn-on-reflection* true)

(def ^:private ansi
  {:quote       {:open "\"" :close "\""}
   :limit-style :suffix
   :returning-style :returning
   :tables-sql  "SELECT table_name AS name FROM information_schema.tables WHERE table_schema NOT IN ('pg_catalog','information_schema') ORDER BY table_name"
   :cols-sql    (fn [t]
                  (str "SELECT column_name AS name, data_type AS type FROM "
                       "information_schema.columns WHERE table_name = '" t
                       "' AND table_schema NOT IN ('pg_catalog','information_schema') ORDER BY "
                       "ordinal_position"))
   :rels-sql    (str "SELECT tc.table_name AS from_table, kcu.column_name AS from_col, "
                     "ccu.table_name AS to_table, ccu.column_name AS to_col, tc.constraint_name AS "
                     "constraint_name "
                     "FROM information_schema.table_constraints tc "
                     "JOIN information_schema.key_column_usage kcu ON tc.constraint_name = "
                     "kcu.constraint_name AND tc.table_schema = kcu.table_schema "
                     "JOIN information_schema.constraint_column_usage ccu ON ccu.constraint_name = "
                     "tc.constraint_name AND ccu.table_schema = tc.table_schema "
                     "WHERE tc.constraint_type = 'FOREIGN KEY' AND tc.table_schema NOT IN "
                     "('pg_catalog','information_schema') "
                     "ORDER BY from_table, from_col")})

(def ^:private dialects
  {:mysql    {:quote       {:open "`" :close "`"}
              :limit-style :suffix
              :returning-style :generated-keys
              :tables-sql  "SELECT table_name AS name FROM information_schema.tables WHERE table_schema = DATABASE() AND table_type IN ('BASE TABLE','VIEW') ORDER BY table_name"
              :cols-sql    (fn [t]
                             (str "SELECT column_name AS name, data_type AS type FROM "
                                  "information_schema.columns WHERE table_schema = DATABASE() AND "
                                  "table_name = '" t "' ORDER BY ordinal_position"))
              :rels-sql    (str "SELECT table_name AS from_table, column_name AS from_col, "
                                "referenced_table_name AS to_table, referenced_column_name AS "
                                "to_col, constraint_name AS constraint_name "
                                "FROM information_schema.key_column_usage "
                                "WHERE table_schema = DATABASE() AND referenced_table_name IS NOT "
                                "NULL "
                                "ORDER BY from_table, from_col")}
   :mssql    {:quote       {:open "[" :close "]"}
              :limit-style :top
              :returning-style :output
              :guard-cast       (fn [q] (str "CAST(" q " AS varchar(max))"))
              :guard-cast-types #{"text" "ntext"}
              :tables-sql  "SELECT TABLE_NAME AS name FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_TYPE IN ('BASE TABLE','VIEW') ORDER BY TABLE_NAME"
              :cols-sql    (fn [t]
                             (str "SELECT COLUMN_NAME AS name, DATA_TYPE AS type FROM "
                                  "INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = '" t
                                  "' ORDER BY ORDINAL_POSITION"))
              :rels-sql    (str "SELECT fk.TABLE_NAME AS from_table, cu.COLUMN_NAME AS from_col, "
                                "pk.TABLE_NAME AS to_table, pt.COLUMN_NAME AS to_col, "
                                "rc.CONSTRAINT_NAME AS constraint_name "
                                "FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS rc "
                                "JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS fk ON "
                                "rc.CONSTRAINT_NAME = fk.CONSTRAINT_NAME "
                                "JOIN INFORMATION_SCHEMA.TABLE_CONSTRAINTS pk ON "
                                "rc.UNIQUE_CONSTRAINT_NAME = pk.CONSTRAINT_NAME "
                                "JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE cu ON rc.CONSTRAINT_NAME "
                                "= cu.CONSTRAINT_NAME "
                                "JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE pt ON "
                                "rc.UNIQUE_CONSTRAINT_NAME = pt.CONSTRAINT_NAME AND "
                                "pt.ORDINAL_POSITION = cu.ORDINAL_POSITION "
                                "ORDER BY from_table, from_col")}
   :postgres ansi})

(defn- dialect-of [type] (get dialects type ansi))

(defn returning-style [type] (:returning-style (dialect-of type)))

(defn output? [type] (= :output (returning-style type)))

(defn quote-ident
  [type s]
  (let [{:keys [open close]} (:quote (dialect-of type))]
    (kit-sql/quote-with open close s)))

(defn tables-sql [type] (:tables-sql (dialect-of type)))

(defn probe-sql
  [type table column]
  (str "SELECT MAX(" (quote-ident type (policy/safe-ident column)) ") AS m, COUNT(*) AS n FROM "
       (quote-ident type (policy/safe-ident table))))

(defn columns-sql [type table] ((:cols-sql (dialect-of type)) (policy/safe-ident table)))

(defn relations-sql [type] (:rels-sql (dialect-of type)))

(defn cap-wrap
  [type sql n]
  (let [inner (str/replace (str/trim sql) #";\s*$" "")]
    (if (= :top (:limit-style (dialect-of type)))
      (str "SELECT TOP " n " * FROM (" inner ") AS _sutra_q")
      (str "SELECT * FROM (" inner ") AS _sutra_q LIMIT " n))))

(defn capped-refusal
  [^Exception e]
  (when (some-> (ex-message e) (str/includes? "ORDER BY clause is invalid in views"))
    (let [msg (str "the query runs wrapped in a row-capping subquery"
                   " (SELECT TOP n * FROM (<your SQL>)), where SQL Server refuses an ORDER BY"
                   " — drop the ORDER BY and sort afterwards, or add TOP/OFFSET: "
                   (ex-message e))]
      (cond
        (instance? clojure.lang.ExceptionInfo e) (ex-info msg (or (ex-data e) {}) e)
        (instance? java.sql.SQLException e)
        (let [^java.sql.SQLException se e]
          (java.sql.SQLException. msg (.getSQLState se) (.getErrorCode se) se))))))

(def ^:private vendor-kinds
  {"bit" :boolean "datetime2" :timestamp "smalldatetime" :timestamp "datetimeoffset" :timestamp
   "money" :decimal "smallmoney" :decimal "ntext" :text "longvarchar" :text
   "uniqueidentifier" :varchar})

(defn- column-kind [sql-type]
  (let [t (-> (str sql-type) str/lower-case (str/replace #"\s+identity\b.*$" "") str/trim)]
    (or (:kind (ctype/parse t))
        (get vendor-kinds (str/trim (str/replace t #"\(.*\)\s*$" ""))))))

(defn sql-type->ontology-type
  [^String sql-type]
  (vt/of-column-kind (column-kind sql-type)))

(defn ->relation
  [r]
  {:from-table (or (:from-table r) (:from_table r))
   :from-col   (or (:from-col r)   (:from_col r))
   :to-table   (or (:to-table r)   (:to_table r))
   :to-col     (or (:to-col r)     (:to_col r))
   :constraint (or (:constraint r) (:constraint_name r) (:constraint-name r))})

(defn- output-batch
  [dml select]
  (str "SET NOCOUNT ON; "
       "DECLARE @sutra_out TABLE (id sql_variant); "
       dml "; "
       "SET NOCOUNT OFF; "
       select))

(defn cast-columns-stmt
  [type table]
  (let [ts (vec (sort (:guard-cast-types (dialect-of type))))]
    (when (seq ts)
      (into [(str "SELECT COLUMN_NAME AS c FROM INFORMATION_SCHEMA.COLUMNS"
                  " WHERE TABLE_NAME = ? AND DATA_TYPE IN ("
                  (str/join ", " (repeat (count ts) "?")) ")")
             table]
            ts))))

(defn- guard-clauses
  [type expect cast?]
  (let [cast (:guard-cast (dialect-of type))]
    (for [c (sort (keys expect))]
      (let [q   (quote-ident type c)
            lhs (if (contains? cast? (str/upper-case c)) (cast q) q)]
        [(str "(" lhs " = ? OR (" q " IS NULL AND ? IS NULL))") (get expect c)]))))

(defn- pk-clause
  [type pk pk-columns pk-value]
  (if (seq pk-columns)
    [(str/join " AND " (map #(str (quote-ident type %) " = ?") pk-columns))
     (policy/pk-parts pk-columns pk-value)]
    [(str (quote-ident type pk) " = ?") [pk-value]]))

(defn- guarded-where
  [type {:keys [pk pk-columns pk-value expect]} cast?]
  (let [guards             (guard-clauses type expect cast?)
        [where key-params] (pk-clause type pk pk-columns pk-value)]
    [(str " WHERE " where (apply str (map #(str " AND " (first %)) guards)))
     (into (vec key-params) (mapcat (fn [[_ v]] [v v])) guards)
     (quote-ident type (or (first pk-columns) pk))]))

(defn- counted
  [type head tail output params]
  (if (output? type)
    (into [(output-batch (str head " OUTPUT " output " INTO @sutra_out (id)" tail)
                         "SELECT COUNT(*) AS n FROM @sutra_out;")]
          params)
    (into [(str head tail)] params)))

(defn update-stmt
  [type {:keys [table] sets :set :as plan} cast?]
  (let [cols                   (sort (keys sets))
        [where params key-col] (guarded-where type plan cast?)]
    (counted type
             (str "UPDATE " (quote-ident type table) " SET "
                  (str/join ", " (map #(str (quote-ident type %) " = ?") cols)))
             where
             (str "INSERTED." key-col)
             (into (mapv #(get sets %) cols) params))))

(defn delete-stmt
  [type {:keys [table] :as plan} cast?]
  (let [[where params key-col] (guarded-where type plan cast?)]
    (counted type
             (str "DELETE FROM " (quote-ident type table))
             where
             (str "DELETED." key-col)
             params)))

(defn log-stmts
  [type {:keys [table rows]}]
  (for [row rows]
    (let [cols (sort (keys row))]
      (into [(str "INSERT INTO " (quote-ident type table)
                  " (" (str/join ", " (map #(quote-ident type %) cols)) ")"
                  " VALUES (" (str/join ", " (repeat (count cols) "?")) ")")]
            (map #(get row %))
            cols))))

(defn next-in-sequence-stmt
  [type {:keys [table column scope]} scope-value]
  [(str "SELECT ISNULL(MAX(" (quote-ident type column) "), 0) + 1 AS n FROM "
        (quote-ident type table)
        " WHERE " (quote-ident type scope) " = ?")
   scope-value])

(defn allocate-number-stmt
  [type {:keys [table where prefix-column counter-column width-column]}]
  (let [wcols  (sort (keys where))
        alias  (fn [col a] (str (quote-ident type col) " AS " a))
        outs   (cond-> [(alias counter-column "n")]
                 prefix-column (conj (alias prefix-column "p"))
                 width-column  (conj (alias width-column "w")))
        clause (str/join " AND " (map #(str (quote-ident type %) " = ?") wcols))
        bump   (str "UPDATE " (quote-ident type table)
                    " SET " (quote-ident type counter-column)
                    " = " (quote-ident type counter-column) " + 1")]
    (into [(if (output? type)
             (str bump " OUTPUT " (str/join ", " (map #(str "INSERTED." %) outs))
                  " WHERE " clause)
             (str bump " WHERE " clause " RETURNING " (str/join ", " outs)))]
          (map #(get where %))
          wcols)))

(defn allocated-number
  [{:keys [table where width]} rows]
  (when-not (= 1 (count rows))
    (refusal/refuse! :refusal.foreign/counter-rows {:table table :where where :rows (count rows)}))
  (let [{:keys [n p w]} (first rows)
        width  (or width (some-> w str parse-long))
        digits (str n)]
    (when-not width
      (refusal/refuse! :refusal.foreign/counter-no-width {:table table}))
    (when (> (count digits) width)
      (refusal/refuse! :refusal.foreign/counter-exhausted {:table table :counter n :width width}
                       {:digits digits}))
    (str p (apply str (repeat (- width (count digits)) \0)) digits)))

(defn insert-sql
  [type {:keys [table returning returning-columns]} qcols holders style]
  (when (and (seq returning-columns) (= :generated-keys style))
    (refusal/refuse! :refusal.foreign/composite-generated-keys
                     {:table table :pk-columns returning-columns}
                     {:dialect (name type)}))
  (case (if returning style :none)
    :output
    (output-batch (str "INSERT INTO " (quote-ident type table) " (" qcols ") "
                       "OUTPUT "
                       (if (seq returning-columns)
                         (str "CONCAT("
                              (str/join (str ", N'" policy/pk-separator "', ")
                                        (map #(str "RTRIM(CONVERT(nvarchar(4000), INSERTED."
                                                   (quote-ident type %) "))")
                                             returning-columns))
                              ")")
                         (str "INSERTED." (quote-ident type returning)))
                       " INTO @sutra_out (id) "
                       "VALUES (" holders ")")
                  "SELECT id FROM @sutra_out;")
    :returning
    (str "INSERT INTO " (quote-ident type table) " (" qcols ") "
         "VALUES (" holders ") "
         "RETURNING "
         (if (seq returning-columns)
           (str "CONCAT(" (str/join (str ", '" policy/pk-separator "', ")
                                    (map #(quote-ident type %) returning-columns)) ")")
           (quote-ident type returning))
         " AS id")
    (str "INSERT INTO " (quote-ident type table) " (" qcols ") "
         "VALUES (" holders ")")))

(defn insert-stmt
  [type {:keys [insert returning] :as plan}]
  (let [cols    (sort (map name (keys insert)))
        by-name (into {} (map (fn [[k v]] [(name k) v])) insert)
        style   (returning-style type)]
    {:stmt      (into [(insert-sql type plan
                                   (str/join ", " (map #(quote-ident type %) cols))
                                   (str/join ", " (repeat (count cols) "?"))
                                   style)]
                      (map #(get by-name %))
                      cols)
     :read-back (if returning style :none)}))

(defn insert-rows-stmt
  [type table cols rows]
  (let [qcols (str/join ", " (map #(quote-ident type %) cols))
        tuple (str "(" (str/join ", " (repeat (count cols) "?")) ")")]
    (into [(str "INSERT INTO " (quote-ident type table) " (" qcols ") VALUES "
                (str/join ", " (repeat (count rows) tuple)))]
          (mapcat (fn [row] (map #(get row %) cols)))
          rows)))

(defn truncate-stmt
  [type table]
  [(str "TRUNCATE TABLE " (quote-ident type table))])

(def ^:private ext-type-sql
  {:string  "TEXT"
   :number  "DOUBLE PRECISION"
   :boolean "BOOLEAN"})

(defn create-table-sql
  [table columns primary-key]
  (let [pk (some-> primary-key name)]
    (str "CREATE TABLE " (policy/safe-ident table) " ("
         (->> columns
              (map (fn [{:keys [name type]}]
                     (let [n (policy/safe-ident (clojure.core/name name))
                           t (or (ext-type-sql (keyword type))
                                 (refusal/refuse! :refusal.foreign/unknown-column-type
                                                  {:column n :type type}))]
                       (str n " " t (when (= n pk) " PRIMARY KEY")))))
              (str/join ", "))
         ")")))

(def ^:private text-kinds #{:varchar :text})

(def ^:private exact-number-kinds #{:tinyint :smallint :int :bigint :decimal})

(defn- comparable?
  [col v]
  (let [kind (:kind (ctype/parse (:type col)))]
    (cond (string? v)                     (contains? text-kinds kind)
          (or (integer? v) (decimal? v))  (contains? exact-number-kinds kind)
          :else                           false)))

(defn- kind-of [col] (:kind (ctype/parse (:type col))))

(defn- ordered-comparable?
  [col v]
  (and (or (integer? v) (decimal? v))
       (contains? exact-number-kinds (kind-of col))))

(defn- prefix-comparable?
  [col v]
  (and (string? v) (contains? text-kinds (kind-of col))))

(def ^:private like-escape
  {\\ "\\\\" \% "\\%" \_ "\\_" \[ "\\["})

(defn- column-ref [type col]
  (let [q (quote-ident type (:name col))
        {:keys [guard-cast guard-cast-types]} (dialect-of type)]
    (if (and guard-cast (contains? guard-cast-types (str/lower-case (str (:type col)))))
      (guard-cast q)
      q)))

(def ^:private comparison-sql
  {:< "<" :<= "<=" :> ">" :>= ">=" :not= "<>"})

(def ^:private yes ["1=1"])
(def ^:private no ["1=0"])

(defn- where-sql
  [type cols absent [op a b :as pred]]
  (case op
    (:and :or) (let [parts (map #(where-sql type cols absent %) (rest pred))]
                 (into [(str "(" (str/join (if (= :and op) " AND " " OR ") (map first parts)) ")")]
                       (mapcat rest parts)))
    :not       (let [[s & ps] (where-sql type cols absent a)]
                 (into [(str "NOT " s)] ps))
    (if-let [col (get cols a)]
      (let [ref (column-ref type col)]
        (case op
          :nil? [(str ref (if b " IS NULL" " IS NOT NULL"))]
          :=    (cond (nil? b)             no
                      (comparable? col b)  [(str ref " = ?") b]
                      :else                yes)
          :in   (let [vs (remove nil? b)]
                  (cond (empty? vs)                       no
                        (every? #(comparable? col %)
                                vs)
                          (into [(str ref " IN (" (str/join ", " (repeat (count vs) "?")) ")")] vs)
                        :else                             yes))
          (:< :<= :> :>= :not=)
          (cond (nil? b)                       no
                (ordered-comparable? col b)    [(str ref " " (comparison-sql op) " ?") b]
                :else                          yes)
          :starts-with
          (cond (nil? b)                       [(str ref " IS NOT NULL")]
                (prefix-comparable? col b)     [(str ref " LIKE ? ESCAPE '\\'")
                                                (str (str/escape b like-escape) "%")]
                :else                          yes)
          yes))
      (if (absent pred) yes no))))

(defn comparison-judgements
  [sql-type]
  (let [kind    (:kind (ctype/parse (str sql-type)))
        exact?  (contains? exact-number-kinds kind)
        text?   (contains? text-kinds kind)
        nil-ok  {:nil :exact}]
    (cond-> {:=  (cond-> nil-ok exact? (assoc :exact-number :exact) text? (assoc :text :inexact))
             :in (cond-> nil-ok exact? (assoc :exact-number :exact) text? (assoc :text :inexact))}
      exact? (into (for [op [:< :<= :> :>= :not=]]
                     [op (assoc nil-ok :exact-number :exact)]))
      text?  (assoc :starts-with (assoc nil-ok :text :inexact)))))

(defn orderable-attrs
  [columns key]
  (let [ok (into #{} (comp (filter #(contains? exact-number-kinds (:kind (ctype/parse (:type %)))))
                           (map #(keyword (str/lower-case (:name %)))))
                 columns)]
    (cond-> ok
      (and (seq key) (every? ok key)) (conj :db/key))))

(defn- order-sql
  [type cols key order]
  (when (seq order)
    (when (empty? key)
      (refusal/refuse! :refusal.foreign/order-without-key {:order (vec order)}))
    (let [col-of  (fn [a] (or (some-> (get cols a) :name (->> (quote-ident type)))
                              (refusal/refuse! :refusal.foreign/order-unknown-column
                                               {:attribute a :order (vec order)})))
          terms   (fn [[a dir]]
                    (let [qs (if (= :db/key a) (mapv col-of key) [(col-of a)])
                          d  (if (= :desc dir) " DESC" " ASC")]
                      (for [q qs]
                        (str "CASE WHEN " q " IS NULL THEN " (if (= :desc dir) 0 1) " ELSE "
                             (if (= :desc dir) 1 0) " END ASC, " q d))))
          ordered (mapcat terms order)
          tie     (when-not (some #(= :db/key (first %)) order)
                    (map col-of key))]
      (str " ORDER BY " (str/join ", " (concat ordered (map #(str % " ASC") tie)))))))

(defn scan-stmt
  ([type table cols absent scan] (scan-stmt type table cols absent nil scan))
  ([type table cols absent key {:keys [where project order limit]}]
   (let [qt     (quote-ident type (policy/safe-ident table))
         picked (when project (keep #(get cols %) (sort project)))
         sel    (if (seq picked) (str/join ", " (map #(quote-ident type (:name %)) picked)) "*")
         conds  (map #(where-sql type cols absent %) where)
         top?   (and limit (= :top (:limit-style (dialect-of type))))]
     (into [(str "SELECT " (when top? (str "TOP " limit " ")) sel " FROM " qt
                 (when (seq conds) (str " WHERE " (str/join " AND " (map first conds))))
                 (order-sql type cols key order)
                 (when (and limit (not top?)) (str " LIMIT " limit)))]
           (mapcat rest conds)))))

(defn- cdc-select
  [cols deletion deleted-sql]
  (str/join ", " (concat (map #(let [q (quote-ident :mssql (policy/safe-ident (name %)))]
                                 (str q " AS " q))
                              (remove #{deletion} cols))
                         [(str deleted-sql " AS "
                               (quote-ident :mssql (policy/safe-ident (name deletion))))])))

(defn cdc-capable?
  [type]
  (= :mssql (keyword type)))

(defn cdc-instance
  [{:keys [schema table capture-instance]}]
  (policy/safe-ident (or capture-instance (str (or schema "dbo") "_" table))))

(defn cdc-bounds-stmt
  [instance]
  [(str "SELECT CONVERT(varchar(22), sys.fn_cdc_get_min_lsn(N'" (policy/safe-ident instance)
        "'), 1) AS min_lsn, CONVERT(varchar(22), sys.fn_cdc_get_max_lsn(), 1) AS max_lsn")])

(defn cdc-snapshot-stmt
  [{:keys [schema table]} cols deletion]
  [(str "SELECT " (cdc-select cols deletion "0")
        " FROM " (quote-ident :mssql (policy/safe-ident (or schema "dbo")))
        "." (quote-ident :mssql (policy/safe-ident table)))])

(defn cdc-changes-stmt
  [instance cols deletion from to]
  [(str "SELECT " (cdc-select cols deletion "CASE WHEN [__$operation] = 1 THEN 1 ELSE 0 END")
        " FROM cdc.[fn_cdc_get_all_changes_" (policy/safe-ident instance) "]"
        "(sys.fn_cdc_increment_lsn(CONVERT(binary(10), ?, 1)), CONVERT(binary(10), ?, 1), N'all')"
        " ORDER BY [__$start_lsn], [__$seqval]")
   from to])
