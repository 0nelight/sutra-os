(ns sutra.store.pg.dataset
  (:require [clojure.edn]
            [clojure.string :as str]
            [sutra.kit.anomaly :as anomaly]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.pg.ident :as ident]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.pg.sql :as sql]
            [sutra.kit.decisions :as decisions]
            [sutra.kit.sql :as kit-sql])
  (:import [java.io StringReader]
           [java.sql Connection PreparedStatement SQLException Types]
           [java.time LocalDate LocalDateTime]
           [javax.sql DataSource]
           [org.postgresql PGConnection]))

(set! *warn-on-reflection* true)

(defn- safe-ident?
  [s]
  (boolean (and s (re-matches #"[A-Za-z0-9_]+" (str s)))))

(defn- require-ident! [s what]
  (when-not (safe-ident? s)
    (anomaly/raise! :incorrect (str "unsafe dataset " what ": " (pr-str s))
                    {:name s :what what}))
  (ident/refuse-if-too-long! s (str "dataset " what)))

(defn- exec! [^Connection c ^String sql]
  (with-open [s (.createStatement c)] (.execute s sql)))

(defmacro in-one-snapshot
  [c & body]
  `(let [^Connection c# ~c
         prior# (.getAutoCommit c#)]
     (if-not prior#
       (do ~@body)
       (do (.setAutoCommit c# false)
           (try (let [r# (do ~@body)] (.commit c#) r#)
                (catch Throwable t#
                  (try (.rollback c#) (catch java.sql.SQLException _# nil))
                  (throw t#))
                (finally (try (.setAutoCommit c# true)
                              (catch java.sql.SQLException _# nil))))))))

(defn open
  ^Connection [^DataSource ds]
  (jdbc/get-connection ds))

(def ^:private default-search-path "\"public\", \"main\"")

(defn- release!
  [^Connection c]
  (try (with-open [s (.createStatement c)]
         (.execute s (str "SET search_path = " default-search-path)))
       (catch java.sql.SQLException _ nil))
  (.close c))

(defn with-store*
  [ds f]
  (let [^Connection c (open ds)]
    (try (f c) (finally (release! c)))))

(defmacro with-conn
  [[sym ds] & body]
  `(with-store* ~ds (fn [~(vary-meta sym assoc :tag 'java.sql.Connection)] ~@body)))

(def ^:private already-there
  #{"23505"
    "42P06"
    "42P07"})

(defn- create-if-absent!
  [^Connection c & stmts]
  (try
    (doseq [s stmts] (exec! c s))
    (catch SQLException e
      (when-not (already-there (.getSQLState e)) (throw e)))))

(defn current-snapshot
  [^Connection c]
  (-> (jdbc/execute-one!
       c ["SELECT public.sutra_watermark() AS s"]
       {:builder-fn rs/as-unqualified-lower-maps})
      :s))

(defn next-snapshot!
  [^Connection c]
  (-> (jdbc/execute-one! c ["SELECT public.sutra_next_version() AS v"]
                         {:builder-fn rs/as-unqualified-lower-maps})
      :v))

(defn history-name?
  [table]
  (let [t (str table)]
    (or (str/includes? t "__history") (some? (re-find #"_[0-9a-f]{6}__s\d+$" t)))))

(defn- qualified [schema table]
  (sql/qualified (require-ident! schema "schema") (require-ident! table "table")))

(defn- hist-of [schema table] (qualified schema (layout/history-of table)))

(defn- column-present?
  [^Connection c schema table col]
  (some? (jdbc/execute-one!
          c [(str "SELECT 1 FROM information_schema.columns"
                  " WHERE table_schema = ? AND table_name = ? AND column_name = ? LIMIT 1")
             (str schema) (str table) (str col)])))

(defn table-exists?
  [^Connection c schema table]
  (some? (jdbc/execute-one!
          c [(str "SELECT 1 FROM information_schema.tables"
                  " WHERE table_schema = ? AND table_name = ? LIMIT 1")
             (str schema) (str table)])))

(declare overlay-read-schema? own-of-read touched-sql tomb-of overlay-branch-namespace?
         read-schema-of-own)

(defn- require-sys-columns!
  [^Connection c schema table what]
  (when-not (column-present? c (or (own-of-read c schema table) schema) table layout/sys-from)
    (throw (ex-info (str what " needs the row-version columns, and " schema "."
                         table " does not carry them — it was created"
                         " without " layout/sys-from "/" layout/sys-to ", so its history"
                         " cannot be reconstructed")
                    {:schema schema :table table :what what :missing layout/sys-from}))))

(defn- has-history?
  [^Connection c schema table]
  (table-exists? c schema (layout/history-of table)))

(defn branch-of
  [s]
  (let [s (str s)]
    (cond (= "main" s)                                               "main"
          (or (str/starts-with? s "br_") (str/starts-with? s "bo_")) (subs s 3))))

(defn- relation-at
  [cat s table v]
  (if (or (= "main" (str s)) (str/starts-with? (str s) "br_"))
    (layout/relation cat (branch-of s) table {:rev v :root-rev v})
    (layout/table-at cat s table v)))

(defn- mark-terms
  [exists? schema table]
  [(str "(SELECT max(" (sql/dq layout/sys-from) ") FROM " (qualified schema table) ")")
   (if (exists? schema (layout/history-of table))
     (str "(SELECT max(" (sql/dq layout/sys-to) ") FROM " (hist-of schema table) ")")
     "0")])

(defn- overlay-mark-terms
  [exists? own table]
  (into (mark-terms exists? own table)
        cat
        [(when (exists? own (layout/tomb-of table))
           [(str "(SELECT max(" (sql/dq layout/sys-from) ") FROM " (tomb-of own table) ")")])
         (when (exists? own layout/detach-table)
           [(str "(SELECT max(" (sql/dq layout/sys-from) ") FROM "
                 (qualified own layout/detach-table)
                 " WHERE table_name = " (kit-sql/sq (str table)) ")")])
         (when (exists? "main" table)
           (mark-terms exists? "main" table))]))

(defn content-signature
  [^Connection c schema table]
  (when (table-exists? c schema table)
    (require-sys-columns! c schema table "content-signature")
    (let [exists? (fn [s t] (table-exists? c s t))
          terms   (if-let [own (own-of-read c schema table)]
                    (overlay-mark-terms exists? own table)
                    (mark-terms exists? schema table))]
      (some-> (jdbc/execute-one!
               c [(str "SELECT GREATEST(" (str/join ", " terms) ") AS m")]
               {:builder-fn rs/as-unqualified-lower-maps})
              :m str))))

(defn ever-written?
  [^Connection c schema table]
  (boolean
   (or (some? (jdbc/execute-one!
               c [(str "SELECT 1 FROM " (qualified schema table) " LIMIT 1")]))
       (and (has-history? c schema table)
            (some? (jdbc/execute-one!
                    c [(str "SELECT 1 FROM " (hist-of schema table) " LIMIT 1")]))))))

(defn had-rows-at?
  [^Connection c schema table v]
  (when (table-exists? c schema table)
    (require-sys-columns! c schema table "had-rows-at?")
    (some? (jdbc/execute-one!
            c (let [{rel :sql ps :params} (relation-at c schema table v)]
                (into [(str "SELECT 1 FROM " rel " s LIMIT 1")] ps))))))

(defn as-of
  [^Connection c schema table snapshot]
  (relation-at c schema table snapshot))

(defn- pk-set
  [^Connection c sql]
  (into #{}
        (map (comp str first vals))
        (jdbc/execute! c sql {:builder-fn rs/as-unqualified-lower-maps})))

(defn- detached-in?
  [^Connection c schema table low high]
  (when-let [own (own-of-read c schema table)]
    (and (table-exists? c own layout/detach-table)
         (some? (jdbc/execute-one!
                 c [(str "SELECT 1 FROM " (qualified own layout/detach-table)
                         " WHERE table_name = ? AND " (sql/dq layout/sys-from) " > ? AND "
                         (sql/dq layout/sys-from) " <= ? LIMIT 1")
                    (str table) (long low) (long high)])))))

(defn- differing-pks
  [^Connection c cat schema table pk low high]
  (let [cols (layout/data-columns cat (own-of-read c schema table) table)]
    (when (= (set cols) (set (layout/data-columns cat "main" table)))
      (let [{hi :sql hi-ps :params} (relation-at cat schema table high)
            {lo :sql lo-ps :params} (relation-at cat schema table low)
            differs (str/join " OR " (for [col cols :let [q (sql/dq col)]]
                                       (str "h." q "::text IS DISTINCT FROM l." q "::text")))]
        (pk-set c (-> [(str "SELECT h." pk " AS pk FROM " hi " h LEFT JOIN " lo " l ON l." pk
                            " = h." pk " WHERE l." pk " IS NULL OR " differs)]
                      (into hi-ps)
                      (into lo-ps)))))))

(defn table-delta
  [^Connection c {:keys [schema table pk-col low high]
                  :or   {schema "main"}}]
  (cond
    (not (and low high))
    {:no-delta :no-boundary}

    (> (long low) (long high))
    {:no-delta :inverted-range}

    (= (long low) (long high))
    {:removed #{} :added #{} :changed #{}}

    (> (long high) (long (current-snapshot c)))
    {:no-delta :feed-unavailable}

    (not (had-rows-at? c schema table low))
    {:no-delta :absent-at-low}

    :else
    (let [pk    (sql/dq (require-ident! pk-col "pk column"))
          _     (require-sys-columns! c schema table "table-delta")
          _     (when-not (column-present? c schema table pk-col)
                  (anomaly/raise! :incorrect
                                  (str "table-delta over " schema "."
                                       table " was asked to key on '"
                                       pk-col "', which that relation does not"
                                       " carry")
                                  {:schema schema :table table :pk-col pk-col}))
          cat       (layout/remembering-cat c)
          live-at   (fn [v] (let [{rel :sql ps :params} (relation-at cat schema table v)]
                              (pk-set c (into [(str "SELECT " pk " AS pk FROM " rel " s")] ps))))
          low-live  (live-at low)
          high-live (live-at high)
          touched   (or (when (detached-in? c schema table low high)
                          (differing-pks c cat schema table pk low high))
                        (pk-set c [(str "SELECT DISTINCT pk FROM ("
                                        (touched-sql c schema table pk low high) ") t")]))]
      {:removed (into #{} (remove high-live) low-live)
       :added   (into #{} (remove low-live) high-live)
       :changed (into #{} (filter high-live) touched)})))

(defn version-integrity
  [^Connection c schema table]
  (when (and (table-exists? c schema table) (has-history? c schema table))
    (when-let [k (layout/table-key c schema table)]
      (let [kq   (sql/dq k)
            both (str "(SELECT " kq " AS k, " (sql/dq layout/sys-from) " AS f, "
                      (sql/dq layout/sys-to) " AS t FROM " (qualified schema table)
                      " UNION ALL SELECT " kq ", " (sql/dq layout/sys-from) ", "
                      (sql/dq layout/sys-to) " FROM " (hist-of schema table) ") v")
            q    (str "WITH v AS (SELECT k, f, t,"
                      " LEAD(f) OVER (PARTITION BY k ORDER BY f, t NULLS LAST) AS nf"
                      " FROM " both ")"
                      " SELECT"
                      " count(*) FILTER (WHERE t IS NOT NULL AND nf IS NOT NULL AND nf < t) AS "
                      "overlap,"
                      " count(*) FILTER (WHERE t IS NOT NULL AND nf IS NOT NULL AND nf > t) AS gap,"
                      " count(*) FILTER (WHERE t IS NULL AND nf IS NOT NULL) AS orphan"
                      " FROM v")]
        (some-> (jdbc/execute-one! c [q] {:builder-fn rs/as-unqualified-lower-maps})
                (select-keys [:overlap :gap :orphan])
                (update-vals long))))))

(defn vacuum-health
  [^Connection c]
  (let [one (fn [q] (or (some-> (jdbc/execute-one! c [q] {:builder-fn rs/as-unqualified-lower-maps})
                                vals first)
                        0))]
    {:dead-ratio  (double (one "SELECT COALESCE(sum(n_dead_tup)::float8
                                 / NULLIF(sum(n_live_tup) + sum(n_dead_tup), 0), 0) AS r
                                FROM pg_stat_user_tables"))
     :oldest-xact
     (long (one "SELECT COALESCE(max(EXTRACT(EPOCH FROM (now() - xact_start))), 0)::bigint AS s
                              FROM pg_stat_activity WHERE xact_start IS NOT NULL"))
     :idle-in-txn (long (one "SELECT count(*) AS n FROM pg_stat_activity
                              WHERE state = 'idle in transaction'"))
     :stale-slots (long (one "SELECT count(*) AS n FROM pg_replication_slots
                              WHERE NOT active"))}))

(defn- branch-of!
  [s]
  (or (branch-of s)
      (anomaly/raise! :not-found (str "schema " (pr-str (str s)) " is no branch's")
                      {:schema (str s)})))

(defn- ddl!
  [^Connection c b t f]
  (in-one-snapshot
   c
   (let [ops (f (layout/catalog-for c b [(str t)]))]
     (when (seq ops)
       (layout/install! c)
       (layout/execute! c ops))
     (boolean (seq ops)))))

(def ^:private compact-bytes
  (* 256 1024 1024))

(defn- one-row [^Connection c q params]
  (jdbc/execute-one! c (into [q] params) {:builder-fn rs/as-unqualified-lower-maps}))

(defn- history-partitions
  [^Connection c schema h]
  (into []
        (keep (fn [{:keys [name bound bytes tuples]}]
                (when-let [[_ lo hi] (re-find #"FROM \('?(-?\d+)'?\) TO \('?(-?\d+)'?\)"
                                              (str bound))]
                  {:name (str name) :lo (parse-long lo) :hi (parse-long hi)
                   :bytes (long (or bytes 0)) :tuples (max 0 (long (or tuples 0)))})))
        (jdbc/execute! c [(str "SELECT p.relname AS name,"
                               " pg_get_expr(p.relpartbound, p.oid) AS bound,"
                               " pg_total_relation_size(p.oid) AS bytes,"
                               " p.reltuples::bigint AS tuples"
                               " FROM pg_inherits i JOIN pg_class p ON p.oid = i.inhrelid"
                               " WHERE i.inhparent = to_regclass(?)")
                          (qualified schema h)]
                       {:builder-fn rs/as-unqualified-lower-maps})))

(defn- update-count! [^Connection c ^String q]
  (with-open [st (.createStatement c)] (long (.executeUpdate st q))))

(defn- compact! [^Connection c rel target]
  (in-one-snapshot
   c
   (exec! c "SET LOCAL lock_timeout = '5s'")
   (exec! c (str "LOCK TABLE " rel " IN ACCESS EXCLUSIVE MODE"))
   (exec! c (str "CREATE TEMP TABLE _sutra_keep ON COMMIT DROP AS SELECT * FROM " rel
                 " WHERE " (sql/dq layout/sys-to) " > " (long target)))
   (let [before (:n (one-row c (str "SELECT count(*) AS n FROM " rel) []))]
     (exec! c (str "TRUNCATE " rel))
     (let [kept (update-count! c (str "INSERT INTO " rel " SELECT * FROM _sutra_keep"))]
       (- (long before) kept)))))

(defn- sweep-partition! [^Connection c schema {:keys [name lo hi bytes tuples]} target]
  (let [rel (qualified schema name)
        sys-to (sql/dq layout/sys-to)]
    (cond
      (> (long lo) (long target)) 0
      (<= (long hi) (inc (long target)))
      (in-one-snapshot c
                       (exec! c "SET LOCAL lock_timeout = '5s'")
                       (exec! c (str "DROP TABLE " rel))
                       tuples)
      (nil? (one-row c (str "SELECT 1 AS x FROM " rel " WHERE " sys-to " <= " (long target)
                            " LIMIT 1") []))
      0
      (> (long bytes) (long compact-bytes)) (compact! c rel target)
      :else (update-count! c (str "DELETE FROM " rel " WHERE " sys-to " <= " (long target))))))

(defn- sweep-segment! [^Connection c schema {:keys [seg rev]} target]
  (let [rel (qualified schema seg)]
    (if (<= (long rev) (long target))
      (let [n (:n (one-row c (str "SELECT count(*) AS n FROM " rel) []))]
        (exec! c (str "DROP TABLE " rel))
        (long n))
      (update-count! c (str "DELETE FROM " rel " WHERE " (sql/dq layout/sys-to) " <= "
                            (long target))))))

(defn past-before?
  [^Connection c schema table v]
  (boolean
   (or (seq (layout/sealed c schema (str table)))
       (and (has-history? c schema table)
            (some? (one-row c (str "SELECT 1 AS x FROM " (hist-of schema table) " WHERE "
                                   (sql/dq layout/sys-to) " < " (long v) " LIMIT 1")
                            []))))))

(defn sweep-history!
  [^Connection c schema table target]
  (reduce +
          0
          (for [rel   [(str table) (layout/tomb-of table)]
                :when (table-exists? c schema rel)
                n     (concat
                       (when (has-history? c schema rel)
                         (for [p (sort-by :lo (history-partitions c schema
                                                                  (layout/history-of rel)))]
                           (sweep-partition! c schema p target)))
                       (for [sg (layout/sealed c schema rel)]
                         (sweep-segment! c schema sg target)))]
            (long n))))

(defn create-table!
  [^Connection c schema table cols]
  (qualified schema table)
  (ddl! c (branch-of! schema) table #(layout/create-plan % (str schema) (str table) cols)))

(defn columns
  [^Connection c schema table]
  (into #{}
        (map (comp str :column_name))
        (jdbc/execute! c [(str "SELECT column_name FROM information_schema.columns"
                               " WHERE table_schema = ? AND table_name = ?")
                          (str schema) (str table)]
                       {:builder-fn rs/as-unqualified-lower-maps})))

(defn- data-columns
  [^Connection c schema table]
  (let [cs (->> (columns c schema table) (map str) (remove layout/sys?) sort vec)]
    (when (seq cs) cs)))

(defn projection
  ([^Connection c schema table] (projection c schema table nil))
  ([^Connection c schema table alias]
   (let [q (fn [col] (str (when alias (str (sql/dq alias) ".")) (sql/dq col)))]
     (if-let [cs (data-columns c schema table)]
       (str/join "," (map q cs))
       (if alias (str (sql/dq alias) ".*") "*")))))

(defn- alter!
  [^Connection c schema table clauses]
  (ddl! c (branch-of! schema) table #(layout/alter-plan % (str schema) (str table) clauses)))

(defn add-column!
  [^Connection c schema table col type]
  (alter! c schema table [[:add (str col) (layout/pg-type type)]]))

(defn rename-column!
  [^Connection c schema table from to]
  (alter! c schema table [[:rename (str from) (str to)]]))

(defn drop-column!
  [^Connection c schema table col]
  (when (contains? (columns c schema table) (str col))
    (alter! c schema table [[:drop (str col)]])))

(defn non-null-count
  [^Connection c schema table col]
  (long (or (:n (jdbc/execute-one!
                 c [(str "SELECT count(*) AS n FROM " (qualified schema table)
                         " WHERE " (sql/dq col) " IS NOT NULL")]
                 {:builder-fn rs/as-unqualified-lower-maps}))
            0)))

(defn column-types
  [^Connection c schema table]
  (into {}
        (map (juxt (comp str :column_name) (comp str :typ)))
        (jdbc/execute! c [(str "SELECT a.attname AS column_name,"
                               " format_type(a.atttypid, a.atttypmod) AS typ"
                               " FROM pg_attribute a"
                               " JOIN pg_class rel ON rel.oid = a.attrelid"
                               " JOIN pg_namespace n ON n.oid = rel.relnamespace"
                               " WHERE n.nspname = ? AND rel.relname = ?"
                               " AND a.attnum > 0 AND NOT a.attisdropped")
                          (str schema) (str table)]
                       {:builder-fn rs/as-unqualified-lower-maps})))

(def ^:private unmodified-family
  {"character varying" "character varying"
   "text"              "text"
   "numeric"           "numeric"})

(defn- family-of [t] (str/replace (str t) #"\s*\(.*\)\s*$" ""))

(defn- modifier-of [t]
  (some-> (re-find #"\(\s*(\d+)" (str t)) second parse-long))

(defn promotable?
  [from to]
  (let [f (layout/pg-type from) t (layout/pg-type to)
        ff (family-of f) tf (family-of t)]
    (boolean
     (or (= f t)
         (and (contains? unmodified-family ff)
              (or (and (= ff "character varying") (= tf "text"))
                  (= ff tf))
              (let [fm (modifier-of f) tm (modifier-of t)]
                (or (nil? tm)
                    (and (some? fm) (some? tm) (< (long fm) (long tm))))))))))

(defn retype-column!
  [^Connection c schema table col from to]
  (let [target (layout/pg-type to)
        stored (layout/pg-type from)]
    (when-not (= stored target)
      (alter! c schema table [[:retype (str col) target]])
      (if (promotable? stored target) :promoted :rewritten))))

(defn ensure-versioned!
  [^Connection c schema table]
  (ddl! c (branch-of! schema) table #(layout/version-plan % (str schema) (str table))))

(defn drop-table!
  [^Connection c schema table]
  (qualified schema table)
  (let [b (branch-of! schema)]
    (ddl! c b table #(layout/drop-plan % b (str table)))))

(defn ensure-table!
  [^Connection c branch table cols tag]
  (qualified (pgns/own-schema branch) table)
  (ddl! c branch table #(layout/table-plan % branch (str table) cols tag)))

(defn satisfied-by-main?
  [^Connection c branch table cols]
  (layout/satisfied-by-main? (layout/catalog-for c branch [(str table)]) branch (str table) cols))

(defn- coerce-param
  [v]
  (condp instance? v
    java.sql.Date        v
    java.sql.Timestamp   v
    java.sql.Time        v
    java.time.Instant    (java.sql.Timestamp/from ^java.time.Instant v)
    java.util.Date       (java.sql.Timestamp. (.getTime ^java.util.Date v))
    clojure.lang.BigInt  (.toBigInteger ^clojure.lang.BigInt v)
    clojure.lang.Ratio   (.decimalValue ^clojure.lang.Ratio v)
    clojure.lang.Keyword (subs (str v) 1)
    Character            (str v)
    v))

(def ^:private max-params-per-statement 4000)

(defn- bind-param!
  [^PreparedStatement ps ^long i v]
  (if (string? v)
    (.setObject ps (int i) v Types/OTHER)
    (.setObject ps (int i) v)))

(defn- insert-values!
  ([^Connection c target cols rows] (insert-values! c target cols rows nil))
  ([^Connection c target cols rows consts]
   (let [cols  (vec cols)
         ncols (count cols)
         carried (set (map str cols))
         consts (vec (remove (comp carried str first) consts))
         per   (max 1 (quot max-params-per-statement (max 1 ncols)))
         head  (str "INSERT INTO " target " ("
                    (str/join "," (concat (map sql/dq cols) (map (comp sql/dq first) consts)))
                    ") VALUES ")
         tuple (str "(" (str/join "," (concat (repeat ncols "?") (map second consts))) ")")]
     (doseq [chunk (partition-all per rows)]
       (let [sql (str head (str/join "," (repeat (count chunk) tuple)))]
         (with-open [ps (.prepareStatement c sql)]
           (let [i (volatile! 0)]
             (doseq [row chunk, col cols]
               (bind-param! ps (vswap! i inc) (coerce-param (get row col)))))
           (.execute ps))))
     (count rows))))

(def ^:private whole-number-types #{"smallint" "integer" "bigint"})
(def ^:private fractional-types #{"numeric" "real" "double precision"})

(defn- four-digit-year? [^long y] (<= 1 y 9999))

(defn- reads-the-same?
  [t v]
  (cond
    (nil? v)     true
    (string? v)  true
    (boolean? v) (= "boolean" t)
    (or (integer? v) (instance? BigInteger v))
    (boolean (or (whole-number-types t) (fractional-types t)))
    (or (float? v) (decimal? v)) (boolean (fractional-types t))
    (instance? LocalDate v)
    (and (= "date" t) (four-digit-year? (.getYear ^LocalDate v)))
    (instance? LocalDateTime v)
    (and (#{"date" "timestamp without time zone"} t)
         (four-digit-year? (.getYear ^LocalDateTime v))
         (zero? (mod (.getNano ^LocalDateTime v) 1000)))
    :else        false))

(defn copy-refusal
  [types cols rows]
  (let [typed-cols (mapv (fn [col]
                           (let [t (get types (str col))]
                             {:col col :col-type t :family (family-of t)}))
                         cols)]
    (first (for [row rows
                 {:keys [col col-type family]} typed-cols
                 :let [v (coerce-param (get row col))]
                 :when (not (reads-the-same? family v))]
             {:column (str col) :type col-type :class (.getName (class v))}))))

(defn copy-ready? [types cols rows] (nil? (copy-refusal types cols rows)))

(defn- copy-field
  [v]
  (if (nil? v)
    "\\N"
    (-> (str v)
        (str/replace "\\" "\\\\")
        (str/replace "\t" "\\t")
        (str/replace "\n" "\\n")
        (str/replace "\r" "\\r"))))

(defn copy-text
  [cols rows]
  (let [sb (StringBuilder.)]
    (doseq [row rows]
      (.append sb ^String (str/join "\t" (map #(copy-field (coerce-param (get row %))) cols)))
      (.append sb "\n"))
    (str sb)))

(def ^:dynamic *landing*
  nil)

(defn- note-landing! [path f & args]
  (when *landing* (apply swap! *landing* update-in path f args)))

(defmacro ^:private timed
  [step k & body]
  `(let [t0# (System/nanoTime)]
     (try ~@body
          (finally
            (note-landing! [~step ~k]
                           (fnil + 0) (quot (- (System/nanoTime) t0#) 1000000))))))

(defn- copy-in!
  [^Connection c step stage cols rows]
  (let [mgr  (.getCopyAPI ^PGConnection (.unwrap c PGConnection))
        text (timed step :encode-ms (copy-text cols rows))]
    (.copyIn mgr
             (str "COPY " (sql/dq stage) " (" (str/join "," (map sql/dq cols)) ") FROM STDIN")
             (StringReader. text))))

(defn- stage-rows!
  [^Connection c step schema table stage cols rows]
  (let [many?   (< (quot max-params-per-statement (max 1 (count cols))) (count rows))
        refusal (when many?
                  (timed step :decide-ms (copy-refusal (column-types c schema table) cols rows)))
        copy?   (and many? (nil? refusal))]
    (when refusal (note-landing! [step :copy-refused] (constantly refusal)))
    (note-landing! [step :modes] #(vec (distinct (conj (vec %) (if copy? "copy" "insert")))))
    (timed step :stage-ms
           (if copy?
             (copy-in! c step stage cols rows)
             (insert-values! c (sql/dq stage) cols rows)))))

(defn- next-version!
  [^Connection c] (long (next-snapshot! c)))

(defn- versioned?
  [^Connection c schema table]
  (column-present? c schema table layout/sys-from))

(defn- open-version
  [^Connection c schema table v]
  (when (versioned? c schema table)
    [[layout/sys-from (str v)] [layout/sys-to "NULL"]]))

(defn- missing-from-stage
  [^String tgt ^String pk ^String stage]
  (str "NOT EXISTS (SELECT 1 FROM " (sql/dq stage) " AS _s WHERE _s." pk " = " tgt "." pk ")"))

(defn- retire!
  [^Connection c schema table ^String where v]
  (in-one-snapshot c (first (layout/execute! c [[:displace schema table where v]]))))

(defn insert-rows!
  [^Connection c schema table cols rows]
  (if (empty? rows)
    0
    (insert-values! c (qualified schema table) cols rows
                    (open-version c schema table (next-version! c)))))

(defn- unique-suffix [] (str/replace (str (random-uuid)) "-" ""))

(defn- empty-like-sql
  [temp tgt]
  (str "CREATE TEMP TABLE " (sql/dq temp) " AS SELECT * FROM " tgt " LIMIT 0"))

(defn- drop-temp! [^Connection c temp] (exec! c (str "DROP TABLE IF EXISTS " (sql/dq temp))))

(defn- differs-sql
  [pk-col cols]
  (let [value-cols (remove #(= (str %) (str pk-col)) cols)]
    (if (seq value-cols)
      (str/join " OR " (map #(str "t." (sql/dq %) " IS DISTINCT FROM s." (sql/dq %)) value-cols))
      "false")))

(defn- changed-rows-sql
  [select {:keys [stage src pk pk-col cols]}]
  (str select " FROM " (sql/dq stage) " AS s"
       " LEFT JOIN " src " AS t ON t." pk " = s." pk
       " WHERE t." pk " IS NULL OR (" (differs-sql pk-col cols) ")"))

(defn- count-of
  [^Connection c q]
  (long (or (first (vals (jdbc/execute-one! c [q]))) 0)))

(defn rows-differ?
  [^Connection c schema table pk-col cols rows]
  (let [cols  (vec cols)
        tgt   (qualified schema table)
        probe (str "_probe_" (unique-suffix))]
    (exec! c (empty-like-sql probe tgt))
    (try
      (stage-rows! c :probe schema table probe cols rows)
      (pos? (timed :probe :diff-ms
                   (count-of c (changed-rows-sql "SELECT count(*) AS n"
                                                 {:stage probe :src tgt :cols cols
                                                  :pk-col pk-col
                                                  :pk (sql/dq (require-ident! pk-col
                                                                              "pk column"))}))))
      (finally (drop-temp! c probe)))))

(defn- post-merge-columns-sql
  [all-cols cols]
  (let [carried (set (map str cols))]
    (->> (remove layout/sys? all-cols)
         (map (fn [col]
                (str (if (carried (str col)) "s." "t.") (sql/dq col) " AS " (sql/dq col))))
         (str/join ", "))))

(defn plan-merge
  [{:keys [tgt src pk pk-col cols all-cols uniq full-refresh?]}]
  (let [stage (str "_stage_" uniq)
        delta (str "_delta_" uniq)
        gone  (missing-from-stage tgt pk stage)]
    {:stage  stage
     :delta  delta
     :gone   gone
     :create [[:exec (empty-like-sql stage tgt)]]
     :derive [[:exec (str "CREATE TEMP TABLE " (sql/dq delta) " AS "
                          (changed-rows-sql
                           (str "SELECT " (post-merge-columns-sql all-cols cols))
                           {:stage stage :src src :pk pk :pk-col pk-col :cols cols}))]]
     :counts (cond-> {:changed (str "SELECT count(*) AS n FROM " (sql/dq delta))}
               full-refresh? (assoc :went (str "SELECT count(*) AS n FROM " tgt " WHERE " gone)))
     :drop   [delta stage]}))

(defn merge-write-ops
  [{:keys [schema table tgt pk all-cols delta gone]} {:keys [changed went v lits]}]
  (let [sel (->> all-cols
                 (map (fn [col] (or (get lits (str col)) (sql/dq col))))
                 (str/join ","))
        hit (str pk " IN (SELECT " pk " FROM " (sql/dq delta) ")")]
    (cond-> []
      (pos? changed) (conj [:displace schema table hit v]
                           [:exec (str "INSERT INTO " tgt " ("
                                       (str/join "," (map sql/dq all-cols)) ")"
                                       " SELECT " sel " FROM " (sql/dq delta))])
      (pos? went)    (conj [:displace schema table gone v]))))

(defn merge-rows!
  [^Connection c schema table pk-col cols rows {:keys [full-refresh? read-scope]}]
  (let [cols     (vec cols)
        pk       (sql/dq (require-ident! pk-col "pk column"))
        tgt      (qualified schema table)
        all-cols (vec (sort (columns c schema table)))
        {:keys [stage delta gone counts] :as plan}
        (decisions/decide
         :build/merge
         {:tgt tgt :src (qualified (or read-scope schema) table) :pk pk
          :pk-col pk-col :cols cols :all-cols all-cols :uniq (unique-suffix)
          :full-refresh? full-refresh?})]
    (layout/execute! c (:create plan))
    (try
      (stage-rows! c :merge schema table stage cols rows)
      (let [[changed went]
            (timed :merge :diff-ms
                   (layout/execute! c (:derive plan))
                   [(count-of c (:changed counts))
                    (if-let [q (:went counts)] (count-of c q) 0)])]
        (note-landing! [:merge :changed] (fnil + 0) changed)
        (note-landing! [:merge :went] (fnil + 0) went)
        (when (or (pos? changed) (pos? went))
          (timed :merge :write-ms
                 (let [v    (next-version! c)
                       lits (into {} (open-version c schema table v))]
                   (in-one-snapshot
                    c
                    (layout/execute!
                     c (merge-write-ops {:schema schema :table table :tgt tgt :pk pk
                                         :all-cols all-cols :delta delta :gone gone}
                                        {:changed changed :went went :v v :lits lits})))))))
      (finally (run! #(drop-temp! c %) (:drop plan))))))

(defn retain-pks!
  [^Connection c schema table pk-col pks]
  (let [pk    (sql/dq (require-ident! pk-col "pk column"))
        stage (str "_keep_" (str/replace (str (random-uuid)) "-" ""))]
    (exec! c (str "CREATE TEMP TABLE " (sql/dq stage) " (" pk " VARCHAR)"))
    (try
      (with-open [ps (.prepareStatement c (str "INSERT INTO " (sql/dq stage) " VALUES (?)"))]
        (doseq [p pks]
          (.setObject ps 1 (str p))
          (.addBatch ps))
        (.executeBatch ps))
      (retire! c schema table
               (missing-from-stage (qualified schema table) pk stage)
               (next-version! c))
      (finally
        (exec! c (str "DROP TABLE IF EXISTS " (sql/dq stage)))))))

(defn delete-pks!
  [^Connection c schema table pk-col pks]
  (let [pk (sql/dq (require-ident! pk-col "pk column"))
        _  (qualified schema table)
        v  (next-version! c)]
    (reduce (fn [n chunk]
              (+ n
                 (long (retire! c schema table
                                (str pk " IN (" (str/join "," (map kit-sql/sq chunk)) ")") v))))
            0
            (partition-all 1000 (map str pks)))))

(defn- restore-sidecar!
  [^Connection c rel where snapshot v]
  (exec! c (str "UPDATE " rel " SET " (sql/dq layout/sys-to) " = " (long v)
                " WHERE " where (sql/dq layout/sys-to) " IS NULL"
                " AND " (sql/dq layout/sys-from) " > " (long snapshot)))
  (exec! c (str "UPDATE " rel " SET " (sql/dq layout/sys-to) " = NULL"
                " WHERE " where (sql/dq layout/sys-from) " <= " (long snapshot)
                " AND " (sql/dq layout/sys-to) " > " (long snapshot))))

(defn- restore-rows!
  [^Connection c schema table snapshot v]
  (let [tgt  (qualified schema table)
        cols (->> (columns c schema table)
                  (remove #{layout/sys-from layout/sys-to})
                  sort
                  vec)
        cl   (str/join "," (map sql/dq cols))
        {src :sql ps :params} (layout/table-at c schema table snapshot)]
    (layout/execute! c [[:displace schema table "TRUE" v]
                        [:update
                         (str "INSERT INTO " tgt " (" cl "," (sql/dq layout/sys-from) ","
                              (sql/dq layout/sys-to) ")"
                              " SELECT " cl "," v ",NULL FROM " src " q")
                         ps]])))

(defn- restore-to-at!
  [^Connection c schema table snapshot v]
  (let [tgt (qualified schema table)]
    (in-one-snapshot
     c
     (restore-rows! c schema table snapshot v)
     (when (overlay-branch-namespace? schema)
       (when (table-exists? c schema (layout/tomb-of table))
         (restore-rows! c schema (layout/tomb-of table) snapshot v))
       (when (table-exists? c schema layout/detach-table)
         (restore-sidecar! c (qualified schema layout/detach-table)
                           (str "table_name = " (kit-sql/sq (str table)) " AND ")
                           snapshot v))))
    (let [rs  (read-schema-of-own schema)
          rel (if (and rs (table-exists? c rs table)) (qualified rs table) tgt)]
      (long (or (first (vals (jdbc/execute-one!
                              c [(str "SELECT count(*) AS n FROM " rel)])))
                0)))))

(defn restore-to!
  [^Connection c schema table snapshot]
  (in-one-snapshot c (restore-to-at! c schema table snapshot (next-version! c))))

(defn expire-through!
  [^Connection c schema table through]
  (retire! c schema table (str (sql/dq layout/sys-from) " <= " (long through)) (next-version! c)))

(defn- tomb-of [schema table] (qualified schema (layout/tomb-of table)))

(defn- overlay-branch-namespace?
  [s] (str/starts-with? (str s) "bo_"))

(defn- read-schema-of-own
  [own] (when (overlay-branch-namespace? own) (str "br_" (subs (str own) 3))))

(defn overlay-read-schema?
  [s] (str/starts-with? (str s) "br_"))

(defn- own-of-read
  [^Connection c schema table]
  (when-let [own (when (overlay-read-schema? schema) (pgns/own-schema (branch-of schema)))]
    (when (table-exists? c own (str table)) own)))

(defn- touched-sql
  [^Connection c schema table pk low high]
  (let [win  (fn [rel] (str "SELECT " pk " AS pk FROM " rel
                            " WHERE " (sql/dq layout/sys-from) " > " (long low)
                            " AND " (sql/dq layout/sys-from) " <= " (long high)))
        arms (fn [sch]
               (cons (win (qualified sch table))
                     (when (has-history? c sch table) [(win (hist-of sch table))])))]
    (str/join " UNION ALL "
              (if-let [own (own-of-read c schema table)]
                (concat (arms own)
                        (when (table-exists? c "main" table) (arms "main")))
                (arms schema)))))

(defn rebuild-overlay-view!
  [^Connection c branch table]
  (let [own (pgns/own-schema branch) rs (pgns/read-schema branch) t (str table)]
    (when (and (not (pgns/main? branch)) (table-exists? c own t))
      (when-not (= (sort (layout/data-columns c own t)) (map :name (layout/columns c rs t)))
        (ddl! c branch t #(layout/view-plan % branch t)))
      true)))

(defn detach!
  [^Connection c own table]
  (when (overlay-branch-namespace? own)
    (qualified own table)
    (let [b (branch-of own) v (next-version! c)]
      (ddl! c b table #(layout/detach-plan % b v (str table) false)))
    true))

(defn tomb!
  [^Connection c own table pks]
  (when (and (overlay-branch-namespace? own) (seq pks))
    (qualified own table)
    (in-one-snapshot
     c
     (layout/stamp! c (next-version! c))
     (layout/tomb! c (branch-of own) (str table) pks))
    true))

(defn create-branch!
  [^Connection c branch]
  (exec! c (str "CREATE SCHEMA IF NOT EXISTS " (sql/dq (pgns/read-schema branch))))
  (let [own (pgns/own-schema branch)]
    (when-not (= "main" own)
      (exec! c (str "CREATE SCHEMA IF NOT EXISTS " (sql/dq own))))))

(defn drop-branch!
  [^Connection c branch]
  (let [s (pgns/read-schema branch) own (pgns/own-schema branch)]
    (when-not (= "main" s)
      (exec! c (str "DROP SCHEMA IF EXISTS " (sql/dq s) " CASCADE"))
      (exec! c (str "DROP SCHEMA IF EXISTS " (sql/dq own) " CASCADE")))))

(defn- schema-names
  [^Connection c]
  (into #{}
        (map (comp str :schema_name))
        (jdbc/execute! c ["SELECT schema_name FROM information_schema.schemata"]
                       {:builder-fn rs/as-unqualified-lower-maps})))

(defn list-branches
  [^Connection c]
  (->> (schema-names c)
       (remove #(or (str/starts-with? % "pg_")
                    (= "information_schema" %)
                    (overlay-branch-namespace? %)))
       sort
       vec))

(defn tables-visible
  [^Connection c branch]
  (let [s (pgns/read-schema branch)]
    (into #{}
          (comp (map (comp str :table_name)) (remove history-name?))
          (jdbc/execute! c [(str "SELECT table_name FROM information_schema.tables"
                                 " WHERE table_schema IN (?, 'main')") s]
                         {:builder-fn rs/as-unqualified-lower-maps}))))

(defn tables-own
  [^Connection c branch]
  (let [s (pgns/read-schema branch)]
    (into #{}
          (comp (map (comp str :table_name)) (remove history-name?))
          (jdbc/execute! c [(str "SELECT table_name FROM information_schema.tables"
                                 " WHERE table_schema = ?") s]
                         {:builder-fn rs/as-unqualified-lower-maps}))))

(defn resolve-schema
  [^Connection c branch table]
  (let [s (pgns/read-schema branch)]
    (if (and (not= "main" s) (table-exists? c s table)) s "main")))

(defn fork-table!
  [^Connection c branch table]
  (let [t (str table)]
    (boolean
     (when (and (not (pgns/main? branch))
                (not (table-exists? c (pgns/own-schema branch) t))
                (table-exists? c "main" t))
       (ddl! c branch t #(layout/own-plan % branch t))))))

(defn- ensure-backups!
  [^Connection c]
  (create-if-absent!
   c
   "CREATE SCHEMA IF NOT EXISTS public"
   (str "CREATE TABLE IF NOT EXISTS public.store_backups ("
        " id BIGSERIAL PRIMARY KEY,"
        " label TEXT NOT NULL,"
        " snapshot BIGINT NOT NULL,"
        " created_at TIMESTAMPTZ NOT NULL DEFAULT now())"))
  (when-not (column-present? c "public" "store_backups" "tables")
    (exec! c "ALTER TABLE public.store_backups ADD COLUMN IF NOT EXISTS tables TEXT")))

(declare versioned-tables)

(defn backup!
  [^Connection c label]
  (ensure-backups! c)
  (in-one-snapshot
   c
   (-> (jdbc/execute-one!
        c ["INSERT INTO public.store_backups (label, snapshot, tables) VALUES (?, ?, ?)
            RETURNING id, label, snapshot, created_at"
           (str label) (long (current-snapshot c)) (pr-str (versioned-tables c))]
        {:builder-fn rs/as-unqualified-kebab-maps}))))

(defn- manifest
  [^Connection c snapshot]
  (ensure-backups! c)
  (some-> (jdbc/execute-one!
           c ["SELECT tables FROM public.store_backups WHERE snapshot = ? AND tables IS NOT NULL
               ORDER BY id DESC LIMIT 1" (long snapshot)]
           {:builder-fn rs/as-unqualified-lower-maps})
          :tables
          clojure.edn/read-string))

(defn backups
  [^Connection c]
  (ensure-backups! c)
  (vec (jdbc/execute!
        c ["SELECT id, label, snapshot, created_at FROM public.store_backups ORDER BY id DESC"]
        {:builder-fn rs/as-unqualified-kebab-maps})))

(defn versioned-tables
  [^Connection c]
  (mapv (juxt (comp str :s) (comp str :t) (comp str :tag))
        (jdbc/execute!
         c
         [(str "SELECT n.nspname AS s, cl.relname AS t, d.description AS tag FROM pg_class cl"
               " JOIN pg_namespace n ON n.oid = cl.relnamespace"
               " JOIN pg_description d ON d.objoid = cl.oid AND d.objsubid = 0"
               " JOIN pg_attribute a ON a.attrelid = cl.oid AND a.attname = ? AND NOT "
               "a.attisdropped"
               " WHERE cl.relkind IN ('r', 'p') AND NOT cl.relispartition"
               " AND d.description IN ('sutra:datasets', 'sutra:scratch')"
               " ORDER BY 1, 2")
          layout/sys-from]
         {:builder-fn rs/as-unqualified-lower-maps})))

(defn- recreate-from-segment!
  [^Connection c schema table tag seg snapshot v]
  (let [cols (vec (remove (comp layout/sys? :name) (layout/columns c schema seg)))
        cl   (str/join "," (map (comp sql/dq :name) cols))]
    (create-table! c schema table cols)
    (exec! c (str "COMMENT ON TABLE " (qualified schema table) " IS " (kit-sql/sq tag)))
    (exec! c (str "INSERT INTO " (qualified schema table) " (" cl "," (sql/dq layout/sys-from) ","
                  (sql/dq layout/sys-to) ") SELECT " cl "," (long v) ",NULL FROM "
                  (qualified schema seg)
                  " q WHERE coalesce(q." (sql/dq layout/sys-from) ", 0) <= " (long snapshot)
                  " AND q." (sql/dq layout/sys-to) " > " (long snapshot)))
    (long (or (first (vals (jdbc/execute-one! c
                                              [(str "SELECT count(*) AS n FROM "
                                                    (qualified schema table))])))
              0))))

(defn restore-store-to!
  ([^Connection c snapshot] (restore-store-to! c snapshot nil))
  ([^Connection c snapshot scopes]
   (let [want  (when (seq scopes) (set (map str scopes)))
         scoped #(cond->> % want (filter (fn [[s _]] (contains? want s))))
         now   (scoped (versioned-tables c))
         here  (into #{} (map (fn [[s t]] [s t])) now)
         gone  (for [[s t tag] (scoped (manifest c snapshot))
                     :when (not (here [s t]))]
                 [s t (if (str/blank? tag) "sutra:datasets" tag)
                  (:seg (layout/sealed-at c s t snapshot))])]
     (when-let [lost (seq (filter (comp nil? last) gone))]
       (anomaly/raise! :conflict
                       (str "restore point " snapshot " covers table(s) whose history is gone: "
                            (str/join ", " (map (fn [[s t]] (str s "." t)) lost)))
                       {:error :as-of-expired :snapshot snapshot
                        :tables (mapv (fn [[s t]] (str s "." t)) lost)}))
     (in-one-snapshot
      c
      (let [v (next-version! c)]
        (as-> {} acc
          (reduce (fn [acc [schema table]]
                    (assoc acc (str schema "." table)
                           (restore-to-at! c schema table snapshot v)))
                  acc now)
          (reduce (fn [acc [schema table tag seg]]
                    (assoc acc (str schema "." table)
                           (recreate-from-segment! c schema table tag seg snapshot v)))
                  acc gone)))))))
