(ns sutra.store.pg.layout
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [sutra.types.column :as column]
            [sutra.store.pg.ident :as ident]
            [sutra.store.pg.ns :refer [main? read-schema own-schema]]
            [sutra.store.pg.sql :as sql]
            [sutra.store.memo :as memo]
            [sutra.store.core.orakel :as orakel]
            [sutra.types.coltype :as ctype]
            [sutra.types.value-types :as vt]
            [sutra.kit.sql :as kit-sql])
  (:import (java.sql Connection)))

(set! *warn-on-reflection* true)

(def pk "internal_uuid")
(def link-pk "id")
(def markings "_markings")
(def sys-from "sys_from")
(def sys-to "sys_to")
(def sys? #{sys-from sys-to})
(def detach-table "_detached")

(declare relation)

(defn object-table [T] (str "d_" (column/sql-name T)))
(defn link-table [L] (str "c_" (column/sql-name L)))
(defn key-of [table] (if (str/starts-with? (str table) "c_") link-pk pk))

(defn history-of [t] (ident/fit (str t "__history")))
(defn tomb-of [t] (ident/fit (str t "__tomb")))
(defn column [attr] (column/sql-name attr))

(def ^:private json-heads #{:array :struct :vector})

(defn json-attribute? [value-type]
  (boolean (and (vector? value-type) (json-heads (first value-type)))))

(defn- col-type [value-type]
  (or (when (and (vector? value-type) (= :decimal (first value-type)))
        (let [[_ p s] value-type] (ctype/col-type :decimal {:precision p :scale s})))
      (when (json-attribute? value-type) (ctype/col-type :json))
      (vt/coltype-of value-type)
      (anomaly/raise! :incorrect
                      (str "no column type for value type " (pr-str value-type))
                      {:value-type value-type})))

(defn pg-type
  [t]
  (if-let [v (ctype/parse t)] (ctype/render v :postgres) (str t)))

(defn- store-type [value-type] (ctype/render (ctype/for-store (col-type value-type)) :postgres))

(defn attrs
  [sig T]
  (or (get-in sig [:types T :attrs]) (get-in sig [:interfaces T :props])))

(def codepoint
  "C")

(defn- text-type? [t] (contains? #{:text :varchar} (:kind (ctype/parse t))))

(defn- by-codepoint [cols]
  (mapv #(cond-> % (text-type? (:type %)) (assoc :collation codepoint)) cols))

(defn- object-columns
  [sig T]
  (-> [{:name pk :type "character varying" :not-null? true}]
      (into (comp (remove #(= pk (column (:name %))))
                  (map (fn [{a :name t :type}]
                         (cond-> {:name (column a) :type (store-type t)}
                           (= :decimal t) (assoc :any-width? true)))))
            (attrs sig T))
      (conj {:name markings :type "character varying"})
      by-codepoint))

(defn fk-link? [sig L] (some? (get-in sig [:links L :fk])))

(defn link-columns
  [_sig _L]
  (by-codepoint [{:name link-pk :type "character varying" :not-null? true}
                 {:name "from_id" :type "character varying" :not-null? true}
                 {:name "to_id" :type "character varying" :not-null? true}
                 {:name markings :type "character varying"}]))

(defn- stored-link-types
  [sig]
  (remove #(fk-link? sig %) (keys (:links sig))))

(defn- tables
  [^Connection c schemas]
  (reduce (fn [m {:keys [table_schema table_name]}]
            (update m table_schema (fnil conj #{}) table_name))
          (zipmap schemas (repeat #{}))
          (sql/rows c (str "SELECT table_schema, table_name FROM information_schema.tables"
                           " WHERE table_schema IN (" (sql/placeholders (count schemas)) ")")
                    (vec schemas))))

(defprotocol Catalog
  (table? [cat s t])
  (columns [cat s t])
  (sealed [cat s t]))

(defn- schema? [c s]
  (some? (sql/one c "SELECT 1 AS x FROM information_schema.schemata WHERE schema_name = ?" [s])))

(def ^:private coll-sql
  [", a.attnotnull AS nn, co.collname AS coll"
   " LEFT JOIN pg_collation co ON co.oid = a.attcollation AND a.attcollation <> 100"])

(defn- column-entry [{:keys [attname typ nn coll]}]
  (cond-> {:name attname :type typ :not-null? (boolean nn)} coll (assoc :collation coll)))

(defn- live-columns
  [^Connection c s t]
  (mapv column-entry
        (sql/rows c
                  (str "SELECT a.attname, format_type(a.atttypid, a.atttypmod) AS typ"
                       (first coll-sql)
                       " FROM pg_attribute a" (second coll-sql)
                       " JOIN pg_class r ON r.oid = a.attrelid"
                       " JOIN pg_namespace n ON n.oid = r.relnamespace"
                       " WHERE n.nspname = ? AND r.relname = ? AND a.attnum > 0 AND NOT "
                       "a.attisdropped"
                       " ORDER BY a.attnum")
                  [s (str t)])))

(def ^:private sealed-mark "sutra:sealed ")

(defn- live-sealed
  [^Connection c s t]
  (->> (sql/rows c
                 (str "SELECT cl.relname AS seg, d.description AS tag FROM pg_class cl"
                      " JOIN pg_namespace n ON n.oid = cl.relnamespace"
                      " JOIN pg_description d ON d.objoid = cl.oid AND d.objsubid = 0"
                      " WHERE n.nspname = ? AND d.description = ? || ' ' || "
                      "split_part(d.description, ' ', 3)")
                 [s (str sealed-mark t)])
       (keep (fn [{:keys [seg tag]}]
               (when-let [r (parse-long (subs tag (inc (str/last-index-of tag " "))))]
                 {:seg seg :rev r})))
       (sort-by :rev)
       vec))

(extend-protocol Catalog
  Connection
  (table? [c s t] (contains? (get (tables c [s]) s) (str t)))
  (columns [c s t] (live-columns c s t))
  (sealed [c s t] (live-sealed c s (str t))))

(defn remembering-cat
  [cat]
  (let [table-held? (memoize (fn [s t] (table? cat s t)))
        columns-of  (memoize (fn [s t] (columns cat s t)))
        sealed-of   (memoize (fn [s t] (sealed cat s t)))]
    (reify Catalog
      (table? [_ s t] (table-held? s (str t)))
      (columns [_ s t] (columns-of s (str t)))
      (sealed [_ s t] (sealed-of s (str t))))))

(defn data-columns
  [cat s t]
  (into [] (comp (map :name) (remove sys?)) (columns cat s t)))

(def ^:private retention-tables-sql
  [(str "CREATE TABLE IF NOT EXISTS public.dataset_transactions ("
        "schema_name TEXT NOT NULL, table_name TEXT NOT NULL, version BIGINT NOT NULL,"
        " kind VARCHAR(16) NOT NULL, rows BIGINT,"
        " committed_at TIMESTAMPTZ NOT NULL DEFAULT now(),"
        " PRIMARY KEY (schema_name, table_name, version))")
   (str "CREATE INDEX IF NOT EXISTS dataset_transactions_at_idx"
        " ON public.dataset_transactions (committed_at)")
   (str "CREATE TABLE IF NOT EXISTS public.dataset_retention_marks ("
        "schema_name TEXT NOT NULL, table_name TEXT NOT NULL, horizon BIGINT NOT NULL,"
        " marked_at TIMESTAMPTZ NOT NULL DEFAULT now(), swept_at TIMESTAMPTZ,"
        " PRIMARY KEY (schema_name, table_name, horizon))")])

(def ^:private partition-span 100000)

(def ^:private version-key-base 1509949440)

(def ^:private version-functions
  [(str "CREATE OR REPLACE FUNCTION public.sutra_next_version() RETURNS bigint
    LANGUAGE plpgsql AS $fn$
    DECLARE v BIGINT;
    BEGIN
      PERFORM pg_advisory_lock(" (dec version-key-base) ", 0);
      BEGIN
        v := nextval('public.dataset_seq');
        PERFORM pg_advisory_xact_lock((" version-key-base " + (v >> 31))::int,
                                     (v & 2147483647)::int);
      EXCEPTION WHEN others THEN
        PERFORM pg_advisory_unlock(" (dec version-key-base) ", 0);
        RAISE;
      END;
      PERFORM pg_advisory_unlock(" (dec version-key-base) ", 0);
      RETURN v;
    END $fn$")
   (str "CREATE OR REPLACE FUNCTION public.sutra_watermark() RETURNS bigint
    LANGUAGE plpgsql AS $fn$
    DECLARE l BIGINT; m BIGINT;
    BEGIN
      PERFORM pg_advisory_lock_shared(" (dec version-key-base) ", 0);
      BEGIN
        SELECT CASE WHEN is_called THEN last_value ELSE 0 END INTO l FROM public.dataset_seq;
        SELECT min(((classid::bigint - " version-key-base ") << 31) | objid::bigint) INTO m
          FROM pg_locks
         WHERE locktype = 'advisory' AND objsubid = 2 AND granted
           AND database = (SELECT oid FROM pg_database WHERE datname = current_database())
           AND classid >= " version-key-base " AND classid < " (+ version-key-base 16777216) ";
      EXCEPTION WHEN others THEN
        PERFORM pg_advisory_unlock_shared(" (dec version-key-base) ", 0);
        RAISE;
      END;
      PERFORM pg_advisory_unlock_shared(" (dec version-key-base) ", 0);
      RETURN CASE WHEN m IS NULL THEN l ELSE least(l, m - 1) END;
    END $fn$")])

(def ^:private functions
  ["CREATE OR REPLACE FUNCTION public.sutra_history_partition(s TEXT, h TEXT, p TEXT,
                                                              lo BIGINT, hi BIGINT)
    RETURNS void LANGUAGE plpgsql AS $fn$
    BEGIN
      EXECUTE format('CREATE TABLE IF NOT EXISTS %I.%I PARTITION OF %I.%I '
                     || 'FOR VALUES FROM (%s) TO (%s)', s, p, s, h, lo, hi);
    EXCEPTION WHEN invalid_object_definition THEN
      -- a partition of an earlier span already covers these versions
      NULL;
    END $fn$"
   (str "CREATE OR REPLACE FUNCTION public.sutra_stmt_version() RETURNS trigger
    LANGUAGE plpgsql AS $fn$
    DECLARE v BIGINT; lo BIGINT; span BIGINT := " partition-span ";
    BEGIN
      IF coalesce(current_setting('sutra.v', true), '') <> '' THEN
        v := current_setting('sutra.v')::bigint;
      ELSE
        v := public.sutra_next_version();
        PERFORM set_config('sutra.v', v::text, true);
      END IF;
      IF TG_OP IN ('UPDATE', 'DELETE') THEN
        lo := (v / span) * span;
        PERFORM public.sutra_history_partition(TG_TABLE_SCHEMA, TG_TABLE_NAME || '__history',
                                               TG_TABLE_NAME || '__history_p' || lo,
                                               lo, lo + span);
      END IF;
      RETURN NULL;
    END $fn$")
   "CREATE OR REPLACE FUNCTION public.sutra_displace() RETURNS trigger
    LANGUAGE plpgsql AS $fn$
    DECLARE v TEXT := coalesce(current_setting('sutra.v', true), '');
    BEGIN
      IF v = '' THEN
        RETURN CASE WHEN TG_OP = 'DELETE' THEN OLD ELSE NEW END;
      END IF;
      IF TG_OP = 'INSERT' THEN
        IF NEW.sys_from IS NULL THEN NEW.sys_from := v::bigint; END IF;
        RETURN NEW;
      END IF;
      IF current_setting('sutra.conductor', true) = 'on' THEN
        RETURN CASE WHEN TG_OP = 'DELETE' THEN OLD ELSE NEW END;
      END IF;
      -- a row written earlier in this version already left its prior state at sys_to = v;
      -- a second write in the same version is an intermediate state, never history
      IF OLD.sys_from IS DISTINCT FROM v::bigint THEN
        EXECUTE format('INSERT INTO %I.%I SELECT * FROM jsonb_populate_record(NULL::%I.%I, $1)',
                       TG_TABLE_SCHEMA, TG_TABLE_NAME || '__history',
                       TG_TABLE_SCHEMA, TG_TABLE_NAME)
          USING to_jsonb(OLD) || jsonb_build_object('sys_to', v::bigint);
      END IF;
      IF TG_OP = 'UPDATE' THEN
        NEW.sys_from := v::bigint;
        RETURN NEW;
      END IF;
      RETURN OLD;
    END $fn$"
   "CREATE OR REPLACE FUNCTION public.sutra_seal(s TEXT, t TEXT, h TEXT) RETURNS TEXT
    LANGUAGE plpgsql AS $fn$
    DECLARE v BIGINT; seg TEXT; cols TEXT; allc TEXT;
    BEGIN
      IF to_regclass(format('%I.%I', s, t)) IS NULL THEN RETURN NULL; END IF;
      SELECT string_agg(quote_ident(attname), ',' ORDER BY attnum)
               FILTER (WHERE attname NOT IN ('sys_from', 'sys_to')),
             string_agg(quote_ident(attname), ',' ORDER BY attnum)
        INTO cols, allc
        FROM pg_attribute WHERE attrelid = format('%I.%I', s, t)::regclass
         AND attnum > 0 AND NOT attisdropped;
      IF NOT EXISTS (SELECT 1 FROM pg_attribute WHERE attrelid = format('%I.%I', s, t)::regclass
                     AND attname = 'sys_from' AND NOT attisdropped) THEN
        RETURN NULL;
      END IF;
      IF coalesce(current_setting('sutra.v', true), '') <> '' THEN
        v := current_setting('sutra.v')::bigint;
      ELSE
        v := public.sutra_next_version();
        PERFORM set_config('sutra.v', v::text, true);
      END IF;
      seg := left(t, 30) || '_' || left(md5(s || '.' || t), 6) || '__s' || v;
      IF to_regclass(format('%I.%I', s, seg)) IS NOT NULL THEN RETURN seg; END IF;
      EXECUTE format('CREATE TABLE %I.%I (LIKE %I.%I)', s, seg, s, t);
      IF to_regclass(format('%I.%I', s, h)) IS NOT NULL THEN
        EXECUTE format('INSERT INTO %I.%I (%s) SELECT %s FROM %I.%I', s, seg, allc, allc, s, h);
        EXECUTE format('TRUNCATE %I.%I', s, h);
      END IF;
      EXECUTE format('INSERT INTO %I.%I (%s, sys_from, sys_to) SELECT %s, sys_from, %s FROM %I.%I',
                     s, seg, cols, cols, v, s, t);
      EXECUTE format('COMMENT ON TABLE %I.%I IS %L', s, seg, 'sutra:sealed ' || t || ' ' || v);
      RETURN seg;
    END $fn$"
   "CREATE OR REPLACE FUNCTION public.sutra_castable(v TEXT, t TEXT) RETURNS BOOLEAN
    LANGUAGE plpgsql IMMUTABLE AS $fn$
    DECLARE x TEXT;
    BEGIN
      IF v IS NULL THEN RETURN TRUE; END IF;
      EXECUTE format('SELECT (%L::%s)::text', v, t) INTO x;
      RETURN TRUE;
    EXCEPTION WHEN others THEN RETURN FALSE;
    END $fn$"])

(defn install!
  [^Connection c]
  (sql/exec! c "SELECT pg_advisory_xact_lock(hashtext('store_pg_layout'))")
  (doseq [s ["CREATE SCHEMA IF NOT EXISTS public" "CREATE SCHEMA IF NOT EXISTS main"
             "CREATE SEQUENCE IF NOT EXISTS public.dataset_seq"]]
    (sql/exec! c s))
  (doseq [s retention-tables-sql] (sql/exec! c s))
  (doseq [f (concat version-functions functions)] (sql/exec! c f)))

(defn stamp!
  [^Connection c rev]
  (sql/one c "SELECT set_config('sutra.v', ?, true), set_config('sutra.conductor', '', true)"
           [(str rev)]))

(defn- collate-sql [coll] (str " COLLATE " (sql/dq (or coll "default"))))

(defn- col-defs [cols]
  (str/join ", " (for [{:keys [name type not-null? collation]} cols]
                   (str (sql/dq name) " " (pg-type type) (when collation (collate-sql collation))
                        (when not-null? " NOT NULL")))))

(def ^:private owned-mark "sutra:store")

(defn- views
  ([c] (views c nil))
  ([^Connection c only]
   (reduce (fn [m {:keys [table_schema table_name]}]
             (update m table_name (fnil conj #{}) (subs table_schema 3)))
           {}
           (sql/rows c
                     (str
                      "SELECT n.nspname AS table_schema, cl.relname AS table_name FROM pg_class cl"
                      " JOIN pg_namespace n ON n.oid = cl.relnamespace"
                      " WHERE cl.relkind = 'v' AND n.nspname LIKE 'br\\_%'"
                      (when only " AND cl.relname = ANY (?)"))
                     (if only [(into-array String (map str only))] [])))))

(defn schemas-for
  [views ns ts]
  (distinct (concat ["main" (own-schema ns) (read-schema ns)]
                    (for [t ts b (get views (str t)) s [(str "bo_" b) (str "br_" b)]] s))))

(defn- nsp-in [schemas] (str "n.nspname IN (" (sql/placeholders (count schemas)) ")"))

(defn- names-in
  [schemas only r]
  [(cond-> (nsp-in schemas) only (str " AND " r ".relname = ANY (?)"))
   (cond-> (vec schemas) only (conj (into-array String (map str only))))])

(defn- relations
  ([c schemas] (relations c schemas nil))
  ([^Connection c schemas only]
   (let [[where ps] (names-in schemas only "cl")
         [cwhere cps] (names-in schemas only "r")
         rels  (sql/rows c (str "SELECT n.nspname AS s, cl.relname AS t FROM pg_class cl"
                                " JOIN pg_namespace n ON n.oid = cl.relnamespace"
                                " WHERE cl.relkind IN ('r', 'p', 'v')"
                                " AND NOT cl.relispartition AND "
                                where)
                         ps)
         cols  (sql/rows c (str "SELECT n.nspname AS s, r.relname AS t, a.attname,"
                                " format_type(a.atttypid, a.atttypmod) AS typ" (first coll-sql)
                                " FROM pg_attribute a" (second coll-sql)
                                " JOIN pg_class r ON r.oid = a.attrelid"
                                " JOIN pg_namespace n ON n.oid = r.relnamespace"
                                " WHERE r.relkind IN ('r', 'p') AND NOT r.relispartition"
                                " AND a.attnum > 0 AND NOT a.attisdropped AND " cwhere
                                " ORDER BY n.nspname, r.relname, a.attnum")
                         cps)]
     {:tables  (reduce (fn [m {:keys [s t]}] (update m s (fnil conj #{}) t)) {} rels)
      :columns (reduce (fn [m {:keys [s t] :as r}] (update m [s t] (fnil conj []) (column-entry r)))
                       {} cols)})))

(defn catalog
  ([c schemas] (catalog c schemas (views c)))
  ([c schemas views] (catalog c schemas views nil))
  ([^Connection c schemas views only]
   (let [[where ps]   (names-in schemas only "cl")
         [kwhere kps] (names-in schemas only "r")
         owned (sql/rows c
                         (str "SELECT n.nspname AS s, cl.relname AS t, d.description AS tag FROM "
                              "pg_class cl"
                              " JOIN pg_namespace n ON n.oid = cl.relnamespace"
                              " JOIN pg_description d ON d.objoid = cl.oid AND d.objsubid = 0"
                              " WHERE d.description LIKE 'sutra:%' AND " where)
                         ps)
         keys  (sql/rows c
                         (str "SELECT n.nspname AS s, r.relname AS t, i.relname AS idx, a.attname "
                              "AS col, k.ord"
                              " FROM pg_index x"
                              " JOIN pg_class i ON i.oid = x.indexrelid JOIN pg_class r ON r.oid = "
                              "x.indrelid"
                              " JOIN pg_namespace n ON n.oid = r.relnamespace"
                              " CROSS JOIN LATERAL unnest(x.indkey) WITH ORDINALITY AS k(attnum, "
                              "ord)"
                              " JOIN pg_attribute a ON a.attrelid = r.oid AND a.attnum = k.attnum"
                              " WHERE i.relname LIKE r.relname || '_key_%' AND " kwhere)
                         kps)]
     (assoc (relations c schemas only)
            :owned   (reduce (fn [m {:keys [s t tag]}] (assoc-in m [s t] tag)) {} owned)
            :key-idx (into {} (for [[s+t rows] (group-by (juxt :s :t) keys)]
                                [s+t (vec (for [[_ idx] (sort-by key (group-by :idx rows))]
                                            (mapv :col (sort-by :ord idx))))]))
            :views   views))))

(defn- relations-of
  [t]
  [(str t) (history-of t) (tomb-of t) (history-of (tomb-of t))])

(defn catalog-for
  [c ns ts]
  (let [rs (mapcat relations-of ts)
        vs (views c rs)]
    (catalog c (schemas-for vs ns ts) vs (into #{detach-table} rs))))

(defn- k-table? [K s t] (contains? (get-in K [:tables s]) (str t)))
(defn- k-columns [K s t] (get-in K [:columns [s (str t)]] []))
(defn catalog-columns [K s t] (k-columns K s t))
(defn- k-data-columns [K s t] (into [] (comp (map :name) (remove sys?)) (k-columns K s t)))
(defn- k-owned
  [K s]
  (into #{} (keep (fn [[t tag]] (when (= owned-mark tag) t))) (get-in K [:owned s])))

(defn- key-column
  [cols]
  (some #(when (and (:not-null? %) (not (sys? (:name %)))) (:name %)) cols))

(defn- k-key [K s t] (key-column (k-columns K s t)))

(defn- k-add [K s t cols]
  (-> K (update-in [:tables s] (fnil conj #{}) t) (assoc-in [:columns [s t]] (vec cols))))

(defn- k-drop [K s t]
  (-> K
      (update-in [:tables s] disj t)
      (update :columns dissoc [s t])
      (update-in [:owned s] dissoc t)
      (update :key-idx dissoc [s t])))

(defn- k-alter
  [K s t [op a b coll]]
  (update-in K [:columns [s t]]
             (fn [cols]
               (case op
                 :add    (conj (vec cols)
                               (cond-> {:name a :type b :not-null? false}
                                 coll (assoc :collation coll)))
                 :collate (mapv #(cond-> % (= a (:name %))
                                         (-> (dissoc :collation)
                                             (cond-> coll (assoc :collation coll))))
                                cols)
                 :retype (mapv #(cond-> % (= a (:name %))
                                        (-> (assoc :type b)
                                            (cond-> (not (text-type? b)) (dissoc :collation)
                                                    coll (assoc :collation coll))))
                               cols)
                 :rename (mapv #(cond-> % (= a (:name %)) (assoc :name b)) cols)
                 :drop   (filterv #(not= a (:name %)) cols)))))

(defn- retype-using
  [a b]
  (let [q (sql/dq a)]
    (if (#{"json" "jsonb"} (str/lower-case (str b)))
      (str "CASE WHEN " q " IS NULL THEN NULL WHEN " q "::text IS JSON THEN " q "::text::" b
           " ELSE to_" b "(" q ") END")
      (str q "::" b))))

(defn- clause-sql [[op a b coll]]
  (case op
    :add    (str "ADD COLUMN IF NOT EXISTS " (sql/dq a) " " b (when coll (collate-sql coll)))
    :collate (str "ALTER COLUMN " (sql/dq a) " TYPE " b (collate-sql coll))
    :retype (str "ALTER COLUMN " (sql/dq a) " TYPE " b (when coll (collate-sql coll)) " USING "
                 (retype-using a b))
    :rename (str "RENAME COLUMN " (sql/dq a) " TO " (sql/dq b))
    :drop   (str "DROP COLUMN " (sql/dq a))))

(defn- table-entry
  [^Connection c s t]
  (let [rs (sql/rows c
                     (str "SELECT a.attname, format_type(a.atttypid, a.atttypmod) AS typ"
                          (first coll-sql)
                          " FROM pg_class r JOIN pg_namespace n ON n.oid = r.relnamespace"
                          " LEFT JOIN pg_attribute a ON a.attrelid = r.oid AND a.attnum > 0 AND "
                          "NOT a.attisdropped"
                          (second coll-sql)
                          " WHERE n.nspname = ? AND r.relname = ? AND r.relkind IN ('r', 'p', 'v', "
                          "'f')"
                          " ORDER BY a.attnum")
                     [s (str t)])]
    {:table?  (boolean (seq rs))
     :columns (into [] (comp (filter :attname)
                             (map column-entry))
                    rs)}))

(def ^:private memo-spec {::cached {:cap 20000}})

(defn- cached
  [{:keys [cache k]} x f]
  (memo/through cache memo-spec ::cached [k x] f))

(defn- cached-entry [{:keys [c] :as cat} s t] (cached cat [:entry s (str t)] #(table-entry c s t)))

(defrecord ^:private Cached [c cache k]
  Catalog
  (table? [this s t] (:table? (cached-entry this s t)))
  (columns [this s t] (:columns (cached-entry this s t)))
  (sealed [this s t] (cached this [:sealed s (str t)] #(live-sealed c s (str t)))))

(defn cached-cat
  [c cache k]
  (->Cached c cache k))

(defn- tx-kept
  [{:keys [c kept]} s t]
  (let [st [s (str t)]
        {:keys [read k]} @kept]
    (if (read st)
      k
      (let [r (relations c [s] [(str t)])
            k {:tables  (merge-with into (:tables k) (:tables r))
               :columns (merge (:columns k) (:columns r))}]
        (reset! kept {:read (conj read st) :k k})
        k))))

(defrecord ^:private Tx [c kept]
  Catalog
  (table? [this s t] (k-table? (tx-kept this s t) s t))
  (columns [this s t] (k-columns (tx-kept this s t) s t))
  (sealed [_ s t] (live-sealed c s (str t))))

(defn tx-cat
  [c]
  (->Tx c (atom {:read #{} :k {}})))

(defn forget!
  [cat]
  (when (instance? Tx cat) (reset! (:kept cat) {:read #{} :k {}})))

(defrecord ^:private Planned [K]
  Catalog
  (table? [_ s t] (k-table? K s t))
  (columns [_ s t] (k-columns K s t))
  (sealed [_ s t] (get-in K [:sealed [s (str t)]] [])))

(defn- op [p o] (update p :ops conj o))
(defn- exec [p s] (op p [:exec s]))

(defn- version
  [p s t]
  (let [t    (str t)
        cols (k-columns (:k p) s t)
        k    (key-column cols)
        rel  (sql/qualified s t)
        his  (sql/qualified s (history-of t))]
    (-> p
        (exec (str "CREATE TABLE IF NOT EXISTS " his " (LIKE " rel ") PARTITION BY RANGE ("
                   (sql/dq sys-to) ")"))
        (cond-> k
          (exec (str "ALTER TABLE " his " ADD CONSTRAINT "
                     (sql/dq (ident/fit (str (history-of t) "_uq")))
                     " UNIQUE (" (sql/dq k) ", " (sql/dq sys-to) ")")))
        (exec (str "CREATE INDEX IF NOT EXISTS " (sql/dq (ident/fit (str (history-of t) "_brin")))
                   " ON " his " USING brin (" (sql/dq sys-from) ")"))
        (as-> p (reduce (fn [p [suffix level f]]
                          (exec p
                                (str "CREATE OR REPLACE TRIGGER "
                                     (sql/dq (ident/fit (str t suffix)))
                                     " BEFORE INSERT OR UPDATE OR DELETE ON " rel
                                     " FOR EACH " level " EXECUTE FUNCTION public." f "()")))
                        p
                        [["_sutra_stmt" "STATEMENT" "sutra_stmt_version"]
                         ["_sutra_row" "ROW" "sutra_displace"]]))
        (update :k k-add s (history-of t) cols))))

(defn- create-table
  [p s t cols]
  (let [t    (str t)
        k    (:name (first (filter :not-null? cols)))
        rel  (sql/qualified s t)
        cols (concat (remove (comp sys? :name) cols)
                     [{:name sys-from :type "bigint"} {:name sys-to :type "bigint"}])]
    (-> p
        (exec (str "CREATE SCHEMA IF NOT EXISTS " (sql/dq s)))
        (exec
         (str "CREATE TABLE " rel " (" (col-defs cols) ")"
              " WITH (fillfactor=80, autovacuum_vacuum_scale_factor=0.01, "
              "autovacuum_vacuum_threshold=1000)"))
        (cond-> k
          (exec (str "ALTER TABLE " rel " ADD CONSTRAINT " (sql/dq (ident/fit (str t "_live_uq")))
                     " UNIQUE (" (sql/dq k) ")")))
        (update :k k-add s t cols)
        (version s t))))

(defn- versioned-relations [K s t]
  (cond-> [(sql/qualified s t)]
    (k-table? K s (history-of t)) (conj (sql/qualified s (history-of t)))))

(defn- ensure-detach-table [p own]
  (if (k-table? (:k p) own detach-table)
    p
    (-> p
        (exec (str "CREATE TABLE IF NOT EXISTS " (sql/qualified own detach-table)
                   " (table_name text NOT NULL, " (sql/dq sys-from) " BIGINT, "
                   (sql/dq sys-to) " BIGINT)"))
        (update :k k-add own detach-table
                [{:name "table_name" :type "text" :not-null? true}
                 {:name sys-from :type "bigint"} {:name sys-to :type "bigint"}]))))

(defn- ensure-sidecars [p own t]
  (let [k (k-key (:k p) own t)]
    (cond-> (ensure-detach-table p own)
      (and k (not (k-table? (:k p) own (tomb-of t))))
      (-> (cond-> (k-table? (:k p) own (history-of (tomb-of t)))
            (-> (exec (str "DROP TABLE IF EXISTS " (sql/qualified own (history-of (tomb-of t)))
                           " CASCADE"))
                (update :k k-drop own (history-of (tomb-of t)))))
          (create-table own (tomb-of t)
                        (by-codepoint [{:name k :type "character varying" :not-null? true}]))))))

(defn- overlay-sql
  [K own t]
  (let [k     (k-key K own t)
        cols  (sort (k-data-columns K own t))
        mtype (into {} (map (juxt :name :type)) (k-columns K "main" t))
        otype (into {} (map (juxt :name :type)) (k-columns K own t))
        sel-m (str/join "," (for [col cols
                                  :let [mt (mtype col) ot (otype col)]]
                              (cond (nil? mt)     (str "NULL::" (or ot "text") " AS " (sql/dq col))
                                    (not= ot
                                          mt)  (str "m." (sql/dq col) "::" ot " AS " (sql/dq col))
                                    :else         (str "m." (sql/dq col)))))]
    (str "(SELECT * FROM (SELECT " (str/join "," (map sql/dq cols)) " FROM " (sql/qualified own t)
         ") o"
         " UNION ALL SELECT " sel-m " FROM " (sql/qualified "main" t) " m"
         " WHERE m." (sql/dq sys-to) " IS NULL"
         " AND NOT EXISTS (SELECT 1 FROM " (sql/qualified own t) " o2 WHERE o2." (sql/dq k) " = m."
         (sql/dq k)
         " AND o2." (sql/dq sys-to) " IS NULL)"
         " AND NOT EXISTS (SELECT 1 FROM " (sql/qualified own (tomb-of t)) " x WHERE x." (sql/dq k)
         " = m." (sql/dq k)
         " AND x." (sql/dq sys-to) " IS NULL)"
         " AND NOT EXISTS (SELECT 1 FROM " (sql/qualified own detach-table)
         " d WHERE d.table_name = "
         (kit-sql/sq t) " AND d." (sql/dq sys-to) " IS NULL))")))

(defn- view-sql [K b t]
  (let [own (str "bo_" b)]
    (str "CREATE VIEW " (sql/qualified (str "br_" b) t) " AS SELECT * FROM "
         (if (and (k-table? K "main" t) (k-key K own t)) (str (overlay-sql K own t) " v")
             (sql/qualified own t)))))

(defn- drop-view [p b t]
  (-> p
      (exec (str "DROP VIEW IF EXISTS " (sql/qualified (str "br_" b) t)))
      (update-in [:k :tables (str "br_" b)] (fnil disj #{}) t)
      (update-in [:k :views t] (fnil disj #{}) b)))

(defn- create-view [p b t]
  (-> p
      (exec (view-sql (:k p) b t))
      (update-in [:k :tables (str "br_" b)] (fnil conj #{}) t)
      (update-in [:k :views t] (fnil conj #{}) b)))

(defn- rebuild-view [p ns t]
  (let [own (own-schema ns) rs (read-schema ns) t (str t)]
    (if (and (not (main? ns)) (k-table? (:k p) own t))
      (-> p
          (ensure-sidecars own t)
          (exec (str "CREATE SCHEMA IF NOT EXISTS " (sql/dq rs)))
          (drop-view (subs rs 3) t)
          (create-view (subs rs 3) t))
      p)))

(defn- around-views
  [p s t f]
  (let [bs (distinct (cond-> (vec (sort (get-in p [:k :views t])))
                       (str/starts-with? s "bo_") (conj (subs s 3))))
        p  (f (reduce #(drop-view %1 %2 t) p bs))]
    (reduce (fn [p b] (if (k-table? (:k p) (str "bo_" b) t) (create-view p b t) p)) p bs)))

(defn- seal
  [p s t]
  (if (k-table? (:k p) s (history-of t))
    (exec p (str "SELECT public.sutra_seal(" (kit-sql/sq s) ", " (kit-sql/sq (str t)) ", "
                 (kit-sql/sq (history-of t)) ")"))
    p))

(def ^:private layout-changes #{:retype :rename :drop})

(defn- keeping-collation
  [K s t clauses]
  (let [coll (into {} (map (juxt :name :collation)) (k-columns K s t))]
    (mapv (fn [[op a b :as cl]]
            (if (and (= :retype op) (= 3 (count cl)) (coll a) (text-type? b)) [op a b (coll a)] cl))
          clauses)))

(defn- alter-table [p s t clauses]
  (if (empty? clauses)
    p
    (around-views (cond-> p (some (comp layout-changes first) clauses) (seal s t)) s t
                  (fn [p]
                    (reduce
                     (fn [p rel-t]
                       (-> p
                           (exec (str "ALTER TABLE " (sql/qualified s rel-t) " "
                                      (str/join ", "
                                                (map clause-sql
                                                     (keeping-collation (:k p) s rel-t clauses)))))
                           (update :k #(reduce (fn [K cl] (k-alter K s rel-t cl)) % clauses))))
                     p
                     (cond-> [t] (k-table? (:k p) s (history-of t)) (conj (history-of t))))))))

(defn- key-index [p s t cols]
  (if (some #{(vec cols)} (get-in p [:k :key-idx [s (str t)]]))
    p
    (-> p
        (exec (str "CREATE INDEX IF NOT EXISTS "
                   (sql/dq (ident/fit (str t "_key_" (str/join "_" cols))))
                   " ON " (sql/qualified s t) " (" (str/join ", " (map sql/dq cols)) ")"))
        (update-in [:k :key-idx [s (str t)]] #(vec (distinct (conj (vec %) (vec cols))))))))

(defn- own
  [p s t tag]
  (if (or (= tag (get-in p [:k :owned s (str t)]))
          (and (not= owned-mark tag) (= owned-mark (get-in p [:k :owned s (str t)]))))
    p
    (-> p
        (exec (str "COMMENT ON TABLE " (sql/qualified s t) " IS '" tag "'"))
        (assoc-in [:k :owned s (str t)] tag))))

(defn- ensure-own
  [p ns t]
  (let [own-s (own-schema ns) t (str t) K (:k p)]
    (if (and (not (main? ns)) (not (k-table? K own-s t)) (k-table? K "main" t))
      (-> p
          (create-table own-s t (remove (comp sys? :name) (k-columns K "main" t)))
          (as-> p (reduce #(key-index %1 own-s t %2) p (get-in K [:key-idx ["main" t]])))
          (ensure-sidecars own-s t)
          (rebuild-view ns t)
          (as-> p (if-let [tag (get-in K [:owned "main" t])] (own p own-s t tag) p)))
      p)))

(defn satisfied-by-main?
  [K ns t cols]
  (and (not (main? ns))
       (not (k-table? K (own-schema ns) t))
       (k-table? K "main" t)
       (let [have (into {} (map (juxt :name :type)) (k-columns K "main" t))]
         (every? (fn [{:keys [name type any-width?]}]
                   (when-let [from (have name)]
                     (or (= (pg-type from) (pg-type type))
                         (and any-width? (= :decimal (:kind (ctype/parse from)))))))
                 cols))))

(defn- change-columns
  [p s t clauses]
  (let [have (into {} (map (juxt :name :type)) (k-columns (:k p) s t))]
    (-> p
        (as-> p (reduce (fn [p [o col to]]
                          (cond-> p (= :retype o)
                                  (op [:assert-castable s t col (have col) to
                                       (versioned-relations (:k p) s t)])))
                        p clauses))
        (alter-table s t clauses))))

(defn- ensure-table
  [p ns t cols tag]
  (let [s (own-schema ns) t (str t)]
    (if (satisfied-by-main? (:k p) ns t cols)
      p
      (let [p (ensure-own p ns t)]
        (-> (if-not (k-table? (:k p) s t)
              (-> p (create-table s t cols) (rebuild-view ns t))
              (let [have    (into {} (map (juxt :name :type)) (k-columns (:k p) s t))
                    coll    (into {} (map (juxt :name :collation)) (k-columns (:k p) s t))
                    adds    (for [{:keys [name type collation]} cols :when (not (have name))]
                              [:add name (pg-type type) collation])
                    recoll  (for [{:keys [name type collation]} cols
                                  :when (and (have name) (not= collation (coll name))
                                             (= (pg-type type) (pg-type (have name))))]
                              [:collate name (pg-type type) collation])
                    retypes (for [{:keys [name type any-width? collation]} cols
                                  :let [from (have name) to (pg-type type)]
                                  :when (and from (not= (pg-type from) to)
                                             (not (and any-width?
                                                       (= :decimal (:kind (ctype/parse from))))))]
                              [:retype name to collation])]
                (-> p
                    (change-columns s t (concat adds recoll retypes))
                    (cond-> (seq adds) (rebuild-view ns t)))))
            (cond-> tag (own s t tag)))))))

(defn- drop-table [p ns t]
  (let [s (own-schema ns) t (str t)
        p (-> p (seal s t) (seal s (tomb-of t)))]
    (-> (reduce (fn [p x] (-> p
                              (exec (str "DROP TABLE IF EXISTS " (sql/qualified s x) " CASCADE"))
                              (update :k k-drop s x)))
                p [(history-of t) t (history-of (tomb-of t)) (tomb-of t)])
        (cond-> (and (not (main? ns)) (k-table? (:k p) s detach-table))
          (op [:update (str "DELETE FROM " (sql/qualified s detach-table) " WHERE table_name = ?")
               [t]])))))

(defn declared-tables
  [sig]
  (merge (into {} (map (fn [T] [(object-table T) :object])) (keys (:types sig)))
         (into {} (map (fn [L] [(link-table L) :link])) (stored-link-types sig))))

(defn- copy-rows
  [p s t k cols from]
  (let [cl (str/join "," (map sql/dq cols))]
    (op p [:update (str "INSERT INTO " (sql/qualified s t) " (" cl ") SELECT "
                        (str/join "," (map #(str "v." (sql/dq %)) cols))
                        " FROM " from " v WHERE NOT EXISTS (SELECT 1 FROM " (sql/qualified s t) " o"
                        " WHERE o." (sql/dq k) " = v." (sql/dq k) ")")
           []])))

(defn- detach
  [p ns rev t materialize?]
  (let [own-s (own-schema ns) t (str t)]
    (cond
      (main? ns) p
      materialize?
      (if (k-table? (:k p) "main" t)
        (-> p (ensure-own ns t) (ensure-sidecars own-s t) (op [:stamp rev])
            (op [:detach own-s t {:materialize? true}]))
        p)
      :else
      (-> p
          (cond-> (not (k-table? (:k p) own-s detach-table))
            (exec (str "CREATE SCHEMA IF NOT EXISTS " (sql/dq own-s))))
          (ensure-sidecars own-s t)
          (op [:stamp rev])
          (op [:detach own-s t {:materialize? false}])))))

(defn- step [p ns rev [k T a b]]
  (case k
    (:rename-attribute :drop-attribute)
    (let [t (object-table T) s (own-schema ns) p (ensure-own p ns t)]
      (if (k-table? (:k p) s t)
        (let [have (set (k-data-columns (:k p) s t))]
          (if (= :rename-attribute k)
            (cond-> p (and (have (column a)) (not (have (column b))))
                    (alter-table s t [[:rename (column a) (column b)]]))
            (cond-> p (have (column a))
                    (alter-table s t [[:drop (column a)]]))))
        p))
    :clear-attribute
    (let [t (object-table T) s (own-schema ns) col (sql/dq (column a)) p (op p [:stamp rev])]
      (if (main? ns)
        (cond-> p (k-table? (:k p) s t)
                (op [:update
                     (str "UPDATE " (sql/qualified s t) " SET " col " = NULL WHERE " col
                          " IS NOT NULL") []]))
        (if-let [rel (:sql (relation (->Planned (:k p)) ns t nil))]
          (let [p    (ensure-own p ns t)
                cols (k-data-columns (:k p) s t)]
            (op p
                [:update
                 (str "INSERT INTO " (sql/qualified s t) " (" (str/join "," (map sql/dq cols)) ")"
                      " SELECT "
                      (str/join "," (map #(if (= % (column a)) "NULL" (str "v." (sql/dq %))) cols))
                      " FROM " rel " v WHERE v." col " IS NOT NULL"
                      " ON CONFLICT (" (sql/dq pk) ") DO UPDATE SET " col " = NULL")
                 []]))
          p)))
    :drop-type        (drop-table p ns (object-table T))
    :drop-link-type   (drop-table p ns (link-table T))
    :detach-type      (detach p ns rev (object-table T) true)
    :detach-link-type (detach p ns rev (link-table T) true)
    (:retype-attribute :relayout :drop-unclaimed) p))

(defn- type-indexes [{:keys [key index]}]
  (cond-> (mapv vector index) (seq key) (conj key)))

(defn- laid
  [sig only]
  (if only
    [(keep #(find (:types sig) %) (:types only))
     (filter #(and (contains? (:links sig) %) (not (fk-link? sig %))) (:links only))]
    [(:types sig) (stored-link-types sig)]))

(defn- ensure-tables
  [p ns sig0 sig all? only]
  (let [s       (own-schema ns)
        [types links] (laid sig only)
        K0      (:k p)
        has?    (fn [t] (or (k-table? K0 "main" t) (k-table? K0 s t)))
        p       (reduce (fn [p [T spec]]
                          (let [t (object-table T)]
                            (if (or all? (not= spec (get-in sig0 [:types T])) (not (has? t)))
                              (ensure-table p ns t (object-columns sig T) owned-mark)
                              p)))
                        p types)
        p       (reduce (fn [p L]
                          (let [t (link-table L)]
                            (if (or all? (not= (get-in sig [:links L]) (get-in sig0 [:links L]))
                                    (not (has? t)))
                              (ensure-table p ns t (link-columns sig L) owned-mark)
                              p)))
                        p links)]
    (reduce (fn [p [T props]]
              (let [t    (object-table T)
                    cols (mapv column props)]
                (if (and (k-table? (:k p) s t)
                         (every? (set (k-data-columns (:k p) s t)) cols))
                  (key-index p s t cols)
                  p)))
            p (for [[T spec] types, props (type-indexes spec)] [T props]))))

(defn layout-plan
  [K ns rev {sig0 :old sig :new steps :steps only :only}]
  (let [all? (some #{[:relayout]} steps)
        s    (own-schema ns)
        p    (reduce #(step %1 ns rev %2) {:k K :ops []}
                     (remove #(= :clear-attribute (first %)) steps))
        p    (ensure-tables p ns sig0 sig all? only)
        p    (reduce #(step %1 ns rev %2) p (filter #(= :clear-attribute (first %)) steps))
        p    (if (some #{[:drop-unclaimed]} steps)
               (let [keep (if only
                            (let [[types links] (laid sig only)]
                              (into (set (map (comp object-table key) types))
                                    (map link-table) links))
                            (declared-tables sig))]
                 (reduce #(drop-table %1 ns %2) p
                         (sort (remove #(contains? keep %) (k-owned (:k p) s)))))
               p)]
    (:ops p)))

(defn- inherit
  [p from to rev]
  (let [src (own-schema from) dst (own-schema to)
        p   (op p [:stamp rev])
        p   (reduce (fn [p t]
                      (let [p (if (k-table? (:k p) "main" t)
                                (ensure-own p to t)
                                (if (k-table? (:k p) dst t)
                                  p
                                  (-> p
                                      (create-table
                                       dst t (remove (comp sys? :name) (k-columns (:k p) src t)))
                                      (own dst t owned-mark))))
                            p (ensure-sidecars p dst t)
                            k (k-key (:k p) dst t)
                            p (copy-rows p dst t k
                                         (filterv (set (k-data-columns (:k p) dst t))
                                                  (k-data-columns (:k p) src t))
                                         (sql/qualified src t))
                            p (cond-> p (k-table? (:k p) src (tomb-of t))
                                      (copy-rows dst (tomb-of t) k [k]
                                                 (sql/qualified src (tomb-of t))))]
                        (rebuild-view p to t)))
                    p (sort (k-owned (:k p) src)))]
    (cond-> p (k-table? (:k p) src detach-table)
            (-> (ensure-detach-table dst)
                (op [:update
                     (str "INSERT INTO " (sql/qualified dst detach-table) " (table_name, "
                          (sql/dq sys-from) ")"
                          " SELECT d.table_name, ? FROM " (sql/qualified src detach-table) " d"
                          " WHERE d." (sql/dq sys-to) " IS NULL")
                     [rev]])))))

(defn fork-plan
  [K from to sig rev]
  (let [p (reduce (fn [p s] (exec p (str "CREATE SCHEMA IF NOT EXISTS " (sql/dq s))))
                  {:k K :ops []} [(read-schema to) (own-schema to)])]
    (:ops (if (main? from)
            p
            (-> p (ensure-tables to {} sig false nil) (inherit from to rev))))))

(defn- uncastable [c rels col target]
  (let [src (str/join " UNION ALL " (map #(str "SELECT " (sql/dq col) " AS v FROM " %) rels))]
    (long (:n (sql/one c (str "SELECT count(*) AS n FROM (" src ") q WHERE q.v IS NOT NULL"
                              " AND NOT public.sutra_castable(q.v::text, ?)")
                       [target])))))

(defn- exec-count! [^Connection c ^String s]
  (with-open [stmt (.createStatement c)] (long (.executeUpdate stmt s))))

(defn- displace!
  [^Connection c s t where v]
  (sql/one c "SELECT set_config('sutra.v', ?, true), set_config('sutra.conductor', 'on', true)"
           [(str v)])
  (when (table? c s (history-of t))
    (let [lo   (* partition-span (quot (long v) partition-span))
          cols (sort (remove #{sys-to} (map :name (columns c s t))))
          cl   (str/join "," (map sql/dq cols))]
      (sql/one c "SELECT public.sutra_history_partition(?, ?, ?, ?, ?)"
               [s (history-of t) (ident/fit (str (history-of t) "_p" lo)) lo
                (+ lo partition-span)])
      (sql/exec! c
                 (str "INSERT INTO " (sql/qualified s (history-of t)) " (" cl "," (sql/dq sys-to)
                      ")"
                      " SELECT " cl "," (long v) " FROM " (sql/qualified s t) " WHERE " where))))
  (exec-count! c (str "DELETE FROM " (sql/qualified s t) " WHERE " where)))

(defn- tomb-keys!
  [c s t ks]
  (let [tomb (sql/qualified s (tomb-of t))
        k    (sql/dq (key-column (columns c s (tomb-of t))))]
    (sql/update! c (str "INSERT INTO " tomb " (" k ")"
                        " SELECT DISTINCT x FROM unnest(?::text[]) AS v(x)"
                        " WHERE NOT EXISTS (SELECT 1 FROM " tomb " e WHERE e." k " = v.x)")
                 [(sql/text-array ks)])))

(defn- detach-table!
  [c s t materialize?]
  (when materialize?
    (let [b    (subs s 3)
          cols (data-columns c s t)
          k    (key-column (columns c s t))
          cl   (str/join "," (map sql/dq cols))
          {rel :sql ps :params} (relation c b t nil)]
      (sql/update! c (str "INSERT INTO " (sql/qualified s t) " (" cl ") SELECT "
                          (str/join "," (map #(str "v." (sql/dq %)) cols))
                          " FROM " rel " v WHERE NOT EXISTS (SELECT 1 FROM " (sql/qualified s t)
                          " o"
                          " WHERE o." (sql/dq k) " = v." (sql/dq k) ")")
                   ps)))
  (sql/update! c
               (str "INSERT INTO " (sql/qualified s detach-table) " (table_name, " (sql/dq sys-from)
                    ")"
                    " SELECT ?, current_setting('sutra.v')::bigint WHERE NOT EXISTS (SELECT 1 FROM "
                    (sql/qualified s detach-table) " d WHERE d.table_name = ? AND d."
                    (sql/dq sys-to) " IS NULL)")
               [t t]))

(defn execute!
  [^Connection c ops]
  (mapv (fn [[k a b :as o]]
          (case k
            :exec     (sql/exec! c a)
            :update   (sql/update! c a b)
            :stamp    (stamp! c a)
            :displace (let [[_ s t where v] o] (displace! c s t where v))
            :tomb     (let [[_ s t ks] o] (tomb-keys! c s t ks))
            :detach   (let [[_ s t {:keys [materialize?]}] o]
                        (detach-table! c s (str t) materialize?))
            :assert-castable
            (let [[_ s t col from to rels] o
                  bad (uncastable c rels col to)]
              (when (pos? bad)
                (anomaly/raise! :incorrect
                                (str "SCHEMA-MISMATCH: column " col " of " s "." t " holds " bad
                                     " value(s) that do not cast from " from " to " to)
                                {:error :schema-mismatch :schema s :table t :column col :from from
                                 :to to
                                 :uncastable bad})))))
        ops))

(defn view-plan
  [K ns t]
  (:ops (rebuild-view {:k K :ops []} ns t)))

(defn table-plan
  [K ns t cols tag]
  (:ops (ensure-table {:k K :ops []} ns t cols tag)))

(defn create-plan
  [K s t cols]
  (if (k-table? K s t) [] (:ops (create-table {:k K :ops []} s t cols))))

(defn version-plan
  [K s t]
  (let [t (str t)]
    (if (or (not (k-table? K s t)) (str/includes? t "__history"))
      []
      (let [have (set (map :name (k-columns K s t)))
            p    (reduce (fn [p col]
                           (if (have col)
                             p
                             (-> p
                                 (exec (str "ALTER TABLE " (sql/qualified s t)
                                            " ADD COLUMN IF NOT EXISTS " (sql/dq col) " BIGINT"))
                                 (update :k k-alter s t [:add col "bigint"]))))
                         {:k K :ops []} [sys-from sys-to])]
        (:ops (cond-> p (not (k-table? (:k p) s (history-of t))) (version s t)))))))

(defn alter-plan
  [K s t clauses]
  (:ops (change-columns {:k K :ops []} s (str t) clauses)))

(defn drop-plan
  [K ns t]
  (:ops (drop-table {:k K :ops []} ns t)))

(defn detach-plan
  [K ns rev t materialize?]
  (:ops (detach {:k K :ops []} ns rev t materialize?)))

(defn own-plan
  [K ns t]
  (:ops (ensure-own {:k K :ops []} ns t)))

(def ^:private step-table
  {:rename-attribute object-table :drop-attribute object-table :clear-attribute object-table
   :drop-type object-table :detach-type object-table
   :drop-link-type link-table :detach-link-type link-table})

(defn- named-tables
  [only steps]
  (-> #{}
      (into (map object-table) (:types only))
      (into (map link-table) (:links only))
      (into (keep (fn [[k T]] (when-let [f (step-table k)] (f T)))) steps)))

(defn- whole-catalog
  [c ns {sig0 :old sig :new steps :steps}]
  (let [vs (views c)
        ts (cond-> (set (keys (merge (declared-tables sig0) (declared-tables sig))))
             (some #{[:drop-unclaimed]} steps) (into (keys vs)))]
    (catalog c (schemas-for vs ns ts) vs)))

(defn- narrowed?
  [{:keys [only steps]}]
  (and only (not-any? #{[:relayout]} steps)))

(defn layout!
  [c ns rev {:keys [only steps] :as change}]
  (if (narrowed? change)
    (let [ops (layout-plan (catalog-for c ns (named-tables only steps)) ns rev change)]
      (when orakel/on?
        (let [whole (layout-plan (whole-catalog c ns change) ns rev (dissoc change :only))]
          (orakel/ran! :layout)
          (when (not= (frequencies ops) (frequencies whole))
            (throw (ex-info "the narrowed layout is not the whole one"
                            {:oracle :layout :ns ns :narrowed ops :whole whole})))))
      (execute! c ops))
    (let [change (dissoc change :only)]
      (execute! c (layout-plan (whole-catalog c ns change) ns rev change)))))

(defn recollate-plan
  [K s t]
  (let [t       (str t)
        tomb    (tomb-of t)
        segs    (sort (keep (fn [[seg tag]]
                              (when (and (str/starts-with? tag sealed-mark)
                                         (#{t tomb} (second (str/split tag #" "))))
                                seg))
                            (get-in K [:owned s])))
        clauses (fn [x] (vec (for [{:keys [name type collation]} (k-columns K s x)
                                   :when (and (text-type? type) (not= codepoint collation))]
                               [:collate name type codepoint])))
        with-h  (fn [x] (cond-> [x] (k-table? K s (history-of x)) (conj (history-of x))))
        plain   (fn [p x]
                  (reduce (fn [p y]
                            (let [cls (clauses y)]
                              (cond-> p (seq cls)
                                      (exec (str "ALTER TABLE " (sql/qualified s y) " "
                                                 (str/join ", " (map clause-sql cls)))))))
                          p (with-h x)))]
    (:ops (as-> {:k K :ops []} p
            (cond-> p (k-table? K s t) (alter-table s t (clauses t)))
            (cond-> p (and (k-table? K s tomb) (some (comp seq clauses) (with-h tomb)))
                    (around-views s t #(plain % tomb)))
            (reduce plain p segs)))))

(defn- recollate-owners
  [^Connection c]
  (let [rows  (sql/rows c
                        (str "SELECT DISTINCT n.nspname AS s, r.relname AS t, d.description AS tag"
                             " FROM pg_attribute a JOIN pg_class r ON r.oid = a.attrelid"
                             " JOIN pg_namespace n ON n.oid = r.relnamespace"
                             " LEFT JOIN pg_description d ON d.objoid = r.oid AND d.objsubid = 0"
                             " WHERE (n.nspname = 'main' OR n.nspname LIKE 'bo\\_%')"
                             " AND r.relkind IN ('r', 'p') AND NOT r.relispartition"
                             " AND a.attnum > 0 AND NOT a.attisdropped"
                             " AND a.atttypid IN ('text'::regtype, 'character varying'::regtype)"
                             " AND a.attcollation <> (SELECT oid FROM pg_collation WHERE collname "
                             "= 'C' LIMIT 1)")
                        [])
        owner (fn [{:keys [t tag]}]
                (let [x (if (and tag (str/starts-with? tag sealed-mark))
                          (second (str/split tag #" ")) t)
                      x (cond-> x (str/ends-with? x "__history") (subs 0 (- (count x) 9)))]
                  (cond-> x (str/ends-with? x "__tomb") (subs 0 (- (count x) 6)))))
        owned (into #{} (map (juxt :s :t))
                    (sql/rows c
                              (str "SELECT n.nspname AS s, r.relname AS t FROM pg_class r"
                                   " JOIN pg_namespace n ON n.oid = r.relnamespace"
                                   " JOIN pg_description d ON d.objoid = r.oid AND d.objsubid = 0"
                                   " WHERE d.description = ? AND (n.nspname = 'main' OR n.nspname "
                                   "LIKE 'bo\\_%')")
                              [owned-mark]))]
    (into [] (comp (map (juxt :s owner)) (distinct) (filter owned)) rows)))

(defn- recollate-schemas
  [vs s t]
  (distinct (concat ["main" s] (when (str/starts-with? s "bo_") [(str "br_" (subs s 3))])
                    (for [b (get vs t) x [(str "bo_" b) (str "br_" b)]] x))))

(defn recollate!
  [^Connection c]
  (let [vs (views c)]
    (doseq [[s t] (recollate-owners c)]
      (execute! c (recollate-plan (catalog c (recollate-schemas vs s t) vs) s t)))))

(defn recollate-in-steps!
  [ds]
  (with-open [lock (jdbc/get-connection ds)]
    (if-not (:ok (sql/one lock "SELECT pg_try_advisory_lock(hashtext('store_pg_recollate')) AS ok"
                          []))
      0
      (try
        (let [owners (with-open [c (jdbc/get-connection ds)] (recollate-owners c))]
          (reduce (fn [n [s t]]
                    (try
                      (with-open [c (jdbc/get-connection ds)]
                        (jdbc/with-transaction [tx c]
                          (let [vs (views tx)]
                            (execute! tx
                                      (recollate-plan
                                       (catalog tx (recollate-schemas vs s t) vs) s t)))))
                      (inc n)
                      (catch InterruptedException e (throw e))
                      (catch Exception e
                        (binding [*out* *err*]
                          (println (str "WARNING: " s "." t " not brought to code point: "
                                        (.getMessage e))))
                        n)))
                  0 owners))
        (finally (sql/one lock "SELECT pg_advisory_unlock(hashtext('store_pg_recollate')) AS ok"
                          []))))))

(defn fork!
  [c from to sig rev]
  (if (main? from)
    (execute! c (fork-plan {} from to sig rev))
    (let [vs   (views c)
          ts   (keys (declared-tables sig))
          mine (k-owned (catalog c [(own-schema from)] vs) (own-schema from))
          only (into #{detach-table}
                     (mapcat relations-of)
                     (concat ts mine))]
      (execute! c
                (fork-plan (catalog c
                                    (distinct
                                     (concat (schemas-for vs from ts) (schemas-for vs to ts)))
                                    vs only)
                           from to sig rev)))))

(defn ensure-own!
  ([c ns t] (ensure-own! c c ns t))
  ([c cat ns t]
   (let [own-s (own-schema ns) t (str t)]
     (when (and (not (main? ns)) (not (table? cat own-s t)) (table? cat "main" t))
       (execute! c
                 (:ops (ensure-own {:k (catalog c ["main" own-s (read-schema ns)]) :ops []} ns t)))
       (forget! cat)
       true))))

(defn tomb!
  ([c ns t ks] (tomb! c c ns t ks))
  ([c cat ns t ks]
   (let [t (str t)]
     (when (and (seq ks) (not (main? ns)) (table? cat "main" t))
       (ensure-own! c cat ns t)
       (first (execute! c [[:tomb (own-schema ns) t ks]]))))))

(defn table-key
  [c s t]
  (key-column (columns c s t)))

(defn inspect
  [c ns sig]
  (let [s       (own-schema ns)
        decl    (declared-tables sig)
        vs      (views c)
        K       (if (main? ns)
                  (catalog c ["main"] vs)
                  (merge-with merge (catalog c ["main"] vs (keys decl)) (catalog c [s] vs)))
        present (fn [schema] (get-in K [:tables schema] #{}))
        has?    (fn [t] (or (contains? (present "main") t) (contains? (present s) t)))
        home    (fn [t] (if (contains? (present s) t) s "main"))
        unindexed (for [[T spec] (:types sig)
                        :let [t     (object-table T)
                              cols  (set (k-data-columns K (home t) t))
                              have  (set (get-in K [:key-idx [(home t) t]]))
                              wants (filter #(every? cols %)
                                            (map #(mapv column %) (type-indexes spec)))]
                        :when (and (has? t) (not-every? have wants))]
                    T)
        orphans (for [t (sort (k-owned K s))
                      :when (not (contains? decl t))]
                  {:name t
                   :rows (long (:n (sql/one c (str "SELECT count(*) AS n FROM " (sql/qualified s t))
                                            [])))})]
    {:missing   {:object-types (vec (remove #(has? (object-table %)) (keys (:types sig))))
                 :link-types   (vec (remove #(has? (link-table %)) (stored-link-types sig)))
                 :indexes      (vec unindexed)}
     :orphans   (vec orphans)
     :local     (into #{} (filter #(contains? (present s) (object-table %))) (keys (:types sig)))
     :inherited (if (main? ns)
                  #{}
                  (into #{} (filter #(and (not (contains? (present s) (object-table %)))
                                          (contains? (present "main") (object-table %))))
                        (keys (:types sig))))}))

(defn ns-exists? [c ns] (or (main? ns) (schema? c (read-schema ns))))

(defn drop-ns! [c ns]
  (when (main? ns) (throw (ex-info "branch main is not dropped" {:ns ns})))
  (doseq [s [(read-schema ns) (own-schema ns)]]
    (sql/exec! c (str "DROP SCHEMA IF EXISTS " (sql/dq s) " CASCADE"))))

(defn- cols-sql [cols] (str/join "," (map #(str "q." (sql/dq %)) cols)))

(defn- live-at
  [cat s t cols r]
  (let [sel (cols-sql cols)]
    (if (nil? r)
      (sql/frag (str "(SELECT " sel " FROM " (sql/qualified s t) " q)"))
      (let [r (long r)]
        (sql/frag-cat
         [(sql/frag (str "(SELECT " sel " FROM " (sql/qualified s t) " q WHERE coalesce(q."
                         (sql/dq sys-from) ", 0) <= ?") [r])
          (when (table? cat s (history-of t))
            (sql/frag (str " UNION ALL SELECT " sel " FROM " (sql/qualified s (history-of t)) " q"
                           " WHERE coalesce(q." (sql/dq sys-from) ", 0) <= ? AND q." (sql/dq sys-to)
                           " > ?")
                      [r r]))
          (sql/frag ")")])))))

(defn- segment-at
  [cat s t r]
  (when r (first (filter #(> (long (:rev %)) (long r)) (sealed cat s t)))))

(defn- has-at? [cat s t r] (or (table? cat s t) (some? (segment-at cat s t r))))

(defn- columns-at [cat s t r]
  (if-let [{:keys [seg]} (segment-at cat s t r)] (columns cat s seg) (columns cat s t)))

(defn- data-columns-at [cat s t r]
  (into [] (comp (map :name) (remove sys?)) (columns-at cat s t r)))

(defn- rows-at
  [cat s t cols r]
  (if-let [{:keys [seg]} (segment-at cat s t r)]
    (sql/frag (str "(SELECT " (cols-sql cols) " FROM " (sql/qualified s seg) " q"
                   " WHERE coalesce(q." (sql/dq sys-from) ", 0) <= ? AND q." (sql/dq sys-to)
                   " > ?)")
              [(long r) (long r)])
    (live-at cat s t cols r)))

(defn- detached-at
  [own r]
  (if r
    (sql/frag (str "(SELECT table_name FROM " (sql/qualified own detach-table)
                   " d WHERE coalesce(d." (sql/dq sys-from)
                   ", 0) <= ? AND (d." (sql/dq sys-to) " IS NULL OR d." (sql/dq sys-to) " > ?))")
              [(long r) (long r)])
    (sql/frag (str "(SELECT table_name FROM " (sql/qualified own detach-table) " d WHERE d."
                   (sql/dq sys-to) " IS NULL)"))))

(defn- overlay
  [t k m-sel o-rel m-rel tomb det]
  (let [kq (sql/dq k)]
    (sql/frag-cat
     [(sql/frag-wrap "(SELECT * FROM " o-rel (str " o UNION ALL SELECT " m-sel " FROM "))
      (sql/frag-wrap "" m-rel " m WHERE NOT EXISTS (SELECT 1 FROM ")
      (sql/frag-wrap "" o-rel (str " o2 WHERE o2." kq " = m." kq ")"))
      (when tomb
        (sql/frag-wrap " AND NOT EXISTS (SELECT 1 FROM " tomb
                       (str " x WHERE x." kq " = m." kq ")")))
      (when det
        (sql/frag-wrap " AND NOT EXISTS (SELECT 1 FROM " det
                       (str " dd WHERE dd.table_name = " (kit-sql/sq t) ")")))
      (sql/frag ")")])))

(defn- relation*
  [cat ns t as-of]
  (let [t     (str t)
        own   (own-schema ns)
        r     (:rev as-of)
        r0    (if (main? ns) r (:root-rev as-of))
        mine? (and (not (main? ns)) (has-at? cat own t r))
        root? (has-at? cat "main" t r0)
        k     (when (and mine? root?) (key-column (columns-at cat own t r)))]
    (cond
      (and (main? ns) root?) (rows-at cat "main" t (data-columns-at cat "main" t r) r)
      (main? ns)             nil
      k
      (let [cols  (data-columns-at cat own t r)
            mtype (into {} (map (juxt :name :type)) (columns-at cat "main" t r0))
            otype (into {} (map (juxt :name :type)) (columns-at cat own t r))
            m-sel (str/join ","
                            (for [col cols :let [mt (mtype col) ot (otype col)]]
                              (cond (nil? mt)    (str "NULL::" ot " AS " (sql/dq col))
                                    (not= mt
                                          ot) (str "m." (sql/dq col) "::" ot " AS " (sql/dq col))
                                    :else        (str "m." (sql/dq col)))))]
        (overlay t k m-sel
                 (rows-at cat own t cols r)
                 (rows-at cat "main" t (data-columns-at cat "main" t r0) r0)
                 (when (has-at? cat own (tomb-of t) r) (rows-at cat own (tomb-of t) [k] r))
                 (when (table? cat own detach-table) (detached-at own r))))
      mine? (rows-at cat own t (data-columns-at cat own t r) r)
      root? (rows-at cat "main" t (data-columns-at cat "main" t r0) r0)
      :else nil)))

(defn relation
  [cat ns t as-of]
  (if (instance? Cached cat)
    (cached cat [:relation ns (str t) as-of] #(relation* cat ns t as-of))
    (relation* cat ns t as-of)))

(defn table-at
  [c s t r]
  (when (has-at? c s t r)
    (rows-at c s t (data-columns-at c s t r) r)))

(defn sealed-at
  [c s t r]
  (segment-at c s t r))

(defn- fk-match
  [sig {pk-type :target pk-attribute :target-attr fk-type :holder fk-attribute :attr}]
  (let [type-of (fn [T a] (some #(when (= a (:name %)) (:type %)) (attrs sig T)))
        want    (type-of fk-type fk-attribute)
        array?  (and (vector? want) (= :array (first want)))
        elem    (if array? (second want) want)
        fk-col  (str "f." (sql/dq (column fk-attribute)))
        holds   (fn [p-col have]
                  (cond array? (str fk-col "::jsonb @> jsonb_build_array(" p-col
                                    (when (and have (not= elem have)) "::text") ")")
                        (and want have (not= want have)) (str fk-col "::text = " p-col "::text")
                        :else (str fk-col " = " p-col)))]
    (if pk-attribute
      (holds (str "p." (sql/dq (column pk-attribute))) (type-of pk-type pk-attribute))
      (let [cols (into [] (comp (filter #(= elem (:type %)))
                                (remove (comp sequential? :type))
                                (map (comp sql/dq column :name)))
                       (attrs sig pk-type))]
        (if (empty? cols) "FALSE"
            (str "(" (str/join " OR " (map #(holds (str "p." %) elem) cols)) ")"))))))

(defn- link-relation*
  [cat ns sig L as-of]
  (let [spec (get-in sig [:links L :fk])
        cols (str "l." (sql/dq link-pk) ", l.from_id, l.to_id, l." (sql/dq markings))]
    (if-not spec
      (when-let [stored (relation cat ns (link-table L) as-of)]
        (sql/frag-wrap (str "(SELECT " cols " FROM ") stored " l)"))
      (let [{fk-type :holder pk-type :target fk-side :side} spec
            f      (relation cat ns (object-table fk-type) as-of)
            p      (relation cat ns (object-table pk-type) as-of)
            [fe te] (case fk-side :from [(str "f." (sql/dq pk)) (str "p." (sql/dq pk))]
                          :to   [(str "p." (sql/dq pk)) (str "f." (sql/dq pk))])
            derived (when (and f p)
                      (sql/frag-cat
                       [(sql/frag (str "(SELECT " fe " AS from_id, " te " AS to_id FROM "))
                        (sql/frag-wrap "" f " f JOIN ")
                        (sql/frag-wrap "" p (str " p ON " (fk-match sig spec) ")"))]))]
        (when derived
          (sql/frag-wrap (str "(SELECT NULL AS " (sql/dq link-pk) ", d.from_id, d.to_id, NULL AS "
                              (sql/dq markings) " FROM ")
                         derived " d)"))))))

(defn link-relation
  [cat ns sig L as-of]
  (if (instance? Cached cat)
    (cached cat [:link-relation ns L as-of sig] #(link-relation* cat ns sig L as-of))
    (link-relation* cat ns sig L as-of)))

