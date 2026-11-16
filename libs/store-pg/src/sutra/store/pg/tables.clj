(ns sutra.store.pg.tables
  (:require [sutra.types.value-types :as vt]
            [sutra.types.naming :as naming :refer [dataset-table]]
            [sutra.types.column :refer [sql-name]]
            [clojure.string :as str]
            [sutra.kit.sql :as kit-sql]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.ns :as pgns]
            [sutra.types.coltype :as ctype]))

(set! *warn-on-reflection* true)

(def ^:private pk-col naming/pk-col)

(defn- store-type-of [value-type]
  (ctype/render (ctype/for-store (vt/col-type-of value-type)) :postgres))

(defn- col-defs
  [columns]
  (into [{:name pk-col :type "VARCHAR" :not-null? true}]
        (comp (remove #(= pk-col (sql-name (:attribute-name %))))
              (map (fn [{:keys [attribute-name attribute-type]}]
                     (cond-> {:name (sql-name attribute-name) :type (store-type-of attribute-type)}
                       (= :decimal attribute-type) (assoc :any-width? true)))))
        (or columns [])))

(defn create-temp-shape!
  [c alias columns]
  (jdbc/execute! c [(str "CREATE TEMP TABLE " (kit-sql/dq alias) " ("
                         (str/join ", "
                                   (map (fn [{:keys [name type]}] (str (kit-sql/dq name) " " type))
                                        (col-defs columns)))
                         ")")]))

(defn- folded-twins
  [existing desired]
  (let [want (set desired)]
    (into {} (for [e existing
                   :let [f (sql-name e)]
                   :when (and (not= f e) (want f))]
               [e f]))))

(defn- fold-folded-columns! [c s tbl defs]
  (let [existing (pgds/columns c s tbl)]
    (doseq [[old new] (folded-twins existing (map :name defs))]
      (if-not (contains? existing new)
        (pgds/rename-column! c s tbl old new)
        (let [n (pgds/non-null-count c s tbl old)]
          (if (zero? n)
            (pgds/drop-column! c s tbl old)
            (throw (ex-info (str "FOLD-COLLISION: " s "." tbl " carries BOTH " (pr-str old) " and "
                                 (pr-str new) ", and the pre-fold one holds " n " value(s). "
                                 "Merge them by hand — a rename would collide and a drop "
                                 "would delete data.")
                            {:error :fold-collision :table (str s "." tbl) :from old :to new
                             :rows n}))))))))

(defn ensure-table!
  ([c branch tbl columns] (ensure-table! c branch tbl columns nil))
  ([c branch tbl columns tag]
   (let [defs (col-defs columns)
         s    (pgns/own-schema branch)]
     (pgds/in-one-snapshot
      c
      (when-not (pgds/satisfied-by-main? c branch tbl defs)
        (pgds/fork-table! c branch tbl)
        (when (pgds/table-exists? c s tbl) (fold-folded-columns! c s tbl defs))
        (pgds/ensure-table! c branch tbl defs tag)
        (pgds/rebuild-overlay-view! c branch tbl)))
     tbl)))

(defn- ensure-tables*
  [c branch tables tag]
  (pgds/in-one-snapshot
   c
   (let [defs  (update-vals tables col-defs)
         K     (when (pgns/main? branch) (layout/catalog-for c branch (map str (keys tables))))
         s     (pgns/own-schema branch)
         done? (fn [t]
                 (and K
                      (empty? (folded-twins (map :name (layout/catalog-columns K s (str t)))
                                            (map :name (defs t))))
                      (empty? (layout/table-plan K branch (str t) (defs t) tag))))]
     (doseq [t (keys tables) :when (not (done? t))]
       (ensure-table! c branch t (tables t) tag)))))

(defn ensure-tables!
  [c branch tables tag]
  (when (seq tables)
    (ensure-tables* c branch tables tag)))

(defonce ^:private verified (java.util.concurrent.ConcurrentHashMap/newKeySet))

(defn- col-def [{:keys [attribute-name attribute-type]}]
  (str (kit-sql/dq (sql-name attribute-name)) " "
       (ctype/render (vt/col-type-of attribute-type) :postgres)))

(defn- existing-columns [c table]
  (into #{} (map :column_name)
        (jdbc/execute! c ["SELECT column_name FROM information_schema.columns
                           WHERE table_schema = current_schema() AND table_name = ?" table]
                       {:builder-fn rs/as-unqualified-maps})))

(defn- repair-kernel-table! [c table columns]
  (let [tbl  (kit-sql/dq table)
        have (existing-columns c table)]
    (if (empty? have)
      (let [attr-cols (str/join ", " (map col-def (or columns [])))]
        (jdbc/execute-one! c
                           [(str "CREATE TABLE IF NOT EXISTS " tbl " (" pk-col
                                 " VARCHAR(36) PRIMARY KEY"
                                 (when (seq attr-cols) (str ", " attr-cols)) ")")]))
      (doseq [col (or columns [])
              :when (not (contains? have (sql-name (:attribute-name col))))]
        (jdbc/execute-one! c [(str "ALTER TABLE " tbl " ADD COLUMN " (col-def col))])))))

(defn ensure-kernel-table!
  [ds rid columns]
  (let [table (dataset-table rid)
        k     [ds table (hash columns)]]
    (when-not (.contains ^java.util.Set verified k)
      (locking verified
        (when-not (.contains ^java.util.Set verified k)
          (with-open [c (jdbc/get-connection ds)]
            (repair-kernel-table! c table columns))
          (.add ^java.util.Set verified k))))
    nil))

(defn- index-validity
  [c table]
  (into {} (map (juxt :relname :indisvalid))
        (jdbc/execute! c [(str "SELECT ic.relname, i.indisvalid FROM pg_index i"
                               " JOIN pg_class ic ON ic.oid = i.indexrelid"
                               " WHERE i.indrelid = to_regclass(?)")
                          (kit-sql/dq table)]
                       {:builder-fn rs/as-unqualified-maps})))

(defn ensure-kernel-indexes!
  [ds rid indexes]
  (let [table (dataset-table rid)]
    (with-open [c (jdbc/get-connection ds)]
      (let [have (index-validity c table)]
        (doseq [[nm cols] indexes :when (not (true? (get have nm)))]
          (try
            (when (false? (get have nm))
              (jdbc/execute-one! c [(str "DROP INDEX CONCURRENTLY IF EXISTS " (kit-sql/dq nm))]))
            (jdbc/execute-one! c [(str "CREATE INDEX CONCURRENTLY IF NOT EXISTS " (kit-sql/dq nm)
                                       " ON " (kit-sql/dq table) " (" cols ")")])
            (catch java.sql.SQLException e
              (binding [*out* *err*]
                (println (str "kernel index " nm " on " table " not built: " (ex-message e)))))))))
    nil))
