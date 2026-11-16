(ns sutra.store.pg.dataset-store
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.kit.sql :as kit-sql]
            [sutra.kit.err :as err]
            [clojure.string :as str]
            [sutra.store.pg.landing :as landing]
            [sutra.datasets.landing]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.pg.sql :as sql]
            [sutra.store.pg.dataset-exec :as pgexec]
            [sutra.types.coltype :as ctype])
  (:import [java.sql Connection]))

(set! *warn-on-reflection* true)

(defprotocol DatasetTables
  (create-table!   [s scope table cols])
  (drop-table!     [s scope table])
  (add-column!     [s scope table col coltype])
  (retype-column!  [s scope table col from to])
  (rename-column!  [s scope table from to])
  (drop-column!    [s scope table col])
  (non-null-count  [s scope table col])
  (ensure-versioned! [s scope table])
  (columns         [s scope table])
  (column-types    [s scope table])
  (table-exists?   [s scope table])
  (qualify         [s scope table])
  (tables          [s branch mode]))

(defprotocol DatasetRows
  (insert-rows! [s scope table cols rows])
  (merge-rows!  [s scope table pk cols rows opts])
  (rows-differ?  [s scope table pk cols rows])
  (delete-pks!  [s scope table pk pks])
  (retain-pks!  [s scope table pk pks])
  (truncate!    [s scope table])
  (count-rows   [s scope table])
  (select-rows  [s scope table opts])
  (select-pks   [s scope table pk]))

(defprotocol DatasetVersions
  (snapshot          [s])
  (next-snapshot!    [s])
  (as-of             [s scope table snap])
  (table-delta       [s spec])
  (content-signature [s scope table])
  (had-rows-at?      [s scope table snap])
  (ever-written?     [s scope table])
  (restore-to!       [s scope table snap])
  (expire-through!   [s scope table through]))

(defprotocol DatasetBranches
  (read-scope     [s branch table])
  (write-scope    [s branch])
  (fork-table!    [s branch table])
  (settle-read-view! [s branch table])
  (create-branch! [s branch])
  (drop-branch!   [s branch])
  (retract!           [s branch table pk ids])
  (retract-all!       [s branch table])
  (retract-inherited! [s branch table]))

(defprotocol DatasetOps
  (capabilities [s])
  (table-health [s scope table])
  (store-health [s]))

(defprotocol StoreBackup
  (backup!           [s label])
  (backups           [s])
  (restore-store-to! [s snap] [s snap scopes]))

(defn- parse-types [m]
  (update-vals m #(or (ctype/parse %) %)))

(defn- render-pg [t]
  (cond
    (nil? t)    nil
    (string? t) t
    (map? t)    (ctype/render t :postgres)
    :else       (str t)))

(defn expose-input!
  [^Connection c alias {:keys [scope table ids bounded?]}]
  (let [a (str alias) t (str table)]
    (with-open [stmt (.createStatement c)]
      (.execute stmt (str "DROP VIEW IF EXISTS pg_temp." (sql/dq a)))
      (.execute stmt (str "CREATE TEMP VIEW " (sql/dq a)
                          " AS SELECT " (pgds/projection c (str scope) t "q0")
                          " FROM " (sql/dq (str scope)) "." (sql/dq t) " q0"
                          (when bounded?
                            (if (seq (:keys ids))
                              (str " WHERE " (sql/dq (:pk-col ids "internal_uuid")) " IN ("
                                   (str/join "," (map kit-sql/sq (:keys ids))) ")")
                              " WHERE false")))))
    true))

(defrecord PgDataset [^Connection conn]
  DatasetTables
  (create-table! [_ scope table cols]
    (pgds/create-table! conn scope table
                        (mapv #(update % :type render-pg) cols))
    (when-let [b (pgds/branch-of scope)] (pgds/rebuild-overlay-view! conn b table)))
  (drop-table! [_ scope table] (pgds/drop-table! conn scope table))
  (add-column! [_ scope table col t] (pgds/add-column! conn scope table col (render-pg t)))
  (retype-column! [_ scope table col from to]
    (pgds/retype-column! conn scope table col (render-pg from) (render-pg to)))
  (rename-column! [_ scope table from to] (pgds/rename-column! conn scope table from to))
  (drop-column! [_ scope table col] (pgds/drop-column! conn scope table col))
  (non-null-count [_ scope table col] (pgds/non-null-count conn scope table col))
  (ensure-versioned! [_ scope table] (pgds/ensure-versioned! conn scope table))
  (columns [_ scope table] (into #{} (remove layout/sys?) (pgds/columns conn scope table)))
  (column-types [_ scope table]
    (parse-types (apply dissoc (pgds/column-types conn scope table) layout/sys?)))
  (table-exists? [_ scope table] (pgds/table-exists? conn scope table))
  (qualify [_ scope table] (str (sql/dq (str scope)) "." (sql/dq (str table))))
  (tables [_ branch mode]
    (case mode
      :own     (pgds/tables-own conn branch)
      :visible (pgds/tables-visible conn branch)))

  DatasetRows
  (insert-rows! [_ scope table cols rows] (pgds/insert-rows! conn scope table cols rows))
  (merge-rows! [_ scope table pk cols rows opts]
    (pgds/merge-rows! conn scope table pk cols rows opts))
  (rows-differ? [_ scope table pk cols rows]
    (pgds/rows-differ? conn scope table pk cols rows))
  (delete-pks! [_ scope table pk pks] (pgds/delete-pks! conn scope table pk pks))
  (retain-pks! [_ scope table pk pks] (pgds/retain-pks! conn scope table pk pks))
  (truncate! [s scope table]
    (jdbc/execute! conn [(str "DELETE FROM " (qualify s scope table))]))
  (count-rows [_ scope table]
    (long (or (:n (jdbc/execute-one!
                   conn [(str "SELECT count(*) AS n FROM "
                              (sql/dq (str scope)) "." (sql/dq (str table)))]
                   {:builder-fn rs/as-unqualified-lower-maps}))
              0)))
  (select-rows [s scope table {:keys [as-of ids pk-col keys] :or {keys :lower}}]
    (let [{rel-sql :sql rel-ps :params} (when as-of (pgds/as-of conn scope table as-of))
          rel  (if as-of
                 (str "(" rel-sql ") q0")
                 (str (qualify s scope table) " q0"))
          sql  (str "SELECT " (pgds/projection conn scope table "q0") " FROM " rel
                    (when ids
                      (str " WHERE q0." (sql/dq (str pk-col)) " IN ("
                           (str/join "," (repeat (count ids) "?")) ")")))
          bldr (if (= :kebab keys) rs/as-unqualified-kebab-maps rs/as-unqualified-lower-maps)]
      (mapv #(update-vals % pgexec/->jdbc-value)
            (jdbc/execute! conn (-> [sql] (into rel-ps) (into ids)) {:builder-fn bldr}))))
  (select-pks [s scope table pk]
    (mapv (comp str first vals)
          (jdbc/execute! conn
                         [(str "SELECT " (sql/dq (str pk)) " FROM " (qualify s scope table))])))

  DatasetVersions
  (snapshot [_] (pgds/current-snapshot conn))
  (next-snapshot! [_] (pgds/next-snapshot! conn))
  (as-of [_ scope table snap] (pgds/as-of conn scope table snap))
  (table-delta [_ spec] (pgds/table-delta conn spec))
  (content-signature [_ scope table] (pgds/content-signature conn scope table))
  (had-rows-at? [_ scope table snap] (pgds/had-rows-at? conn scope table snap))
  (ever-written? [_ scope table] (pgds/ever-written? conn scope table))
  (restore-to! [_ scope table snap] (pgds/restore-to! conn scope table snap))
  (expire-through! [_ scope table through] (pgds/expire-through! conn scope table through))

  DatasetBranches
  (read-scope [_ branch table] (pgds/resolve-schema conn branch table))
  (write-scope [_ branch] (pgns/own-schema branch))
  (fork-table! [_ branch table] (pgds/fork-table! conn branch (str table)))
  (settle-read-view! [_ branch table] (pgds/rebuild-overlay-view! conn branch (str table)))
  (create-branch! [_ branch] (pgds/create-branch! conn branch))
  (drop-branch! [_ branch] (pgds/drop-branch! conn branch))
  (retract! [s branch table pk ids]
    (let [own (pgns/own-schema branch)]
      (when (table-exists? s own table) (delete-pks! s own table pk ids))
      (pgds/tomb! conn own table ids)))
  (retract-all! [s branch table]
    (let [own (pgns/own-schema branch)]
      (pgds/detach! conn own table)
      (when (table-exists? s own table) (truncate! s own table))))
  (retract-inherited! [_ branch table]
    (pgds/detach! conn (pgns/own-schema branch) table))

  DatasetOps
  (capabilities [_] #{:sql :time-travel :snapshot-reserve :rollback :backup
                      :branches :concurrent-writers :distinct-on})
  (table-health [_ scope table] (pgds/version-integrity conn scope table))
  (store-health [_] (pgds/vacuum-health conn))

  StoreBackup
  (backup! [_ label] (pgds/backup! conn label))
  (backups [_] (pgds/backups conn))
  (restore-store-to! [_ snap] (pgds/restore-store-to! conn snap))
  (restore-store-to! [_ snap scopes] (pgds/restore-store-to! conn snap scopes)))

(defn pg-store [^Connection c] (->PgDataset c))

(defn with-store*
  [ds f]
  (pgds/with-store* ds (fn [^Connection c] (f (->PgDataset c)))))

(defn supports? [s cap] (contains? (capabilities s) cap))

(defn- conflicting-columns
  [store scope table rows]
  (let [stored (column-types store scope table)]
    (into []
          (keep (fn [[col t]]
                  (when-let [have (get stored (str col))]
                    (let [want (landing/storable t)]
                      (when-not (ctype/holds? have want)
                        {:column (str col) :stored have :emitted want})))))
          (landing/col-types rows))))

(def ^:private mismatch-sql-states
  #{"22P02"
    "22003"
    "22001"
    "22007"
    "22008"
    "42804"
    "42703"})

(defn- mismatch-cause?
  [^Exception e]
  (boolean
   (err/cause (fn [^Throwable t]
                (or (and (instance? java.sql.SQLException t)
                         (contains? mismatch-sql-states (.getSQLState ^java.sql.SQLException t)))
                    (let [m (str (.getMessage t))]
                      (or (str/includes? m "Conversion Error")
                          (str/includes? m "Binder Error")
                          (str/includes? m "Invalid Input Error")))))
              e)))

(defn- schema-mismatch
  [store scope table rows ^Exception e]
  (let [msg (str (.getMessage e))]
    (when (mismatch-cause? e)
      (let [cols (conflicting-columns store scope table rows)]
        (anomaly/ex
         (anomaly/refused
          :unprocessable
          (str "SCHEMA-MISMATCH: a write to " scope "." table " carries value(s) its "
               "stored column(s) cannot hold"
               (when (seq cols)
                 (str " — " (str/join ", " (map (fn [{:keys [column stored emitted]}]
                                                  (str column " is " (ctype/label stored)
                                                       ", emitted " (ctype/label emitted)))
                                                cols))))
               ". Declare the new type on the dataset so the install retypes it, or "
               "fix the producer. Underlying: " msg)
          {:error :schema-mismatch :schema scope :table table :columns cols})
         e)))))

(defn- landed-types
  [pk inferred]
  (-> inferred
      (assoc pk (ctype/col-type :varchar))
      (update-vals landing/storable)))

(defn- missing-columns
  [store scope table pk inferred]
  (if-not (table-exists? store scope table)
    [pk]
    (let [have (columns store scope table)]
      (mapv (comp str key)
            (remove (fn [[k _]] (contains? have (str k)))
                    (dissoc (landed-types pk @inferred) pk))))))

(defn- ensure-columns!
  [store scope table pk declared? inferred]
  (let [inferred @inferred
        types    (landed-types pk inferred)]
    (doseq [n (landing/narrowed inferred)]
      (binding [*out* *err*]
        (println (str "NOTE: " scope "." table "." (:column n) " landed as "
                      (ctype/label (:stored n)) " — the batch carried "
                      (ctype/label (:inferred n)) " and the store keeps the integer "
                      "half. Declare the column to control this."))))
    (if-not (table-exists? store scope table)
      (do
        (when declared?
          (binding [*out* *err*]
            (println (str "NOTE: landing created declared table " scope "." table
                          " from row types — the schema install should own it; "
                          "its column types may be coarser than declared."))))
        (create-table! store scope table
                       (into [{:name pk :type (ctype/col-type :varchar) :not-null? true}]
                             (map (fn [[k t]] {:name k :type t}))
                             (dissoc types pk)))
        [])
      (let [have    (columns store scope table)
            missing (remove (fn [[k _]] (contains? have (str k))) (dissoc types pk))]
        (doseq [[k t] missing] (add-column! store scope table k t))
        (mapv (comp str key) missing)))))

(defn- would-change?
  [store branch table pk rows inferred]
  (let [rs (read-scope store branch table)]
    (or (not (table-exists? store rs table))
        (seq (missing-columns store rs table pk inferred))
        (try
          (boolean
           (some (fn [[cols rs*]] (rows-differ? store rs table pk cols rs*))
                 (landing/shape-groups pk rows)))
          (catch java.sql.SQLException _ true)))))

(defn- inherits-whole-table?
  [store branch table]
  (and (not (table-exists? store (write-scope store branch) table))
       (table-exists? store (read-scope store branch table) table)))

(defn- refresh-would-change?
  [store branch table pk rows inferred]
  (or (not= (count rows) (count-rows store (read-scope store branch table) table))
      (would-change? store branch table pk rows inferred)))

(defn- naming-every-column
  [store branch table pk rows]
  (let [rs (read-scope store branch table)]
    (if (table-exists? store rs table)
      (let [blank (into {} (for [c (columns store rs table) :when (not= (str c) (str pk))]
                             [(str c) nil]))]
        (mapv #(merge blank %) rows))
      rows)))

(defn- run-landing!
  [store branch table {:keys [rows pk declared? inferred scope]} plan]
  (reduce
   (fn [out [step cols step-rows opts]]
     (case step
       :fork              (do (fork-table! store branch table) out)
       :retract-inherited (do (retract-inherited! store branch table) out)
       :ensure-columns    (assoc out :added (ensure-columns! store scope table pk declared?
                                                             inferred))
       :merge
       (try
         (merge-rows! store scope table pk cols step-rows
                      (assoc opts :read-scope (let [rs (read-scope store branch table)]
                                                (if (table-exists? store rs table) rs scope))))
         out
         (catch Exception e
           (throw (or (schema-mismatch store scope table rows e) e))))
       :retain            (assoc out :retained
                                 (retain-pks! store scope table pk (map #(get % pk) rows)))))
   {:added []}
   plan))

(defn- land-rows!
  [store branch table {:keys [rows pk replace? fork? declared? whole-row?]}]
  (let [fork?  (if (some? fork?) fork? (not replace?))
        rows   (cond->> (landing/dedupe-by-pk pk rows)
                 whole-row? (naming-every-column store branch table pk))
        inferred (delay (landing/col-types rows))
        scope  (write-scope store branch)
        _ (settle-read-view! store branch table)]
    (if (or (and fork? (not (would-change? store branch table pk rows inferred)))
            (and replace? (not fork?)
                 (inherits-whole-table? store branch table)
                 (not (refresh-would-change? store branch table pk rows inferred))))
      {:version (snapshot store) :rows (count rows) :columns-added [] :unchanged? true}
      (let [own-alone? (and replace?
                            (= (str scope) (str (read-scope store branch table)))
                            (table-exists? store scope table))
            {:keys [added retained]}
            (run-landing! store branch table
                          {:rows rows :pk pk :declared? declared? :inferred inferred
                           :scope scope}
                          (sutra.datasets.landing/plan
                           {:fork? fork? :replace? replace?
                            :groups (landing/shape-groups pk rows)}))
            {:keys [changed went] :or {changed 0 went 0}} (:merge @pgds/*landing*)]
        (cond-> {:version (snapshot store) :rows (count rows) :columns-added added}
          (and own-alone? (empty? added) (zero? (+ changed went (long (or retained 0)))))
          (assoc :unchanged? true))))))

(defn- land!
  [store branch table opts]
  (binding [pgds/*landing* (atom {})]
    (let [t0 (System/nanoTime)
          r  (land-rows! store branch table opts)]
      (assoc r :landing (assoc @pgds/*landing*
                               :total-ms (quot (- (System/nanoTime) t0) 1000000)
                               :rows (:rows r)
                               :unchanged (boolean (:unchanged? r)))))))

(def ^:private transaction-types
  #{:snapshot :update :delete})

(defn commit!
  [store branch table {:keys [type rows pk ids all? fork? declared? whole-row?]}]
  (when-not (transaction-types type)
    (anomaly/raise! :incorrect
                    (str "no such transaction type: " (pr-str type))
                    {:given type :known transaction-types}))
  (if (= :delete type)
    (do
      (fork-table! store branch table)
      (if all?
        (retract-all! store branch table)
        (retract! store branch table pk ids))
      {:transaction (snapshot store) :rows (if all? nil (count ids)) :columns-added []})
    (land! store branch table {:rows rows :pk pk
                               :replace?  (= :snapshot type)
                               :fork?     fork?
                               :declared? declared?
                               :whole-row? whole-row?})))

(defn rows
  [store v opts]
  (select-rows store (read-scope store (:branch v) (:table v)) (:table v)
               (assoc opts :as-of (:at v))))

