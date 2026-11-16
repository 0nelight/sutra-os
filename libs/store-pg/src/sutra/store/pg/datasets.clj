(ns sutra.store.pg.datasets
  (:require [sutra.kit.anomaly :as anomaly]
            [sutra.store.pg.tables :as pg-tables]
            [clojure.string :as str]
            [sutra.datasets.api :as datasets]
            [sutra.kit.read-only-sql :as read-only]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.api :as api]
            [sutra.store.records :as records]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.pg.sql :as sql]
            [sutra.store.pg.dataset-exec :as pgexec]
            [sutra.store.pg.dataset-store :as dstore])
  (:import (java.sql Connection)))

(set! *warn-on-reflection* true)

(defn- table [addr] (str (:dataset addr)))

(defn- pk [addr] (or (:pk addr) "id"))

(defn- at-of [addr opts] (or (:at opts) (:at addr)))

(defn- refuse! [what data]
  (api/refuse! :as-of-unsupported what data))

(defn- resolve-overlay
  [b key-fn rows]
  (let [win (reduce (fn [m r]
                      (let [k (key-fn r)]
                        (if (or (not (contains? m k)) (= b (:branch r)))
                          (assoc m k r)
                          m)))
                    {} rows)]
    (filterv #(identical? % (get win (key-fn %))) rows)))

(defn- control-rows
  [control addr at]
  (when at (refuse! (str "dataset " (:rid addr) " lives in control tables: no point-in-time read")
                    {:dataset (:rid addr) :at at}))
  (let [{:keys [model key]} (:control addr)
        b (or (some-> (:branch addr) str) records/trunk)
        [scope params] (case model
                         :event [" WHERE branch = ?" [b]]
                         :state [" WHERE branch IN (?, ?)" [b records/trunk]])
        raw (jdbc/execute! control
                           (into [(str "SELECT * FROM \"" (table addr) "\"" scope)] params)
                           {:builder-fn rs/as-unqualified-lower-maps})]
    (if (and key (= :state model))
      (resolve-overlay b (apply juxt (map #(keyword (str/lower-case (name %))) key)) raw)
      raw)))

(defn- on-table
  [ds addr f absent]
  (dstore/with-store* ds
    (fn [s]
      (let [t  (table addr)
            sc (dstore/read-scope s (:branch addr) t)]
        (if (dstore/table-exists? s sc t) (f s sc t) (absent))))))

(defn- select-rows [s sc t at ids pk-col]
  (dstore/select-rows s sc t (cond-> {:pk-col pk-col :keys :lower}
                               at  (assoc :as-of at)
                               ids (assoc :ids (mapv str ids)))))

(defn- write-versions
  [^Connection c sc t]
  (let [sc   (if (pgds/overlay-read-schema? sc) (pgns/own-schema (pgds/branch-of sc)) sc)
        hist (str t "__history")
        q    (fn [tbl] (str "SELECT sys_from AS v FROM " (sql/dq sc) "." (sql/dq tbl)
                            " UNION SELECT sys_to AS v FROM " (sql/dq sc) "." (sql/dq tbl)
                            " WHERE sys_to IS NOT NULL"))
        sql  (str (q t) (when (pgds/table-exists? c sc hist) (str " UNION " (q hist)))
                  " ORDER BY v DESC")]
    (into [] (keep (comp #(some-> % long) :v))
          (jdbc/execute! c [sql] {:builder-fn rs/as-unqualified-lower-maps}))))

(defn- log-transaction!
  [^Connection c schema t version kind rows]
  (jdbc/execute! c [(str "INSERT INTO public.dataset_transactions"
                         " (schema_name, table_name, version, kind, rows) VALUES (?, ?, ?, ?, ?)"
                         " ON CONFLICT DO NOTHING")
                    (str schema) (str t) (long version) (name kind)
                    (some-> rows long)]))

(defn- transactions
  ([c schema t] (transactions c schema t nil))
  ([^Connection c schema t limit]
   (mapv (fn [{:keys [version kind rows committed_at]}]
           (cond-> {:version (long version) :kind (keyword kind)
                    :at (java.util.Date. (.getTime ^java.sql.Timestamp committed_at))}
             (some? rows) (assoc :rows (long rows))))
         (jdbc/execute! c [(str "SELECT version, kind, rows, committed_at"
                                " FROM public.dataset_transactions"
                                " WHERE schema_name = ? AND table_name = ? ORDER BY version DESC"
                                (when limit (str " LIMIT " (long limit))))
                           (str schema) (str t)]
                        {:builder-fn rs/as-unqualified-lower-maps}))))

(defn- replaced-in?
  [^Connection c schema t lo hi]
  (some? (jdbc/execute-one! c [(str "SELECT 1 FROM public.dataset_transactions"
                                    " WHERE schema_name = ? AND table_name = ? AND kind = 'snapshot'"
                                    " AND version > ? AND version <= ? LIMIT 1")
                               (str schema) (str t) (long lo) (long hi)])))

(defonce ^:private unlogged-newest (atom {}))

(defn- newest-write
  [^Connection c sc t all-history]
  (if-let [tx (first (transactions c sc t 1))]
    [tx]
    (let [k [(str sc) (str t)]]
      (or (get @unlogged-newest k)
          (let [r (vec (take 1 (all-history)))]
            (swap! unlogged-newest assoc k r)
            r)))))

(defn- log-began
  [^Connection c]
  (some-> (:t (jdbc/execute-one! c ["SELECT min(committed_at) AS t FROM public.dataset_transactions"]
                                 {:builder-fn rs/as-unqualified-lower-maps}))
          (#(java.util.Date. (.getTime ^java.sql.Timestamp %)))))

(defn- marked-horizon
  ([c schemas t] (marked-horizon c schemas t nil))
  ([^Connection c schemas t due]
   (long (or (:h (jdbc/execute-one!
                  c (into [(str "SELECT max(horizon) AS h FROM public.dataset_retention_marks"
                                " WHERE table_name = ? AND schema_name = ANY (?)"
                                (when due " AND marked_at <= ?"))
                           (str t) (into-array String (map str schemas))]
                          (when due [(java.sql.Timestamp. (long due))]))
                  {:builder-fn rs/as-unqualified-lower-maps}))
             0))))

(defn- horizon-schemas
  [b]
  (distinct [(pgns/own-schema b) (pgns/own-schema "main")]))

(defn- mark!
  [^Connection c s t tag]
  (let [cur (:d (first (jdbc/execute! c
                                      [(str
                                        "SELECT obj_description(to_regclass(?), 'pg_class') AS d")
                                       (str (sql/dq s) "." (sql/dq t))]
                                      {:builder-fn rs/as-unqualified-lower-maps})))]
    (when (and (some? (:r (jdbc/execute-one! c
                                             [(str "SELECT to_regclass(?) AS r")
                                              (str (sql/dq s) "." (sql/dq t))]
                                             {:builder-fn rs/as-unqualified-lower-maps})))
               (not= cur "sutra:store"))
      (jdbc/execute! c [(str "COMMENT ON TABLE " (sql/dq s) "." (sql/dq t) " IS '" tag "'")]))))

(defn- marked
  [^Connection c s tag]
  (into #{} (map :t)
        (jdbc/execute! c [(str "SELECT cl.relname AS t FROM pg_class cl"
                               " JOIN pg_namespace n ON n.oid = cl.relnamespace"
                               " JOIN pg_description d ON d.objoid = cl.oid AND d.objsubid = 0"
                               " WHERE n.nspname = ? AND d.description = ?") s tag]
                       {:builder-fn rs/as-unqualified-lower-maps})))

(def ^:private query-role
  (delay (let [r (str/trim (or (System/getenv "SUTRA_PG_QUERY_ROLE") "sutra_query"))]
           (when-not (#{"" "none"} r) r))))

(def ^:private query-seconds
  (delay (or (some-> (System/getenv "SUTRA_PG_QUERY_SECONDS") str/trim parse-long) 120)))

(defonce ^:private ready-roles (atom #{}))

(defn- ensure-query-role!
  [ds role]
  (when-not (@ready-roles [ds role])
    (pgds/with-conn [c ds]
      (let [found #(jdbc/execute-one! c ["SELECT rolsuper FROM pg_roles WHERE rolname = ?" role]
                                      {:builder-fn rs/as-unqualified-lower-maps})]
        (when-not (found)
          (try (jdbc/execute! c [(str "CREATE ROLE " (sql/dq role) " NOLOGIN")])
               (catch java.sql.SQLException e
                 (when-not (= "42710" (.getSQLState e)) (throw e)))))
        (when (:rolsuper (found))
          (throw (ex-info (str "query role " role " is a superuser") {:role role})))))
    (swap! ready-roles conj [ds role])))

(defn- seal!
  [^Connection c aliases]
  (let [role @query-role]
    (with-open [stmt (.createStatement c)]
      (when role
        (doseq [a aliases]
          (.execute stmt (str "GRANT SELECT ON pg_temp." (sql/dq a) " TO " (sql/dq role)))))
      (.execute stmt "SET TRANSACTION READ ONLY")
      (.execute stmt (str "SET LOCAL statement_timeout = " (* 1000 (long @query-seconds))))
      (.execute stmt "SET LOCAL lock_timeout = 10000")
      (when role (.execute stmt (str "SET LOCAL ROLE " (sql/dq role)))))))

(defn- run-query
  [ds {:keys [inputs sql params explain?]}]
  (let [rendered (read-only/assert-read-only-sql! (pgexec/render-sql sql) :postgres)]
    (some->> @query-role (ensure-query-role! ds))
    (with-open [^Connection c (pgds/open ds)]
      (.setAutoCommit c false)
      (try
        (dstore/with-store* ds
          (fn [s]
            (doseq [[alias addr] inputs]
              (if (datasets/shape? addr)
                (pg-tables/create-temp-shape! c alias (:columns addr))
                (let [t  (table addr)
                      sc (dstore/read-scope s (:branch addr) t)]
                  (when-not (dstore/table-exists? s sc t)
                    (throw (ex-info (str "query input " alias " (" t ") has no table")
                                    {:datasets/missing-input alias})))
                  (dstore/expose-input! c alias
                                        (cond-> {:scope sc :table t}
                                          (contains? addr :ids)
                                          (assoc :bounded? true :ids
                                                 {:pk-col (pk addr)
                                                  :keys (vec (:ids addr))}))))))))
        (seal! c (map str (keys inputs)))
        (if explain?
          (do (jdbc/execute! c (into [(str "EXPLAIN " rendered)] (pgexec/->jdbc-params params)))
              [])
          (mapv #(update-vals % pgexec/->jdbc-value)
                (jdbc/execute! c (into [rendered] (pgexec/->jdbc-params params))
                               {:builder-fn rs/as-unqualified-lower-maps})))
        (finally
          (try (.rollback c) (catch java.sql.SQLException _ nil))
          (try (with-open [stmt (.createStatement c)] (.execute stmt "DISCARD TEMP"))
               (catch java.sql.SQLException _ nil)))))))

(defrecord PgDatasets [ds control]
  datasets/Datasets
  (-profile [_] {:query {:dialects #{:postgres}} :landing #{:merge :diff}})
  (-layout! [_ branch {:keys [old new drop-unclaimed? head]}]
    (pgds/with-conn [c ds]
      (pgds/in-one-snapshot
       c
       (jdbc/execute! c
                      ["SELECT pg_advisory_xact_lock(hashtext(?))"
                       (str "sutra:datasets-layout:" branch)])
       (let [new (if head (head) new)]
         (pg-tables/ensure-tables! c branch
                                   (into {} (remove (fn [[t cols]] (= cols (get old t)))) new)
                                   "sutra:datasets")
         (when drop-unclaimed?
           (doseq [t (remove (set (keys new)) (keys old))]
             (pgds/drop-table! c (pgns/own-schema branch) t)))))))
  (-list [_ branch {:keys [prefix]}]
    (dstore/with-store* ds
      (fn [s] (vec (sort (filter #(or (nil? prefix) (str/starts-with? (str %) prefix))
                                 (map str (dstore/tables s branch :visible))))))))
  (-inspect [_ branch decls]
    (pgds/with-conn [c ds]
      (let [own   (pgns/own-schema branch)
            names (set (keys decls))
            mine  (marked c own "sutra:datasets")
            have? (into #{} (map :t)
                        (jdbc/execute! c [(str "SELECT cl.relname AS t FROM pg_class cl"
                                               " JOIN pg_namespace n ON n.oid = cl.relnamespace"
                                               " WHERE n.nspname = ? AND cl.relname = ANY (?)")
                                          own (into-array String (map str names))]
                                       {:builder-fn rs/as-unqualified-lower-maps}))]
        {:missing (vec (sort (remove have? names)))
         :orphans (vec (for [t (sort mine) :when (not (contains? names t))]
                         {:name t
                          :rows
                          (long
                           (:n
                            (jdbc/execute-one! c
                                               [(str "SELECT count(*) AS n FROM " (sql/dq own)
                                                     "." (sql/dq t))]
                                               {:builder-fn
                                                rs/as-unqualified-lower-maps})))}))})))
  (-write! [_ addr {:keys [kind rows ids all?] :as tx}]
    (dstore/with-store* ds
      (fn [s]
        (when-let [cols (:columns tx)]
          (pgds/with-conn [c ds]
            (pg-tables/ensure-table! c (:branch addr) (table addr) cols
                                     (if (:scratch? addr) "sutra:scratch" "sutra:datasets"))))
        (let [own    (dstore/write-scope s (:branch addr))
              fresh? (not (dstore/table-exists? s own (table addr)))
              r (dstore/commit! s (:branch addr) (table addr)
                                (cond-> {:type kind
                                         :pk (or (:pk tx) (pk addr))}
                                  (not= :delete
                                        kind)
                                  (assoc :rows
                                         (mapv #(-> % (update-keys name)
                                                    (update-vals sql/->sql-param)) rows))
                                  ids  (assoc :ids ids)
                                  all? (assoc :all? true)
                                  (:whole-row? tx) (assoc :whole-row? true)
                                  (contains? tx :fork?) (assoc :fork? (:fork? tx))))]
          (when fresh?
            (pgds/with-conn [c ds]
              (mark! c own (table addr) (if (:scratch? addr) "sutra:scratch" "sutra:datasets"))))
          (let [v (or (:version r) (:transaction r))]
            (when-not (or (:unchanged? r) (:scratch? addr))
              (pgds/with-conn [c ds]
                (log-transaction! c (pgns/own-schema (:branch addr)) (table addr) v kind
                                  (:rows r))))
            (cond-> {:version v :rows (:rows r)}
              (:landing r) (assoc :landing (:landing r))))))))
  (-read [_ addr {:keys [ids columns limit] :as opts}]
    (let [at   (at-of addr opts)
          rows (if (:control addr)
                 (cond->> (control-rows control addr at)
                   ids (filter (comp (set (map str ids)) str #(get % (keyword (pk addr))))))
                 (on-table ds addr
                           (fn [s sc t] (select-rows s sc t at ids (pk addr)))
                           (fn []
                             (when at
                               (refuse! (str "dataset " (table addr) " has no table") {:at at}))
                             [])))]
      (cond->> rows
        columns (mapv #(select-keys % (map (comp keyword str/lower-case) columns)))
        limit   (take limit)
        :always vec)))
  (-columns [_ addr]
    (on-table ds addr (fn [s sc t] (dstore/column-types s sc t)) (constantly {})))
  (-count [this addr opts]
    (if (or (at-of addr opts) (:control addr))
      (count (datasets/-read this addr opts))
      (on-table ds addr (fn [s sc t] (dstore/count-rows s sc t)) (constantly 0))))
  (-version [_ _] (pgds/with-store* ds (fn [^Connection c] (pgds/current-snapshot c))))
  (-delta [_ addr lo hi]
    (on-table ds addr
              (fn [s sc t]
                (let [d (dstore/table-delta s
                                            {:schema sc :table t :pk-col (pk addr) :low lo
                                             :high hi})]
                  (when (:no-delta d)
                    (refuse! (str "no delta of " t " in (" lo ", " hi "]: " (name (:no-delta d)))
                             {:reason (:no-delta d) :lo lo :hi hi}))
                  {:added (set (:added d)) :removed (set (:removed d)) :changed (set (:changed d))
                   :replaced? (pgds/with-conn [c ds]
                                (replaced-in? c (pgns/own-schema (:branch addr)) (table addr)
                                              lo hi))}))
              (fn [] (refuse! (str "dataset " (table addr) " has no table") {:lo lo :hi hi}))))
  (-horizon [_ addr]
    (if (:control addr)
      0
      (pgds/with-conn [c ds]
        (marked-horizon c (horizon-schemas (:branch addr)) (table addr)))))
  (-retire-below! [_ addr v]
    (if (:control addr)
      {:horizon 0}
      (pgds/with-conn [c ds]
        (let [own (pgns/own-schema (:branch addr)) t (table addr)]
          (when (> (long v) (marked-horizon c [own] t))
            (jdbc/execute! c [(str "INSERT INTO public.dataset_retention_marks"
                                   " (schema_name, table_name, horizon) VALUES (?, ?, ?)"
                                   " ON CONFLICT DO NOTHING")
                              own (str t) (long v)]))
          {:horizon (marked-horizon c (horizon-schemas (:branch addr)) t)}))))
  (-sweep! [_ addr {:keys [grace-ms] :or {grace-ms 0}}]
    (if (:control addr)
      {:swept 0 :rows 0}
      (pgds/with-conn [c ds]
        (let [own    (pgns/own-schema (:branch addr))
              t      (table addr)
              target (marked-horizon c [own] t (- (System/currentTimeMillis) (long grace-ms)))
              swept  (long (or (:h (jdbc/execute-one!
                                    c [(str "SELECT max(horizon) AS h"
                                            " FROM public.dataset_retention_marks"
                                            " WHERE schema_name = ? AND table_name = ?"
                                            " AND swept_at IS NOT NULL") own (str t)]
                                    {:builder-fn rs/as-unqualified-lower-maps}))
                               0))]
          (if (<= target swept)
            {:swept 0 :rows 0}
            (let [n (if (pgds/table-exists? c own t) (pgds/sweep-history! c own t target) 0)]
              (jdbc/execute! c [(str "UPDATE public.dataset_retention_marks SET swept_at = now()"
                                     " WHERE schema_name = ? AND table_name = ? AND horizon = ?")
                                own (str t) target])
              (jdbc/execute! c [(str "DELETE FROM public.dataset_retention_marks"
                                     " WHERE schema_name = ? AND table_name = ? AND horizon < ?")
                                own (str t) target])
              {:swept target :rows (long n)}))))))
  (-history [_ addr {:keys [before limit typed?]}]
    (if typed?
      (pgds/with-conn [c ds]
        (let [own  (pgns/own-schema (:branch addr))
              t    (table addr)
              txns (transactions c own t)
              v0   (some-> (peek txns) :version)]
          (cond->> (cond-> txns
                     (and v0 (pgds/past-before? c own t v0))
                     (conj {:version (dec (long v0)) :at (log-began c)}))
            before (remove #(>= (:version %) before))
            limit  (take limit)
            true   vec)))
      (on-table ds addr
                (fn [_ sc t]
                  (pgds/with-store* ds
                    (fn [^Connection c]
                      (let [all (fn []
                                  (let [overlay (some-> (pgds/content-signature c sc t)
                                                        parse-long)
                                        txns    (transactions c (pgns/own-schema (:branch addr))
                                                              t)
                                        first-v (some-> (peek txns) :version)
                                        began   (delay (log-began c))
                                        stamped (into []
                                                      (comp (remove (set (map :version txns)))
                                                            (map (fn [v]
                                                                   (cond-> {:version v}
                                                                     (and first-v (< v first-v))
                                                                     (assoc :at @began)))))
                                                      (cond-> (write-versions c sc t)
                                                        overlay (conj overlay)))]
                                    (->> (concat txns stamped)
                                         (sort-by :version >)
                                         (remove #(and before (>= (:version %) before))))))]
                        (if (and (= 1 limit) (nil? before)
                                 (not (:scratch? addr)) (not (:control addr))
                                 (not (pgds/overlay-read-schema? sc))
                                 (= (str sc) (str (pgns/own-schema (:branch addr)))))
                          (newest-write c sc t all)
                          (cond->> (all)
                            limit (take limit)
                            true  vec))))))
                (constantly []))))
  (-query [_ plan] (run-query ds plan))
  (-restore! [_ addr v]
    (on-table ds addr
              (fn [s _ t]
                (dstore/fork-table! s (:branch addr) t)
                (let [n  (dstore/restore-to! s (dstore/write-scope s (:branch addr)) t v)
                      v' (dstore/snapshot s)]
                  (pgds/with-conn [c ds]
                    (log-transaction! c (pgns/own-schema (:branch addr)) t v' :snapshot n))
                  {:version v'}))
              (fn []
                (refuse! (str "dataset " (table addr) " has no table — no version to restore")
                         {:at v}))))
  (-expire! [this addr through]
    (when-not (pgns/main? (:branch addr))
      (refuse! "retention expires rows on main only" {:branch (:branch addr)}))
    (if (:control addr)
      {:version (datasets/-version this addr) :rows 0}
      (on-table ds addr
                (fn [s _ t]
                  (let [own (pgns/own-schema (:branch addr))
                        n   (long (or (dstore/expire-through! s own t through) 0))
                        v   (dstore/snapshot s)]
                    (when (pos? n)
                      (pgds/with-conn [c ds] (log-transaction! c own t v :delete n)))
                    {:version v :rows n}))
                (fn [] {:version (datasets/-version this addr) :rows 0}))))
  (-fork! [_ _ _] nil)
  (-drop! [_ _] nil)
  (-drop-dataset! [_ addr]
    (pgds/with-conn [c ds]
      (let [own (pgns/own-schema (:branch addr))]
        (pgds/drop-table! c own (table addr))
        (doseq [books ["public.dataset_transactions" "public.dataset_retention_marks"]]
          (jdbc/execute! c [(str "DELETE FROM " books " WHERE schema_name = ? AND table_name = ?")
                            own (table addr)]))))
    nil))

(defn restore-points
  [ds]
  (dstore/with-store* ds (fn [s] {:points (dstore/backups s) :snapshot (dstore/snapshot s)})))

(defn take-restore-point!
  [ds label]
  (dstore/with-store* ds (fn [s] (dstore/backup! s label))))

(defn roll-back-to-point!
  [ds id]
  (dstore/with-store* ds
    (fn [s]
      (let [id    (long id)
            point (first (filter #(= id (long (:id %))) (dstore/backups s)))]
        (when-not point
          (anomaly/raise! :incorrect
                          (str "no restore point " id) {:id id}))
        {:point point :moved (dstore/restore-store-to! s (:snapshot point))}))))

(def module
  {:name :datasets-pg
   :requires [:pg/ds]
   :opens {:datasets/db
           (fn [_ {pools :pg/ds}] (fn [lane] (->PgDatasets (pools lane) (pools nil))))
           :datasets/restore-points
           (fn [_ {pools :pg/ds}]
             {:list       #(restore-points (pools nil))
              :take!      #(take-restore-point! (pools nil) %)
              :roll-back! #(roll-back-to-point! (pools nil) %)})}})
