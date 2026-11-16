(ns sutra.store.pg.token-index
  (:require [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.pg.sql :as sql]
            [sutra.store.pg.tokens :as tokens]))

(set! *warn-on-reflection* true)

(def ^:private as-maps {:builder-fn rs/as-unqualified-maps})

(defn- catalog
  [c table]
  (let [rel (str "main." (sql/dq table))]
    (when (:has (jdbc/execute-one! c ["SELECT to_regclass(?) IS NOT NULL AS has" rel] as-maps))
      (into {}
            (map (juxt :relname :indisvalid))
            (jdbc/execute! c [(str "SELECT ic.relname, i.indisvalid FROM pg_index i"
                                   " JOIN pg_class ic ON ic.oid = i.indexrelid"
                                   " WHERE i.indrelid = to_regclass(?)")
                              rel]
                           as-maps)))))

(defn- analyzed?
  [c index]
  (:has (jdbc/execute-one! c [(str "SELECT EXISTS (SELECT 1 FROM pg_stats WHERE schemaname = 'main'"
                                   " AND tablename = ?) AS has")
                              index]
                           as-maps)))

(defn- with-lock
  [c nm f]
  (let [lock (str "hashtextextended('" nm "', 0)")]
    (when (:ok (jdbc/execute-one! c [(str "SELECT pg_try_advisory_lock(" lock ") AS ok")] as-maps))
      (try (f)
           (finally (jdbc/execute-one! c [(str "SELECT pg_advisory_unlock(" lock ")")]))))))

(defn- build!
  [ds table attrs]
  (let [wanted (tokens/wanted-index table attrs)]
    (with-open [c (jdbc/get-connection ds)]
      (when-let [have (catalog c table)]
        (when-let [stmts (seq (tokens/statements table wanted have (analyzed? c (:name wanted))))]
          (with-lock c (:name wanted)
            #(doseq [s stmts] (jdbc/execute-one! c [s]))))))))

(defn- ensure!
  [{:keys [asked builder]} ds table attrs]
  (let [k   [ds table attrs]
        now (System/currentTimeMillis)]
    (when (tokens/due? @asked k now)
      (swap! asked assoc k now)
      (send-off builder
                (fn [_]
                  (try (build! ds table attrs)
                       (catch InterruptedException e (throw e))
                       (catch Exception e
                         (binding [*out* *err*]
                           (println (str "token index on " table " not built: "
                                         (ex-message e))))))
                  nil))))
  nil)

(defn sweep!
  [{:keys [swept] :as state} ds sig]
  (let [now (System/currentTimeMillis)]
    (when (tokens/sweep-due? @swept sig now)
      (reset! swept {:sig sig :at now})
      (doseq [[table attrs] (tokens/searchable-tables sig)]
        (ensure! state ds table attrs))))
  nil)
