(ns sutra.store.pg
  (:require [clojure.edn :as edn]
            [next.jdbc :as jdbc]
            [sutra.kit.noted :as noted]
            [sutra.store.core :as core]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.ns :as pg-ns]
            [sutra.store.pg.records :as records]
            [sutra.store.pg.rows :as rows]
            [sutra.store.pg.select :as select]
            [sutra.store.pg.sql :as sql]
            [sutra.store.pg.token-index :as token-index]
            [sutra.store.pg.tokens :as tokens]
            [sutra.store.scan :as scan]
            [sutra.store.spi :as spi])
  (:import (java.sql Connection)
           (javax.sql DataSource)))

(set! *warn-on-reflection* true)

(def ^:private kv-ddl
  ["CREATE TABLE IF NOT EXISTS public.store_kv
      (space text, part text, k text COLLATE \"C\", v text NOT NULL, PRIMARY KEY (space, part,
      k))"])

(defn- install!
  [^Connection c]
  (layout/install! c)
  (doseq [s kv-ddl] (sql/exec! c s))
  (when (sql/one c "SELECT 1 AS x FROM pg_class WHERE relname = 'store_rev_seq' AND relkind = 'S'"
                 [])
    (sql/exec! c
               (str "SELECT setval('public.dataset_seq', greatest("
                    "(SELECT CASE WHEN is_called THEN last_value ELSE 1 END FROM "
                    "public.dataset_seq), "
                    "(SELECT CASE WHEN is_called THEN last_value ELSE 1 END FROM "
                    "public.store_rev_seq)))"))))

(defn- data-source
  (^DataSource [opts] (data-source opts nil))
  (^DataSource [{:keys [datasource]} lane]
   (if (instance? DataSource datasource) datasource (datasource lane))))

(defn- ensure-installed! [{:keys [installed]} ds]
  (when-not (@installed ds)
    (with-open [c (jdbc/get-connection ds)]
      (jdbc/with-transaction [t c] (install! t)))
    (swap! installed conj ds)
    (future
      (noted/quietly "store-pg recollate"
                       (let [n (layout/recollate-in-steps! ds)]
                         (when (pos? n)
                           (binding [*out* *err*]
                             (println (str "store-pg: " n " tables brought to code point")))))))))

(defn- release! [^Connection c commit?]
  (try (if commit? (.commit c) (.rollback c))
       (finally (try (.setAutoCommit c true) (catch java.sql.SQLException _ nil))
                (.close c))))

(defn- reading
  [{:keys [catalog]} {:keys [conn cat]} {:keys [ns basis as-of layout] :as cx}]
  (assoc cx :view (cond as-of  conn
                        layout (layout/cached-cat conn catalog [ns :layout layout])
                        basis  (layout/cached-cat conn catalog [ns basis])
                        :else  (or cat conn))))

(defn- writing
  [{:keys [conn cat stamped]} {:keys [rev] :as cx}]
  (when (not= @stamped rev)
    (layout/stamp! conn rev)
    (vreset! stamped rev))
  (assoc cx :view cat))

(defn- laid-out!
  [{:keys [cat]}]
  (layout/forget! cat))

(defn- compiled
  [{:keys [ns sig as-of view]} plan denied seen]
  (let [missing (volatile! false)
        or-empty #(or % (do (vreset! missing true) select/empty-relation))
        cx      {:sig sig :denied denied
                 :rel   (fn [t] (vswap! seen conj (str t))
                          (or-empty (layout/relation view ns t as-of)))
                 :edges (fn [L] (vswap! seen conj (layout/link-table L))
                          {:from (or-empty (layout/link-relation view ns sig L as-of))
                           :derived? false})}
        {s :sql ps :params} (select/compile-plan cx plan)]
    {:sql s :params ps :missing? @missing}))

(defn- select*
  [c {:keys [sig] :as cx} plan denied]
  (let [{s :sql ps :params :keys [missing?]} (compiled cx plan denied (volatile! #{}))]
    (if missing?
      spi/decline
      (let [decoder (memoize (partial rows/decoder sig))]
        (select/shape sig plan
                      (mapv #(update-vals % sql/value) (sql/rows c s ps))
                      (fn [T r] (rows/->row (decoder T) r)))))))

(defn- index-main!
  [opts {:keys [ns sig]}]
  (when (and sig (pg-ns/main? ns))
    (token-index/sweep! (:token-indexes opts) (data-source opts) sig)))

(defn- explain*
  [cx plan denied]
  (let [seen (volatile! #{})
        {s :sql ps :params :keys [missing?]} (compiled cx plan denied seen)]
    {:sql s :param-count (count ps) :tables (vec (sort @seen)) :missing? missing?}))

(defn- table-of
  [opts {:keys [ns]} coll]
  (when (seq ns)
    (throw (ex-info (str coll
                         " is kept in a table of its own, which holds :scope :global records only")
                    {:coll coll :ns ns})))
  (get-in opts [:records coll]))

(defn- space-text [space] (subs (str space) 1))

(defn- kv-columns
  [ks]
  (let [ks (distinct ks)]
    [(sql/text-array (map (comp space-text first) ks)) (sql/text-array (map second ks))
     (sql/text-array (map peek ks))]))

(def ^:private profile
  {:driver      :pg
   :history?    true
   :consistency :transactional
   :reads       #{:self}
   :latency     {:startup-ms 0}
   :scan        {:sources  #{:rows :links :records}
                 :preds    (zipmap [:= :not= :in :nil? :< :<= :> :>= :starts-with :has]
                                   (repeat :exact))
                 :order?   true :page? true :project? true
                 :agg      #{:count :group-by :pick} :agg-mode :complete}
   :plan        {:ops   #{:scan :static :filter :search-around :with :compute :union :intersect
                          :subtract
                          :order :page :project :aggregate :fk-join}
                 :aggs  (zipmap [:count :sum :avg :min :max :cardinality] (repeat :all))
                 :folds #{:attribute :expr :nested}
                 :preds (zipmap [:= :not= :< :<= :> :>= :nil? :starts-with :contains :contains-ci
                                 :has :id-in :having-all :within-distance :within-bounding-box
                                 :matches-tokens :matches-tokens-in]
                                (repeat :exact))}
   :explain?    true
   :semantics-overrides {}})

(defn- in-parts
  [s sql-ops sql]
  (let [{in true out false} (group-by #(every? sql-ops (scan/ops-of %)) (:where s))]
    (if (empty? out)
      (sql s)
      (scan/run (sql {:source (:source s) :where (vec in)}) (assoc s :where (vec out))))))

(defrecord PgEngine [opts]
  spi/Engine
  (-profile [_] profile)
  (-begin [_ mode lane]
    (let [ds (data-source opts lane)
          _  (ensure-installed! opts (if lane (data-source opts) ds))
          c  (doto ^Connection (jdbc/get-connection ds) (.setAutoCommit false))]
      (try
        (if (= :read mode)
          (do (sql/exec! c "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY")
              {:conn c :mode mode})
          (do (sql/exec! c "SET LOCAL lock_timeout = '20s'")
              {:conn c :mode mode :cat (layout/tx-cat c) :stamped (volatile! nil)}))
        (catch Throwable t (release! c false) (throw t)))))
  (-lock! [_ {:keys [conn]} ks]
    (sql/rows conn (str "SELECT pg_advisory_xact_lock(hashtextextended(k, 0)) AS l"
                        " FROM unnest(?::text[]) WITH ORDINALITY AS t(k, i) ORDER BY i")
              [(sql/text-array (map pr-str ks))]))
  (-next-rev [_ {:keys [conn stamped]}]
    (let [rev (parse-long
               (:rev
                (sql/one conn
                         (str "SELECT set_config('sutra.v', public.sutra_next_version()::text, "
                              "true) AS rev,"
                              " set_config('sutra.conductor', '', true) AS conductor")
                         [])))]
      (some-> stamped (vreset! rev))
      rev))
  (-commit! [_ {:keys [conn]}] (release! conn true))
  (-rollback! [_ {:keys [conn]}] (release! conn false))

  (-kv-read [_ {:keys [conn]} ks]
    (into {}
          (map (fn [{:keys [space part k v]}] [[(keyword space) part k] (edn/read-string v)]))
          (sql/rows conn
                    (str "SELECT s.space, s.part, s.k, s.v FROM unnest(?::text[], ?::text[], "
                         "?::text[]) AS w(space, part, k)"
                         " JOIN public.store_kv s ON s.space = w.space AND s.part = w.part AND s.k "
                         "= w.k COLLATE \"C\"")
                    (kv-columns ks))))
  (-kv-write! [_ {:keys [conn]} writes]
    (let [{puts true dels false} (group-by (comp some? val) writes)]
      (when (seq puts)
        (sql/update! conn (str "INSERT INTO public.store_kv (space, part, k, v)"
                               " SELECT * FROM unnest(?::text[], ?::text[], ?::text[], ?::text[])"
                               " ON CONFLICT (space, part, k) DO UPDATE SET v = EXCLUDED.v")
                     (conj (kv-columns (keys puts)) (sql/text-array (map (comp pr-str val) puts)))))
      (when (seq dels)
        (sql/update! conn
                     (str "DELETE FROM public.store_kv s USING unnest(?::text[], ?::text[], "
                          "?::text[]) AS w(space, part, k)"
                          " WHERE s.space = w.space AND s.part = w.part AND s.k = w.k COLLATE \"C\"")
                     (kv-columns (keys dels))))))
  (-kv-scan [_ {:keys [conn]} space part {:keys [after upto limit reverse? keys-only?]}]
    (mapv (fn [r] (if keys-only? [(:k r)] [(:k r) (edn/read-string (:v r))]))
          (sql/rows conn (str "SELECT k" (when-not keys-only? ", v")
                              " FROM public.store_kv WHERE space = ? AND part = ?"
                              (when after " AND k > ?") (when upto " AND k <= ?")
                              " ORDER BY k" (when reverse? " DESC") (when limit " LIMIT ?"))
                    (cond-> [(space-text space) part] after (conj after) upto (conj upto)
                            limit (conj limit)))))
  (-kv-parts [_ {:keys [conn]} space]
    (mapv :part (sql/rows conn (str "SELECT DISTINCT part COLLATE \"C\" AS part"
                                    " FROM public.store_kv WHERE space = ? ORDER BY 1")
                          [(space-text space)])))

  (-ns-exists? [_ {:keys [conn]} ns] (layout/ns-exists? conn ns))
  (-ns-fork! [_ {:keys [conn] :as h} from to {:keys [sig rev]}]
    (layout/fork! conn from to sig rev)
    (laid-out! h))
  (-ns-drop! [_ {:keys [conn] :as h} ns]
    (layout/drop-ns! conn ns)
    (laid-out! h))
  (-layout! [_ {:keys [conn] :as h} {:keys [ns rev]} change]
    (layout/layout! conn ns rev change)
    (laid-out! h))
  (-inspect [_ {:keys [conn]} {:keys [ns sig]}] (layout/inspect conn ns sig))

  (-rows [_ h cx T ids] (rows/rows (:conn h) (reading opts h cx) T ids))
  (-links [_ h cx L where] (rows/stored-links (:conn h) (reading opts h cx) L where))

  (-write-rows! [_ h cx T rs] (rows/write-rows! (:conn h) (writing h cx) T rs))
  (-delete-rows! [_ h cx T ids] (rows/delete-rows! (:conn h) (writing h cx) T ids))
  (-write-links! [_ h cx L ls] (rows/write-links! (:conn h) (writing h cx) L ls))
  (-delete-links! [_ h cx L ids] (rows/delete-links! (:conn h) (writing h cx) L ids))

  spi/Pushdown
  (-scan [_ h cx {[kind x] :source :as s}]
    (when (and (:pick (:agg s)) (not= :records kind))
      (throw (ex-info (str "a :pick scan reads records, not " kind) {:scan s})))
    (case kind
      :rows    (rows/scan-rows (:conn h) (reading opts h cx) x s)
      :links   (rows/scan-links (:conn h) (reading opts h cx) x s)
      :records (in-parts s records/sql-ops
                         #(records/scan-records (:conn h) (table-of opts cx x) %))))
  (-execute [_ h cx {:keys [plan denied]}]
    (index-main! opts cx)
    (select* (:conn h) (reading opts h cx) plan denied))
  (-explain [_ h cx {:keys [plan denied]}] (explain* (reading opts h cx) plan denied))

  spi/Records
  (-rec-colls [_] (set (keys (:records opts))))
  (-rec-get [_ h cx coll k] (records/get-record (:conn h) (table-of opts cx coll) k))
  (-rec-put! [_ h cx coll kvs] (records/put-records! (:conn h) (table-of opts cx coll) kvs))
  (-rec-scan [_ h cx coll] (records/scan-records (:conn h) (table-of opts cx coll) {}))
  (-rec-copy! [_ _ coll _ _]
    (throw (ex-info (str coll " is kept :scope :global in its table; a fork does not copy it")
                    {:coll coll})))

  spi/RecordsClock
  (-rec-clock [_ h coll]
    (when-let [spec (get-in opts [:records coll])]
      (records/clock (:conn h) spec coll))))

(defn engine
  [opts]
  (->PgEngine (-> opts
                  (update :installed #(or % (atom #{})))
                  (update :token-indexes
                          #(or % {:asked (atom {}) :swept (atom nil) :builder (agent nil)}))
                  (update :catalog #(or % (atom {}))))))

(defn store
  [engine-opts store-opts]
  (core/db (engine engine-opts) store-opts))
