(ns sutra.store.pg.records
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [sutra.store.api :as api]
            [sutra.kit.sql :as kit-sql]
            [sutra.store.pg.sql :as sql])
  (:import (java.sql Connection Timestamp)
           (java.time Instant)
           (java.util Date)))

(set! *warn-on-reflection* true)

(def ^:private col (memoize (fn [k] (sql/dq (str/replace (name k) "-" "_")))))
(defn- attr [c] (keyword (str/replace (name c) "_" "-")))

(defn- encode [codec v]
  (cond
    (= :bool codec)        (boolean v)
    (nil? v)               nil
    (= :edn codec)         (pr-str v)
    (= :keyword codec)     (name v)
    (instance? Instant v)  (Timestamp/from ^Instant v)
    (and (instance? Date v) (not (instance? Timestamp v))) (Timestamp. (.getTime ^Date v))
    (keyword? v)           (name v)
    :else                  v))

(defn- decode [codec v]
  (case codec
    :bool    (boolean v)
    :edn     (some-> v edn/read-string)
    :keyword (some-> v keyword)
    v))

(defn- ->value [{:keys [codec]} row]
  (persistent! (reduce-kv (fn [m c v] (let [a (attr c)] (assoc! m a (decode (get codec a) v))))
                          (transient {}) row)))

(defn- key-of [{:keys [key-cols]} v]
  (if (= 1 (count key-cols)) (get v (first key-cols)) (mapv #(get v %) key-cols)))

(defn- key-vals [{:keys [key-cols]} k]
  (if (= 1 (count key-cols)) [k] (vec k)))

(defn- key-where [{:keys [key-cols codec] :as m} k]
  [(str/join " AND " (map #(str (col %) " = ?") key-cols))
   (mapv (fn [c v] (encode (get codec c) v)) key-cols (key-vals m k))])

(defn get-record [^Connection c m k]
  (let [[w ps] (key-where m k)]
    (some->> (sql/one c (str "SELECT * FROM " (:table m) " WHERE " w " LIMIT 1") ps) (->value m))))

(defn- upsert-sql
  [{:keys [table key-cols auto]} cols n]
  (let [sets (remove (set key-cols) cols)]
    (str "INSERT INTO " table " (" (str/join ", " (map col cols)) ")"
         (when (and auto (some #{auto} cols)) " OVERRIDING SYSTEM VALUE")
         " VALUES "
         (str/join ", " (repeat n (str "(" (sql/placeholders (count cols)) ")")))
         " ON CONFLICT (" (str/join ", " (map col key-cols)) ") DO "
         (if (seq sets)
           (str "UPDATE SET " (str/join ", " (map #(str (col %) " = EXCLUDED." (col %)) sets)))
           "NOTHING"))))

(defn put-record!
  [^Connection c {:keys [table key-cols auto codec generated] :as m} k v]
  (cond
    (nil? v)
    (let [[w ps] (key-where m k)] (sql/update! c (str "DELETE FROM " table " WHERE " w) ps) k)

    (= api/auto k)
    (let [cols (vec (remove #{auto} (keys v)))
          r    (sql/one c (str "INSERT INTO " table " (" (str/join ", " (map col cols)) ") VALUES ("
                               (sql/placeholders (count cols)) ") RETURNING " (col auto))
                        (mapv #(encode (get codec %) (get v %)) cols))]
      (get r (keyword (str/replace (name auto) "-" "_"))))

    :else
    (let [v    (merge (apply dissoc v generated) (zipmap key-cols (key-vals m k)))
          cols (vec (keys v))]
      (sql/update! c (upsert-sql m cols 1) (mapv #(encode (get codec %) (get v %)) cols))
      k)))

(def ^:private max-params 30000)

(defn put-records!
  [^Connection c {:keys [key-cols generated codec] :as m} kvs]
  (let [ks (into [] (comp (map first) (remove #{api/auto})) kvs)]
    (if (not= (count ks) (count (set ks)))
      (mapv (fn [[k v]] (put-record! c m k v)) kvs)
      (let [{dels true
             ups false} (group-by (comp nil? second) (remove (comp #{api/auto} first) kvs))]
        (doseq [part (partition-all (quot max-params (count key-cols)) (map first dels))
                :let [ws (map (partial key-where m) part)]]
          (sql/update! c
                       (str "DELETE FROM " (:table m) " WHERE "
                            (str/join " OR " (map #(str "(" (first %) ")") ws)))
                       (into [] (mapcat second) ws)))
        (doseq [[cols rows] (group-by (comp vec sort keys)
                                      (for [[k v] ups]
                                        (merge (apply dissoc v generated)
                                               (zipmap key-cols (key-vals m k)))))
                part (partition-all (quot max-params (count cols)) rows)]
          (sql/update! c (upsert-sql m cols (count part))
                       (into [] (mapcat (fn [r] (map #(encode (get codec %) (get r %)) cols)))
                             part)))
        (mapv (fn [[k v]] (if (= api/auto k) (put-record! c m k v) k)) kvs)))))

(def sql-ops
  #{:= :not= :in :nil? :< :<= :> :>= :starts-with})

(defn- text-col [{:keys [text]} a] (cond-> (col a) (contains? text a) (str " COLLATE \"C\"")))

(defn- operand
  [{:keys [key-cols codec] :as m} a v]
  (if (= :db/key a)
    [(str/join ", " (map (partial text-col m) key-cols))
     (mapv #(encode (get codec %1) %2) key-cols (key-vals m v))]
    [(text-col m a) [(encode (get codec a) v)]]))

(defn- pred-sql
  [{:keys [key-cols] :as m} [op a b :as p]]
  (let [junction (fn [j ps] (case (count ps)
                              0 [(if (= " AND " j) "TRUE" "FALSE") []]
                              1 (pred-sql m (first ps))
                              (let [xs (map (partial pred-sql m) ps)]
                                [(str "(" (str/join j (map first xs)) ")")
                                 (into [] (mapcat second) xs)])))
        cmp      (fn [sym v] (let [[l vs] (operand m a v)]
                               [(str "(" l ") " sym " (" (sql/placeholders (count vs)) ")") vs]))]
    (case op
      :and  (junction " AND " (rest p))
      :or   (junction " OR " (rest p))
      :not  (let [[s ps] (pred-sql m a)] [(str "NOT coalesce(" s ", FALSE)") ps])
      :in   (junction " OR " (map #(vector := a %) (remove nil? b)))
      :nil? (if (= :db/key a)
              [(if b "FALSE" "TRUE") []]
              [(str (col a) (if b " IS NULL" " IS NOT NULL")) []])
      :starts-with (let [c (if (= :db/key a) (first key-cols) a)]
                     [(str (col c) "::text LIKE ?")
                      [(str (str/replace (str b) #"[\\%_]" #(str "\\" %)) "%")]])
      (if (nil? b)
        ["FALSE" []]
        (cmp (case op := "=" :not= "<>" :< "<" :<= "<=" :> ">" :>= ">=") b)))))

(defn- order-sql
  [{:keys [key-cols] :as m} order]
  (let [by  (fn [a dir] (str (text-col m a) (if (= :desc dir) " DESC NULLS FIRST" " NULLS LAST")))
        key (fn [dir] (map #(by % dir) key-cols))]
    (str " ORDER BY "
         (str/join ", "
                   (concat (mapcat (fn [[a dir]] (if (= :db/key a) (key dir) [(by a dir)])) order)
                           (key :asc))))))

(defn- pick-sql
  [{:keys [table key-cols]} w {:keys [by max min]}]
  (when (not= 1 (count key-cols))
    (throw (ex-info "a :pick scan takes a table of one key column" {:table table})))
  (let [k    (col (first key-cols))
        rank (str "COALESCE(" (str/join ", " (map #(str (col %) "::text") (or max min))) ", '')")]
    (str "SELECT t.* FROM " table " t JOIN (SELECT " (if max "max" "min")
         "(ARRAY[" rank " COLLATE \"C\", " k "::text COLLATE \"C\"]) AS w FROM " table w
         " GROUP BY " (str/join ", " (map col by)) ") p ON t." k "::text = p.w[2]"
         " ORDER BY t." k " COLLATE \"C\"")))

(defn scan-records
  [^Connection c {:keys [table codec key-cols] :as m} {:keys [where order after limit project agg]}]
  (let [[w ps] (pred-sql m (cond-> (into [:and] where) (some? after) (conj [:> :db/key after])))
        w      (str " WHERE " w)
        cols   (if project
                 (str/join ", " (map col (distinct (concat key-cols (remove #{:db/key} project)))))
                 "*")
        entry  (fn [r] (let [v (->value m r)]
                         [(key-of m v) (cond-> v project (select-keys project))]))]
    (cond
      (:pick agg)
      (mapv entry (sql/rows c (pick-sql m w (:pick agg)) ps))

      (:group-by agg)
      (let [g (:group-by agg)]
        (into {} (map (fn [r] [(decode (get codec g) (get r :g)) (long (get r :n))]))
              (sql/rows c
                        (str "SELECT " (col g) " AS g, count(*) AS n FROM " table w " GROUP BY "
                             (col g)) ps)))

      (:count? agg)
      (long (:n (sql/one c (str "SELECT count(*) AS n FROM " table w) ps)))

      :else
      (mapv entry
            (sql/rows c
                      (str "SELECT " cols " FROM " table w (order-sql m order)
                           (when limit (str " LIMIT " (long limit)))) ps)))))

(def clock-ddl
  ["CREATE TABLE IF NOT EXISTS public.store_records_clock
      (coll text PRIMARY KEY, n bigint NOT NULL)"
   "CREATE OR REPLACE FUNCTION public.sutra_records_clock_bump() RETURNS trigger
    LANGUAGE plpgsql AS $fn$
    BEGIN
      INSERT INTO public.store_records_clock (coll, n) VALUES (TG_ARGV[0], 1)
        ON CONFLICT (coll) DO UPDATE SET n = public.store_records_clock.n + 1;
      RETURN NULL;
    END $fn$"])

(defn- counting-sql [table coll]
  (str "DO $$ BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_trigger
                  WHERE tgname = 'sutra_records_clock'
                    AND tgrelid = " (kit-sql/sq table) "::regclass) THEN
    CREATE TRIGGER sutra_records_clock AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE
      ON " table " FOR EACH STATEMENT
      EXECUTE FUNCTION public.sutra_records_clock_bump(" (kit-sql/sq (str coll)) ");
  END IF;
END $$"))

(defn count-writes!
  [^javax.sql.DataSource ds colls]
  (with-open [c (.getConnection ds)]
    (.setAutoCommit c false)
    (try
      (sql/rows c "SELECT pg_advisory_xact_lock(hashtextextended('store_records_clock', 0)) AS l"
                [])
      (doseq [s clock-ddl] (sql/exec! c s))
      (doseq [[coll {:keys [table]}] colls] (sql/exec! c (counting-sql table coll)))
      (.commit c)
      (catch Throwable t (.rollback c) (throw t)))))

(defn clock
  [^Connection c {:keys [table]} coll]
  (let [{:keys [n counted]}
        (sql/one c (str "SELECT (SELECT n FROM public.store_records_clock WHERE coll = ?) AS n,"
                        " EXISTS (SELECT 1 FROM pg_trigger WHERE tgname = 'sutra_records_clock'"
                        " AND tgrelid = to_regclass(?)) AS counted")
                 [(str coll) table])]
    (when-not counted
      (api/refuse! :unavailable (str coll " is not counted: its table " table
                                     " has no sutra_records_clock trigger")
                   {:coll coll :table table}))
    (or n 0)))
