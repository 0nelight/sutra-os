(ns ^:integration platform.pg-scan-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [sutra.store.api :as api]
            [sutra.store.core :as core]
            [sutra.types.world-rules :as rules]
            [sutra.store.pg :as store-pg]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.sql :as sql]
            [sutra.store.scan :as scan]
            [sutra.store.spi :as spi]
            [support.pod :as pod])
  (:import (java.time Instant)))

(def ^:private probe "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000000a1")

(def ^:private world
  {:object-types
   {probe {:object-type-name :probe
           :attribute-list [{:attribute-name :s :attribute-type :string}
                            {:attribute-name :n :attribute-type :long}
                            {:attribute-name :d :attribute-type [:decimal 12 2]}
                            {:attribute-name :x :attribute-type :double}
                            {:attribute-name :b :attribute-type :boolean}
                            {:attribute-name :t :attribute-type :timestamp}
                            {:attribute-name :k :attribute-type :keyword}
                            {:attribute-name :tags :attribute-type [:array :string]}
                            {:attribute-name :nums :attribute-type [:array :long]}]
           :plural "Proben"}}})

(def ^:private astral (String. (Character/toChars 0x1F600)))

(defn- at [s] (Instant/parse s))

(defn- stamp
  [s]
  (java.sql.Timestamp/valueOf
   (java.time.LocalDateTime/ofInstant (Instant/parse s) java.time.ZoneOffset/UTC)))

(def ^:private rows
  [{:s "alpha"  :n 1   :d 2.50M :x 1.5    :b true  :t (stamp "2026-01-01T00:00:00.000500Z") :k "a"
    :tags ["x" "y"] :nums [1 2]}
   {:s "alpha"  :n 2   :d 2.5M  :x -0.5   :b false :t (stamp "2026-01-01T00:00:00Z")        :k "b"
    :tags ["y"]     :nums [2]}
   {:s "beta"   :n nil :d nil   :x 1e300  :b nil   :t nil                                :k nil
    :tags []       :nums []}
   {:s astral   :n 3   :d 10M   :x 3.0    :b true  :t (stamp "2025-12-31T23:59:59.999Z")    :k "a"
    :tags nil      :nums [3]}
   {:s "￿" :n 3   :d -1.25M :x 0.0   :b false :t (stamp "2026-06-01T12:00:00Z")        :k "c"
    :tags ["z"]     :nums nil}
   {:s nil      :n -7  :d 0M    :x nil    :b true  :t (stamp "2026-01-01T00:00:00.001Z")    :k "b"
    :tags ["x"]     :nums [1]}
   {:s "Alpha"  :n 100 :d 99.99M :x 2.0   :b nil   :t (stamp "2024-02-29T00:00:00Z")        :k nil
    :tags ["a" "b"] :nums [100 -7]}])

(defn- id [i] (format "00000000-0000-0000-0000-%012d" (inc i)))

(def ^:private attrs [:s :n :d :x :b :t :k])

(def ^:private scans
  (let [src {:source [:rows probe]}]
    (concat
     (for [[a v] [[:s "alpha"] [:s astral] [:n 2] [:n 2.0M] [:d 2.5] [:d 2.50M] [:x 1.5] [:b true]
                  [:t (at "2026-01-01T00:00:00Z")] [:k "a"] [:s 7] [:n "2"] [:t "2026-01-01"]]
           op [:= :not= :< :<= :> :>=]]
       (assoc src :where [[op a v]]))
     [(assoc src :where [[:in :n #{1 3 nil}]])
      (assoc src :where [[:in :s ["alpha" "￿"]]])
      (assoc src :where [[:in :n #{}]])
      (assoc src :where [[:nil? :s true]])
      (assoc src :where [[:nil? :tags false]])
      (assoc src :where [[:starts-with :s "al"]])
      (assoc src :where [[:starts-with :s ""]])
      (assoc src :where [[:starts-with :n "1"]])
      (assoc src :where [[:has :tags "y"]])
      (assoc src :where [[:has :nums 2]])
      (assoc src :where [[:has :nums 2.0]])
      (assoc src :where [[:not [:= :b true]]])
      (assoc src :where [[:not [:< :n 3]]])
      (assoc src :where [[:or [:= :n 1] [:= :n 3]]])
      (assoc src :where [[:or [:and [:= :s "alpha"] [:= :n 1]] [:and [:= :s "beta"] [:= :n 2]]]])
      (assoc src :where [[:= :db/id (id 2)]])
      (assoc src :where [[:> :db/id (id 3)] [:< :tags 3]])]
     (for [a attrs dir [:asc :desc]]
       (assoc src :where [] :order [[a dir]]))
     [(assoc src :where [] :order [[:b :asc] [:s :desc]])
      (assoc src :where [] :order [[:n :asc]] :limit 3)
      (assoc src :where [[:nil? :n false]] :order [[:d :desc]] :after (id 1) :limit 2)
      (assoc src :where [] :after (id 3))
      (assoc src :where [] :limit 2)
      (assoc src :where [] :order [[:tags :asc]] :limit 2)
      (assoc src :where [[:> :n 0]] :project #{:s :d})
      (assoc src :where [] :project #{:db/id :t})]
     [(assoc src :where [] :agg {:count? true})
      (assoc src :where [[:= :s "alpha"]] :agg {:count? true})
      (assoc src :where [[:has :tags "x"]] :agg {:count? true})]
     (for [a (conj attrs :db/id)]
       (assoc src :where [] :agg {:group-by a})))))

(def ^:private ^:dynamic *ds* nil)

(defn- with-db [f]
  (let [{:keys [url user password] :as h} (pod/start-empty-pg!)
        ds (jdbc/get-datasource {:jdbcUrl url :user user :password password})]
    (try
      (binding [*ds* ds]
        (f (doto (store-pg/store {:datasource ds} {}) core/adopt!)))
      (finally (pod/stop-pg! h)))))

(defn- head [db] (with-open [s (api/snapshot db {:branch "main"})] (api/basis s)))

(deftest pg-scans-equal-the-reference
  (with-db
    (fn [db]
      (api/transact! db
                     {:basis (head db) :actor {:principal "t"}
                      :schema {:authored world :migration []}
                      :ops (map-indexed (fn [i v]
                                          {:op :create-object :type probe
                                           :payload
                                             {:id (id i)
                                              :values (into {} (remove (comp nil? val)) v)}})
                                        rows)})
      (let [e   (:engine db)
            sig (rules/signature world)
            h   (spi/-begin e :read nil)
            cx  {:ns "main" :sig sig :as-of nil}]
        (try
          (let [entries (mapv (juxt :id :values) (spi/-rows e h cx probe nil))]
            (is (= (count rows) (count entries)))
            (testing "a commit stores an instant as its millisecond"
              (is (= (Instant/parse "2026-01-01T00:00:00Z")
                     (get-in (into {} entries) [(id 0) :t]))))
            (doseq [s scans]
              (let [a (spi/-scan e h cx s)
                    r (scan/reference entries s)]
                (is (scan/same-answer? s a r) (pr-str {:scan s :pg a :reference r}))))
            (testing "what pg can judge runs in SQL, not in the driver"
              (let [sql-of (fn [s]
                             (let [seen (atom [])
                                   rows sql/rows
                                   one  sql/one]
                               (with-redefs [sql/rows
                                               (fn [c q ps] (swap! seen conj q) (rows c q ps))
                                             sql/one
                                               (fn [c q ps] (swap! seen conj q) (one c q ps))]
                                 (spi/-scan e h cx s))
                               (last @seen)))
                    src    {:source [:rows probe]}]
                (is (re-find #"ORDER BY q.\"s\" DESC NULLS FIRST.* LIMIT"
                             (sql-of (assoc src :where [[:< :n 50]] :order [[:s :desc]] :limit 2)))
                    "text orders by code point as its column is laid out, with no COLLATE at the query")
                (is (re-find #"count\(\*\)"
                             (sql-of (assoc src :where [[:starts-with :s "a"]]
                                            :agg {:count? true}))))
                (is (re-find #"GROUP BY" (sql-of (assoc src :where [] :agg {:group-by :b}))))
                (is (re-find #"@> \?::jsonb" (sql-of (assoc src :where [[:has :tags "x"]]))))
                (is (re-find #"date_trunc"
                             (sql-of (assoc src :where [[:>= :t (at "2026-01-01T00:00:00Z")]]))))
                (is (re-find #"date_trunc.* = \?"
                             (sql-of (assoc src :where [[:= :t (at "2026-01-01T00:00:00Z")]])))
                    "an instant equals by its millisecond, so pg takes = too")
                (is (not (re-find #"ORDER BY" (sql-of (assoc src :where [] :order [[:tags :asc]]))))
                    "an array orders element-wise, which SQL does not: the driver sorts"))))
          (finally (spi/-rollback! e h)))))))

(deftest pg-scans-as-of-a-revision
  (with-db
    (fn [db]
      (api/transact! db
                     {:basis (head db) :actor {:principal "t"}
                      :schema {:authored world :migration []}
                      :ops [{:op :create-object :type probe
                             :payload {:id (id 0) :values {:s "old" :n 1}}}]})
      (let [before (:rev (head db))]
        (api/transact! db {:basis (head db) :actor {:principal "t"}
                           :ops [{:op :update-object :type probe
                                  :payload {:id (id 0) :values {:s "new" :n 1}}}]})
        (let [e  (:engine db)
              h  (spi/-begin e :read nil)
              cx {:ns "main" :sig (rules/signature world)}
              q  {:source [:rows probe] :where [[:= :s "old"]]}]
          (try
            (testing "a scan as of a revision reads that revision, not the head"
              (is (= [(id 0)] (map first (spi/-scan e h (assoc cx :as-of {:rev before}) q))))
              (is (= [] (spi/-scan e h (assoc cx :as-of nil) q))))
            (finally (spi/-rollback! e h))))))))

(def ^:private keyed "ri.sutra.main.object-type.00000000-0000-0000-0000-0000000000a2")

(def ^:private keyed-world
  {:object-types
   {keyed {:object-type-name :keyed :match-keys [:code]
           :attribute-list [{:attribute-name :code :attribute-type :string}
                            {:attribute-name :n :attribute-type :long}]
           :plural "Keyeds"}}})

(defn- not-by-codepoint
  [c]
  (mapv (juxt :t :col)
        (sql/rows c
                  (str "SELECT r.relname AS t, a.attname AS col FROM pg_attribute a"
                       " JOIN pg_class r ON r.oid = a.attrelid JOIN pg_namespace n ON n.oid = "
                       "r.relnamespace"
                       " WHERE n.nspname = 'main' AND r.relname LIKE 'd\\_%' AND r.relkind IN "
                       "('r', 'p')"
                       " AND a.attnum > 0 AND NOT a.attisdropped"
                       " AND a.atttypid IN ('text'::regtype, 'character varying'::regtype)"
                       " AND a.attcollation <> (SELECT oid FROM pg_collation WHERE collname = 'C' "
                       "LIMIT 1)")
                  [])))

(defn- key-lookup-plan
  [e cx]
  (let [h (spi/-begin e :read nil)
        q (atom nil)
        rows sql/rows]
    (try
      (with-redefs [sql/rows (fn [c s ps] (when (re-find #"\) q WHERE" s) (reset! q [s ps]))
                               (rows c s ps))]
        (spi/-scan e h cx
                   {:source [:rows keyed] :where [[:or [:and [:= :code "k-2"]]]]
                    :project #{:code}}))
      (sql/exec! (:conn h) "SET LOCAL enable_seqscan = off")
      (let [[s ps] @q]
        (str/join "\n" (map (comp str first vals) (rows (:conn h) (str "EXPLAIN " s) ps))))
      (finally (spi/-rollback! e h)))))

(defn- on-write! [e f]
  (let [h (spi/-begin e :write nil)]
    (try (let [r (f (:conn h))] (spi/-commit! e h) r)
         (catch Throwable t (spi/-rollback! e h) (throw t)))))

(deftest text-is-laid-out-by-code-point
  (with-db
    (fn [db]
      (api/transact! db
                     {:basis (head db) :actor {:principal "t"}
                      :schema {:authored keyed-world :migration []}
                      :ops (for [i (range 5)]
                             {:op :create-object :type keyed
                              :payload {:id (id i) :values {:code (str "k-" i) :n i}}})})
      (let [e    (:engine db)
            cx   {:ns "main" :sig (rules/signature keyed-world) :as-of nil}
            t    (layout/object-table keyed)
            read (fn [] (on-write! e not-by-codepoint))]
        (testing "every text column of the store's tables compares by code point, so every index on one does"
          (is (= [] (read)))
          (is (re-find #"Index (Only )?Scan|Bitmap Index Scan" (key-lookup-plan e cx)))
          (is (not (re-find #"Seq Scan" (key-lookup-plan e cx)))))
        (testing "a table laid out in the default collation before is brought to code point at install"
          (on-write! e
                     #(sql/exec! %
                                 (str "ALTER TABLE main." t
                                      " ALTER COLUMN code TYPE character varying COLLATE "
                                      "\"default\"")))
          (is (= [[t "code"]] (read)))
          (on-write! e layout/recollate!)
          (is (= [] (read)))
          (is (not (re-find #"Seq Scan" (key-lookup-plan e cx))))
          (let [h (spi/-begin e :read nil)]
            (try (is (= [(id 2)]
                        (map first
                             (spi/-scan e h cx {:source [:rows keyed] :where [[:= :code "k-2"]]}))))
                 (finally (spi/-rollback! e h)))))))))

(defn- old-layout!
  [c views tables]
  (let [defs (mapv (fn [v]
                     [v
                      (:d (first (sql/rows c (str "SELECT pg_get_viewdef('" v "'::regclass) AS d")
                                           [])))]) views)]
    (doseq [v views] (sql/exec! c (str "DROP VIEW " v)))
    (doseq [t tables
            {:keys [col
                    typ]} (sql/rows c
                                    (str "SELECT a.attname AS col, format_type(a.atttypid, "
                                         "a.atttypmod) AS typ"
                                             " FROM pg_attribute a WHERE a.attrelid = '" t
                                         "'::regclass AND a.attnum > 0"
                                             " AND NOT a.attisdropped AND a.atttypid IN "
                                         "('text'::regtype, 'character varying'::regtype)")
                                        [])]
      (sql/exec! c
                 (str "ALTER TABLE " t " ALTER COLUMN \"" col "\" TYPE " typ
                      " COLLATE \"default\"")))
    (doseq [[v d] defs] (sql/exec! c (str "CREATE VIEW " v " AS " d)))))

(defn- branch-text-not-by-codepoint [c]
  (mapv (juxt :s :t :col)
        (sql/rows c
                  (str "SELECT n.nspname AS s, r.relname AS t, a.attname AS col FROM pg_attribute a"
                       " JOIN pg_class r ON r.oid = a.attrelid JOIN pg_namespace n ON n.oid = "
                       "r.relnamespace"
                       " WHERE (n.nspname = 'main' OR n.nspname LIKE 'bo\\_%') AND r.relname LIKE "
                       "'d\\_%'"
                       " AND r.relkind IN ('r', 'p') AND a.attnum > 0 AND NOT a.attisdropped"
                       " AND a.atttypid IN ('text'::regtype, 'character varying'::regtype)"
                       " AND a.attcollation <> (SELECT oid FROM pg_collation WHERE collname = 'C' "
                       "LIMIT 1)")
                  [])))

(deftest a-branch-with-its-own-rows-is-brought-to-code-point
  (with-db
    (fn [db]
      (api/transact! db
                     {:basis (head db) :actor {:principal "t"}
                      :schema {:authored keyed-world :migration []}
                      :ops (for [i (range 3)]
                             {:op :create-object :type keyed
                              :payload {:id (id i) :values {:code (str "k-" i) :n i}}})})
      (api/branch! db {:op :fork :name "zweig" :from (head db)})
      (let [at (fn [] (with-open [s (api/snapshot db {:branch "zweig"})] (api/basis s)))]
        (api/transact! db {:basis (at) :actor {:principal "t"}
                           :ops [{:op :update-object :type keyed
                                  :payload {:id (id 0) :values {:code "k-0" :n 10}}}
                                 {:op :delete-object :type keyed :payload {:id (id 1)}}]}))
      (let [e (:engine db)
            t (layout/object-table keyed)
            [{own :s}] (on-write! e
                                  #(sql/rows %
                                             (str "SELECT table_schema AS s FROM "
                                                  "information_schema.tables"
                                                  " WHERE table_schema LIKE 'bo\\_%' AND "
                                                  "table_name = ?") [t]))
            br (str "br_" (subs own 3))]
        (is (some? own) "the branch holds its own copy of the type")
        (on-write! e #(old-layout! % [(str br "." t)]
                                   [(str "main." t) (str own "." t)
                                    (str own "." (layout/tomb-of t))]))
        (is (seq (on-write! e branch-text-not-by-codepoint)))
        (is (= 2 (layout/recollate-in-steps! *ds*))
            "main's table and the branch's with its tomb, a step each")
        (is (= 0 (layout/recollate-in-steps! *ds*)) "and nothing once done")
        (is (= [] (on-write! e branch-text-not-by-codepoint)))
        (testing "and the branch still reads what it read"
          (with-open [s (api/snapshot db {:branch "zweig"})]
            (is (= #{"k-0" "k-2"}
                   (set (map #(get-in % [:values :code])
                             (api/read s {:principal "t"} [:objects keyed])))))))))))
