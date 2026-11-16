(ns ^:integration platform.flaw-history-test
  (:require [support.system :as tsys]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.ref.install :as ri]
            [sutra.ref.clock :as clock]
            [sutra.store.access.scope :as ctx]
            [sutra.store.access.substrate :as sub]
            [sutra.catalog.lookup :as lookup]
            [sutra.catalog.kernel :as kernel]
            [sutra.catalog.signature :as signature]
            [sutra.store.core.commit :as commit]
            [sutra.store.core.engine :as engine]
            [sutra.store.core.kv :as kv]
            [sutra.store.core.schema-log :as schema-log]
            [sutra.store.pg.layout :as layout]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.world :as world]
            [support.pod :as pod]
            [support.store :as store]))

(use-fixtures :once pod/with-stores-only)

(defn- ot [nm attrs]
  {:object-type-name nm
   :attribute-list (mapv (fn [[n t]] {:attribute-name n :attribute-type t}) attrs)})

(defn- derived
  ([W] (ri/derived {:sigma clock/system} W))
  ([W old] (ri/derived {:sigma clock/system} W old)))

(defn- install!
  [b W]
  (world/install! (ctx/store b) W {:actor {:principal "test"} :dataset-decls signature/datasets-of
                                :steps [[:drop-unclaimed]] :drop-unclaimed? true}))

(defn- sys [b] (ctx/as-system (ctx/store b)))

(defn- read-at
  [b T rev]
  (try (vec (sort-by :id (map #(select-keys % [:id :values]) (sub/read-type (sys b) T rev))))
       (catch Exception e [::refused (ex-message e)])))

(defn- history
  [b]
  (vec (reverse (map #(select-keys % [:rev :message :parents]) (world/history (ctx/store b))))))

(defn- prefix? [old new] (= old (vec (take (count old) new))))

(defn- columns [b t]
  (let [of (fn [s] (into #{} (map (comp keyword :column_name))
                         (jdbc/execute! (tsys/ds)
                                        ["SELECT column_name FROM information_schema.columns
                                          WHERE table_schema = ? AND table_name = ?" s (str t)]
                                        {:builder-fn rs/as-unqualified-maps})))]
    (not-empty (or (not-empty (of (pgns/own-schema b))) (of "main")))))

(deftest past-rewritten
  (store/with-fresh-branch [b "flaw-past"]
    (let [W1 (derived {:object-types {:zaehler (ot :zaehler [[:nr :string] [:stand :string]])}})
          T  (lookup/rid-of W1 :object-types :zaehler)]
      (install! b W1)
      (store/insert-object! b T {:id "z-1" :values {:nr "z-1" :stand "5"}})
      (let [r1     (:rev (world/head (ctx/store b)))
            then   (read-at b T r1)
            world1 (world/world (ctx/store b) r1)]
        (is (= [{:id "z-1" :values {:nr "z-1" :stand "5"}}]
               (mapv #(update % :values select-keys [:nr :stand]) then)))

        (testing "DDL: a retype of the attribute leaves the past as it was read"
          (let [W2
                (derived {:object-types
                          {:zaehler (ot :zaehler [[:nr :string] [:stand :integer]])}} W1)]
            (install! b W2)
            (is (= then (read-at b T r1)) "as of r1 the value is still the string \"5\"")
            (is (= world1 (world/world (ctx/store b) r1)) "the world as of r1 is unchanged")))

        (testing "DDL: deleting the type (its table dropped) leaves the past as it was read"
          (install! b (derived {:object-types {:anderes (ot :anderes [[:nr :string]])}} W1))
          (is (= then (read-at b T r1)) "as of r1 the object is still there")
          (is (= world1 (world/world (ctx/store b) r1))))))))

(deftest history-rewritten
  (store/with-fresh-branch [b "flaw-hist"]
    (let [W1 (derived {:object-types {:zaehler (ot :zaehler [[:nr :string] [:stand :string]])}})
          T  (lookup/rid-of W1 :object-types :zaehler)
          step! (fn [label f]
                  (let [before (history b)]
                    (f)
                    (is (prefix? before (history b))
                        (str label
                             ": the history up to the old head is a prefix of the new one"))))]
      (step! "install" #(install! b W1))
      (step! "write" #(store/insert-object! b T {:id "z-1" :values {:nr "z-1" :stand "5"}}))
      (step! "retype"
             #(install! b
                        (derived {:object-types
                                  {:zaehler (ot :zaehler [[:nr :string] [:stand :integer]])}}
                                 W1)))
      (step! "update" #(store/update-object! b T "z-1" {:stand 6}))
      (step! "drop"
             #(install! b (derived {:object-types {:anderes (ot :anderes [[:nr :string]])}} W1))))))

(deftest dropped-live-table
  (store/with-fresh-branch [b "flaw-drop"]
    (let [W1   (derived {:object-types {:host  (ot :host [[:nr :string]])
                                        :zahl  (ot :zahl [[:nr :string] [:v :string]])}})
          host (lookup/rid-of W1 :object-types :host)
          zahl (lookup/rid-of W1 :object-types :zahl)
          host-table (layout/object-table host)
          alive? (fn [] (and (some? (columns b host-table))
                             (= #{"h-1"} (set (map :id (store/objects b host))))))]
      (install! b W1)
      (store/insert-object! b host {:id "h-1" :values {:nr "h-1"}})
      (store/insert-object! b zahl {:id "z-1" :values {:nr "z-1" :v "keine zahl"}})
      (is (alive?))

      (testing "a run that deletes the type and then fails the install gate drops nothing"
        (is (thrown? Exception
                     (let [W (world/world (ctx/store b))
                           r (kernel/eval-steps
                              [{:op :delete-object-type :name :host}
                               {:op :create-object-type :name :kaputt
                                :attributes [[:nr :string]] :implements [:gibt-es-nicht]
                                :plural "Kaputte"}]
                              W {} {:installer {}})]
                       (kernel/judge! nil (:installed r) W))))
        (is (alive?) "host's table and row are still there"))

      (testing "a commit whose world drops the type and is refused in the same commit keeps the table"
        (is (thrown? Exception
                     (install! b
                               (derived
                                {:object-types {:zahl (ot :zahl [[:nr :string] [:v :integer]])}}
                                W1)))
            "\"keine zahl\" does not cast to an integer: the commit is refused")
        (is (alive?) "the drop of host's table went with the refused commit")
        (is (contains? (set (keys (:object-types (world/world (ctx/store b))))) host)
            "and the type is still declared")))))

(defn- unlog!
  [b revs through]
  (let [db (:db (ctx/store b))
        h  (engine/begin db :write nil)]
    (commit/apply! db h (into [(kv/kv-fx (kv/schema-logged-k b) through)]
                              (for [r revs] (kv/kv-fx (kv/schema-log-k b r) nil))))
    (engine/commit! db h)))

(deftest schema-commits-are-the-own-schema-history-of-b
  (store/with-fresh-branch [b "flaw-schema-log"]
    (let [W1 (derived {:object-types {:zaehler (ot :zaehler [[:nr :string]])}})
          T  (lookup/rid-of W1 :object-types :zaehler)
          W2 (derived {:object-types {:zaehler (ot :zaehler [[:nr :string] [:stand :string]])}}
                      W1)
          s       #(world/at (ctx/store b) b)
          history (fn [after] (mapv :rev (filter #(= (str b) (:branch %))
                                                 (world/history (s) {:schema? true
                                                                     :after after}))))
          commits (fn [opts] (mapv :rev (world/schema-commits (s) opts)))
          row!    #(store/insert-object! b T {:id (str (random-uuid)) :values {:nr "x"}})]
      (install! b W1)
      (let [r1 (:rev (world/head (ctx/store b)))]
        (dotimes [_ 3] (row!))
        (install! b W2)
        (world/drop-unclaimed! (s))
        (testing "after a revision, oldest first, a commit that changed nothing included"
          (is (= (history r1) (commits {:after r1})))
          (is (= 2 (count (commits {:after r1})))))
        (testing "from the fork on, and the newest alone"
          (is (= (history 0) (commits {})))
          (is (= [(peek (history 0))] (commits {:newest? true :limit 1}))))
        (testing "a schema commit that wrote no entry is repaired on the next read"
          (let [all (history 0)
                [r-1 r-last] (take-last 2 all)]
            (unlog! b [r-last] r-1)
            (row!)
            (is (= all (commits {})))
            (testing "and a later read scans nothing"
              (row!)
              (with-redefs-fn {#'schema-log/repair! (fn [& _] (throw (ex-info "repaired" {})))}
                #(is (= all (commits {})))))))
        (testing "a branch with no mark is filled on its first read"
          (unlog! b (history 0) nil)
          (row!)
          (is (= (history 0) (commits {}))))
        (testing "the boot backfill makes the list the history names"
          (unlog! b (history 0) nil)
          (is (= (count (history 0)) (get (schema-log/index! (:db (ctx/store b))) (str b))))
          (row!)
          (with-redefs-fn {#'schema-log/repair! (fn [& _] (throw (ex-info "repaired" {})))}
            #(is (= (history 0) (commits {})))))))))

(defn- deleted-by-worlds [store depth]
  (let [now   (world/world store)
        revs  (mapv :rev (world/history store {:schema? true :limit depth}))
        at    #(world/world store %)
        pairs (map vector revs (concat (rest revs) [nil]))]
    (->> (for [[r prev] pairs
               :let [after (at r) before (if prev (at prev) {})]
               [section m] before
               :when (map? m)
               [k spec] m
               :when (and (not (contains? (get after section) k))
                          (not (contains? (get now section) k)))]
           {:kind section :rid k :seq r :spec spec})
         (reduce (fn [[seen out] {:keys [kind rid] :as d}]
                   (if (seen [kind rid]) [seen out] [(conj seen [kind rid]) (conj out d)]))
                 [#{} []])
         second)))

(defn- converted-at!
  [b since]
  (let [db (:db (ctx/store b))
        h  (engine/begin db :read nil)
        ks (mapv first (kv/kv-scan db h [:wlog b] {:upto (kv/rev-key since) :keys-only? true}))
        _  (engine/commit! db h)
        h  (engine/begin db :write nil)]
    (commit/apply! db h (into [(kv/kv-fx [:wformat "" b] {:since since :n 0})]
                              (for [k ks] (kv/kv-fx [:wlog b k] nil))))
    (engine/commit! db h)))

(deftest the-trash-is-what-the-world-log-removed
  (store/with-fresh-branch [b "flaw-schema-removed"]
    (let [types (fn [& ks] (into {} (for [k ks] [k (ot k [[:nr :string]])])))
          s     #(world/at (ctx/store b) b)
          W1    (derived {:object-types (types :alt :mittel :neu)})
          W2    (derived {:object-types (types :alt :neu)} W1)
          W3    (derived {:object-types (types :neu)} W2)
          W4    (derived {:object-types (types :neu :mittel)} W3)
          same  (fn [depth]
                  (is (= (set (deleted-by-worlds (s) depth)) (set (world/deleted (s) depth)))
                      (str "depth " depth)))]
      (install! b W1)
      (install! b W2)
      (let [since (:rev (world/head (ctx/store b)))]
        (install! b W3)
        (install! b W4)
        (testing "what a commit removed, each once, by the commit that removed it"
          (let [gone (world/deleted (s) 100)]
            (is (some #(= :alt (get-in % [:spec :object-type-name])) gone))
            (is (= (count gone) (count (set (map (juxt :kind :rid) gone)))))
            (is (true? (:complete? (world/removed (s) 100))))))
        (testing "a window inside b, one that ends at its fork, and one that reaches past it"
          (doseq [depth [1 2 3 4 5 40]] (same depth)))
        (testing "a log that begins at a conversion says where the trash ends"
          (let [want (filter #(> (:seq %) since) (deleted-by-worlds (s) 40))]
            (is (seq want))
            (converted-at! b since)
            (let [{:keys [deleted complete? reaches-back-to]} (world/removed (s) 40)]
              (is (= (set want) (set deleted)))
              (is (false? complete?))
              (is (= since reaches-back-to)))
            (is (true? (:complete? (world/removed (s) 1))) "a window inside the log is whole")))))))
