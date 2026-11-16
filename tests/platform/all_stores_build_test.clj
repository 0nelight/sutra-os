(ns ^:api platform.all-stores-build-test
  (:require [support.system :as tsys]
            [clojure.string :as str]
            [sutra.store.access.datasets :as dsets]
            [sutra.store.access.scope :as ctx]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [sutra.types.naming :as naming]
            [sutra.compute.sandbox :as sandbox]
            [sutra.builds.ledger]
            [sutra.builds.run.rows :as run-rows]
            [sutra.store.pg.dataset :as pgds]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.pg.sql :as sql]
            [support.build-scenario :as bs]
            [support.pod :as pod]
            [sutra.wire.api :as api]
            [sutra.wire.transport :as tx]
            [sutra.store.control.tables :as cc]
            [sutra.builds.import.materialize :as mat]
            [sutra.catalog.lookup :as lookup]
            [sutra.store.seat :as seat]))

(use-fixtures :once pod/with-all-stores-pod)

(def ^:private built
  (delay
    (bs/install-chain!)
    (bs/run-and-assert-convergence!)))

(defn- skip? []
  (when-not (sandbox/available?)
    (println "  (all-stores-build SKIPPED — python sandbox unavailable)")
    true))

(deftest a-full-build-converges-with-every-store-live
  (when-not (skip?)
    (is (bs/ok? @built) "the import succeeded with the store on Postgres underneath")))

(deftest the-kernel-really-answers-from-postgres
  (testing "the kernel answers from Postgres"
    (with-open [c (jdbc/get-connection (tsys/ds))]
      (is (= "PostgreSQL" (.. c getMetaData getDatabaseProductName))))))

(deftest the-builds-rows-are-in-the-LAKE
  (when-not (skip?)
    @built
    (let [dss      (:datasets (:body (api/get! :datasets)))
          seed-rid (some (fn [[rid d]]
                           (when (= :seed (keyword (:dataset-name d))) rid))
                         dss)]
      (is (some? seed-rid) "the seed dataset resolves to a rid")
      (with-open [c (pgds/open (tsys/ds))]
        (let [n (-> (jdbc/execute-one!
                     c [(str "SELECT count(*) AS n FROM main."
                             (sql/dq (naming/dataset-table seed-rid)))])
                    first val long)]
          (is (= 2 n) "the seeded rows are physically in the lake"))))))

(deftest a-bulk-datasets-TABLE-is-not-in-the-control-store-at-all
  (when-not (skip?)
    @built
    (let [dss      (:datasets (:body (api/get! :datasets)))
          seed-rid (some (fn [[rid d]]
                           (when (= :seed (keyword (:dataset-name d))) rid))
                         dss)
          elsewhere? (fn [t]
                       (nil? (jdbc/execute-one!
                              (tsys/ds) ["SELECT 1 FROM information_schema.tables
                                               WHERE table_name = ? AND table_schema =
                                                 (SELECT table_schema FROM information_schema.tables
                                                   WHERE table_name = 'control_runs' LIMIT 1)" t])))
          obj-rid  (some (fn [[rid ot]]
                           (when (= :thing (keyword (:object-type-name ot))) rid))
                         (:object-types (:body (api/get! :schema))))]
      (is (some? seed-rid) "the seed dataset resolves to a rid")
      (is (elsewhere? (naming/dataset-table seed-rid))
          "a BULK dataset's table is the lake's — no other store got the CREATE")
      (with-open [c (pgds/open (tsys/ds))]
        (is (pgds/table-exists? c "main" (naming/dataset-table seed-rid))
            "…and the lake did"))
      (is (some? obj-rid) "the :thing object-type resolves to a rid")
      (is (elsewhere? (naming/serving-table obj-rid))
          "and so is the SERVING instance table — that is Phase 5")
      (with-open [c (pgds/open (tsys/ds))]
        (is (pgds/table-exists? c "main" (naming/serving-table obj-rid))
            "…in the lake, where a branch namespace can give it per-branch rows")))))

(deftest the-builds-provenance-is-on-POSTGRES
  (when-not (skip?)
    (let [run-id (get-in @built [:body :run-id])]
      (testing "the run row is in control_runs on Postgres"
        (let [r (run-rows/fetch-run (ctx/store "main") "main" run-id)]
          (is (some? r) "the row was found on Postgres")
          (is (= "succeeded" (:status r)))))
      (testing "every terminal Job COMPLETED, and the ledger that says so is Postgres"
        (let [jobs (:jobs (:body (api/get! :run-jobs {:id run-id})))]
          (is (<= 3 (count jobs)) "one Job per terminal transform")
          (is (= {"COMPLETED" 4 "DID_NOT_RUN" 1} (frequencies (map (comp str :status) jobs)))
              (pr-str (mapv (juxt :transform_rid :status :error) jobs))))
        (is (some? (jdbc/execute-one!
                    (tsys/ds)
                    [(str "SELECT 1 FROM " (naming/dataset-table cc/ledger-jobs-rid)
                          " WHERE run_id = ?") run-id]))
            "the jobs rows are physically on Postgres"))
      (testing "and the build's dataset transactions are Postgres rows too"
        (is (some? (jdbc/execute-one!
                    (tsys/ds)
                    [(str "SELECT 1 FROM " (naming/dataset-table cc/ledger-txns-rid)
                          " WHERE run_id = ?") run-id]))
            "a transaction stamped with this run is on Postgres")))))

(deftest a-rebuild-that-changes-nothing-moves-nothing-in-the-LAKE
  (when-not (skip?)
    @built
    (with-open [c (pgds/open (tsys/ds))]
      (let [tables (vec (pgds/tables-own c "main"))]
        (is (seq tables) "the store holds the build's tables at all")
        (doseq [cycle (range 1 4)]
          (testing (str "rebuild " cycle " of 3 — nothing changed upstream")
            (let [low (pgds/current-snapshot c)
                  r   (api/post! :import-run)]
              (is (bs/ok? r) "the rebuild succeeded")
              (is (nil? (get-in r [:body :warnings]))
                  (str "no reprocessing smoke-alarm: " (pr-str (get-in r [:body :warnings]))))
              (is (= 2 (count (bs/objs :thing)))  "still two things")
              (is (= 2 (count (bs/objs :summary))) "still two summaries")
              (doseq [t tables]
                (let [d (pgds/table-delta c {:schema "main" :table t
                                             :pk-col "internal_uuid"
                                             :low (long low)
                                             :high (long (pgds/current-snapshot c))})]
                  (when d
                    (is (empty? (:changed d))
                        (str t " moved rows on a no-op rebuild — the snapshot write"
                             " is reinserting rather than merging"))
                    (is (empty? (:removed d))
                        (str t " lost rows on a no-op rebuild"))))))))))))

(deftest the-branch-build-reads-isolated-and-leaves-the-trunk-alone
  (when-not (skip?)
    @built
    (let [child (str "scen-" (subs (str (random-uuid)) 0 8))]
      (try
        (is (bs/ok? (api/post! :branches {:parent "main" :name child})) "fork a scenario branch")
        (tx/with-branch child
          (is (= 2 (count (bs/objs :thing))) "the branch starts by reading the trunk's rows")
          (is (bs/ok? (api/post! :dataset-write {:name "seed"}
                                 {:rows [{:values {:tid "t3" :label "gamma"}}]
                                  :message "seed on the branch"}))
              "a third seed row, on the branch only")
          (is (bs/ok? (api/post! :import-run)) "build on the branch")
          (testing "the branch sees its own row ON TOP OF the trunk's — the copy-on-write"
            (is (= 3 (count (bs/objs :thing))))
            (is (= #{"t1" "t2" "t3"} (set (map #(bs/val-of % :pk) (bs/objs :thing)))))
            (is (= 3 (count (bs/objs :summary))) "and the chain rebuilt on the branch")))
        (testing "and the trunk never learned about it"
          (is (= 2 (count (bs/objs :thing))))
          (is (= 2 (count (bs/objs :summary)))))
        (finally
          (api/delete! :branch {:name child})))
      (testing "dropping the branch takes its lake namespace with it"
        (with-open [c (pgds/open (tsys/ds))]
          (is (not (contains? (set (pgds/list-branches c)) (pgns/read-schema child)))
              "a stranded namespace would be inherited by the next branch of this name"))))))

(deftest the-run-left-no-fork-behind
  (when-not (skip?)
    @built
    (let [branches (set (map :name (:branches (:body (api/list-branches)))))]
      (is (not-any? #(str/starts-with? (str %) "run/") branches)
          (str "a build must not fork a branch — saw " (pr-str branches))))))

(deftest the-run-dropped-the-object-types-it-landed-for-sql
  (when-not (skip?)
    @built
    (let [things (bs/objs :thing) echoes (bs/objs :echo)]
      (is (seq things) "the chain built :thing")
      (is (= (set (map #(bs/val-of % :label) things)) (set (map #(bs/val-of % :echo) echoes)))
          "mk-echo's SQL read every :thing through its landed scratch dataset"))
    (let [store     (ctx/store)
          schema (seat/branch-schema store)
          t      (get-in schema [:transforms (lookup/rid-of schema :transforms :mk-echo)])]
      (is (= {:state :ok} (mat/explain-transform store schema t))
          "mk-echo's SQL plans against :thing's declared shape, nothing landed or read"))
    (is (empty? (dsets/dataset-names (ctx/store) "l_"))
        "mk-echo's SQL read :thing landed as a scratch dataset; the run drops it")))

(deftest a-table-the-lake-does-not-hold-reads-empty
  (when-not (skip?)
    @built
    (let [seed-rid (some (fn [[rid d]] (when (= :seed (keyword (:dataset-name d))) rid))
                         (:datasets (:body (api/get! :datasets))))
          tbl      (naming/dataset-table seed-rid)
          absent   (str tbl "_never")]
      (is (some? seed-rid))
      (testing "a physical table nobody holds has no fingerprint, and does not throw"
        (is (nil? (dsets/content-signature (ctx/store "main") absent))
            "nil is 'nothing to compare', which is what a change probe needs"))

      (testing "a store-resident dataset still answers from the lake"
        (with-open [c (pgds/open (tsys/ds))]
          (is (pgds/table-exists? c "main" tbl)))
        (is (some? (dsets/content-signature (ctx/store "main") tbl))
            "the build wrote it, so it has a fingerprint")
        (is (= 2 (dsets/table-count (ctx/store "main") seed-rid))
            "and its rows, off the store that holds them")))))
