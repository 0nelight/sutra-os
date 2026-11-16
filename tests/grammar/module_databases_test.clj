(ns ^:unit grammar.module-databases-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.kit.anomaly :as anomaly]
            [sutra.store.api :as api]
            [sutra.store.control.tables :as cc]
            [sutra.store.storage.module-dbs :as mdb]))

(def ^:private resource-file "resources/sutra/table-owners.edn")

(defn- modules [] (:modules (edn/read-string (slurp "cores.edn"))))

(def ^:private sql-names
  (into {} (keep (fn [{:keys [name table]}] (when table [name table]))) cc/tables))

(defn- sources-of [m]
  (let [d (io/file "src/sutra" (str/replace (str m) "-" "_"))]
    (when (.isDirectory d)
      (str/join "\n" (for [f (file-seq d) :when (re-find #"\.clj[cs]?$" (.getName f))]
                       (slurp f))))))

(defn derived
  []
  (let [{:keys [tables cross-module-tx]} (modules)]
    {:tables          (into (sorted-map) tables)
     :cross-module-tx cross-module-tx
     :raw-sql         (into (sorted-map)
                            (for [[m ts] tables
                                  :let [text (str (sources-of m))
                                        hits (filterv #(some->> (sql-names (keyword %))
                                                                (str/includes? text))
                                                      ts)]
                                  :when (seq hits)]
                              [m hits]))}))

(defn render []
  (str ";; Derived from cores.edn by grammar.module-databases-test — do not edit.\n"
       (with-out-str (pp/pprint (derived)))))

(deftest the-resource-is-current
  (is (= (render) (slurp resource-file))
      (str resource-file " is stale — run (grammar.module-databases-test/-main)")))

(deftest every-declared-crossing-is-well-formed
  (let [{:keys [tables cross-module-tx]} (modules)
        known (into '#{world lease store} (keys tables))]
    (doseq [{ms :modules why :why :as e} cross-module-tx]
      (testing (pr-str ms)
        (is (< 1 (count ms)) "a crossing names at least two parties")
        (is (= #{} (set (remove known ms))) "names modules that own tables, world or lease")
        (is (seq (str/trim (str why))) "says why")
        (is (= (count ms) (count (set ms))) e)))))

(def ^:private owned
  {:tables          '{llm [llm-budget] health [health-checks] access [sessions]}
   :cross-module-tx '[{:modules [llm world] :why "spends budget in the commit it records"}]
   :raw-sql         '{access [sessions]}})

(deftest a-module-that-cannot-move-is-refused
  (let [why (fn [routes] (map :why (mdb/refusals owned routes)))]
    (testing "a module in a crossing"
      (is (some #(str/includes? % "commits together") (why '{llm "jdbc:postgresql://x/y"}))))
    (testing "a module reaching its tables by SQL"
      (is (some #(str/includes? % "by SQL") (why '{access "jdbc:postgresql://x/y"}))))
    (testing "a module whose collections go with a branch"
      (is (some #(str/includes? % "branches") (why '{health "jdbc:postgresql://x/y"}))))
    (testing "an unknown module and a url that is not postgres"
      (is (= 2 (count (why '{nobody "mysql://x"})))))
    (testing "the setting is read as module=url pairs"
      (is (= '{llm "jdbc:postgresql://h/llm" apps "jdbc:postgresql://h/apps"}
             (mdb/routes {mdb/setting " llm=jdbc:postgresql://h/llm, apps=jdbc:postgresql://h/apps"}))))
    (testing "check! names every reason in one refusal"
      (is (thrown? Exception (mdb/check! owned '{llm "jdbc:postgresql://x/y"}))))))

(defn- recording-db [name calls]
  (reify api/Db
    (-snapshot [_ _] nil)
    (-prepare [_ tx] (swap! calls conj [name (mapv :coll (:records tx))]) name)
    (-branch! [_ _] nil)
    (-read-at [_ _ _ q] [name q])))

(deftest a-routed-db-sends-a-modules-records-to-its-own-db
  (let [calls (atom [])
        main  (recording-db :main calls)
        own   (recording-db :llm calls)
        db    (mdb/routed main '{:tables {llm [llm-budget]}} {'llm own})
        rec   (fn [c] {:coll c :key 1 :value {}})
        base  {:basis {:branch "main" :rev 1} :actor {:principal "t"}}]
    (is (= :llm (api/-prepare db (assoc base :records [(rec :ctl/llm-budget)]))))
    (is (= :main (api/-prepare db (assoc base :records [(rec :ctl/sessions)]))))
    (is (= :main (api/-prepare db (assoc base :ops []))) "a world commit stays")
    (is (anomaly/refusal? (api/-prepare db (assoc base :records [(rec :ctl/llm-budget)
                                                                  (rec :ctl/sessions)])))
        "records of a routed module and another never share a commit")
    (is (anomaly/refusal? (api/-prepare db (assoc base :records [(rec :ctl/llm-budget)]
                                                 :log [{:kind :x/y :payload 1}])))
        "nor a routed module's records and the world")
    (is (= [:llm [:records/get :ctl/llm-budget 1]]
           (api/-read-at db {:branch "main" :rev 7} nil [:records/get :ctl/llm-budget 1])))
    (is (= [:main [:records/get :ctl/sessions 1]]
           (api/-read-at db {:branch "main" :rev 7} nil [:records/get :ctl/sessions 1])))
    (is (= main (mdb/routed main '{:tables {}} {})) "nothing routed: the Db as it was")))

(defn -main [& _]
  (spit resource-file (render))
  (println "wrote" resource-file))
