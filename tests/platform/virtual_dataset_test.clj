(ns ^:unit platform.virtual-dataset-test
  (:require [sutra.parser.spec :as spec]
            [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [sutra.builds.import.sources :as import-sources]
            [sutra.builds.plan.inputs :as inputs]
            [sutra.builds.plan :as plan]
            [sutra.parser.lint :as lint]
            [sutra.store.access.scope :as scope]
            [sutra.builds.import.materialize :as mat]
            [sutra.builds.plan.virtual :as virtual]
            [sutra.schedules.triggers :as triggers]
            [sutra.sources.dialect :as dialect]
            [sutra.sources.jdbc :as fjdbc]
            [sutra.sources.mem :as mem]
            [sutra.parser.ent :as ent]
            [sutra.types.virtual :as vt]
            [sutra.authoring.install :as install]
            [sutra.authoring.install.plan :as iplan]))

(defn- reach-dir [] (doto (java.io.File. "/tmp/sutra-reach") .mkdirs))

(def ^:private customers
  {:columns [{:name "ID" :type "INTEGER"} {:name "Name" :type "TEXT"}
             {:name "Modified" :type "TEXT"}]
   :rows    [{"ID" 2 "Name" "Bea" "Modified" "2026-10-02"}
             {"ID" 1 "Name" "Ada" "Modified" "2026-10-01"}]})

(defn- ds [virtual & {:as more}]
  (merge {:dataset-name :customers :kind :virtual
          :virtual (merge {:source :erp :table "customers"} virtual)}
         more))

(defn- mem-world [state]
  [(fn [spec] ((mem/connector state) spec))
   {:sources {"src" {:source-name :erp :type :mem}}}])

(deftest the-grammar-pairs-kind-and-declaration
  (let [explain #(ent/explain {:schema {:datasets {:customers %}}})]
    (is (nil? (explain {:kind :virtual :virtual {:source :erp :table "customers"
                                                 :primary-key [:id]
                                                 :detect {:column :modified :every-ms 5000}}})))
    (testing "one without the other names nothing or is never read"
      (is (some? (explain {:kind :virtual})))
      (is (some? (explain {:kind :standalone :virtual {:source :erp :table "customers"}}))))
    (testing "no feed beside the source"
      (is (some? (explain {:kind :virtual :virtual {:source :erp :table "customers"}
                           :external-source {:datasource :erp :table "customers"}}))))
    (testing "the declaration is closed"
      (is (some? (explain {:kind :virtual
                           :virtual {:source :erp :table "customers" :where "x"}}))))))

(deftest a-virtual-dataset-is-judged-against-its-source
  (let [sources {:erp {:type :postgres} :mail {:type :imap} :land {:type :filesystem}}
        probs   (fn [d & [writers]]
                  (set (map :problem (vt/problems sources (or writers {}) :customers d))))]
    (is (empty? (probs (ds {}))))
    (is (= #{:unknown-source} (probs (ds {:source :nope}))))
    (is (= #{:source-cannot-scan} (probs (ds {:source :mail}))))
    (is (= #{:source-cannot-scan} (probs (ds {:source :land}))))
    (is (= #{:unsafe-table} (probs (ds {:table "customers; DROP TABLE x"}))))
    (is (= #{:key-not-a-column :detect-not-a-column}
           (probs (ds {:primary-key [:id] :detect {:column :modified}}
                      :columns [{:attribute-name :name :attribute-type :string}]))))
    (is (= #{:written-by-transform} (probs (ds {}) {:customers #{:t}})))))

(deftest a-read-scans-the-source-in-place
  (let [state (atom {:tables {"customers" customers}})
        [connect schema] (mem-world state)]
    (testing "every row, as the source names its columns, in key order when one is declared"
      (is (= [{:id 1 :name "Ada" :modified "2026-10-01"} {:id 2 :name "Bea" :modified "2026-10-02"}]
             (virtual/rows connect schema (ds {:primary-key [:id]})))))
    (testing "declared :columns are the projection the source is asked"
      (is (= #{{:name "Ada"} {:name "Bea"}}
             (set (virtual/rows connect schema
                                (ds {} :columns
                                    [{:attribute-name :name :attribute-type :string}]))))))
    (testing "a change at the source is the next read"
      (swap! state update-in [:tables "customers" :rows] conj
             {"ID" 3 "Name" "Cy" "Modified" "2026-10-03"})
      (is (= 3 (count (virtual/rows connect schema (ds {}))))))
    (testing "an undeclared source is refused"
      (is (thrown? clojure.lang.ExceptionInfo
                   (virtual/rows connect schema (ds {:source :other})))))))

(deftest a-sql-transform-reads-it-through-a-staging-table
  (is (= "v_customers" (mat/resolve-input-table
                        {:datasets {"r" (ds {})}}
                        nil {:kind :dataset :dataset :customers} nil)))
  (is (= "v_kunden_aktiv" (mat/virtual-staging-table {:dataset-name :kunden-aktiv}))))

(deftest a-probe-tells-a-change
  (testing "without SQL: max and count of the :detect column over the rows"
    (let [state (atom {:tables {"customers" customers}})
          [connect schema] (mem-world state)
          d   (ds {:detect {:column :modified :every-ms 1000}})
          s1  (virtual/signature connect schema d true)]
      (is (= [:virtual [:max "2026-10-02" :count 2]] s1))
      (is (= s1 (virtual/signature connect schema d)) "inside :every-ms the last probe answers")
      (swap! state assoc-in [:tables "customers" :rows 0 "Modified"] "2026-10-09")
      (is (= [:virtual [:max "2026-10-09" :count 2]] (virtual/signature connect schema d true)))))
  (testing "without a :column: a digest of every row, blind to their order"
    (let [state (atom {:tables {"customers" customers}})
          [connect schema] (mem-world state)
          d   (ds {:detect {}})
          s1  (virtual/signature connect schema d true)]
      (swap! state update-in [:tables "customers" :rows] (comp vec reverse))
      (is (= s1 (virtual/signature connect schema d true)))
      (swap! state assoc-in [:tables "customers" :rows 0 "Name"] "Bee")
      (is (not= s1 (virtual/signature connect schema d true)))))
  (testing "without :detect nothing stands for the rows"
    (let [[connect schema] (mem-world (atom {:tables {"customers" customers}}))]
      (is (nil? (virtual/signature connect schema (ds {}) true))))))

(deftest a-database-is-probed-by-one-aggregate
  (is (= "SELECT MAX([Modified]) AS m, COUNT(*) AS n FROM [Customers]"
         (dialect/probe-sql :mssql "Customers" "Modified")))
  (is (= "SELECT MAX(\"modified\") AS m, COUNT(*) AS n FROM \"customers\""
         (dialect/probe-sql :postgres "customers" "modified")))
  (let [f   (doto (java.io.File/createTempFile "virtual-probe" ".db" (reach-dir)) .deleteOnExit)
        url (str "jdbc:sqlite:" (.getPath f))]
    (with-open [c (jdbc/get-connection {:jdbcUrl url})]
      (jdbc/execute! c
                     ["CREATE TABLE customers (id INTEGER PRIMARY KEY, name TEXT, modified TEXT)"])
      (jdbc/execute! c
                     ["INSERT INTO customers VALUES (1, 'Ada', '2026-10-01'), (2, 'Bea', '2026-10-02')"]))
    (let [connect (fn [spec] (fjdbc/source spec {}))
          schema {:sources {"src" {:source-name :erp :type :sqlite :jdbc-url url}}}
          d      (ds {:detect {:column :modified}})]
      (is (= [:virtual [:max "2026-10-02" :count 2]] (virtual/signature connect schema d true)))
      (with-open [c (jdbc/get-connection {:jdbcUrl url})]
        (jdbc/execute! c ["UPDATE customers SET modified = '2026-10-05' WHERE id = 1"]))
      (is (= [:virtual [:max "2026-10-05" :count 2]] (virtual/signature connect schema d true))))))

(deftest a-trigger-cannot-watch-a-blind-virtual-dataset
  (let [schema  {:datasets {"r1" (ds {}) "r2" (ds {:detect {}} :dataset-name :watched)}}
        trigger (fn [of] {:kind :parent-updated :of of})]
    (is (nil? (triggers/assert-watchable! schema (trigger :watched))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"declare no :detect"
                          (triggers/assert-watchable! schema (trigger :customers))))))

(deftest install-plans-it-as-a-dataset-and-judges-it
  (let [schema (fn [virtual] {:projects {:p {}}
                              :sources
                                {:erp {:project :p :type :postgres
                                       :jdbc-url "jdbc:postgresql://db:5432/erp"
                                       :egress ["db:5432"]}}
                              :datasets {:customers {:project :p :kind :virtual :virtual virtual}}})
        ok     (schema {:source :erp :table "customers" :detect {:column :modified}})
        plan   (install/plan-schema ok {} {})
        step   (first (filter #(and (= :dataset (:kind %)) (= :customers (:entity %))) plan))]
    (testing "a dataset like any, its declaration carried"
      (is (= :created (:outcome step)))
      (is (= {:source :erp :table "customers" :detect {:column :modified}}
             (get-in step [:args :virtual]))))
    (testing "no producer is minted for it"
      (is (not-any? #(= :transform (:kind %)) plan)))
    (testing "a source that does not exist refuses the install"
      (let [bad (schema {:source :nope :table "customers"})]
        (is (some #(= :virtual-dataset-invalid (:rule %))
                  (iplan/plan-problems bad {} (install/plan-schema bad {} {}))))))))

(deftest an-object-type-may-be-backed-by-it
  (let [state  (atom {:tables {"customers" customers}})
        [connect w] (mem-world state)
        store     {:authority nil}
        schema (assoc w
                      :datasets {"rv" (ds {:primary-key [:id]
                                           :detect {:column :modified :every-ms 1000}})}
                      :object-types {"ri.kunde" {:object-type-name :kunde
                                                 :attribute-list
                                                   [{:attribute-name :id :attribute-type :integer}
                                                    {:attribute-name :name :attribute-type :string}]
                                                 :backing {:dataset :customers :primary-key :id}
                                                 :plural "Kunden"}})
        p      (plan/derive-plan-from-schema schema)
        from   (get-in p [:objects "ri.kunde" :from])]
    (with-redefs [scope/connect (fn [_] connect)]
      (testing "its objects are indexed from a scan of the source"
        (is (= {:dataset :customers} from))
        (is (= #{"Ada" "Bea"}
               (set (map :name
                         (inputs/read-source-rows {:store store :schema schema :row-cache (atom {})}
                                                   from nil))))))
      (testing "the reindex poller's signature stands on its probe, so a change at the source reindexes"
        (with-redefs [import-sources/read-spec (fn [_] {:datasources {} :import p})
                      scope/at           (fn [store _] store)]
          (reset! @#'virtual/probes {})
          (let [before (import-sources/source-signature store schema "main")]
            (reset! @#'virtual/probes {})
            (is (= before (import-sources/source-signature store schema "main"))
                "unchanged source, same signature")
            (reset! @#'virtual/probes {})
            (swap! state update-in [:tables "customers" :rows] conj
                   {"ID" 3 "Name" "Cy" "Modified" "2026-10-03"})
            (is (not= before (import-sources/source-signature store schema "main")))))))))

(deftest install-and-lint-take-a-virtual-backing
  (let [schema {:projects {:p {}}
                :sources  {:erp {:project :p :type :postgres
                                 :jdbc-url "jdbc:postgresql://db:5432/erp" :egress ["db:5432"]}}
                :datasets {:customers {:project :p :kind :virtual
                                       :virtual
                                         {:source :erp :table "customers" :primary-key [:id]
                                          :detect {:column :modified}}
                                       :columns [{:attribute-name :id :attribute-type :integer}
                                                 {:attribute-name :name :attribute-type :string}
                                                 {:attribute-name :modified
                                                  :attribute-type :string}]}}
                :objects  {:kunde {:project :p :primary-key :id
                                   :attributes {:id :integer :name :string}
                                   :backing {:dataset :customers :primary-key :id}
                                   :plural "Kunden"}}}
        plan   (install/plan-schema schema {} {})]
    (is (some #(= [:object-type :kunde :created] ((juxt :kind :entity :outcome) %)) plan))
    (is (empty? (iplan/plan-problems schema {} plan)))
    (is (not-any? #(= :backing-without-producer (:id %)) (lint/run-lint schema (spec/vocabulary)))
        "a virtual dataset is filled by its source")))

(deftest a-limited-read-is-the-first-rows-by-key
  (testing "the limit cuts AFTER the key order, not before it"
    (let [state (atom {:tables {"customers"
                                {:columns (:columns customers)
                                 :rows    (vec (for [i [5 3 1 4 2]]
                                                 {"ID" i "Name" (str "n" i)
                                                  "Modified" "2026-10-01"}))}}})
          [connect schema] (mem-world state)]
      (is (= [1 2] (mapv :id (virtual/rows connect schema (ds {:primary-key [:id]}) 2)))
          "a source free to cut first would answer whichever two it holds first, sorted")
      (is (= [1 2 3 4 5] (mapv :id (virtual/rows connect schema (ds {:primary-key [:id]})))))))
  (testing "and over SQL the order reaches the statement, so the source cuts the right rows"
    (let [f   (doto (java.io.File/createTempFile "virtual-order" ".db" (reach-dir)) .deleteOnExit)
          url (str "jdbc:sqlite:" (.getPath f))]
      (with-open [c (jdbc/get-connection {:jdbcUrl url})]
        (jdbc/execute! c
                       ["CREATE TABLE customers (id INTEGER PRIMARY KEY, name TEXT, modified TEXT)"])
        (doseq [i [5 3 1 4 2]]
          (jdbc/execute! c ["INSERT INTO customers VALUES (?, ?, '2026-10-01')" i (str "n" i)])))
      (let [connect (fn [spec] (fjdbc/source spec {}))
            schema {:sources {"src" {:source-name :erp :type :sqlite :jdbc-url url}}}]
        (is (= [1 2] (mapv :id (virtual/rows connect schema (ds {:primary-key [:id]}) 2))))
        (is (= [1 2 3 4 5] (mapv :id (virtual/rows connect schema (ds {:primary-key [:id]})))))))))
