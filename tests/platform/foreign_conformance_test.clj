(ns ^:unit platform.foreign-conformance-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [sutra.sources.protocols :as api]
            [sutra.sources.conformance :as conformance]
            [sutra.sources.dialect :as dialect]
            [sutra.sources.http-connector :as fhttp]
            [sutra.sources.jdbc :as fjdbc]
            [sutra.sources.mem :as mem]
            [sutra.sources.policy :as policy]
            [sutra.sources.rest :as rest]
            [sutra.parser.spec :as spec]
            [sutra.store.scan :as scan]
            [sutra.types.source :as source]
            [next.jdbc :as jdbc]))

(defn- reach-dir [] (doto (java.io.File. "/tmp/sutra-reach") .mkdirs))

(def ^:private t0 (java.time.LocalDateTime/of 2026 10 7 9 30 5))

(defn- mem-source [{:keys [tables relations]} spec]
  ((mem/connector (atom {:tables tables :relations relations}) {:clock (constantly t0)})
   (assoc spec :type :mem)))

(defn- create-sql [table {:keys [columns pk references]}]
  (str "CREATE TABLE " table " ("
       (str/join ", " (for [{:keys [name type]} columns]
                        (str name " " type
                             (when (= name pk) " PRIMARY KEY")
                             (when-let [[t c] (get references name)]
                               (str " REFERENCES " t "(" c ")")))))
       ")"))

(defn- sqlite-source [{:keys [tables]} spec]
  (let [f   (doto (java.io.File/createTempFile "foreign-conformance" ".db" (reach-dir))
              .deleteOnExit)
        url (str "jdbc:sqlite:" (.getPath f))]
    (with-open [c (jdbc/get-connection {:jdbcUrl url})]
      (doseq [[t d] (sort-by key tables)]
        (jdbc/execute! c [(create-sql t d)])
        (doseq [row (:rows d)
                :let [cols (sort (keys row))]]
          (jdbc/execute! c (into [(str "INSERT INTO " t " (" (str/join ", " cols) ") VALUES ("
                                       (str/join ", " (repeat (count cols) "?")) ")")]
                                 (map row cols))))))
    (fjdbc/source (assoc spec :type :sqlite :jdbc-url url) {:clock (constantly t0)})))

(deftest mem-conforms
  (conformance/check-all mem-source))

(deftest jdbc-on-sqlite-conforms
  (conformance/check-all sqlite-source))

(deftest mem-invokes
  (let [state (atom {})]
    (conformance/check-invoke
     (fn [{:keys [tables relations]} spec]
       (swap! state assoc :tables tables :relations relations)
       ((mem/connector state {:clock (constantly t0)}) (assoc spec :type :mem)))
     (fn [_] (:calls @state)))))

(deftest rest-as-data-conforms
  (conformance/check-rest fhttp/source))

(deftest glpi-is-a-preset
  (conformance/with-fake-api
    (fn [base log]
      (let [f    (doto (java.io.File/createTempFile "glpi" ".json") .deleteOnExit)
            spec {:type :glpi-api :path (.getPath f) :egress [(subs base 7)]
                  :rest {:base-url base
                         :auth {:mode :glpi-session :user-token "U" :app-token "A"}
                         :read-itemtypes #{"Computer"}}}]
        (testing "GLPI's session, its range paging and its itemtypes come from the preset"
          (is (= {:path "/apirest.php/Computer" :paging {:style :range :size 500}
                  :query {"expand_dropdowns" "false" "get_hateoas" "false"} :key :id}
                 (get-in (fhttp/declaration spec) [:resources :Computer]))))
        (testing "the landed bundle is every record as the API answered it"
          (rest/land-glpi-bundle! spec)
          (let [b (json/read-str (slurp f))]
            (is (= ["Computer"] (keys b)))
            (is (= 7 (count (get b "Computer"))))
            (is (= {"id" 1 "name" "n1" "status" 1} (first (get b "Computer"))))))
        (testing "opening and closing the session carry the App-Token as well"
          (let [seen (into {} (map (juxt :uri :app-token)) @log)]
            (is (= "A" (seen "/apirest.php/initSession")))
            (is (= "A" (seen "/apirest.php/killSession")))))))))

(deftest a-rest-resource-lands-only-its-declared-columns
  (conformance/with-fake-api
    (fn [base _]
      (let [spec {:type :rest :egress [(subs base 7)]
                  :rest {:base-url base
                         :auth {:mode :glpi-session :user-token "U"}
                         :resources {:alle   {:path "/none"}
                                     :wenige {:path "/none"
                                              :columns [{:name "id" :type :long}
                                                        {:name "NAME" :type :string}]}}}}
            src  (fhttp/source spec)]
        (testing "without :columns every field of the record lands"
          (is (= #{:id :name :status} (set (mapcat keys (api/scan src "alle"))))))
        (testing "with :columns only those, matched without regard to case"
          (is (= #{:id :name} (set (mapcat keys (api/scan src "wenige")))))
          (is (= 7 (count (api/scan src "wenige")))))))))

(deftest a-connector-is-asked-by-its-profile
  (testing "an :inexact negation is never pushed; a single-comparison connector takes no structure"
    (let [p (:scan (source/profile :mssql))]
      (is (= [:inexact :none :exact :none]
             (:where (api/offer p
                                {:where [[:= :a 1] [:not [:= :a 1]] [:nil? :a true]
                                         [:not= :a 1]]}))))
      (is (false? (:page (api/offer p {:where [[:= :a 1]] :limit 3}))))
      (is (true? (:page (api/offer p {:where [[:nil? :a false]] :limit 3}))))
      (is (= [:none :inexact]
             (:where (api/offer (fhttp/scan-profile {:filters {:= {:param "q"}}})
                                {:where [[:or [:= :a 1]] [:= :a 1]]}))))))
  (testing "capabilities are the profile's"
    (is (policy/capable? {:type :mssql} :write))
    (is (not (policy/capable? {:type :csv} :write)))
    (is (= :rest (source/read-mode {:type :rest})))
    (is (= :landed (source/read-mode {:type :glpi-api})))))

(deftest mssql-scans-as-dialect-builders
  (let [cols   {:name {:name "NAME" :type "nvarchar(50)"} :id {:name "ID" :type "int"}
                :memo {:name "MEMO" :type "ntext"} :at {:name "AT" :type "datetime"}
                :note {:name "NOTE" :type "text"}}
        absent (fn [p] (scan/holds? [nil {}] p))
        stmt   #(dialect/scan-stmt :mssql "ITEMS" cols absent %)]
    (is (= ["SELECT TOP 5 [ID], [NAME] FROM [ITEMS] WHERE [NAME] = ? AND [ID] IN (?, ?)" "a" 1 2]
           (stmt {:where [[:= :name "a"] [:in :id [1 2 nil]]] :project #{:id :name} :limit 5})))
    (testing "a value the column would have to convert is not bound: 1=1, the core filters"
      (is (= ["SELECT * FROM [ITEMS] WHERE 1=1 AND 1=1 AND 1=1"]
             (stmt {:where [[:= :id "1"] [:= :name 1] [:= :at "2026-01-01"]]}))))
    (testing "text is compared through the guard cast; ntext not at all"
      (is (= ["SELECT * FROM [ITEMS] WHERE CAST([NOTE] AS varchar(max)) = ? AND 1=1" "x" ]
             (stmt {:where [[:= :note "x"] [:= :memo "x"]]}))))
    (testing "an absent column is the truth the semantics give an absent value"
      (is (= ["SELECT * FROM [ITEMS] WHERE 1=0 AND 1=1 AND NOT ([ID] IS NULL OR 1=0)"]
             (stmt {:where [[:= :gone 1] [:nil? :gone true]
                            [:not [:or [:nil? :id true] [:nil? :gone false]]]]}))))
    (is (= ["SELECT * FROM `ITEMS` WHERE `NAME` = ? LIMIT 3" "a"]
           (dialect/scan-stmt :mysql "ITEMS" cols absent {:where [[:= :name "a"]] :limit 3})))))

(deftest a-source-spec-is-typed-by-its-type
  (let [ok? #(spec/source-valid? %)]
    (is (ok? {:type :mssql :jdbc-url "jdbc:sqlserver://h:1433" :egress ["h:1433"]}))
    (is (not (ok? {:type :msql :jdbc-url "jdbc:sqlserver://h:1433" :egress ["h:1433"]}))
        "an unknown type")
    (is (not (ok? {:type :mssql :egress ["h:1433"]})) "a needed field missing")
    (is (ok? {:type :rest :egress ["h:443"] :external-transform true
              :config {:base-url "https://h"}}))
    (is (not (ok? {:type :rest :egress ["h:443"] :access :read-write
                   :webhooks {:x {:method :post :path-tmpl "/x"}}})) "a misspelt webhook")
    (is (ok? {:type :rest :egress ["h:443"]
              :rest {:base-url "https://h" :auth {:mode :bearer}
                     :resources {:items {:path "/items" :paging {:style :cursor :size 50}}}}}))
    (is (not (ok? {:type :rest :egress ["h:443"]
                   :rest {:resources {:items {:path "/items" :paging {:style :pages}}}}}))
        "an unknown paging style")
    (is (not (ok? {:type :rest :egress ["h:443"] :rest {:auth {:mode :kerberos}}}))
        "an unknown auth mode")))

(deftest a-column-is-typed-as-the-value-type-it-holds
  (is (= [:integer :long :byte :short :double :double :decimal :decimal :string :string :date
          :timestamp :boolean :string]
         (map dialect/sql-type->ontology-type
              ["INTEGER" "bigint" "tinyint" "smallint" "float" "double precision" "numeric(18,4)"
               "money"
               "nvarchar(50)" "ntext" "date" "datetime2(7)" "bit" "geography"])))
  (is (= :integer (dialect/sql-type->ontology-type "int identity"))
      "an MSSQL identity column is its base type")
  (is (= :string (dialect/sql-type->ontology-type nil))))

(deftest a-rest-resource-declares-its-columns-and-key
  (let [spec {:type :rest :egress ["api.example:443"]
              :rest {:base-url "https://api.example"
                     :resources {:computers {:path "/c" :key :ID
                                             :columns
                                               [{:name "id" :type :long}
                                                {:name "name" :type :string}]}}}}]
    (is (= [{:name "id" :type "long" :ontology-type :long}
            {:name "name" :type "string" :ontology-type :string}]
           (fhttp/declared-columns spec "computers")))
    (is (nil? (fhttp/declared-columns spec "other")))
    (is (= [:id] (policy/scan-key spec "computers")))
    (is (spec/source-valid? spec) "a declared column list is part of the typed spec")
    (is (not (spec/source-valid? (assoc-in spec [:rest :resources :computers :columns 0 :type]
                                           :nonsense))))))

(deftest a-scan-key-is-what-a-writeback-declares
  (is (= [:id] (policy/scan-key conformance/writeback "ITEMS")))
  (is (= [:a :b] (policy/scan-key {:writeback {"T" {:pk-columns ["A" "B"]}}} "T")))
  (is (nil? (policy/scan-key conformance/writeback "ITEM_LOG")))
  (is (= "1_|_x" (policy/row-key [:a :b] {:a 1 :b "x"})) "a composite key is a composite :pk-value")
  (is (= ["1" "x"] (policy/pk-parts ["A" "B"] (policy/row-key [:a :b] {:a 1 :b "x"})))))

(def ^:private sql-lowering-allowed
  {:not-built   #{[:< :text] [:< :varchar] [:<= :text] [:<= :varchar]
                  [:> :text] [:> :varchar] [:>= :text] [:>= :varchar]
                  [:not= :text] [:not= :varchar]}
   :coarse      #{[:= :text] [:= :varchar] [:in :text] [:in :varchar]
                  [:starts-with :text] [:starts-with :varchar]}
   :scans-empty :all})

(deftest sql-dialects-lower-what-they-can
  (let [cols (vec (vals conformance/lowering-columns))]
    (doseq [t [:sqlite :postgres :mysql :mssql]]
      (conformance/check-lowering t (fjdbc/table-scan-profile t cols nil) sql-lowering-allowed))))

(deftest lowering-gaps-are-judged-without-a-dialect
  (testing "the verdict follows from the three statements alone"
    (is (= [:not-built]
           (map :verdict (conformance/lowering-gaps
                          [{:op :> :kind :int :wanted :narrowed :emitted :any :declared :none}]))))
    (is (= [:scans-empty]
           (map :verdict (conformance/lowering-gaps
                          [{:op := :kind :int :wanted :false :emitted :any :declared :none}]))))
    (is (= [:coarse]
           (map :verdict (conformance/lowering-gaps
                          [{:op := :kind :text :wanted :narrowed :emitted :narrowed
                            :declared :inexact}]))))
    (is (empty? (conformance/lowering-gaps
                 [{:op := :kind :int :wanted :narrowed :emitted :narrowed :declared :exact}]))
        "built, declared exactly: nothing to say")))

(deftest a-numeric-column-is-judged-exactly-and-a-text-one-is-not
  (testing "the profile judges per [column value-class], so one = is three judgements"
    (let [p (fjdbc/table-scan-profile
             :sqlite [{:name "ID" :type "INTEGER"} {:name "NAME" :type "TEXT"}] [:id])
          j #(first (:where (scan/offer p {:where [%]})))]
      (is (= :exact   (j [:= :id 1]))      "an exact numeric column compares as the semantics do")
      (is (= :inexact (j [:= :name "a"]))  "a text column only up to its collation")
      (is (= :none    (j [:= :id "a"]))    "a value the dialect cannot bind is left to the core")
      (is (= :exact   (j [:= :name nil]))  "nil is 1=0 and holds for nobody")))
  (testing "an exact conjunct is what lets a limit reach the source"
    (let [p (fjdbc/table-scan-profile
             :sqlite [{:name "ID" :type "INTEGER"} {:name "NAME" :type "TEXT"}] [:id])]
      (is (:page (scan/offer p {:where [[:= :id 1]] :limit 1})))
      (is (not (:page (scan/offer p {:where [[:= :name "a"]] :limit 1})))))))

(deftest a-negation-is-not-pushed-into-sql
  (testing "[:not p] is two-valued and SQL's NOT is not: a NULL row the semantics keep"
    (let [src (sqlite-source {:tables {"T" {:columns [{:name "ID" :type "INTEGER"}
                                                      {:name "N" :type "INTEGER"}]
                                            :pk "ID"
                                            :rows [{"ID" 1 "N" 1} {"ID" 2 "N" 2} {"ID" 3}]}}}
                             {:access :read-only :source-name "neg"
                              :writeback {"T" {:pk "ID" :writable-columns []
                                               :insertable-columns []}}})]
      (try
        (let [all (api/scan src "T")]
          (doseq [sc [{:where [[:not [:= :n 1]]]}
                      {:where [[:not [:in :n [1]]]]}
                      {:where [[:not= :n 1]]}
                      {:where [[:not [:nil? :n true]]]}
                      {:where [[:and [:not [:= :n 1]] [:> :id 0]]]}
                      {:where [[:starts-with :n nil]]}
                      {:where [[:not= :n nil]]}
                      {:where [[:< :n nil]]}
                      {:where [[:> :n 1] [:<= :n 2]]}
                      {:where [[:> :n 0]] :limit 1}]]
            (is (= (frequencies (api/run-local all sc))
                   (frequencies (api/scan src "T" sc)))
                (pr-str sc))))
        (finally (api/close! src)))))
  (testing "the profile says so, rather than the dialect being trusted to"
    (let [p (fjdbc/table-scan-profile :sqlite [{:name "N" :type "INTEGER"}] nil)]
      (is (false? (:not? p)))
      (is (= :exact (first (:where (scan/offer (dissoc p :not?) {:where [[:not [:= :n 1]]]}))))
          "and it is the declaration doing the work: without it the conjunct is pushed")
      (is (= :none (first (:where (scan/offer p {:where [[:not [:= :n 1]]]}))))
          "an exact comparison is still not pushed under a :not")
      (is (= :exact (first (:where (scan/offer p {:where [[:not= :n 1]]}))))
          ":not= is a comparison SQL answers as the semantics do, not a structural NOT"))))

(deftest a-one-file-source-is-declared-not-listed
  (testing "the sources whose rows are one file say so in their profile, and scan it"
    (is (= {:csv :csv :sftp :csv}
           (into {} (keep (fn [[t p]] (when (:file p) [t (:file p)]))) source/profiles)))
    (is (every? #(contains? (:capabilities (source/profile %)) :scan)
                (filter source/file-format (keys source/profiles))))
    (is (nil? (source/file-format :postgres)))))
