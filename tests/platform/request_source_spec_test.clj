(ns ^:unit platform.request-source-spec-test
  (:require [sutra.serve.errors :as errors]
            [clojure.string :as str]
            [sutra.store.access.scope :as scope]
            [clojure.test :refer [deftest is testing]]
            [sutra.catalog.grants :as grants]
            [sutra.sources.datasources :as ds]
            [sutra.sources.egress-policies :as policies]
            [sutra.sources.http :as source]
            [sutra.sources.http-connector :as hc]
            [sutra.sources.jdbc :as fjdbc]
            [sutra.sources.policy :as policy]
            [sutra.types.source :as stype]))

(deftest local-sqlite-keeps-its-jdbc-url
  (testing "the source schema requires :jdbc-url for sqlite (parser/spec) — dropping it
            made every UI-created sqlite source read as misconfigured"
    (let [url          "jdbc:sqlite:/tmp/sutra-reach/s.sqlite"
          {:keys [ok]} (source/request->source-spec {:name "s" :type :sqlite :jdbc-url url})]
      (is (= {:name :s :type :sqlite :access :read-only :jdbc-url url} ok))
      (is (nil? (:egress ok)))))
  (testing "without a url it is refused, not stored broken"
    (is (= {:error "sqlite datasource needs :jdbc-url"}
           (source/request->source-spec {:name "s" :type :sqlite})))))

(deftest a-jdbc-url-is-its-types-own-with-one-host
  (doseq [[type url] [[:sqlite "jdbc:mysql://evil/?allowLoadLocalInfile=true&autoDeserialize=true"]
                      [:sqlite "jdbc:sqlite:s.sqlite"]
                      [:sqlite "jdbc:sqlite:file:/etc/x.db?mode=ro"]
                      [:mysql "jdbc:mysql://h:3306/d?autoDeserialize=true"]
                      [:mysql "jdbc:mysql://h:3306,evil:3306/d"]
                      [:mysql "jdbc:mysql://address=(host=evil)(port=3306)/d"]
                      [:postgres "jdbc:postgresql://h:5432,evil:5432/d"]
                      [:postgres "jdbc:postgresql://h/d?socketFactory=x.Y&socketFactoryArg=z"]
                      [:postgres "jdbc:postgresql://h/d?sslFactory%3D=x"]
                      [:mssql "jdbc:sqlserver://h:1433;serverName=evil"]
                      [:oracle "jdbc:oracle:thin:@h:1521/x"]]]
    (is (:error (source/request->source-spec {:name "s" :type type :jdbc-url url})) url))
  (doseq [[type url] [[:postgres "jdbc:postgresql://db.example:5432/d?sslmode=require"]
                      [:mysql "jdbc:mysql://10.0.0.5:3306/d?useSSL=true"]
                      [:mssql "jdbc:sqlserver://h\\SQLEXPRESS:1433;databaseName=d;encrypt=true"]
                      [:sqlite "jdbc:sqlite:/tmp/sutra-reach/x.db"]]]
    (is (nil? (policy/jdbc-url-problem type url)) url)))

(deftest a-sqlite-file-lies-under-the-reach-roots
  (is (= :refusal.foreign/path-outside
         (try (fjdbc/open! {:type :sqlite :jdbc-url "jdbc:sqlite:/etc/sutra.db"}) nil
              (catch clojure.lang.ExceptionInfo e (:refusal/code (ex-data e)))))))

(deftest coded-postgres-shape
  (let [{:keys [ok]} (source/request->source-spec
                      {:name "db" :type :postgres
                       :jdbc-url "jdbc:postgresql://h:5432/d"})]
    (is (= "jdbc:postgresql://h:5432/d" (:jdbc-url ok)))
    (is (= ["h:5432"] (:egress ok)))
    (is (= :read-only (:access ok)))))

(deftest writeback-governance
  (let [{:keys [ok]} (source/request->source-spec
                      {:name "db" :type :mysql :jdbc-url "jdbc:mysql://h:3306/d"
                       :access "read-write" :secret-cipher "SEALED"
                       :writeback {:orders true}
                       :markings ["pii"] :export-markings ["eu"]
                       :password "should-be-ignored"})]
    (is (= :read-write (:access ok)))
    (is (= "SEALED" (:secret-cipher ok)))
    (is (= {:orders true} (:writeback ok)))
    (is (= [:pii] (:markings ok)))
    (is (= [:eu] (:export-markings ok)))
    (is (not (contains? ok :password)))))

(defn- post-test
  ([body] (post-test body nil))
  ([body branch] (post-test body branch []))
  ([body branch egress-policies]
   (with-redefs [scope/connect (fn [auth] (partial ds/connect auth))
                 policies/all  (constantly egress-policies)]
     ((errors/wrap-exceptions source/test-datasource) {:body-params body :sutra/store {} :sutra/schema {}
                              :sutra/branch branch}))))

(deftest datasource-test-candidate-needs-an-egress-policy-once-there-are-any
  (let [reached (atom 0)]
    (with-redefs [ds/read-rows (fn [& _] (swap! reached inc) [])]
      (let [resp (post-test {:type "postgres" :jdbc-url "jdbc:postgresql://evil:5432/d"
                             :secret-cipher "SEALED"}
                            nil [{:id "erp:5432" :state "active"}])]
        (is (false? (get-in resp [:body :ok])))
        (is (zero? @reached))))))

(deftest datasource-test-refuses-plaintext-password
  (let [reached (atom 0)]
    (with-redefs [ds/read-rows (fn [& _] (swap! reached inc) [])]
      (let [resp (post-test {:type "postgres" :jdbc-url "jdbc:postgresql://h:5432/d"
                             :user "u" :password "hunter2"})]
        (is (= 400 (:status resp)))
        (is (re-find #"plaintext :password" (get-in resp [:body :error])))
        (is (not (str/includes? (pr-str resp) "hunter2")) "the body is not echoed")
        (is (zero? @reached) "a refused request reaches nothing")))))

(deftest datasource-test-candidate-goes-through-the-agent
  (let [seen (atom nil)]
    (with-redefs [ds/read-rows (fn [spec query & _] (reset! seen {:spec spec :query query})
                                 [{:one 1}])]
      (let [resp (post-test {:type "postgres" :jdbc-url "jdbc:postgresql://h:5432/d"
                             :secret-cipher "SEALED" :agent "dmz"})]
        (is (= 200 (:status resp)))
        (is (= {:ok true :via "agent"} (:body resp)))
        (is (= "SELECT 1" (:query @seen)))
        (is (= "SEALED" (get-in @seen [:spec :secret-cipher])) "only the sealed secret travels")
        (is (= :dmz (get-in @seen [:spec :agent])))
        (is (= ["h:5432"] (get-in @seen [:spec :egress])))
        (is (not-any? #(contains? (:spec @seen) %) [:user :password]))))))

(deftest datasource-test-agent-down
  (with-redefs [ds/read-rows (fn [& _] (throw (ex-info "connection refused" {})))
                ds/agent-reachable? (constantly false)]
    (let [resp (post-test {:type "mssql" :jdbc-url "jdbc:sqlserver://h:1433;databaseName=d"
                           :secret-cipher "SEALED"})]
      (is (= 200 (:status resp)))
      (is (= {:ok false :code "agent-down" :agent "default"}
             (select-keys (:body resp) [:ok :code :agent]))))))

(deftest datasource-test-replayed-cipher-keeps-its-clearance
  (let [reached (atom 0)]
    (with-redefs [ds/read-rows (fn [& _] (swap! reached inc) [])
                  source/datasources
                  (constantly {:hr {:source-name :hr :type :postgres :secret-cipher "HR-SEALED"
                                    :markings [:pii]}})
                  grants/clearances (constantly #{})]
      (let [resp (post-test {:type "postgres" :jdbc-url "jdbc:postgresql://h:5432/d"
                             :secret-cipher "HR-SEALED"}
                            "main")]
        (is (false? (get-in resp [:body :ok])))
        (is (re-find #"not cleared for source :hr" (get-in resp [:body :error])))
        (is (zero? @reached))))))

(deftest datasource-test-saved-source-uses-its-own-route
  (let [seen (atom nil)]
    (with-redefs [ds/read-rows (fn [spec query & _] (reset! seen [spec query]) [])
                  source/datasources
                  (constantly {:erp {:source-name :erp :type :postgres :secret-cipher "ERP-SEALED"
                                     :jdbc-url "jdbc:postgresql://erp:5432/d"
                                     :egress ["erp:5432"]}})]
      (is (= {:ok true :via "agent"} (:body (post-test {:source "erp"} "main"))))
      (is (= :erp (:source-name (first @seen))))
      (is (= 404 (:status (post-test {:source "nope"} "main")))))))

(deftest datasource-test-local-sqlite-stays-local
  (with-redefs [ds/read-rows (fn [& _] (throw (ex-info "the agent must not be asked" {})))]
    (is (= {:ok true :via "direct"}
           (:body (post-test {:type "sqlite" :jdbc-url "jdbc:sqlite::memory:"}))))))

(deftest what-a-type-may-do-is-read-from-its-profile
  (testing "local (no egress, no agent): exactly the types the request once named by hand"
    (is (= #{:csv :sqlite}
           (set (filter #(stype/local? (stype/profile %))
                        (keys stype/profiles)))))
    (is (not (stype/local? nil)) "an unknown type is not local"))
  (testing "a file the platform reads itself: :csv alone"
    (is (= #{:csv}
           (set (filter #(stype/csv-route? (stype/profile %))
                        (keys stype/profiles))))))
  (testing "a csv source without :path is refused by its :needs"
    (is (= {:error "csv datasource needs :path"}
           (source/request->source-spec {:name "c" :type :csv})))
    (is (= "/data/x.csv"
           (:path (:ok (source/request->source-spec {:name "c" :type :csv
                                                     :path "/data/x.csv"})))))))

(deftest glpi-and-csv-are-capabilities-of-the-profile
  (testing "a :glpi-api source takes the GLPI resources by its profile's :preset"
    (is (contains? (:resources (hc/declaration {:type :glpi-api :base-url "https://g"
                                                :rest {:read-itemtypes ["Computer"]}}))
                   :Computer))
    (is (empty? (:resources (hc/declaration {:type :rest :base-url "https://g"
                                             :rest {:read-itemtypes ["Computer"]}})))))
  (testing "csv? is the :csv route"
    (is (policy/csv? {:type :csv}))
    (is (not (policy/csv? {:type :sftp})) "rows that are a file fetched by an agent are landed")))

(deftest a-listed-source-shows-that-it-is-sealed-not-its-seal
  (let [schema {:sources {:r1 {:source-name :erp :type :postgres :secret-cipher "ERP-SEALED"
                               :secrets {:token "t"}}}}]
    (is (= {:erp {:source-name :erp :type :postgres :sealed? true}}
           (:datasources (:body (source/list-datasources {:sutra/schema schema})))))))
