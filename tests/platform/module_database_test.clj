(ns ^:integration platform.module-database-test
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [sutra.kit.system :as sys]
            [sutra.llm.budget :as budget]
            [sutra.serve.server :as server]
            [sutra.store.access.scope :as scope]
            [sutra.catalog.grants :as grants]
            [support.pod :as pod]))

(defn- rows [{:keys [url user password]} table]
  (with-open [c (jdbc/get-connection url {:user user :password password})]
    (let [there (jdbc/execute-one! c [(str "SELECT to_regclass('public." table "') AS t")])]
      (when (:t there)
        (:n (jdbc/execute-one! c [(str "SELECT count(*) AS n FROM " table)]))))))

(defn- with-system [config f]
  (let [prev (sys/process)
        base (server/system)
        s    (sys/configure base (merge (:config base) config))]
    (try (sys/install! s) (f s)
         (finally (sys/close! s) (reset! @#'sys/installed prev)))))

(deftest a-module-keeps-its-records-in-its-own-database
  (let [main (pod/start-pg!)
        own  (pod/start-empty-pg!)]
    (try
      (with-system (assoc (pod/pg-config main)
                          "SUTRA_MODULE_DATABASES" (str "llm=" (:url own)))
        (fn [s]
          (let [store (scope/store-of s "main" grants/system-authority)]
            (is (nil? (budget/admit! store "call-1" 0.9 60000 "interactive")))
            (testing "the module reads back what it wrote: the held 0.9 leaves no room"
              (is (some? (budget/admit! store "call-2" 0.2 60000 "interactive"))))
            (testing "the row is in the module's database, none in the main one"
              (is (= 1 (rows own "control_llm_budget")))
              (is (contains? #{nil 0} (rows main "control_llm_budget")))))))
      (finally (pod/stop-pg! own) (pod/stop-pg! main)))))

(deftest a-module-that-cannot-move-is-refused-at-configuration
  (let [main (pod/start-pg!)]
    (try
      (is (thrown-with-msg?
           Exception #"cannot have a database of its own"
           (with-system (assoc (pod/pg-config main)
                               "SUTRA_MODULE_DATABASES" "health=jdbc:postgresql://x/y")
             (fn [s] (scope/db s)))))
      (finally (pod/stop-pg! main)))))
