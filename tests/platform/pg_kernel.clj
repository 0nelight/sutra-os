(ns ^:integration platform.pg-kernel
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.storage.pg :as pg]
            [support.pod :as pod]))

(def ^:private ^:dynamic *pg* nil)

(defn- with-pg
  [f]
  (let [handle (pod/start-pg!)]
    (try
      (binding [*pg* handle] (f))
      (finally (pod/stop-pg! handle)))))

(use-fixtures :once with-pg)

(deftest the-pools-are-a-value-on-their-configuration
  (with-open [pools (pg/pools (pg/settings (pod/pg-config *pg*)))]
    (testing "a lane is opened once and is the same datasource after; nil is :control"
      (is (identical? (pools :control) (pools :control)))
      (is (identical? (pools nil) (pools :control)))
      (is (not (identical? (pools :build) (pools :control)))))
    (testing "…and it is the database the configuration names"
      (jdbc/with-transaction [tx (pools nil)]
        (is (= "PostgreSQL" (.. ^java.sql.Connection tx getMetaData getDatabaseProductName)))
        (is (= (:db *pg*) (:db (jdbc/execute-one! tx ["SELECT current_database() AS db"]
                                                  {:builder-fn rs/as-unqualified-maps}))))))
    (testing "an unknown lane is refused"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unknown control-plane pool lane"
                            (pools :nope))))))

(deftest a-pool-that-cannot-open-is-tried-again
  (with-open [pools (pg/pools (pg/settings {"SUTRA_PG_URL" "jdbc:postgresql://127.0.0.1:1/none"
                                            "SUTRA_PG_CONNECT_TIMEOUT_MS" "1000"}))]
    (is (thrown? Exception (pools :control)))
    (is (thrown? Exception (pools :control)) "the failure is not remembered as a datasource")))
