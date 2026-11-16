(ns sutra.store.storage.pg
  (:require [sutra.kit.anomaly :as anomaly]
            [clojure.string :as str]
            [sutra.foundation.sql-trace :as sqltrace])
  (:import (com.zaxxer.hikari HikariConfig HikariDataSource)
           (javax.sql DataSource)))

(set! *warn-on-reflection* true)

(defn- parse-pool-size
  [s default]
  (let [v (some-> s str/trim parse-long)]
    (if (and v (pos? v)) v default)))

(defn- password
  [config]
  (or (get config "SUTRA_PG_PASSWORD")
      (some-> (get config "SUTRA_PG_PASSWORD_FILE") str/trim not-empty slurp str/trim)
      "sutra"))

(defn settings
  [config]
  (let [n       (fn [k default] (parse-pool-size (get config k) default))
        timeout (some-> (get config "SUTRA_PG_CONNECT_TIMEOUT_MS") str/trim not-empty
                        parse-long)]
    {:url      (or (get config "SUTRA_PG_URL") "jdbc:postgresql://localhost:5432/sutra")
     :user     (or (get config "SUTRA_PG_USER") "sutra")
     :password (password config)
     :min-idle (n "SUTRA_PG_POOL_MIN_IDLE" 2)
     :lanes
     {:control {:pool-name "sutra:pg-control"
                :max-pool-size (n "SUTRA_PG_POOL_CONTROL" 24)
                :idle-tx-s (n "SUTRA_PG_IDLE_TX_CONTROL_S" 120)
                :jit? false
                :connect-timeout-ms timeout}
      :build   {:pool-name "sutra:pg-build"
                :max-pool-size (n "SUTRA_PG_POOL_BUILD" 8)
                :idle-tx-s (n "SUTRA_PG_IDLE_TX_BUILD_S" 900)
                :connect-timeout-ms timeout}
      :auth    {:pool-name "sutra:pg-auth"
                :max-pool-size (n "SUTRA_PG_POOL_AUTH" 8)
                :idle-tx-s (n "SUTRA_PG_IDLE_TX_AUTH_S" 120)
                :jit? false
                :connect-timeout-ms (min 5000 (or timeout 5000))}}}))

(defn- open-pool
  ^DataSource [{:keys [url user password min-idle]}
               {:keys [pool-name max-pool-size idle-tx-s connect-timeout-ms jit?]}]
  (-> (HikariDataSource.
       (cond-> (doto (HikariConfig.)
                 (.setJdbcUrl url)
                 (.setUsername user)
                 (.setPassword password)
                 (.setMaximumPoolSize (int max-pool-size))
                 (.setMinimumIdle (int (min (long max-pool-size) (long min-idle))))
                 (.setIdleTimeout 60000)
                 (.setPoolName pool-name))
         idle-tx-s
         (doto (.setConnectionInitSql (str "SET idle_in_transaction_session_timeout = '"
                                           (long idle-tx-s) "s'")))
         connect-timeout-ms (doto (.setConnectionTimeout (long connect-timeout-ms)))
         (false? jit?) (doto (.addDataSourceProperty "options" "-c jit=off"))))
      (sqltrace/wrap-datasource pool-name false)))

(defn- lane-of
  ^DataSource [settings opened lane]
  (let [lane (or lane :control)]
    (or (get @opened lane)
        (locking opened
          (or (get @opened lane)
              (let [spec (or (get-in settings [:lanes lane])
                             (anomaly/raise! :fault "unknown control-plane pool lane"
                                                    {:lane lane :known (keys (:lanes settings))}))]
                (get (swap! opened assoc lane (open-pool settings spec)) lane)))))))

(deftype Pools [settings opened]
  clojure.lang.IFn
  (invoke [_ lane] (lane-of settings opened lane))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))
  java.lang.AutoCloseable
  (close [_]
    (locking opened
      (doseq [^DataSource ds (vals @opened)]
        (.close ^HikariDataSource (.unwrap ds HikariDataSource)))
      (reset! opened {}))))

(defn pools
  ^Pools [settings]
  (->Pools settings (atom {})))

(defn lane-stats
  [^Pools pools]
  (into (sorted-map)
        (for [[lane ^DataSource ds] @(.-opened pools)
              :let [h  (.unwrap ds HikariDataSource)
                    mx (.getHikariPoolMXBean ^HikariDataSource h)]
              :when mx]
          [lane {:active  (.getActiveConnections mx)
                 :idle    (.getIdleConnections mx)
                 :waiting (.getThreadsAwaitingConnection mx)
                 :max     (.getMaximumPoolSize ^HikariDataSource h)}])))
