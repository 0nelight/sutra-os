(ns sutra.lib.test-store-pg
  (:require [next.jdbc :as jdbc]
            [sutra.store.core :as core]
            [sutra.store.core.engine]
            [sutra.store.pg :as spg])
  (:import (io.zonky.test.db.postgres.embedded EmbeddedPostgres)))

(defn start! []
  (alter-var-root #'sutra.store.core.engine/*verify* (constantly true))
  (let [pg (-> (EmbeddedPostgres/builder) (.setServerConfig "listen_addresses" "127.0.0.1")
               (.start))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. ^Runnable #(.close pg)))
    {:pg pg :n (atom 0)}))

(defn fresh [{:keys [^EmbeddedPostgres pg n]} store-opts]
  (let [db-name (str "lib_" (swap! n inc))]
    (with-open [c (.getConnection (.getPostgresDatabase pg))]
      (jdbc/execute! c [(str "CREATE DATABASE " db-name)]))
    (doto (spg/store {:datasource (.getDatabase pg "postgres" db-name)} store-opts)
      core/adopt!)))
