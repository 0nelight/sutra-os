(ns ^:integration platform.control-tables-lock-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.control.tables :as cc]
            [sutra.store.pg.control-tables :as ct]
            [sutra.store.storage.pg :as pg]
            [support.pod :as pod]))

(def ^:private ^:dynamic *ds* nil)
(def ^:private ^:dynamic *pg* nil)

(use-fixtures :once
  (fn [t]
    (let [h (pod/start-pg!)]
      (try
        (binding [*pg* h
                  *ds* (jdbc/get-datasource {:jdbcUrl (:url h) :user (:user h)
                                             :password (:password h)})]
          (ct/install! *ds* cc/tables cc/renamed-tables)
          (t))
        (finally (pod/stop-pg! h))))))

(deftest a-start-beside-an-open-read-does-not-wait
  (with-open [reader (jdbc/get-connection *ds*)]
    (.setAutoCommit reader false)
    (jdbc/execute! reader ["SELECT * FROM control_runs"])
    (let [t0 (System/nanoTime)]
      (ct/install! *ds* cc/tables cc/renamed-tables)
      (is (< (/ (- (System/nanoTime) t0) 1e6) 4000.0)
          "no ALTER TABLE waits for the reader's lock"))
    (with-open [c (jdbc/get-connection *ds*)]
      (jdbc/execute-one! c ["SET lock_timeout = '500ms'"])
      (is (thrown-with-msg? java.sql.SQLException #"lock timeout"
                            (doseq [stmt (ct/ddl cc/tables cc/renamed-tables)]
                              (jdbc/execute-one! c [stmt])))
          "the unconditional DDL a start ran before does wait for it"))
    (.rollback reader)))

(defn- one [sql] (jdbc/execute-one! *ds* [sql] {:builder-fn rs/as-unqualified-maps}))

(deftest a-retired-outbox-under-the-notifications-name-moves-aside
  (one "DROP TABLE IF EXISTS control_notifications, control_notifications_outbox_retired")
  (one "CREATE TABLE control_notifications (id VARCHAR(36) NOT NULL, branch VARCHAR(255) NOT NULL,
          channel VARCHAR(255) NOT NULL, payload TEXT, actor VARCHAR(255),
          created_at TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP, dispatched_at TIMESTAMP(6),
          PRIMARY KEY (id))")
  (one "CREATE INDEX control_notifications_pending_idx ON control_notifications (branch, created_at) WHERE dispatched_at IS NULL")
  (one "INSERT INTO control_notifications (id, branch, channel) VALUES ('n1', 'main', 'c')")
  (ct/install! *ds* cc/tables cc/renamed-tables)
  (is (= "n1" (:id (one "SELECT id FROM control_notifications_outbox_retired")))
      "the old rows are kept aside")
  (one "INSERT INTO control_notifications (id, recipient, heading) VALUES ('n2', 'u', 'h')")
  (ct/install! *ds* cc/tables cc/renamed-tables)
  (is (= 1 (:n (one "SELECT count(*)::int AS n FROM control_notifications")))
      "a table of the new shape stays where it is"))

(deftest a-pooled-connection-ends-an-idle-transaction
  (with-open [pools (pg/pools (pg/settings (pod/pg-config *pg*)))
              c     (jdbc/get-connection (pools :control))]
    (is (= "2min" (:idle_in_transaction_session_timeout
                   (jdbc/execute-one! c ["SHOW idle_in_transaction_session_timeout"]))))))
