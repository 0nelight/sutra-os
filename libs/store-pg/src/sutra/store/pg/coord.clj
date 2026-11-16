(ns sutra.store.pg.coord
  (:require [sutra.coord.api :as coord]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(set! *warn-on-reflection* true)

(def ^:private table "control_coord_leases")

(defn- ds
  [{:keys [ds-fn installed]}]
  (let [d (ds-fn)]
    (when-not (identical? d @installed)
      (jdbc/execute! d [(str "CREATE TABLE IF NOT EXISTS " table
                             " (name VARCHAR(255) PRIMARY KEY, owner VARCHAR(255),"
                             "  epoch BIGINT NOT NULL, until TIMESTAMPTZ)")])
      (reset! installed d))
    d))

(defn- one [c sql params]
  (jdbc/execute-one! (ds c) (into [sql] params) {:builder-fn rs/as-unqualified-lower-maps}))

(defn- ->lease [{:keys [name owner epoch until]}]
  (when (and owner until)
    {:name name :owner owner :epoch (long epoch)
     :until (java.util.Date. (.getTime ^java.util.Date until))}))

(def ^:private ttl "now() + (? * interval '1 millisecond')")

(defrecord PgCoordinator [ds-fn installed]
  coord/Coordinator
  (-acquire! [c {:keys [name owner ttl-ms wait-ms]}]
    (let [deadline (+ (System/currentTimeMillis) (or wait-ms 0))]
      (loop []
        (let [r (one c
                     (str "INSERT INTO " table " AS t (name, owner, epoch, until) VALUES (?, ?, 1, "
                          ttl ")"
                        " ON CONFLICT (name) DO UPDATE SET"
                        "  epoch = CASE WHEN t.owner = EXCLUDED.owner AND t.until > now() THEN "
                          "t.epoch ELSE t.epoch + 1 END,"
                        "  owner = EXCLUDED.owner, until = EXCLUDED.until"
                        " WHERE t.until IS NULL OR t.until <= now() OR t.owner = EXCLUDED.owner"
                        " RETURNING name, owner, epoch, until")
                     [name owner (long ttl-ms)])]
          (cond
            r                                           (->lease r)
            (< (System/currentTimeMillis) deadline)     (do (Thread/sleep 100) (recur))
            :else                                       nil)))))
  (-renew! [c {:keys [name owner epoch]} ttl-ms]
    (some-> (one c (str "UPDATE " table " SET until = " ttl
                      " WHERE name = ? AND owner = ? AND epoch = ? AND until > now()"
                      " RETURNING name, owner, epoch, until")
                 [(long ttl-ms) name owner (long epoch)])
            ->lease))
  (-release! [c {:keys [name owner epoch]}]
    (pos? (long (:next.jdbc/update-count
                 (jdbc/execute-one! (ds c) [(str "UPDATE " table " SET owner = NULL, until = NULL"
                                               " WHERE name = ? AND owner = ? AND epoch = ?")
                                          name owner (long epoch)])
                 0))))
  (-holder [c name]
    (some-> (one c
                 (str "SELECT name, owner, epoch, until FROM " table
                      " WHERE name = ? AND until > now()") [name])
            ->lease))
  (-revoke! [c name]
    (long
     (:epoch
      (one c
           (str "INSERT INTO " table
                " AS t (name, owner, epoch, until) VALUES (?, NULL, 1, NULL)"
              " ON CONFLICT (name) DO UPDATE SET owner = NULL, until = NULL, epoch = t.epoch + 1"
              " RETURNING epoch")
           [name])))))

(defn coordinator
  [ds-fn]
  (->PgCoordinator ds-fn (atom nil)))

(def module
  {:name :coord-pg :requires [:pg/ds]
   :opens {:coord/db (fn [_ {pools :pg/ds}] (constantly (coordinator #(pools nil))))}})
