(ns sutra.store.pg.streams
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [sutra.store.pg.ns :as pgns]
            [sutra.store.pg.sql :as sql]
            [sutra.streams.api :as s])
  (:import (java.sql Connection Timestamp)))

(set! *warn-on-reflection* true)

(def ^:private heads "h__offsets")

(def ^:private unq {:builder-fn rs/as-unqualified-lower-maps})

(defn- schema-of [stream-ref] (pgns/own-schema (:branch stream-ref)))

(defn- table [stream-ref] (sql/qualified (schema-of stream-ref) (:stream stream-ref)))

(defn- heads-table [stream-ref] (sql/qualified (schema-of stream-ref) heads))

(defn- exists? [^Connection c sc t]
  (some?
   (jdbc/execute-one! c
                      ["SELECT 1 FROM information_schema.tables WHERE table_schema = ? AND table_name = ?" sc t])))

(defn- ensure! [^Connection c stream-ref]
  (sql/exec! c (str "CREATE SCHEMA IF NOT EXISTS " (sql/dq (schema-of stream-ref))))
  (sql/exec! c (str "CREATE TABLE IF NOT EXISTS " (heads-table stream-ref)
                    " (stream TEXT NOT NULL, part INTEGER NOT NULL, next_offset BIGINT NOT NULL,"
                    " PRIMARY KEY (stream, part))"))
  (sql/exec! c
             (str "CREATE TABLE IF NOT EXISTS " (table stream-ref)
                  " (part INTEGER NOT NULL, offs BIGINT NOT NULL, rec_key TEXT, rec TEXT NOT NULL,"
                  " rec_at TIMESTAMP(6) NOT NULL, PRIMARY KEY (part, offs))")))

(defn- ends-in [^Connection c stream-ref]
  (let [sc (schema-of stream-ref) t (:stream stream-ref)
        pairs (fn [q params] (into {} (map (juxt (comp long :part) (comp long :n)))
                                   (jdbc/execute! c (into [q] params) unq)))]
    (merge-with max
                (if (exists? c sc t)
                  (pairs (str "SELECT part, max(offs) + 1 AS n FROM " (table stream-ref)
                              " GROUP BY part")
                         [])
                  {})
                (if (exists? c sc heads)
                  (pairs (str "SELECT part, next_offset AS n FROM " (heads-table stream-ref)
                              " WHERE stream = ?") [t])
                  {}))))

(defn- place
  [next entries]
  (second (reduce (fn [[nx out] {:keys [partition] :as e}]
                    (let [o (get nx partition 0)]
                      [(assoc nx partition (inc o)) (conj out (assoc e :offset o))]))
                  [next []] entries)))

(defn- ->ts [d] (Timestamp. (.getTime ^java.util.Date d)))

(defn- ->record [{:keys [part offs rec_key rec rec_at]}]
  {:partition (long part) :offset (long offs) :key rec_key
   :at (java.util.Date. (.getTime ^Timestamp rec_at))
   :record (json/read-str (str rec) :key-fn keyword)})

(defn- with-conn [ds-fn f]
  (with-open [c (jdbc/get-connection (ds-fn))] (f c)))

(defrecord PgStreams [ds-fn]
  s/Streams
  (-append! [_ stream-ref entries]
    (if (empty? entries)
      []
      (jdbc/with-transaction [c (ds-fn)]
        (jdbc/execute-one! c
                           ["SELECT pg_advisory_xact_lock(?)"
                            (long (hash [(schema-of stream-ref) (:stream stream-ref)]))])
        (ensure! c stream-ref)
        (let [placed (place (ends-in c stream-ref) entries)]
          (jdbc/execute-batch! c
                               (str "INSERT INTO " (table stream-ref)
                                    " (part, offs, rec_key, rec, rec_at) VALUES (?,?,?,?,?)")
                               (mapv (fn [{:keys [partition offset key at record]}]
                                       [(int partition) (long offset) key (json/write-str record)
                                        (->ts at)])
                                     placed)
                               {})
          (doseq [[p
                   nx] (reduce (fn [m {:keys [partition offset]}] (assoc m partition (inc offset)))
                               {} placed)]
            (jdbc/execute-one! c
                               [(str "INSERT INTO " (heads-table stream-ref)
                                     " (stream, part, next_offset) VALUES (?,?,?)"
                                     " ON CONFLICT (stream, part) DO UPDATE SET next_offset ="
                                     " GREATEST(" (heads-table stream-ref)
                                     ".next_offset, EXCLUDED.next_offset)")
                                (:stream stream-ref) (int p) (long nx)]))
          (mapv #(select-keys % [:partition :offset]) placed)))))
  (-read [_ stream-ref {:keys [from partition limit] :or {limit 1000}}]
    (with-conn ds-fn
      (fn [c]
        (if-not (exists? c (schema-of stream-ref) (:stream stream-ref))
          []
          (let [cs     (seq from)
                where  (cond-> (vec (for [_ cs] "NOT (part = ? AND offs < ?)"))
                         partition (conj "part = ?"))
                params (cond-> (vec (mapcat (fn [[p o]] [(int p) (long o)]) cs))
                         partition (conj (int partition)))]
            (mapv ->record
                  (jdbc/execute! c
                                 (-> [(str "SELECT part, offs, rec_key, rec, rec_at FROM "
                                           (table stream-ref)
                                           (when (seq where)
                                             (str " WHERE " (str/join " AND " where)))
                                           " ORDER BY part, offs LIMIT ?")]
                                     (into params) (conj (long limit)))
                                 unq)))))))
  (-ends [_ stream-ref] (with-conn ds-fn #(ends-in % stream-ref)))
  (-stats [_ stream-ref]
    (with-conn ds-fn
      (fn [c]
        (if-not (exists? c (schema-of stream-ref) (:stream stream-ref))
          {:depth {} :last-at nil}
          (let [rows (jdbc/execute! c
                                    [(str "SELECT part, count(*) AS n, max(rec_at) AS at FROM "
                                          (table stream-ref) " GROUP BY part")] unq)]
            {:depth   (into (sorted-map) (map (juxt (comp long :part) (comp long :n))) rows)
             :last-at (some->> (seq (keep :at rows))
                               (reduce #(if (.after ^Timestamp %1 ^Timestamp %2) %1 %2))
                               (#(java.util.Date. (.getTime ^Timestamp %))))})))))
  (-trim! [_ stream-ref {:keys [below before]}]
    (with-conn ds-fn
      (fn [c]
        (if-not (and (seq below) (exists? c (schema-of stream-ref) (:stream stream-ref)))
          0
          (reduce +
                  (for [[p o] below]
                    (or (:next.jdbc/update-count
                         (jdbc/execute-one! c
                                            (cond-> [(str "DELETE FROM " (table stream-ref)
                                                          " WHERE part = ? AND offs < ?"
                                                          (when before " AND rec_at < ?"))
                                                     (int p) (long o)]
                                              before (conj (->ts before)))))
                        0)))))))
  (-reset! [_ stream-ref]
    (with-conn ds-fn
      (fn [c]
        (when (exists? c (schema-of stream-ref) (:stream stream-ref))
          (sql/exec! c (str "DELETE FROM " (table stream-ref)))))) nil)
  (-drop! [_ stream-ref]
    (with-conn ds-fn
      (fn [c]
        (sql/exec! c (str "DROP TABLE IF EXISTS " (table stream-ref)))
        (when (exists? c (schema-of stream-ref) heads)
          (jdbc/execute-one! c
                             [(str "DELETE FROM " (heads-table stream-ref) " WHERE stream = ?")
                              (:stream stream-ref)]))))
    nil))

(def module
  {:name :streams-pg :requires [:pg/ds]
   :opens {:streams/db (fn [_ {pools :pg/ds}] (fn [lane] (->PgStreams #(pools lane))))}})
