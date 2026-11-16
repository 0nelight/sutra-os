(ns sutra.store.pg.sql
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [sutra.store.pg.ident :as ident]
            [sutra.kit.sql :as kit-sql]
            [sutra.kit.time :as time]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [next.jdbc.types :as jdbc-types]
            [sutra.types.sql-ident :as sqli])
  (:import (java.sql Connection)
           (java.time Instant LocalDate LocalDateTime LocalTime OffsetDateTime ZoneOffset)))

(set! *warn-on-reflection* true)

(defn dq
  [s]
  (kit-sql/dq (ident/refuse-if-too-long! s "identifier")))

(defn qualified [schema table] (str (dq schema) "." (dq table)))

(defn render
  ^String [^String sql]
  (let [mark (char sqli/mark)
        n    (.length sql)]
    (if (neg? (.indexOf sql (int mark)))
      sql
      (let [out (StringBuilder. n)]
        (loop [i 0]
          (if (>= i n)
            (.toString out)
            (let [c (.charAt sql i)]
              (if-not (= mark c)
                (do (.append out c) (recur (inc i)))
                (let [id  (StringBuilder.)
                      end (loop [j (inc i)]
                            (cond
                              (>= j
                                  n)
                                (throw (ex-info "unterminated quoted identifier" {:sql sql :at i}))
                              (not= mark
                                    (.charAt sql
                                             j)) (do (.append id (.charAt sql j)) (recur (inc j)))
                              (and (< (inc j) n)
                                   (= mark
                                      (.charAt sql (inc j)))) (do (.append id mark) (recur (+ j 2)))
                              :else (inc j)))]
                  (doto out (.append \") (.append (str/replace (.toString id) "\"" "\"\""))
                    (.append \"))
                  (recur (long end)))))))))))

(defn ->sql-param
  [v]
  (condp instance? v
    java.sql.Timestamp (.toLocalDateTime ^java.sql.Timestamp v)
    Instant            (LocalDateTime/ofInstant v ZoneOffset/UTC)
    java.sql.Date      (time/sql-date->local-date v)
    java.sql.Time      v
    java.util.Date     (LocalDateTime/ofInstant (.toInstant ^java.util.Date v) ZoneOffset/UTC)
    (if (or (sequential? v) (map? v)) (json/write-str v) v)))

(defn param
  [v]
  (cond
    (string? v)           (jdbc-types/as-other v)
    (instance? Instant
               v) (java.sql.Timestamp/valueOf (LocalDateTime/ofInstant ^Instant v ZoneOffset/UTC))
    (keyword? v)          (jdbc-types/as-other (subs (str v) 1))
    :else                 v))

(defrecord TextArray [xs])

(defn text-array
  [xs]
  (->TextArray (mapv str xs)))

(defn- bind [^Connection c v]
  (if (instance? TextArray v) (.createArrayOf c "text" (object-array (:xs v))) (param v)))

(defn value
  [v]
  (condp instance? v
    LocalDate
      (java.sql.Date. (.toEpochMilli (.toInstant (.atStartOfDay ^LocalDate v ZoneOffset/UTC))))
    LocalDateTime  (time/stored-timestamp->instant (java.sql.Timestamp/valueOf ^LocalDateTime v))
    LocalTime      (java.sql.Time/valueOf ^LocalTime v)
    OffsetDateTime (time/stored-timestamp->instant
                    (java.sql.Timestamp/valueOf
                     (.toLocalDateTime (.withOffsetSameInstant ^OffsetDateTime v ZoneOffset/UTC))))
    java.sql.Timestamp (time/stored-timestamp->instant v)
    Byte           (int v)
    Short          (int v)
    v))

(defn- running!
  []
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "interrupted — the work stops before its next statement"))))

(defn exec!
  [^Connection c ^String s]
  (running!)
  (with-open [stmt (.createStatement c)] (.execute stmt s)))

(defn update!
  [^Connection c s params]
  (running!)
  (or (:next.jdbc/update-count (jdbc/execute-one! c
                                                  (into [(render s)] (map (partial bind c))
                                                        params))) 0))

(defn rows
  [^Connection c s params]
  (running!)
  (jdbc/execute! c (into [(render s)] (map (partial bind c)) params)
                 {:builder-fn rs/as-unqualified-maps}))

(defn one [^Connection c s params] (first (rows c s params)))

(defn placeholders [n] (str/join ", " (repeat n "?")))

(defn tuples
  [n arity]
  (str/join ", " (repeat n (str "(" (placeholders arity) ")"))))

(defn frag
  ([s] (frag s nil))
  ([s params] {:sql s :params (vec params)}))

(defn frag-cat
  ([frags] (frag-cat "" frags))
  ([sep frags]
   (let [fs (remove nil? frags)]
     {:sql (str/join sep (map :sql fs)) :params (into [] (mapcat :params) fs)})))

(defn frag-wrap [before f after] (assoc f :sql (str before (:sql f) after)))
