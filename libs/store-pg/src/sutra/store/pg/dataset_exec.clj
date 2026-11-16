(ns sutra.store.pg.dataset-exec
  (:require [next.jdbc.types :as jdbc-types]
            [sutra.store.pg.sql :as sql])
  (:import [java.time Instant LocalDate LocalDateTime LocalTime OffsetDateTime ZoneOffset]))

(set! *warn-on-reflection* true)

(defn render-sql
  [s]
  (sql/render s))

(defn- ->jdbc-param
  [v]
  (cond
    (string? v)            (jdbc-types/as-other v)
    (instance? Instant
               v)  (java.sql.Timestamp/valueOf (LocalDateTime/ofInstant ^Instant v ZoneOffset/UTC))
    :else                  v))

(defn ->jdbc-params
  [params]
  (mapv ->jdbc-param params))

(defn ->jdbc-value
  [v]
  (condp instance? v
    LocalDate
      (java.sql.Date. (.toEpochMilli (.toInstant (.atStartOfDay ^LocalDate v ZoneOffset/UTC))))
    LocalDateTime  (java.sql.Timestamp/valueOf ^LocalDateTime v)
    LocalTime      (java.sql.Time/valueOf ^LocalTime v)
    OffsetDateTime
      (java.sql.Timestamp/valueOf
       (.toLocalDateTime (.withOffsetSameInstant ^OffsetDateTime v ZoneOffset/UTC)))
    Byte           (int v)
    Short          (int v)
    java.sql.Array (mapv ->jdbc-value (.getArray ^java.sql.Array v))
    v))
