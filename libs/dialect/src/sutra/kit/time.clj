(ns sutra.kit.time
  (:require [clojure.string :as str])
  (:import (java.time DateTimeException Instant LocalDate LocalDateTime OffsetDateTime ZoneOffset)))

(set! *warn-on-reflection* true)

(defn sql-date->local-date
  ^LocalDate [^java.sql.Date d]
  (let [ms (.getTime d)]
    (if (zero? (mod ms 86400000))
      (.toLocalDate (.atOffset (Instant/ofEpochMilli ms) ZoneOffset/UTC))
      (.toLocalDate d))))

(defn stored-timestamp->instant
  ^Instant [^java.sql.Timestamp ts]
  (.toInstant (.atOffset (.toLocalDateTime ts) ZoneOffset/UTC)))

(defn parse
  [^String s]
  (let [t (str/trim s)]
    (or (try (OffsetDateTime/parse t) (catch DateTimeException _ nil))
        (try (LocalDateTime/parse t) (catch DateTimeException _ nil))
        (try (LocalDateTime/parse (str/replace-first t " " "T")) (catch DateTimeException _ nil))
        (try (LocalDate/parse t) (catch DateTimeException _ nil)))))

(defn- sql-time?
  [v]
  (= "java.sql.Time" (.getName (class v))))

(defn ->utc-local
  ^LocalDateTime [v]
  (condp instance? v
    String             (some-> (parse v) ->utc-local)
    LocalDateTime      v
    LocalDate          (.atStartOfDay ^LocalDate v)
    OffsetDateTime     (.toLocalDateTime (.withOffsetSameInstant ^OffsetDateTime v ZoneOffset/UTC))
    Instant            (LocalDateTime/ofInstant ^Instant v ZoneOffset/UTC)
    java.sql.Timestamp (.toLocalDateTime ^java.sql.Timestamp v)
    java.sql.Date      (.atStartOfDay (sql-date->local-date v))
    java.util.Date     (when-not (sql-time? v)
                         (LocalDateTime/ofInstant (.toInstant ^java.util.Date v) ZoneOffset/UTC))
    nil))

(defn ->instant
  ^Instant [v]
  (if (instance? Instant v)
    v
    (some-> (->utc-local v) (.toInstant ZoneOffset/UTC))))

(defn ->epoch-ms
  [v]
  (some-> (->instant v) .toEpochMilli))
