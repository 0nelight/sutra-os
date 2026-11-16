(ns sutra.http.responses
  (:require [sutra.foundation.profile :as prof]
            [sutra.kit.anomaly :as anomaly]
            [sutra.kit.redact :as redact]
            [sutra.kit.time :as time]
            [clojure.data.json :as json]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [cognitect.transit :as transit])
  (:import (java.math BigDecimal BigInteger)
           (java.time Instant LocalDate LocalDateTime LocalTime OffsetDateTime ZonedDateTime)))

(set! *warn-on-reflection* true)

(defn ok
  ([body] {:status 200 :body body})
  ([]     {:status 200 :body {:ok true}}))

(defn too-many
  [msg retry-ms]
  (let [s (max 1 (long (Math/ceil (/ (double retry-ms) 1000))))]
    {:status 429 :headers {"Retry-After" (str s)} :body {:error msg :retry-after-s s}}))

(defn fault
  ([e] (fault e {}))
  ([e ctx]
   (if-let [r (anomaly/of e)]
     {:status (anomaly/status r)
      :body   (merge ctx
                     (redact/redact (anomaly/data r))
                     {:error (redact/scrub (anomaly/message r))
                      :category (anomaly/category r)})}
     (throw e))))

(defn temporal->wire
  [v]
  (condp instance? v
    java.sql.Date  (str (time/sql-date->local-date v))
    java.sql.Time  (str (.toLocalTime ^java.sql.Time v))
    java.util.Date (str (.toInstant ^java.util.Date v))
    LocalDate      (str v)
    LocalDateTime  (str v)
    LocalTime      (str v)
    Instant        (str v)
    OffsetDateTime (str v)
    ZonedDateTime  (str (.toOffsetDateTime ^ZonedDateTime v))
    nil))

(def transit-write-handlers
  (let [h (transit/write-handler (constantly "s") temporal->wire)]
    (zipmap [java.sql.Date java.sql.Time java.sql.Timestamp java.util.Date
             LocalDate LocalDateTime LocalTime Instant OffsetDateTime ZonedDateTime]
            (repeat h))))

(defn- json-by-type
  [v]
  (cond
    (temporal->wire v) (temporal->wire v)

    (or (instance? Long v) (instance? BigInteger v) (instance? clojure.lang.BigInt v))
    (str v)

    (instance? BigDecimal v)
    (.toPlainString ^BigDecimal v)

    :else v))

(defn json-response
  [status body]
  {:status  status
   :headers {"Content-Type" "application/json"}
   :body    (prof/with-mark-result :Encode
              (fn [^String s] {:format "json" :bytes (count s)})
              (json/write-str (walk/postwalk json-by-type body)))})

(defn- wants-json?
  [req]
  (boolean (some-> (get-in req [:headers "accept"]) (str/includes? "application/json"))))

(defn negotiated
  ([req body] (negotiated req 200 body))
  ([req status body]
   (if (wants-json? req)
     (json-response status body)
     {:status status :body body})))

(defn object-set-rows
  [rows]
  (mapv (fn [r] {:id (:id r) :values (dissoc r :id)}) rows))
