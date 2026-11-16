(ns ^:unit ontology.importer-temporal-test
  (:require [sutra.builds.import.coerce :as coerce]
            [sutra.kit.time :as time]
            [clojure.test :refer [deftest testing is]]))

(def ^:private ->utc-local time/->utc-local)
(def ^:private coerce-temporal #'coerce/coerce-temporal)

(deftest sql-date-does-not-throw
  (testing "java.sql.Date.toInstant throws by contract — the DATE arm must come
            before java.util.Date or the build dies at run level"
    (let [d (java.sql.Date/valueOf "2026-08-05")]
      (is (= (java.time.LocalDateTime/parse "2026-08-05T00:00:00") (->utc-local d)))
      (is (= "2026-08-05" ((coerce-temporal true) d))
          "a DATE must keep its calendar day — this is the value that went
           missing when the arm order was wrong"))))

(deftest sql-time-yields-nil-not-an-exception
  (testing "a TIME carries no date; nil lets coerce-temporal pass the value
            through untouched instead of inventing an epoch day for it"
    (let [t (java.sql.Time/valueOf "10:30:00")]
      (is (nil? (->utc-local t)))
      (is (= t ((coerce-temporal false) t))))))

(deftest sql-timestamp-keeps-the-drivers-wall-clock
  (testing "read naively, so the host's zone cannot shift a value the connector
            already resolved against UTC"
    (let [ts (java.sql.Timestamp/valueOf "2026-08-05 10:00:00")]
      (is (= (java.time.LocalDateTime/parse "2026-08-05T10:00:00") (->utc-local ts)))
      (is (= "2026-08-05 10:00:00" ((coerce-temporal false) ts))
          "must be the same digits the driver produced, on ANY host zone")
      (is (= "2026-08-05" ((coerce-temporal true) ts))))))

(deftest a-real-instant-is-still-converted-to-utc
  (testing "the inst arm must keep working — only its SUBCLASSES were misrouted"
    (let [i (java.time.Instant/parse "2026-08-05T22:30:00Z")
          d (java.util.Date/from i)]
      (is (= (java.time.LocalDateTime/parse "2026-08-05T22:30:00") (->utc-local i)))
      (is (= (java.time.LocalDateTime/parse "2026-08-05T22:30:00") (->utc-local d))))))

(deftest the-java-time-arms-are-untouched
  (is (= (java.time.LocalDateTime/parse "2026-08-05T10:00:00")
         (->utc-local (java.time.LocalDateTime/parse "2026-08-05T10:00:00"))))
  (is (= (java.time.LocalDateTime/parse "2026-08-05T00:00:00")
         (->utc-local (java.time.LocalDate/parse "2026-08-05"))))
  (testing "an offset-bearing value is normalized TO UTC, not stripped"
    (is (= (java.time.LocalDateTime/parse "2026-08-05T08:00:00")
           (->utc-local (java.time.OffsetDateTime/parse "2026-08-05T10:00:00+02:00"))))))

(deftest non-temporals-pass-through
  (testing "coerce-temporal only ever replaces a value the store would guess at"
    (is (nil? ((coerce-temporal false) nil)))
    (is (= 42 ((coerce-temporal false) 42)))
    (is (= "not a date" ((coerce-temporal false) "not a date")))))
