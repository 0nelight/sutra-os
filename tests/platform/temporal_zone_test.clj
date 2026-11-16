(ns ^:unit platform.temporal-zone-test
  (:require [sutra.builds.import.coerce :as coerce]
            [sutra.kit.time :as time]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [sutra.store.pg.sql :as sql]
            [sutra.catalog.values :as values]
            [sutra.store.pg.dataset-exec :as pgexec])
  (:import [java.time LocalDate LocalDateTime ZoneOffset]
           [java.util TimeZone]))

(def ^:private vienna (TimeZone/getTimeZone "Europe/Vienna"))

(defn- in-zone
  [^TimeZone tz f]
  (let [prev (TimeZone/getDefault)]
    (try (TimeZone/setDefault tz) (f)
         (finally (TimeZone/setDefault prev)))))

(use-fixtures :each (fn [t] (in-zone vienna t)))

(deftest date-reads-back-as-utc-midnight
  (let [d (pgexec/->jdbc-value (LocalDate/parse "2026-07-13"))]
    (is (instance? java.sql.Date d))
    (is (= (.toEpochMilli (.toInstant (.atStartOfDay (LocalDate/parse "2026-07-13")
                                                     ZoneOffset/UTC)))
           (.getTime d))
        "a DATE must not move by the host's offset — it has no time to move")))

(deftest naive-timestamp-reads-back-as-utc
  (let [ts (pgexec/->jdbc-value (LocalDateTime/parse "2026-07-08T08:24:17"))]
    (is (instance? java.sql.Timestamp ts))
    (is (= "2026-07-08T08:24:17Z" (str (time/stored-timestamp->instant ts)))
        "the digits in the store are UTC; reading them as local invents an offset")))

(deftest an-offset-bearing-value-is-still-converted-by-its-own-offset
  (let [ts (pgexec/->jdbc-value (java.time.OffsetDateTime/parse "2026-07-08T10:24:17+02:00"))]
    (is (= "2026-07-08T08:24:17Z" (str (time/stored-timestamp->instant ts))))))

(deftest binding-a-temporal-names-its-zone
  (testing "a stored java.sql.Timestamp binds back as its UTC digits, whatever the host zone"
    (is (= (LocalDateTime/parse "2026-07-08T08:24:17")
           (sql/->sql-param (pgexec/->jdbc-value (LocalDateTime/parse "2026-07-08T08:24:17"))))))
  (testing "a java.sql.Date binds as its UTC calendar day"
    (is (= (LocalDate/parse "2026-07-13")
           (sql/->sql-param
            (java.sql.Date. (.toEpochMilli (.toInstant (.atStartOfDay
                                                        (LocalDate/parse "2026-07-13")
                                                        ZoneOffset/UTC)))))))))

(deftest the-write-read-round-trip-is-an-identity
  (let [ts (pgexec/->jdbc-value (LocalDateTime/parse "2026-07-08T08:24:17"))]
    (is (= ts (pgexec/->jdbc-value (sql/->sql-param ts)))))
  (let [d (java.sql.Date. (.toEpochMilli (.toInstant (.atStartOfDay
                                                      (LocalDate/parse "2026-07-13")
                                                      ZoneOffset/UTC))))]
    (is (= (.getTime d) (.getTime ^java.sql.Date (pgexec/->jdbc-value (sql/->sql-param d)))))))

(deftest a-boolean-reads-back-as-a-boolean
  (testing "a 0/1 is
            invisible to a SQL `=` filter, which casts, and fatal above the
            substrate, where `views/coerce-where-val` makes the wanted side
            `true`."
    (is (true? (pgexec/->jdbc-value true)))
    (is (false? (pgexec/->jdbc-value false)))))

(deftest non-temporal-values-are-untouched
  (is (= "[1,2]" (sql/->sql-param [1 2])))
  (is (= "{\"k\":1}" (sql/->sql-param {:k 1})))
  (is (= "plain" (sql/->sql-param "plain")))
  (is (nil? (sql/->sql-param nil))))

(def ^:private coercion-for #'coerce/coercion-for)

(defn- cast-as [t v] ((coercion-for t) v))

(deftest a-declared-temporal-column-is-cast-like-every-other-type
  (testing "an OFFSET-bearing value is normalized to UTC — the case that was wrong"
    (is (= "2026-07-08 08:24:17" (cast-as :timestamp "2026-07-08T10:24:17+02:00")))
    (is (= "2026-07-08 08:24:17" (cast-as :instant "2026-07-08T08:24:17Z")))
    (is (= "2026-07-08" (cast-as :date "2026-07-08T23:30:00+02:00"))
        "converting first is what keeps a late-evening CEST value on its UTC day"))
  (testing "both ISO spellings of a naive value land identically"
    (is (= "2026-07-08 08:24:17" (cast-as :timestamp "2026-07-08T08:24:17")))
    (is (= "2026-07-08 08:24:17" (cast-as :timestamp "2026-07-08 08:24:17"))))
  (testing "a naive value is NOT moved — nobody said what zone it was in"
    (is (= "2026-07-08 08:24:17" (cast-as :timestamp "2026-07-08 08:24:17"))))
  (testing "a bare date fills in no time of day"
    (is (= "2026-07-13" (cast-as :date "2026-07-13")))
    (is (= "2026-07-13 00:00:00" (cast-as :timestamp "2026-07-13"))))
  (testing "an inst / java.time value casts like its string spelling"
    (is (= "2026-07-08 08:24:17"
           (cast-as :timestamp (java.util.Date/from
                                (java.time.Instant/parse "2026-07-08T08:24:17Z")))))
    (is (= "2026-07-13" (cast-as :date (LocalDate/parse "2026-07-13")))))
  (testing "what we cannot read passes through UNCHANGED, never guessed at"
    (is (= "05.08.2026" (cast-as :date "05.08.2026")))
    (is (= "" (cast-as :timestamp "")))
    (is (nil? (cast-as :timestamp nil))))
  (testing "the non-temporal arms are unchanged"
    (is (= 42 (cast-as :integer "42")))
    (is (= true (cast-as :boolean "true")))
    (is (= "7" (cast-as :string 7)))
    (is (= {:a 1} (cast-as :uuid {:a 1})) "an unhandled type still binds as-is")))

(deftest every-temporal-alias-guards-identically
  (doseq [t [:instant :timestamp :date]]
    (testing (str t)
      (is (values/value-ok? t "2026-07-08T08:24:17Z"))
      (is (values/value-ok? t (java.util.Date.)))
      (is (values/value-ok? t (LocalDate/parse "2026-07-13")))
      (is (not (values/value-ok? t {:a 1}))
          "a map in a temporal column is the shape this guard exists to refuse")
      (is (not (values/value-ok? t [1 2])))))
  (testing ":text is guarded like its :string twin"
    (is (values/value-ok? :text "x"))
    (is (not (values/value-ok? :text 42)))))

(deftest a-stored-date-casts-to-its-own-day-west-of-utc
  (in-zone (TimeZone/getTimeZone "America/New_York")
           #(let [d (java.sql.Date. (.toEpochMilli (.toInstant (.atStartOfDay
                                                                (LocalDate/parse "2026-07-13")
                                                                ZoneOffset/UTC))))]
              (is (= "2026-07-13" (cast-as :date d))
                  "UTC midnight read in the JVM zone is the evening before")
              (is (= (LocalDateTime/parse "2026-07-13T00:00") (time/->utc-local d))))))

(deftest lenient-parse-is-one-rule
  (is (= (LocalDateTime/parse "2026-07-08T08:24:17")
         (time/->utc-local "2026-07-08T10:24:17+02:00")
         (time/->utc-local "2026-07-08 08:24:17")
         (time/->utc-local "2026-07-08T08:24:17Z")))
  (is (nil? (time/->utc-local "05.08.2026"))))
