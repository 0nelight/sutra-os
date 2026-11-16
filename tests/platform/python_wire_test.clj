(ns ^:unit platform.python-wire-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [sutra.compute.external :as ext]))

(deftest every-time-crosses-as-iso-text
  (let [row {:a (java.time.OffsetDateTime/parse "2026-06-29T10:00:00+02:00")
             :b (java.time.ZonedDateTime/parse "2026-06-29T10:00:00+02:00[Europe/Vienna]")
             :c (java.time.LocalDateTime/parse "2026-06-29T10:00")
             :d (java.time.Instant/parse "2026-06-29T08:00:00Z")}
        w   (ext/->wire row)]
    (is (= {:a "2026-06-29T08:00:00Z" :b "2026-06-29T08:00:00Z" :c "2026-06-29T10:00"
            :d "2026-06-29T08:00:00Z"}
           w))
    (is (string? (json/write-str w)))))

(deftest decimals-and-tilde-texts-read-back
  (let [x {:n 12.50M :s "~raw" :l ["~" 1 nil]}]
    (is (= x (ext/<-wire (ext/->wire x))))))
