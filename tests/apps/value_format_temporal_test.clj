(ns ^:unit apps.value-format-temporal-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.ctx :as cctx]
            [sutra.capps.format :as fmt]
            [sutra.capps.table :as tbl]))

(def ^:private instant "2020-07-22T13:00:00Z")

(def ^:private de (cctx/of {:locale :de}))
(def ^:private en (cctx/of {:locale :en}))

(defn- render
  ([format value] (render format value de))
  ([format value c]
   (let [[col] (tbl/with-prop-display [{:prop :freigegeben-am}]
                 {:freigegeben-am {:type :timestamp :format format}})]
     (tbl/cell-text col {:values {:freigegeben-am value}} c))))

(deftest a-timestamp-renders-in-the-declared-style
  (testing "ein Freigabezeitpunkt in jedem deklarierten Stil"
    (is (= "Mi., 22. Juli 2020"       (render {:kind :date :timezone "UTC"} instant)))
    (is (= "22.07.2020, 13:00"        (render {:kind :datetime-short :timezone "UTC"} instant)))
    (is (= "Mi., 22. Juli 2020, 13:00:00" (render {:kind :datetime-long :timezone "UTC"} instant)))
    (is (= "13:00"                    (render {:kind :time :timezone "UTC"} instant)))
    (is (= "2020-07-22T13:00:00.000Z" (render {:kind :iso-instant} instant)))))

(deftest the-declared-timezone-moves-the-wall-clock
  (testing "a zone id pins the timestamp; the SAME instant is another hour elsewhere"
    (is (= "22.07.2020, 15:00" (render {:kind :datetime-short :timezone "Europe/Vienna"} instant)))
    (is (= "22.07.2020, 13:00" (render {:kind :datetime-short :timezone "UTC"} instant)))))

(deftest the-viewers-zone-applies-when-the-property-declares-none
  (testing "no :timezone on the property — the zone is the viewer's, from the context"
    (is (= "22.07.2020, 15:00" (render {:kind :datetime-short} instant
                                       (cctx/of {:locale :de :zone "Europe/Vienna"}))))
    (is (= "22.07.2020, 13:00" (render {:kind :datetime-short} instant de))
        "no zone in the context either: UTC, never the host's")))

(deftest a-date-only-value-is-not-shifted-by-a-zone
  (testing "a plain date has no instant to convert — midnight-in-a-zone would move the day"
    (is (= "Mi., 22. Juli 2020" (render {:kind :date :timezone "Pacific/Kiritimati"} "2020-07-22")))
    (is (= "Mi., 22. Juli 2020" (render {:kind :date} "22.07.2020"
                                        (cctx/of {:locale :de :zone "Pacific/Midway"})))
        "a dotted date is read as calendar fields, not through an epoch")))

(deftest the-style-is-the-property-the-locale-is-the-viewer
  (let [spec {:kind :datetime-short :timezone "UTC"}]
    (is (= "22.07.2020, 13:00" (render spec instant de)))
    (testing "same declaration, another viewer — month name, order and clock follow"
      (is (= "Jul 22, 2020, 1:00 PM" (render spec instant (cctx/of {:locale "en-GB"})))))))

(deftest numbers-follow-the-same-viewer
  (let [spec {:kind :currency :currency "EUR" :decimals 2 :grouping true}]
    (is (= "1.234,50 €" (render spec 1234.5 de)))
    (is (= "1,234.50 €" (render spec 1234.5 en))
        "separators are the viewer's, the currency and the decimals are the property's")))

(deftest an-iso-string-no-longer-leaks-raw
  (testing "the legacy :datum formatter got a Date object and printed anything else
            verbatim — which is how an ISO timestamp reached the page unrendered"
    (is (= "22.07.2020" (fmt/datum instant de)))
    (is (= "22.07.2020" (fmt/datum "2020-07-22" de)))
    (is (nil? (fmt/datum nil de)))
    (is (= "kein Datum" (fmt/datum "kein Datum" de)) "unparseable text still passes through")))

(deftest compact-notation-is-implemented-not-ignored
  (let [f (fmt/by-spec {:notation :compact})]
    (is (= "123 Tsd." (f 123456 de)) "like Intl: three significant digits, no decimal noise")
    (is (= "1,2 Mio." (f 1234567 de)))
    (is (= "1.2M" (f 1234567 en))))
  (is (= "1.23E5" ((fmt/by-spec {:notation :scientific}) 123456 en))))

(deftest a-relative-timestamp-falls-back-after-a-day
  (let [now 1595422800000
        at  #(java.util.Date. (- now %))
        c   (assoc de :now now)]
    (is (= "gerade eben" (fmt/temporal-text (at 10000) {:style :relative} c)))
    (is (= "vor 8 Minuten" (fmt/temporal-text (at 480000) {:style :relative} c)))
    (is (= "vor 1 Stunde" (fmt/temporal-text (at 3600000) {:style :relative} c)))
    (testing "nach einem Tag wird aus „vor …“ wieder ein Datum"
      (is (re-find #"^\w+\., \d{2}\.\d{2}\.\d{4}, "
                   (fmt/temporal-text (at 200000000) {:style :relative} c))))
    (testing "ohne Moment im Kontext gibt es kein „vor …“ — dann das absolute Datum"
      (is (re-find #"^\w+\., \d{2}\.\d{2}\.\d{4}, "
                   (fmt/temporal-text (at 10000) {:style :relative} de))))))
