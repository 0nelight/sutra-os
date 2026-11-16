(ns ^:unit platform.exact-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.capps.ctx :as cctx]
            [sutra.capps.format :as fmt]
            [sutra.capps.table :as tbl]
            [sutra.types.profile :as profile]
            [sutra.types.decimal :as d]
            [sutra.wire.codec :as codec]))

(def ^:private de (cctx/of {:locale :de}))

(def ^:private long-decimal
  "123456789012345678901234.56789")

(deftest transit-carries-every-digit
  (testing "a BigDecimal survives encode → decode with all of its digits"
    (let [v {:betrag (bigdec long-decimal)}
          r (codec/decode (codec/encode v))]
      (is (= (bigdec long-decimal) (:betrag r)))
      (is (= long-decimal (.toPlainString ^java.math.BigDecimal (:betrag r))))))
  (testing "the tags the handlers key on are the ones the writer emits"
    (is
     (= "[\"^ \",\"~:a\",\"~f1.50\",\"~:b\",\"~n9007199254740993\",\"~:c\",\"~i9007199254740993\",\"~:d\",5]"
        (String. ^bytes (codec/encode {:a 1.50M :b (biginteger 9007199254740993)
                                       :c 9007199254740993 :d 5})
                 "UTF-8"))
     "~f/~n/~i are all string-tagged; a Long inside ±(2^53−1) is a bare JSON number")))

(deftest carry-allows-a-double-only-where-it-cannot-lie
  (testing "a value a double round-trips travels as a double — nothing is lost"
    (is (= Double (type (d/carry "1234.50"))))
    (is (= 1234.5 (d/carry "1234.50")))
    (is (= Double (type (d/carry "9007199254740991")))))
  (testing "a value it cannot round-trip stays exact"
    (is (d/exact? (d/carry long-decimal)))
    (is (= long-decimal (d/plain (d/carry long-decimal))))
    (is (d/exact? (d/carry "9007199254740993"))))
  (testing "a non-number carries nothing"
    (is (nil? (d/carry "abc")))
    (is (nil? (d/carry ""))))
  (testing "a magnitude a double overflows to Infinity is not a round-trip either"
    (is (d/exact? (d/carry "1e400")))))

(deftest the-boundary-is-max-safe-integer
  (testing "the JVM has no cliff here — a Long reaches 2^63 — but `carry` draws
            the line at the same place transit-js does, so an id past it never
            becomes a number in either host. The browser half is in the cljs
            smoke test, where the same value must NOT arrive as a goog.math.Long."
    (is (= 9007199254740991 d/max-safe-integer))
    (is (= (double d/max-safe-integer) (d/carry (str d/max-safe-integer))))
    (is (d/exact? (d/carry "9007199254740993")))
    (is (= "9007199254740993" (d/plain (d/carry "9007199254740993"))))))

(deftest round-half-up-is-decided-on-the-digits
  (testing "the ordinary cases"
    (is (= "1234.50" (d/round-half-up "1234.5" 2)))
    (is (= "0.01"    (d/round-half-up "0.005" 2)))
    (is (= "10.0"    (d/round-half-up "9.99" 1)))
    (is (= "1"       (d/round-half-up "0.9" 0)))
    (is (= "0"       (d/round-half-up "0" 0))))
  (testing "a tie rounds AWAY FROM ZERO — `.toFixed`/`%.2f` round the binary
            double underneath instead and answer 1,00 here"
    (is (= "1.01" (d/round-half-up "1.005" 2)))
    (is (= "2.68" (d/round-half-up "2.675" 2))))
  (testing "no digit of a long value is dropped"
    (is (= "123456789012345678901234.56789" (d/round-half-up long-decimal 5)))
    (is (= "123456789012345678901234.5679"  (d/round-half-up long-decimal 4)))))

(deftest comparison-and-shape-read-the-digits
  (is (zero? (d/cmp "1.5" "1.50")))
  (is (= -1 (d/cmp "-2" "1")))
  (is (= 1 (d/cmp "10" "9")))
  (is (zero? (d/cmp "-0.0" "0")))
  (is (= -1 (d/cmp-magnitude 8.881784197001252E-15 1e-9)))
  (is (d/integral? (bigdec "2024.00")))
  (is (not (d/integral? 1234.50M)))
  (testing "an exponent from either host's `str` expands to plain digits"
    (is (= "1000000000000000000000" (d/plain 1e21)))
    (is (= "1000000000000000000000" (d/plain "1e+21")) "the shape JS emits")
    (is (= "0.00001" (d/plain 1.0E-5))
        "the JVM writes this double as `1.0E-5` and JS as `0.00001` — `plain` must
         answer the same either way, padding zeros and all")
    (is (= "1" (d/plain 1.0)) "…for the same reason `1.0` and `1` must agree")
    (is (= "0.000000000000008881784197001252" (d/plain 8.881784197001252E-15))))
  (testing "an exact carrier keeps the scale it was STORED with — those zeros are
            digits, not a host's padding"
    (is (= "1234.50" (d/plain 1234.50M)))))

(deftest an-exact-value-reaches-the-display-unbent
  (testing "29 digits in, 29 digits out"
    (is (= "123.456.789.012.345.678.901.234,56789" (fmt/fmt (bigdec long-decimal) 5 de)))
    (is (= "123.456.789.012.345.678.901.234,57 €"  (fmt/eur (bigdec long-decimal) de)))
    (is (= "123.456.789.012.345.678.901.234,568"   (fmt/menge (bigdec long-decimal) de)))
    (is (= "123.456.789.012.345.678.901.234,568"   (fmt/auto (bigdec long-decimal) de))))
  (testing "an integer past 2^53 keeps its last digit"
    (is (= "9007199254740993" (fmt/auto (d/exact "9007199254740993") de)))
    (is (= "9.007.199.254.740.993,00 €" (fmt/eur (d/exact "9007199254740993") de))))
  (testing "`num` hands the carrier on rather than parsing it away"
    (is (d/exact? (fmt/num long-decimal)))
    (is (= long-decimal (d/plain (fmt/num long-decimal))))
    (is (= "123.456.789.012.345.678.901.234,56789" (fmt/fmt long-decimal 5 de))
        "the same digits whether the value arrived as a carrier or as a string")))

(deftest the-ordinary-numbers-are-unchanged
  (is (= "1.234,50 €" (fmt/eur 1234.5 de)))
  (is (= "1.235 €"    (fmt/eur0 1234.5 de)))
  (is (= "1.234,5 €"  (fmt/eur1 1234.5 de)))
  (is (= "12,5 %"     (fmt/prozent 12.5 de)))
  (is (= "1.234,57"   (fmt/fmt 1234.5678 2 de)))
  (is (= "–"          (fmt/fmt nil 2 de)))
  (is (= "0,00"       (fmt/fmt 8.881784197001252E-15 2 de)) "float noise still snaps to 0")
  (is (= "1.234,5"    (fmt/menge 1234.5 de)))
  (is (= "1.000"      (fmt/menge 1000 de)))
  (is (= "0,001"      (fmt/menge 0.0005 de)))
  (is (= "2024"       (fmt/auto 2024 de)) "a year must not become 2.024")
  (is (= "2024"       (fmt/auto (bigdec "2024.00") de)) "…nor may a NUMERIC(10,2) holding one")
  (is (= "1,5"        (fmt/auto 1.5 de)))
  (is (= "0"          (fmt/auto 1e-14 de)))
  (is (= "Ja"         (fmt/auto true de)))
  (is (nil?           (fmt/auto nil de))))

(deftest an-unformatted-widget-still-gets-a-react-child
  (testing "the carrier is an OBJECT — React refuses one — so the default cell
            renderer spells it out, and leaves every other value alone"
    (is (= long-decimal (tbl/react-safe (bigdec long-decimal))))
    (is (= 1234.5 (tbl/react-safe 1234.5)))
    (is (= "B-1" (tbl/react-safe "B-1")))
    (is (nil? (tbl/react-safe nil)))))

(deftest a-column-profile-counts-the-exact-rows-too
  (testing "min/max/mean must not lose the row whose value is neither `number?`
            nor `string?` in JS"
    (let [rows [{:betrag (bigdec long-decimal)} {:betrag 1M} {:betrag 3M}]
          s    (profile/column-stats rows :betrag)]
      (is (= 3 (:count s)))
      (is (= 1.0 (:min s)))
      (is (= 3 (:distinct s)))
      (is (some? (:mean s))))))

(deftest calc-is-the-only-door-to-arithmetic
  (testing "on the JVM an exact carrier IS a number, so calc keeps it exact"
    (is (= (bigdec long-decimal) (fmt/calc long-decimal)))
    (is (= 5 (fmt/calc 5)))
    (is (nil? (fmt/calc nil))))
  (testing "lossy-number is the door that is allowed to lose, and says so"
    (is (= 1.2345678901234568E23 (d/lossy-number (bigdec long-decimal))))
    (is (nil? (d/lossy-number "abc")))))
