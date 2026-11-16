(ns sutra.calx.format-test
  (:require [clojure.test :refer [deftest is are testing]]
            [sutra.calx.format :as f]
            [sutra.calx.value :as v]))

(deftest general-and-basic-numbers
  (are [x fmt s] (= s (f/format-value x fmt))
    1234.5    "General"   "1234.5"
    1234.5    ""          "1234.5"
    1234.567  "0"         "1235"
    1234.567  "0.00"      "1234.57"
    2.675     "0.00"      "2.68"
    1234567.8 "#,##0"     "1,234,568"
    1234.5    "#,##0.00"  "1,234.50"
    0         "#,##0"     "0"
    5         "0,000"     "0,005"
    0.5       "#.##"      ".5"
    1         "#.##"      "1."
    0.5       ".00"       ".50"
    1.5       "0.0#"      "1.5"
    1.25      "0.0?"      "1.25"
    1.5       "0.0?"      "1.5 "
    -1234.5   "#,##0.00"  "-1,234.50"
    123456789 "000-00-0000" "123-45-6789"
    1e20      "0"         "100000000000000000000"))

(deftest percent-scaling-scientific
  (are [x fmt s] (= s (f/format-value x fmt))
    0.5       "0%"         "50%"
    0.12345   "0.00%"      "12.35%"
    1234567   "#,##0,,\"M\"" "1M"
    1234567   "0.0,"       "1234.6"
    12345     "0.00E+00"   "1.23E+04"
    0.000123  "0.00E+00"   "1.23E-04"
    0.000123  "0.00E-00"   "1.23E-04"
    12345     "0.00E-00"   "1.23E04"
    0         "0.00E+00"   "0.00E+00"
    12345     "##0.0E+0"   "12.3E+3"
    12345     "00.00E+00"  "12.35E+03"
    9.999     "0.00E+00"   "1.00E+01"))

(deftest fractions
  (are [x fmt s] (= s (f/format-value x fmt))
    1.5     "# ?/?"    "1 1/2"
    0.5     "# ?/?"    " 1/2"
    1       "# ?/?"    "1    "
    0.333   "?/?"      "1/3"
    1.5     "?/?"      "3/2"
    3.14159 "# ??/??"  "3 14/99"
    0.3     "# ?/16"   " 5/16"
    -1.25   "# ?/?"    "-1 1/4"))

(deftest literals-sections-conditions
  (are [x fmt s] (= s (f/format-value x fmt))
    5      "\"$\"0.00"          "$5.00"
    5      "0.00 \"kg\""        "5.00 kg"
    5      "0\\x"              "5x"
    5      "$0"                 "$5"
    -5     "0;(0)"              "(5)"
    5      "0;(0)"              "5"
    0      "0;-0;\"zero\""      "zero"
    -5     "0;-0;\"zero\""      "-5"
    5      "0;;"                "5"
    -5     "0;;"                ""
    -3     "[Red]0.0"           "-3.0"
    -3     "0.0;[Red](0.0)"     "(3.0)"
    50     "[<100]\"small\";\"big\"" "small"
    150    "[<100]\"small\";\"big\"" "big"
    1234.5 "$#,##0_);($#,##0)"  "$1,235 "
    1234.5 "[$€-407]#,##0.00"    "€1,234.50"
    5      "* 0"                "5"
    5      "@"                  "5")
  (testing "text values"
    (are [x fmt s] (= s (f/format-value x fmt))
      "abc" "\"pre \"@"    "pre abc"
      "abc" "0.00"         "abc"
      "abc" "0;0;0;[Blue]@\"!\"" "abc!"
      "abc" "0;0;0;"       ""))
  (testing "other values"
    (is (= "" (f/format-value nil "0.00")))
    (is (= "TRUE" (f/format-value true "0.00")))
    (is (= "#N/A" (f/format-value v/ERR-NA "0.00")))))

(deftest dates-and-times
  (are [x fmt s] (= s (f/format-value x fmt))
    45000      "yyyy-mm-dd"                     "2023-03-15"
    45000      "m/d/yy"                         "3/15/23"
    45000      "mmm-yy"                         "Mar-23"
    45000      "mmmmm"                          "M"
    45000      "dddd, mmmm d, yyyy"             "Wednesday, March 15, 2023"
    45000      "ddd dd"                         "Wed 15"
    45000.5    "yyyy-mm-dd hh:mm:ss"            "2023-03-15 12:00:00"
    0.75       "h AM/PM"                        "6 PM"
    0.25       "h:mm am/pm"                     "6:00 AM"
    0.75       "h:mm a/p"                       "6:00 p"
    0          "h:mm AM/PM"                     "12:00 AM"
    0.5001     "hh:mm:ss.00"                    "12:00:08.64"
    1.5        "[h]:mm:ss"                      "36:00:00"
    0.0625     "[mm]:ss"                        "90:00"
    (/ 59.6 86400) "h:mm:ss"                    "0:01:00"
    (/ 90 1440.0) "mm:ss"                       "30:00"
    60         "yyyy-mm-dd"                     "1900-02-29"
    1          "dddd"                           "Sunday")
  (is (= "#####" (f/format-value -1 "yyyy-mm-dd"))))

(deftest parse-is-cached-data
  (is (identical? (f/parse-format "0.00") (f/parse-format "0.00")))
  (is (= :number (:kind (first (f/parse-format "#,##0.00")))))
  (is (= [:date :fraction :text] (map :kind (f/parse-format "yyyy;# ?/?;@")))))
