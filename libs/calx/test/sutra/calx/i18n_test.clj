(ns sutra.calx.i18n-test
  (:require [clojure.test :refer [deftest is]]
            [sutra.calx.core :as c]
            [sutra.calx.i18n :as i18n]))

(deftest translation
  (is (= "=SUM(A1:A3,2.5)" (i18n/delocalize "=SUMME(A1:A3;2,5)" :de)))
  (is (= "=IF(TRUE,\"a;b\",'Mein Blatt'!A1)"
         (i18n/delocalize "=WENN(WAHR;\"a;b\";'Mein Blatt'!A1)" :de)))
  (is (= "={1,2;3,4}" (i18n/delocalize "={1.2;3.4}" :de)))
  (is (= "=SVERWEIS(B1;A1:C9;3;FALSCH)" (i18n/localize "=VLOOKUP(B1,A1:C9,3,FALSE)" :de)))
  (is (= "=SUMME(Tabelle1.A1)" (i18n/localize "=SUM(Tabelle1.A1)" :de))
      "identifiers with dots untouched")
  (is (= "=WENN(A1>0,5;1;0)" (i18n/localize (i18n/delocalize "=WENN(A1>0,5;1;0)" :de) :de))
      "roundtrip"))

(deftest localized-workbook
  (let [w (-> (c/workbook {"Tabelle1" [[1 2 3]]} {:config {:locale :de}})
              (c/set-cell "D1" "=WENN(SUMME(A1:C1)>5;MITTELWERT(A1:C1)*1,5;0)")
              (c/set-cell "E1" "2,5"))]
    (is (= 3.0 (c/value w "D1")))
    (is (= "=WENN(SUMME(A1:C1)>5;MITTELWERT(A1:C1)*1,5;0)" (c/formula w "D1")))
    (is (= 2.5 (c/value w "E1")))))
