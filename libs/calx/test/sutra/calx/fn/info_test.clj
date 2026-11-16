(ns sutra.calx.fn.info-test
  (:require [clojure.test :refer [deftest is are testing]]
            [sutra.calx.core :as c]
            [sutra.calx.value :as v]
            [sutra.calx.fn.text :as text]
            [sutra.calx.fn.info :as info]))

(def wb (c/workbook (array-map "Sheet1" [[1 "abc" true nil "=1/0" "=NA()"]
                                         [2 "=\"\"" "=A1*2"]]
                               "Data" [[10]])
                    {:functions (merge text/functions info/functions)}))

(defn calc [f] (c/calculate wb f))

(deftest is-predicates
  (are [f r] (= r (calc f))
    "=ISBLANK(D1)" true
    "=ISBLANK(B2)" false
    "=ISBLANK(\"\")" false
    "=ISNUMBER(A1)" true
    "=ISNUMBER(\"1\")" false
    "=ISTEXT(B1)" true
    "=ISNONTEXT(D1)" true
    "=ISNONTEXT(B1)" false
    "=ISLOGICAL(C1)" true
    "=ISERROR(E1)" true
    "=ISERROR(A1)" false
    "=ISERR(E1)" true
    "=ISERR(F1)" false
    "=ISNA(F1)" true
    "=ISNA(E1)" false
    "=ISEVEN(2)" true
    "=ISEVEN(-3.5)" false
    "=ISODD(3.9)" true
    "=ISEVEN(D1)" true
    "=ISEVEN(\"x\")" (v/err :value)
    "=ISREF(A1)" true
    "=ISREF(A1:B2)" true
    "=ISREF(1)" false
    "=ISFORMULA(C2)" true
    "=ISFORMULA(A1)" false
    "=FORMULATEXT(C2)" "=A1*2"
    "=FORMULATEXT(A1)" (v/err :na))
  (testing "lifting over arrays"
    (is (= [[false true false]] (calc "=ISTEXT({1,\"a\",TRUE})")))
    (is (= [[true false]] (calc "=ISNUMBER(A1:B1)")))
    (is (= [[false true true]] (calc "=ISFORMULA(A2:C2)")))))

(deftest errors-and-types
  (are [f r] (= r (calc f))
    "=NA()" (v/err :na)
    "=ERROR.TYPE(1/0)" 2.0
    "=ERROR.TYPE(NA())" 7.0
    "=ERROR.TYPE(E1)" 2.0
    "=ERROR.TYPE(1)" (v/err :na)
    "=TYPE(1)" 1.0
    "=TYPE(D1)" 1.0
    "=TYPE(\"a\")" 2.0
    "=TYPE(TRUE)" 4.0
    "=TYPE(NA())" 16.0
    "=TYPE({1,2})" 64.0
    "=TYPE(LAMBDA(x,x))" 128.0
    "=N(5)" 5.0
    "=N(TRUE)" 1.0
    "=N(\"7\")" 0.0
    "=N(NA())" (v/err :na)))

(deftest sheets-cell-info
  (are [f r] (= r (calc f))
    "=SHEET()" 1.0
    "=SHEET(Data!A1)" 2.0
    "=SHEET(\"Data\")" 2.0
    "=SHEET(\"Nope\")" (v/err :na)
    "=SHEETS()" 2.0
    "=SHEETS(A1)" 1.0
    "=CELL(\"address\",B2)" "$B$2"
    "=CELL(\"address\",Data!A1)" "Data!$A$1"
    "=CELL(\"row\",B2)" 2.0
    "=CELL(\"col\",C1)" 3.0
    "=CELL(\"contents\",B1)" "abc"
    "=CELL(\"type\",B1)" "l"
    "=CELL(\"type\",A1)" "v"
    "=CELL(\"type\",D1)" "b"
    "=CELL(\"filename\",A1)" ""
    "=CELL(\"format\",A1)" "G"
    "=CELL(\"bogus\",A1)" (v/err :value)
    "=INFO(\"recalc\")" "Automatic"
    "=INFO(\"system\")" "pcdos"
    "=INFO(\"numfile\")" 2.0
    "=INFO(\"bogus\")" (v/err :value)))
