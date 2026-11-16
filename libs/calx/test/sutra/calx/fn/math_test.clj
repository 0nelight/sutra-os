(ns sutra.calx.fn.math-test
  (:require [clojure.test :refer [deftest is testing are]]
            [sutra.calx.core :as c]
            [sutra.calx.fn.math :as m]
            [sutra.calx.test-helpers :refer [approx= err]]))

(def wb
  (c/workbook {"Sheet1" [[1 2 3 "x" true nil]
                         [4 5 6 "'7" false nil]
                         ["apple" 10 nil nil nil nil]
                         ["banana" 20 nil nil nil nil]
                         ["apple" 30 nil nil nil nil]
                         ["cherry" "#DIV/0!" nil nil nil nil]
                         [nil 99 nil nil nil nil]]}
              {:functions m/functions}))

(defn calc [f] (c/calculate wb f))

(deftest sum-family
  (is (= 21.0 (calc "=SUM(A1:C2)")))
  (testing "text and booleans in ranges are ignored, direct args coerced"
    (is (= 6.0 (calc "=SUM(A1:F1)")))
    (is (= 5.0 (calc "=SUM(\"3\",TRUE,1)")))
    (is (= (err :value) (calc "=SUM(\"x\",1)"))))
  (is (= 0.0 (calc "=SUM(D1:E1)")))
  (is (= (err :div0) (calc "=SUM(B1:B6)")))
  (is (= 14.0 (calc "=SUMSQ(1,2,3)")))
  (is (= 720.0 (calc "=PRODUCT(A1:C2)")))
  (is (= 0.0 (calc "=PRODUCT(D1)")))
  (is (= 6.0 (calc "=SUM({1,2,\"a\";3,TRUE,\"b\"})"))))

(deftest sumproduct-test
  (is (= 32.0 (calc "=SUMPRODUCT(A1:C1,A2:C2)")))
  (is (= 21.0 (calc "=SUMPRODUCT(A1:C2)")))
  (is (= 32.0 (calc "=SUMPRODUCT(A1:E1,A2:E2)")) "text and booleans count as 0")
  (is (= (err :value) (calc "=SUMPRODUCT(A1:C1,A1:B1)")))
  (is (= 2.0 (calc "=SUMPRODUCT(--(A3:A5=\"apple\"))")))
  (is (= 40.0 (calc "=SUMPRODUCT((A3:A5=\"apple\")*B3:B5)"))))

(deftest sumif-test
  (is (= 40.0 (calc "=SUMIF(A3:A6,\"apple\",B3:B6)")))
  (is (= 40.0 (calc "=SUMIF(A3:A6,\"apple\",B3)")) "sum range resized from top-left")
  (is (= 20.0 (calc "=SUMIF(A3:A6,\"b*\",B3:B6)")))
  (is (= 15.0 (calc "=SUMIF(A1:C2,\">3\")")))
  (is (= 99.0 (calc "=SUMIF(A7,\"\",B7)")))
  (is (= (err :div0) (calc "=SUMIF(A3:A6,\"cherry\",B3:B6)")))
  (is (= 30.0 (calc "=SUMIFS(B3:B6,A3:A6,\"apple\",B3:B6,\">15\")")))
  (is (= (err :value) (calc "=SUMIFS(B3:B6,A3:A5,\"apple\")")))
  (is (= [[40.0 20.0]] (calc "=SUMIF(A3:A5,{\"apple\",\"banana\"},B3:B5)"))))

(deftest paired-sums
  (is (= -63.0 (calc "=SUMX2MY2({1,2,3},{4,5,6})")))
  (is (= 91.0 (calc "=SUMX2PY2({1,2,3},{4,5,6})")))
  (is (= 27.0 (calc "=SUMXMY2({1,2,3},{4,5,6})")))
  (is (= (err :na) (calc "=SUMXMY2({1,2,3},{4,5})")))
  (is (= 114.0 (calc "=SERIESSUM(2,1,2,{1,2,3})")))
  (is (= (err :value) (calc "=SERIESSUM(2,1,2,{1,\"a\"})"))))

(deftest basic-math
  (are [f x] (= x (calc f))
    "=ABS(-3.5)" 3.5
    "=SIGN(-2)" -1.0
    "=SIGN(0)" 0.0
    "=SQRT(16)" 4.0
    "=SQRT(-1)" (err :num)
    "=POWER(2,10)" 1024.0
    "=POWER(0,0)" (err :num)
    "=POWER(0,-1)" (err :div0)
    "=POWER(-8,1/3)" (err :num)
    "=LN(0)" (err :num)
    "=LOG(100)" 2.0
    "=LOG(8,2)" 3.0
    "=LOG(8,1)" (err :div0)
    "=LOG10(1000)" 3.0
    "=EXP(0)" 1.0
    "=INT(-2.5)" -3.0
    "=TRUNC(-2.5)" -2.0
    "=TRUNC(3.14159,2)" 3.14
    "=MOD(-3,2)" 1.0
    "=MOD(3,-2)" -1.0
    "=MOD(5.5,2)" 1.5
    "=MOD(1,0)" (err :div0)
    "=QUOTIENT(-7,2)" -3.0
    "=QUOTIENT(7,0)" (err :div0)
    "=SQRT(\"abc\")" (err :value))
  (is (approx= 3.5449077018 (calc "=SQRTPI(4)") 1e-9))
  (is (= [[1.0 2.0] [3.0 4.0]] (calc "=ABS({-1,2;-3,4})")))
  (is (= Math/PI (calc "=PI()"))))

(deftest rounding
  (are [f x] (= x (calc f))
    "=ROUND(2.675,2)" 2.68
    "=ROUND(2.5,0)" 3.0
    "=ROUND(-2.5,0)" -3.0
    "=ROUND(1234.567,-2)" 1200.0
    "=ROUND(0.1+0.2,15)" 0.3
    "=ROUNDUP(3.2,0)" 4.0
    "=ROUNDUP(-3.14159,1)" -3.2
    "=ROUNDUP(31415.92654,-2)" 31500.0
    "=ROUNDDOWN(3.99,0)" 3.0
    "=ROUNDDOWN(-3.14159,1)" -3.1
    "=MROUND(10,3)" 9.0
    "=MROUND(-10,-3)" -9.0
    "=MROUND(1.3,0.2)" 1.4
    "=MROUND(5,-2)" (err :num)
    "=CEILING(2.5,1)" 3.0
    "=CEILING(-2.5,-2)" -4.0
    "=CEILING(-2.5,2)" -2.0
    "=CEILING(1.5,0.1)" 1.5
    "=CEILING(0.234,0.01)" 0.24
    "=CEILING(1.5,-1)" (err :num)
    "=FLOOR(3.7,2)" 2.0
    "=FLOOR(-2.5,-2)" -2.0
    "=FLOOR(-2.5,2)" -4.0
    "=FLOOR(1.58,0.1)" 1.5
    "=FLOOR(2.5,0)" (err :div0)
    "=FLOOR(2.5,-2)" (err :num)
    "=CEILING.MATH(24.3,5)" 25.0
    "=CEILING.MATH(6.7)" 7.0
    "=CEILING.MATH(-8.1,2)" -8.0
    "=CEILING.MATH(-5.5,2,-1)" -6.0
    "=CEILING.PRECISE(-4.3)" -4.0
    "=CEILING.PRECISE(4.3,-2)" 6.0
    "=ISO.CEILING(-4.6,2)" -4.0
    "=FLOOR.MATH(24.3,5)" 20.0
    "=FLOOR.MATH(-8.1,2)" -10.0
    "=FLOOR.MATH(-5.5,2,-1)" -4.0
    "=FLOOR.PRECISE(-3.2,-1)" -4.0
    "=FLOOR.PRECISE(3.2)" 3.0
    "=EVEN(1.5)" 2.0
    "=EVEN(3)" 4.0
    "=EVEN(2)" 2.0
    "=EVEN(-1)" -2.0
    "=EVEN(0)" 0.0
    "=ODD(1.5)" 3.0
    "=ODD(3)" 3.0
    "=ODD(2)" 3.0
    "=ODD(-1)" -1.0
    "=ODD(-2)" -3.0
    "=ODD(0)" 1.0))

(deftest number-theory
  (are [f x] (= x (calc f))
    "=FACT(5)" 120.0
    "=FACT(1.9)" 1.0
    "=FACT(0)" 1.0
    "=FACT(-1)" (err :num)
    "=FACT(171)" (err :num)
    "=FACTDOUBLE(6)" 48.0
    "=FACTDOUBLE(7)" 105.0
    "=COMBIN(8,2)" 28.0
    "=COMBIN(49,6)" 13983816.0
    "=COMBIN(2,3)" (err :num)
    "=COMBINA(4,3)" 20.0
    "=COMBINA(10,3)" 220.0
    "=PERMUT(100,3)" 970200.0
    "=PERMUT(3,2)" 6.0
    "=PERMUT(2,3)" (err :num)
    "=PERMUTATIONA(3,2)" 9.0
    "=MULTINOMIAL(2,3,4)" 1260.0
    "=MULTINOMIAL(-1,2)" (err :num)
    "=GCD(5,2)" 1.0
    "=GCD(24,36)" 12.0
    "=GCD(7.9,1)" 1.0
    "=GCD(0,5)" 5.0
    "=GCD(-1,5)" (err :num)
    "=LCM(5,2)" 10.0
    "=LCM(24,36)" 72.0
    "=LCM(3,0)" 0.0
    "=LCM(A1:C2)" 60.0))

(deftest random
  (let [r (calc "=RAND()")]
    (is (and (>= r 0.0) (< r 1.0))))
  (dotimes [_ 20]
    (let [r (calc "=RANDBETWEEN(1,6)")]
      (is (and (<= 1.0 r 6.0) (== r (Math/floor r))))))
  (is (= 3.0 (calc "=RANDBETWEEN(3,3)")))
  (is (= (err :num) (calc "=RANDBETWEEN(5,1)"))))

(deftest trig
  (are [f x] (approx= x (calc f))
    "=DEGREES(PI())" 180.0
    "=RADIANS(180)" Math/PI
    "=SIN(PI()/2)" 1.0
    "=COS(0)" 1.0
    "=TAN(PI()/4)" 1.0
    "=ASIN(1)" (/ Math/PI 2)
    "=ACOS(-1)" Math/PI
    "=ATAN(1)" (/ Math/PI 4)
    "=ATAN2(1,1)" (/ Math/PI 4)
    "=ATAN2(-1,0)" Math/PI
    "=SINH(1)" 1.1752011936438014
    "=COSH(1)" 1.5430806348152437
    "=TANH(0.5)" 0.46211715726000974
    "=ASINH(-2.5)" -1.6472311463710958
    "=ACOSH(10)" 2.993222846126381
    "=ATANH(TANH(1))" 1.0
    "=COT(30)" -0.156119952
    "=COTH(2)" 1.037314721
    "=CSC(15)" 1.537780562
    "=CSCH(1.5)" 0.469642441
    "=SEC(45)" 1.903594407
    "=SECH(0.5)" 0.886818884
    "=ACOT(2)" 0.463647609
    "=ACOTH(6)" 0.168236118)
  (are [f x] (= x (calc f))
    "=ASIN(2)" (err :num)
    "=ACOSH(0.5)" (err :num)
    "=ATANH(1)" (err :num)
    "=ATAN2(0,0)" (err :div0)
    "=COT(0)" (err :div0)
    "=CSC(0)" (err :div0)
    "=ACOTH(1)" (err :num)
    "=SEC(2^27)" (err :num)
    "=COSH(1000)" (err :num)))

(def wb-sub
  (c/workbook {"Sheet1" [[10 nil]
                         [20 nil]
                         ["=SUBTOTAL(9,A1:A2)" nil]
                         [5 nil]
                         ["=SUBTOTAL(9,A1:A4)" nil]
                         ["#N/A" nil]
                         [3 nil]]}
              {:functions m/functions}))

(deftest subtotal-test
  (is (= 30.0 (c/value wb-sub "A3")))
  (is (= 35.0 (c/value wb-sub "A5")) "nested SUBTOTAL cells are excluded")
  (are [f x] (= x (calc f))
    "=SUBTOTAL(9,A1:C2)" 21.0
    "=SUBTOTAL(109,A1:C2)" 21.0
    "=SUBTOTAL(1,A1:C2)" 3.5
    "=SUBTOTAL(2,A1:E2)" 6.0
    "=SUBTOTAL(3,A1:E2)" 10.0
    "=SUBTOTAL(4,A1:C2)" 6.0
    "=SUBTOTAL(5,A1:C2)" 1.0
    "=SUBTOTAL(6,A1:C2)" 720.0
    "=SUBTOTAL(11,A1:C2)" (/ 17.5 6)
    "=SUBTOTAL(9,A1:A2,C1:C2)" 14.0
    "=SUBTOTAL(9,B1:B6)" (err :div0)
    "=SUBTOTAL(12,A1:C2)" (err :value)
    "=SUBTOTAL(0,A1:C2)" (err :value))
  (is (approx= 1.8708286934 (calc "=SUBTOTAL(7,A1:C2)")))
  (is (approx= 3.5 (calc "=SUBTOTAL(110,A1:C2)"))))

(deftest aggregate-test
  (are [f x] (= x (c/calculate wb-sub f))
    "=AGGREGATE(9,6,A1:A7)" 103.0
    "=AGGREGATE(9,7,A1:A7)" 103.0
    "=AGGREGATE(9,4,A1:A7)" (err :na)
    "=AGGREGATE(9,3,A1:A7)" 38.0
    "=AGGREGATE(4,6,A1:A7)" 35.0
    "=AGGREGATE(14,6,A1:A7,2)" 30.0
    "=AGGREGATE(15,6,A1:A7,1)" 3.0
    "=AGGREGATE(12,6,A1:A7)" 15.0
    "=AGGREGATE(14,6,A1:A7)" (err :value)
    "=AGGREGATE(20,6,A1:A7)" (err :value)
    "=AGGREGATE(9,8,A1:A7)" (err :value)
    "=AGGREGATE(14,6,A1:A7,10)" (err :num))
  (are [f x] (approx= x (calc f))
    "=AGGREGATE(16,0,{1,2,3,4},0.3)" 1.9
    "=AGGREGATE(17,0,{1,2,3,4},1)" 1.75
    "=AGGREGATE(18,0,{1,2,3,4},0.4)" 2.0
    "=AGGREGATE(19,0,{1,2,3,4,5,6,7,8},1)" 2.25
    "=AGGREGATE(13,0,{1,2,2,3,3})" 2.0
    "=AGGREGATE(10,0,{2,4,4,4,5,5,7,9})" (/ 32.0 7)
    "=AGGREGATE(8,0,{2,4,4,4,5,5,7,9})" 2.0)
  (is (= 3.0 (calc "=AGGREGATE(9,6,{1,2,3}/{1,1,0})")))
  (is (= (err :div0) (calc "=AGGREGATE(9,4,{1,2,3}/{1,1,0})"))))

(deftest matrices
  (is (= [[19.0 22.0] [43.0 50.0]] (calc "=MMULT({1,2;3,4},{5,6;7,8})")))
  (is (= 11.0 (calc "=MMULT({1,2},{3;4})")))
  (is (= (err :value) (calc "=MMULT({1,2},{3,4})")))
  (is (= (err :value) (calc "=MMULT({1,\"a\"},{3;4})")))
  (is (= -2.0 (calc "=MDETERM({1,2;3,4})")))
  (is (= 88.0 (calc "=MDETERM({1,3,8,5;1,3,6,1;1,1,1,0;7,3,10,2})")))
  (is (= (err :value) (calc "=MDETERM({1,2,3;4,5,6})")))
  (let [inv (calc "=MINVERSE({4,-1;2,1})")]
    (is (every? true? (map approx= (flatten inv) [(/ 1 6.0) (/ 1 6.0) (/ -1 3.0) (/ 2 3.0)]))))
  (is (= (err :num) (calc "=MINVERSE({1,2;2,4})")))
  (is (= [[1.0 0.0 0.0] [0.0 1.0 0.0] [0.0 0.0 1.0]] (calc "=MUNIT(3)")))
  (is (= (err :value) (calc "=MUNIT(0)"))))

(deftest number-systems
  (are [f x] (= x (calc f))
    "=BASE(7,2)" "111"
    "=BASE(100,16)" "64"
    "=BASE(15,2,10)" "0000001111"
    "=BASE(255,36)" "73"
    "=BASE(-1,2)" (err :num)
    "=BASE(7,1)" (err :num)
    "=BASE(7,37)" (err :num)
    "=DECIMAL(\"FF\",16)" 255.0
    "=DECIMAL(\"zap\",36)" 45745.0
    "=DECIMAL(111,2)" 7.0
    "=DECIMAL(\"12\",2)" (err :num)
    "=DECIMAL(\"1\",37)" (err :num)
    "=ROMAN(499)" "CDXCIX"
    "=ROMAN(499,1)" "LDVLIV"
    "=ROMAN(499,2)" "XDIX"
    "=ROMAN(499,3)" "VDIV"
    "=ROMAN(499,4)" "ID"
    "=ROMAN(1999)" "MCMXCIX"
    "=ROMAN(3999)" "MMMCMXCIX"
    "=ROMAN(0)" ""
    "=ROMAN(4000)" (err :value)
    "=ROMAN(-1)" (err :value)
    "=ARABIC(\"MCMXCIX\")" 1999.0
    "=ARABIC(\"mxmvii\")" 1997.0
    "=ARABIC(\"-IV\")" -4.0
    "=ARABIC(\"\")" 0.0
    "=ARABIC(\"ABC\")" (err :value)))
