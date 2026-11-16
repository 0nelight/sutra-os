(ns sutra.calx.fn.engineering-test
  (:require [clojure.test :refer [deftest is testing are]]
            [sutra.calx.core :as c]
            [sutra.calx.fn.engineering :as e]
            [sutra.calx.test-helpers :refer [approx= err]]))

(def wb (c/workbook {"Sheet1" [["3+4i" "1-2i" "2j" nil]]} {:functions e/functions}))

(defn calc [f] (c/calculate wb f))

(deftest from-decimal
  (are [f x] (= x (calc f))
    "=DEC2BIN(9)" "1001"
    "=DEC2BIN(9,8)" "00001001"
    "=DEC2BIN(-100)" "1110011100"
    "=DEC2BIN(-1,3)" "1111111111"
    "=DEC2BIN(511)" "111111111"
    "=DEC2BIN(512)" (err :num)
    "=DEC2BIN(-513)" (err :num)
    "=DEC2BIN(9,2)" (err :num)
    "=DEC2BIN(9,11)" (err :num)
    "=DEC2BIN(9.9)" "1001"
    "=DEC2BIN(\"x\")" (err :value)
    "=DEC2OCT(58,3)" "072"
    "=DEC2OCT(-100)" "7777777634"
    "=DEC2OCT(536870912)" (err :num)
    "=DEC2HEX(100,4)" "0064"
    "=DEC2HEX(-54)" "FFFFFFFFCA"
    "=DEC2HEX(28)" "1C"
    "=DEC2HEX(549755813887)" "7FFFFFFFFF"
    "=DEC2HEX(549755813888)" (err :num)))

(deftest to-decimal
  (are [f x] (= x (calc f))
    "=BIN2DEC(1100100)" 100.0
    "=BIN2DEC(\"1111111111\")" -1.0
    "=BIN2DEC(\"1000000000\")" -512.0
    "=BIN2DEC(\"12\")" (err :num)
    "=BIN2DEC(\"11111111111\")" (err :num)
    "=BIN2DEC(\"\")" 0.0
    "=BIN2DEC(TRUE)" (err :value)
    "=OCT2DEC(54)" 44.0
    "=OCT2DEC(\"7777777533\")" -165.0
    "=OCT2DEC(8)" (err :num)
    "=HEX2DEC(\"A5\")" 165.0
    "=HEX2DEC(\"a5\")" 165.0
    "=HEX2DEC(\"FFFFFFFF5B\")" -165.0
    "=HEX2DEC(\"3DA408B9\")" 1034160313.0
    "=HEX2DEC(\"G\")" (err :num)))

(deftest base-to-base
  (are [f x] (= x (calc f))
    "=BIN2HEX(11111011,4)" "00FB"
    "=BIN2HEX(1110)" "E"
    "=BIN2HEX(\"1111111111\")" "FFFFFFFFFF"
    "=BIN2OCT(1001,3)" "011"
    "=BIN2OCT(\"1100100\")" "144"
    "=BIN2OCT(\"1111111111\")" "7777777777"
    "=OCT2BIN(3,3)" "011"
    "=OCT2BIN(\"7777777000\")" "1000000000"
    "=OCT2BIN(1000)" (err :num)
    "=OCT2HEX(100,4)" "0040"
    "=OCT2HEX(\"7777777533\")" "FFFFFFFF5B"
    "=HEX2BIN(\"F\",8)" "00001111"
    "=HEX2BIN(\"B7\")" "10110111"
    "=HEX2BIN(\"FFFFFFFFFF\")" "1111111111"
    "=HEX2BIN(\"200\")" (err :num)
    "=HEX2OCT(\"F\",3)" "017"
    "=HEX2OCT(\"3B4E\")" "35516"
    "=HEX2OCT(\"FFFFFFFF00\")" "7777777400"
    "=HEX2OCT(\"FFDFFFFFFF\")" (err :num)))

(deftest bitwise
  (are [f x] (= x (calc f))
    "=BITAND(1,5)" 1.0
    "=BITAND(13,25)" 9.0
    "=BITOR(23,10)" 31.0
    "=BITXOR(5,3)" 6.0
    "=BITLSHIFT(4,2)" 16.0
    "=BITLSHIFT(4,-2)" 1.0
    "=BITRSHIFT(13,2)" 3.0
    "=BITRSHIFT(13,-2)" 52.0
    "=BITAND(-1,5)" (err :num)
    "=BITAND(1.5,5)" (err :num)
    "=BITAND(2^48,1)" (err :num)
    "=BITLSHIFT(2^47,1)" (err :num)
    "=BITLSHIFT(1,54)" (err :num)))

(deftest delta-gestep
  (are [f x] (= x (calc f))
    "=DELTA(5,4)" 0.0
    "=DELTA(5,5)" 1.0
    "=DELTA(0.5)" 0.0
    "=DELTA(0)" 1.0
    "=DELTA(\"a\")" (err :value)
    "=GESTEP(5,4)" 1.0
    "=GESTEP(5,5)" 1.0
    "=GESTEP(-4,-5)" 1.0
    "=GESTEP(-1)" 0.0))

(deftest erf-test
  (are [f x] (approx= x (calc f) 1e-14)
    "=ERF(0.745)" 0.7079289200957377
    "=ERF(1)" 0.842700792949715
    "=ERF(0.5)" 0.520499877813047
    "=ERF(2)" 0.995322265018953
    "=ERF(3)" 0.999977909503001
    "=ERF(-1)" -0.842700792949715
    "=ERF(0,1)" 0.842700792949715
    "=ERF(1,2)" 0.152621472069238
    "=ERF.PRECISE(1)" 0.842700792949715
    "=ERFC(1)" 0.157299207050285
    "=ERFC(-1)" 1.842700792949715
    "=ERFC.PRECISE(1)" 0.157299207050285)
  (is (approx= 0.0046777349810473 (calc "=ERFC(2)") 1e-12))
  (is (approx= 2.20904969985854e-5 (calc "=ERFC(3)") 1e-12))
  (is (approx= 1.5374597944280e-12 (calc "=ERFC(5)") 1e-10))
  (is (= 0.0 (calc "=ERF(0)")))
  (is (= (err :value) (calc "=ERF(\"x\")"))))

(deftest convert-test
  (are [f x] (approx= x (calc f) 1e-12)
    "=CONVERT(1,\"lbm\",\"kg\")" 0.45359237
    "=CONVERT(68,\"F\",\"C\")" 20.0
    "=CONVERT(100,\"C\",\"F\")" 212.0
    "=CONVERT(0,\"C\",\"K\")" 273.15
    "=CONVERT(6,\"ft\",\"m\")" 1.8288
    "=CONVERT(1,\"in\",\"cm\")" 2.54
    "=CONVERT(1,\"mi\",\"km\")" 1.609344
    "=CONVERT(1,\"yd\",\"mm\")" 914.4
    "=CONVERT(16,\"ozm\",\"lbm\")" 1.0
    "=CONVERT(1,\"day\",\"hr\")" 24.0
    "=CONVERT(1,\"yr\",\"day\")" 365.25
    "=CONVERT(90,\"mn\",\"hr\")" 1.5
    "=CONVERT(1,\"gal\",\"l\")" 3.785411784
    "=CONVERT(4,\"qt\",\"gal\")" 1.0
    "=CONVERT(1,\"m3\",\"l\")" 1000.0
    "=CONVERT(1,\"l\",\"ml\")" 1000.0
    "=CONVERT(1,\"cal\",\"J\")" 4.1868
    "=CONVERT(1,\"kcal\",\"J\")" 4186.8
    "=CONVERT(1,\"atm\",\"Pa\")" 101325.0
    "=CONVERT(760,\"mmHg\",\"atm\")" 1.0
    "=CONVERT(1,\"kPa\",\"Pa\")" 1000.0
    "=CONVERT(1,\"km2\",\"m2\")" 1000000.0
    "=CONVERT(1,\"Rank\",\"K\")" (/ 5.0 9.0))
  (are [f x] (= x (calc f))
    "=CONVERT(2.5,\"ft\",\"sec\")" (err :na)
    "=CONVERT(1,\"foo\",\"m\")" (err :na)
    "=CONVERT(1,\"kft\",\"m\")" (err :na)
    "=CONVERT(1,\"M\",\"m\")" (err :na)
    "=CONVERT(\"x\",\"m\",\"m\")" (err :value)))

(deftest complex-test
  (are [f x] (= x (calc f))
    "=COMPLEX(3,4)" "3+4i"
    "=COMPLEX(3,4,\"j\")" "3+4j"
    "=COMPLEX(0,1)" "i"
    "=COMPLEX(0,-1)" "-i"
    "=COMPLEX(1,-1)" "1-i"
    "=COMPLEX(2,0)" "2"
    "=COMPLEX(0,0)" "0"
    "=COMPLEX(0,2.5)" "2.5i"
    "=COMPLEX(3,4,\"k\")" (err :value)
    "=COMPLEX(\"a\",4)" (err :value)
    "=IMREAL(\"6-9i\")" 6.0
    "=IMAGINARY(\"3+4i\")" 4.0
    "=IMAGINARY(\"-i\")" -1.0
    "=IMAGINARY(\"4j\")" 4.0
    "=IMAGINARY(5)" 0.0
    "=IMREAL(\"1.5e2+3i\")" 150.0
    "=IMREAL(\"x\")" (err :num)
    "=IMREAL(\"3+4\")" (err :num)
    "=IMABS(\"5+12i\")" 13.0
    "=IMABS(A1)" 5.0
    "=IMCONJUGATE(\"3+4i\")" "3-4i"
    "=IMCONJUGATE(\"-2j\")" "2j"
    "=IMSUM(\"3+4i\",\"5-3i\")" "8+i"
    "=IMSUM(A1:B1)" "4+2i"
    "=IMSUM(A1:C1)" (err :value)
    "=IMSUB(\"13+4i\",\"5+3i\")" "8+i"
    "=IMPRODUCT(\"3+4i\",\"5-3i\")" "27+11i"
    "=IMPRODUCT(\"1+2i\",30)" "30+60i"
    "=IMDIV(\"-238+240i\",\"10+24i\")" "5+12i"
    "=IMDIV(\"1+i\",0)" (err :num)
    "=IMARGUMENT(\"i\")" (/ Math/PI 2)
    "=IMARGUMENT(0)" (err :div0)
    "=IMSQRT(\"-4\")" "1.22464679914735E-16+2i"
    "=IMPOWER(\"2+3i\",2)" "-5+12i"
    "=IMLN(0)" (err :num))
  (is (approx= 0.92729521800161 (calc "=IMARGUMENT(\"3+4i\")")))
  (is (= "1.6094379124341+0.927295218001612i" (calc "=IMLN(\"3+4i\")")))
  (is (= "-1.13120438375681+2.47172667200482i" (calc "=IMEXP(\"1+2i\")"))))
