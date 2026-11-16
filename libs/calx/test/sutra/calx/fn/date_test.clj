(ns sutra.calx.fn.date-test
  (:require [clojure.test :refer [deftest is are testing]]
            [sutra.calx.core :as c]
            [sutra.calx.fn.date :as date]
            [sutra.calx.test-helpers :refer [approx= err]]))

(def wb (c/workbook {"Sheet1" [[45000 "2024-01-15"]
                               [(+ 39778 0.0) nil]
                               [39786 nil]
                               [39834 nil]]}
                    {:functions date/functions :config {:now 45292.5}}))

(defn- calc [f] (c/calculate wb f))

(deftest construction
  (are [f expected] (= expected (calc f))
    "=DATE(2024,1,15)"   45306.0
    "=DATE(2008,14,2)"   39846.0
    "=DATE(2008,1,-15)"  39432.0
    "=DATE(108,1,2)"     39449.0
    "=DATE(1900,2,29)"   60.0
    "=DATE(1900,3,1)"    61.0
    "=DATE(-1,1,1)"      (err :num)
    "=DATE(10000,1,1)"   (err :num)
    "=DATEVALUE(\"2008-08-22\")"   39682.0
    "=DATEVALUE(\"22-Aug-2008\")"  39682.0
    "=DATEVALUE(\"August 22, 2008\")" 39682.0
    "=DATEVALUE(\"8/22/2008 10:30\")" 39682.0
    "=DATEVALUE(\"nope\")"  (err :value)
    "=DATEVALUE(39682)"     (err :value)
    "=TIME(12,0,0)"   0.5
    "=TIME(27,0,0)"   0.125
    "=TIME(0,-1,0)"   (err :num)
    "=TIMEVALUE(\"6:00 PM\")" 0.75
    "=TIMEVALUE(\"2008-08-22 06:00\")" 0.25
    "=TODAY()" 45292.0
    "=NOW()"   45292.5))

(deftest components
  (are [f expected] (= expected (calc f))
    "=YEAR(A1)"  2023.0
    "=MONTH(A1)" 3.0
    "=DAY(A1)"   15.0
    "=YEAR(B1)"  2024.0
    "=DAY(\"15-Jan-2024\")" 15.0
    "=DAY(60)"   29.0
    "=MONTH(60)" 2.0
    "=DAY(0)"    0.0
    "=YEAR(-1)"  (err :num)
    "=YEAR(\"x\")" (err :value)
    "=HOUR(0.7591435185)"   18.0
    "=MINUTE(\"12:45:00 PM\")" 45.0
    "=SECOND(TIME(1,2,59.6))" 59.0
    "=SECOND(0.99999999)"  0.0
    "=YEAR({45000,45306})" [[2023.0 2024.0]]))

(deftest weekdays-and-weeks
  (are [f expected] (= expected (calc f))
    "=WEEKDAY(DATE(2008,2,14))"    5.0
    "=WEEKDAY(DATE(2008,2,14),2)"  4.0
    "=WEEKDAY(DATE(2008,2,14),3)"  3.0
    "=WEEKDAY(DATE(2008,2,14),11)" 4.0
    "=WEEKDAY(DATE(2008,2,14),14)" 1.0
    "=WEEKDAY(DATE(2008,2,14),17)" 5.0
    "=WEEKDAY(1)"                  1.0
    "=WEEKDAY(A1,4)"               (err :num)
    "=WEEKNUM(DATE(2012,3,9))"     10.0
    "=WEEKNUM(DATE(2012,3,9),2)"   11.0
    "=WEEKNUM(DATE(2012,3,9),15)"  11.0
    "=WEEKNUM(DATE(2021,1,1),21)"  53.0
    "=WEEKNUM(A1,3)"               (err :num)
    "=ISOWEEKNUM(DATE(2012,3,9))"  10.0
    "=ISOWEEKNUM(DATE(2016,1,1))"  53.0))

(deftest month-arithmetic
  (are [f expected] (= expected (calc f))
    "=EDATE(DATE(2011,1,15),1)"   40589.0
    "=EDATE(DATE(2011,1,15),-1)"  40527.0
    "=EDATE(DATE(2011,1,31),1)"   40602.0
    "=EDATE(\"2024-01-31\",1)"    45351.0
    "=EOMONTH(DATE(2011,1,1),1)"  40602.0
    "=EOMONTH(DATE(2011,1,1),-3)" 40482.0
    "=EOMONTH(-1,0)"              (err :num)))

(deftest differences
  (are [f expected] (= expected (calc f))
    "=DATEDIF(DATE(2001,1,1),DATE(2003,1,1),\"Y\")"   2.0
    "=DATEDIF(DATE(2001,6,1),DATE(2002,8,15),\"D\")"  440.0
    "=DATEDIF(DATE(2001,6,1),DATE(2002,8,15),\"YD\")" 75.0
    "=DATEDIF(DATE(2001,6,1),DATE(2002,8,15),\"m\")"  14.0
    "=DATEDIF(DATE(2001,6,1),DATE(2002,8,15),\"YM\")" 2.0
    "=DATEDIF(DATE(2001,6,1),DATE(2002,8,15),\"MD\")" 14.0
    "=DATEDIF(DATE(2020,2,15),DATE(2024,1,10),\"MD\")" 26.0
    "=DATEDIF(DATE(2003,1,1),DATE(2001,1,1),\"Y\")"   (err :num)
    "=DATEDIF(DATE(2001,1,1),DATE(2003,1,1),\"W\")"   (err :num)
    "=DAYS(\"2021-03-15\",\"2021-02-01\")" 42.0
    "=DAYS(DATE(2021,1,1),DATE(2021,12,31))" -364.0
    "=DAYS360(DATE(2011,1,30),DATE(2011,12,31))" 330.0
    "=DAYS360(DATE(2011,1,31),DATE(2011,12,31),TRUE)" 330.0
    "=DAYS360(DATE(2011,1,1),DATE(2011,1,31))" 30.0
    "=DAYS360(DATE(2011,1,1),DATE(2011,1,31),TRUE)" 29.0
    "=DAYS360(DATE(2011,2,28),DATE(2011,3,31))" 30.0)
  (testing "YEARFRAC bases"
    (are [f expected] (approx= expected (calc f) 1e-7)
      "=YEARFRAC(DATE(2012,1,1),DATE(2012,7,30))"   0.58055556
      "=YEARFRAC(DATE(2012,1,1),DATE(2012,7,30),1)" 0.57650273
      "=YEARFRAC(DATE(2012,1,1),DATE(2012,7,30),2)" 0.58611111
      "=YEARFRAC(DATE(2012,1,1),DATE(2012,7,30),3)" 0.57808219
      "=YEARFRAC(DATE(2012,1,1),DATE(2012,7,30),4)" 0.58055556
      "=YEARFRAC(DATE(2012,7,30),DATE(2012,1,1),1)" 0.57650273
      "=YEARFRAC(DATE(2010,3,5),DATE(2013,9,1),1)"  3.49349760
      "=YEARFRAC(DATE(2011,2,28),DATE(2012,2,29))"  1.0)
    (is (= (err :num) (calc "=YEARFRAC(DATE(2012,1,1),DATE(2012,7,30),5)")))))

(deftest working-days
  (are [f expected] (= expected (calc f))
    "=NETWORKDAYS(DATE(2012,10,1),DATE(2013,3,1))"                    110.0
    "=NETWORKDAYS(DATE(2012,10,1),DATE(2013,3,1),DATE(2012,11,22))"   109.0
    "=NETWORKDAYS(DATE(2013,3,1),DATE(2012,10,1))"                    -110.0
    "=NETWORKDAYS(DATE(2008,10,1),DATE(2009,3,1),A2:A4)"              105.0
    "=NETWORKDAYS.INTL(DATE(2006,1,1),DATE(2006,1,31))"               22.0
    "=NETWORKDAYS.INTL(DATE(2006,2,28),DATE(2006,1,31))"              -21.0
    "=NETWORKDAYS.INTL(DATE(2006,1,1),DATE(2006,2,1),7,{\"2006/1/2\",\"2006/1/16\"})" 22.0
    "=NETWORKDAYS.INTL(DATE(2006,1,1),DATE(2006,2,1),\"0010001\",{\"2006/1/2\",\"2006/1/16\"})" 20.0
    "=NETWORKDAYS.INTL(DATE(2006,1,1),DATE(2006,1,31),11)"            26.0
    "=NETWORKDAYS.INTL(DATE(2006,1,1),DATE(2006,1,31),8)"             (err :num)
    "=NETWORKDAYS.INTL(DATE(2006,1,1),DATE(2006,1,31),\"1111111\")"   (err :value)
    "=WORKDAY(DATE(2008,10,1),151)"         39933.0
    "=WORKDAY(DATE(2008,10,1),151,A2:A4)"   39938.0
    "=WORKDAY(DATE(2024,1,8),-5)"           45292.0
    "=WORKDAY(DATE(2024,1,6),0)"            45297.0
    "=WORKDAY.INTL(DATE(2012,1,1),30,0)"    (err :num)
    "=WORKDAY.INTL(DATE(2012,1,1),90,11)"   41013.0
    "=WORKDAY.INTL(DATE(2012,1,1),30,17)"   40944.0
    "=WORKDAY.INTL(DATE(2012,1,1),30,\"0000011\")" (calc "=WORKDAY(DATE(2012,1,1),30)")))
