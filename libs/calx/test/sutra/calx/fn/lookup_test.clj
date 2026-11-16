(ns sutra.calx.fn.lookup-test
  (:require [clojure.test :refer [deftest is testing are]]
            [sutra.calx.core :as c]
            [sutra.calx.value :as v]
            [sutra.calx.fn.lookup :as lookup]
            [sutra.calx.fn.logical :as logical]))

(def fns
  (merge logical/functions lookup/functions
         {"SUM" {:arity [1 nil] :args :refs
                 :impl (fn [ctx & xs]
                         (reduce + 0.0
                                 (filter number?
                                         (mapcat #(flatten (sutra.calx.eval/deref-value ctx %))
                                                 xs))))}}))

(def wb
  (c/workbook
   {"Sheet1" [["Name" "Qty" "Price"]
              ["apple" 3 1.5]
              ["pear" 5 2]
              ["banana" 1 0.5]
              ["Cherry" 7 4]]
    "Data" [[10 20] [30 40]]
    "My Sheet" [["x"]]}
   {:functions fns}))

(defn calc [f] (c/calculate wb f))
(def NA (v/err :na))
(def REF (v/err :ref))
(def VALUE (v/err :value))
(def CALC (v/err :calc))

(deftest vlookup-hlookup
  (are [f x] (= x (calc f))
    "=VLOOKUP(\"pear\",A2:C5,2,FALSE)" 5.0
    "=VLOOKUP(\"PEAR\",A2:C5,3,FALSE)" 2.0
    "=VLOOKUP(\"b*\",A2:C5,2,FALSE)" 1.0
    "=VLOOKUP(\"?ear\",A2:C5,2,FALSE)" 5.0
    "=VLOOKUP(\"kiwi\",A2:C5,2,FALSE)" NA
    "=VLOOKUP(\"pear\",A2:C5,4,FALSE)" REF
    "=VLOOKUP(\"pear\",A2:C5,0,FALSE)" VALUE
    "=VLOOKUP(5,{1,\"a\";3,\"b\";7,\"c\"},2)" "b"
    "=VLOOKUP(0,{1,\"a\";3,\"b\"},2,TRUE)" NA
    "=VLOOKUP(99,{1,\"a\";3,\"b\"},2)" "b"
    "=HLOOKUP(\"Qty\",A1:C5,3,FALSE)" 5.0
    "=HLOOKUP(2,{1,2,3;\"a\",\"b\",\"c\"},2)" "b"
    "=VLOOKUP(\"pear\",A:C,2,FALSE)" 5.0)
  (is (= [[3.0 1.0]] (calc "=VLOOKUP({\"apple\",\"banana\"},A2:C5,2,FALSE)"))))

(deftest lookup-and-match
  (are [f x] (= x (calc f))
    "=LOOKUP(4,{1,3,5},{\"a\",\"b\",\"c\"})" "b"
    "=LOOKUP(0,{1,3,5},{\"a\",\"b\",\"c\"})" NA
    "=LOOKUP(6,{1,2;5,6})" 6.0
    "=LOOKUP(2,{1,2,3;10,20,30})" 20.0
    "=MATCH(5,{1,3,5,7},0)" 3.0
    "=MATCH(6,{1,3,5,7})" 3.0
    "=MATCH(2,{1,2,2,3},1)" 3.0
    "=MATCH(6,{9,7,5,3},-1)" 2.0
    "=MATCH(\"c*\",A1:A5,0)" 5.0
    "=MATCH(\"x\",A1:A5,0)" NA
    "=MATCH(1,{1,2;3,4},0)" NA
    "=MATCH(5,{1,\"a\",3,7})" 3.0))

(deftest xlookup-xmatch
  (are [f x] (= x (calc f))
    "=XLOOKUP(\"pear\",A2:A5,B2:B5)" 5.0
    "=XLOOKUP(\"kiwi\",A2:A5,B2:B5)" NA
    "=XLOOKUP(\"kiwi\",A2:A5,B2:B5,\"none\")" "none"
    "=XLOOKUP(4,B2:B5,A2:A5,,-1)" "apple"
    "=XLOOKUP(4,B2:B5,A2:A5,,1)" "pear"
    "=XLOOKUP(\"ch*\",A2:A5,B2:B5,,2)" 7.0
    "=XLOOKUP(\"ch*\",A2:A5,B2:B5)" NA
    "=XLOOKUP(2,{1,2,2,3},{\"a\",\"b\",\"c\",\"d\"},,0,-1)" "c"
    "=XLOOKUP(5,{1,3,5,7},{\"a\",\"b\",\"c\",\"d\"},,0,2)" "c"
    "=XLOOKUP(6,{1,3,5,7},{\"a\",\"b\",\"c\",\"d\"},,-1,2)" "c"
    "=XLOOKUP(6,{1,3,5,7},{\"a\",\"b\",\"c\",\"d\"},,1,2)" "d"
    "=XLOOKUP(6,{7,5,3,1},{\"a\",\"b\",\"c\",\"d\"},,-1,-2)" "b"
    "=XLOOKUP(\"Qty\",A1:C1,A3:C3)" 5.0
    "=XLOOKUP(1,A2:A5,B2:C4)" VALUE
    "=XLOOKUP(1,{1,2},{1,2},,5)" VALUE
    "=SUM(XLOOKUP(\"pear\",A2:A5,B2:B5):B5)" 13.0
    "=XMATCH(5,{1,3,5,7})" 3.0
    "=XMATCH(4,{1,3,5,7},1)" 3.0
    "=XMATCH(4,{1,3,5,7},-1)" 2.0
    "=XMATCH(\"b?n*\",A1:A5,2)" 4.0
    "=XMATCH(3,{3,1,3},0,-1)" 3.0)
  (testing "whole row returned for 2D return array"
    (is (= [[5.0 2.0]] (calc "=XLOOKUP(\"pear\",A2:A5,B2:C5)")))
    (is (= [[3.0] [5.0] [1.0] [7.0]] (calc "=XLOOKUP(\"Qty\",A1:C1,A2:C5)"))))
  (is (= [[5.0 NA]] (calc "=XLOOKUP({\"pear\",\"x\"},A2:A5,B2:B5)"))))

(deftest index-fn
  (are [f x] (= x (calc f))
    "=INDEX(A2:C5,2,3)" 2.0
    "=INDEX(A2:C5,5,1)" REF
    "=INDEX(A2:C5,1,4)" REF
    "=INDEX(A2:C5,-1,1)" VALUE
    "=INDEX({1,2,3},2)" 2.0
    "=INDEX({1;2;3},3)" 3.0
    "=INDEX({1,2;3,4},2,1)" 3.0
    "=INDEX(B:B,3)" 5.0
    "=SUM(B2:INDEX(B:B,4))" 9.0
    "=ROW(INDEX(A1:C5,3,2))" 3.0
    "=COLUMN(INDEX(A1:C5,3,2))" 2.0
    "=SUM(INDEX(A2:C5,0,2))" 16.0
    "=SUM(INDEX(A2:C5,2,0))" 7.0
    "=INDEX(A2:C5,1,1,2)" REF)
  (is (= [[3.0] [5.0] [1.0] [7.0]] (calc "=INDEX(A2:C5,0,2)")))
  (is (= [["pear" 5.0 2.0]] (calc "=INDEX(A2:C5,2)")))
  (is (= [[1.0 3.0]] (calc "=INDEX({1,2;3,4},{1,2},1)"))))

(deftest offset-indirect
  (are [f x] (= x (calc f))
    "=OFFSET(A1,2,1)" 5.0
    "=SUM(OFFSET(A1,1,1,2,2))" 11.5
    "=ROWS(OFFSET(A1,0,0,3,2))" 3.0
    "=OFFSET(A1,-1,0)" REF
    "=OFFSET(A1,0,0,0,1)" REF
    "=SUM(OFFSET(B5,0,0,-2))" 8.0
    "=OFFSET(1,1,1)" VALUE
    "=INDIRECT(\"B3\")" 5.0
    "=SUM(INDIRECT(\"B2:B5\"))" 16.0
    "=INDIRECT(\"Data!B2\")" 40.0
    "=INDIRECT(\"'My Sheet'!A1\")" "x"
    "=INDIRECT(\"Nope!A1\")" REF
    "=INDIRECT(\"foo bar\")" REF
    "=INDIRECT(\"R3C2\",FALSE)" 5.0
    "=INDIRECT(\"R[2]C[1]\",FALSE)" 5.0
    "=SUM(INDIRECT(\"R2C2:R5C2\",FALSE))" 16.0
    "=SUM(INDIRECT(\"B:B\"))" 16.0
    "=ROW(INDIRECT(\"C7\"))" 7.0)
  (is (= [[3.0 1.5] [5.0 2.0]] (calc "=OFFSET(A1,1,1,2,2)")))
  (testing "stored OFFSET formula spills"
    (is (= [[3.0 1.5] [5.0 2.0]]
           (-> wb (c/set-cell "F1" "=OFFSET(A1,1,1,2,2)") (c/range-values "F1:G2")))))
  (testing "INDIRECT of a defined name"
    (let [w (c/define-name wb "Prices" "=Sheet1!$C$2:$C$5")]
      (is (= 8.0 (c/calculate w "=SUM(INDIRECT(\"Prices\"))"))))))

(deftest row-column-address
  (are [f x] (= x (calc f))
    "=ROW()" 1.0
    "=COLUMN()" 1.0
    "=ROW(C7)" 7.0
    "=COLUMN(C7)" 3.0
    "=ROWS(A1:C5)" 5.0
    "=COLUMNS(A1:C5)" 3.0
    "=ROWS(A:A)" 1048576.0
    "=COLUMNS(1:1)" 16384.0
    "=ROWS({1,2;3,4;5,6})" 3.0
    "=COLUMNS({1,2;3,4;5,6})" 2.0
    "=ROWS(5)" 1.0
    "=AREAS(A1:B2)" 1.0
    "=ROW({1,2})" VALUE
    "=ADDRESS(2,3)" "$C$2"
    "=ADDRESS(2,3,2)" "C$2"
    "=ADDRESS(2,3,3)" "$C2"
    "=ADDRESS(2,3,4)" "C2"
    "=ADDRESS(2,28,1,FALSE)" "R2C28"
    "=ADDRESS(2,3,4,FALSE)" "R[2]C[3]"
    "=ADDRESS(1,1,1,TRUE,\"Data\")" "Data!$A$1"
    "=ADDRESS(1,1,1,TRUE,\"My Sheet\")" "'My Sheet'!$A$1"
    "=ADDRESS(0,1)" VALUE
    "=ADDRESS(1,1,5)" VALUE)
  (is (= [[1.0] [2.0] [3.0]] (calc "=ROW(A1:A3)")))
  (is (= [[2.0 3.0 4.0]] (calc "=COLUMN(B1:D1)")))
  (is (= [[1.0] [2.0] [3.0] [4.0] [5.0]] (calc "=ROW(A:A)")))
  (is (= 4.0 (-> wb (c/set-cell "E4" "=ROW()") (c/value "E4"))))
  (is (= 5.0 (-> wb (c/set-cell "E4" "=COLUMN()") (c/value "E4")))))

(deftest transpose-filter
  (is (= [[1.0 3.0] [2.0 4.0]] (calc "=TRANSPOSE({1,2;3,4})")))
  (is (= [["pear" 5.0] ["Cherry" 7.0]] (calc "=FILTER(A2:B5,B2:B5>4)")))
  (is (= [["apple" 3.0]] (calc "=FILTER(A2:B5,A2:A5=\"apple\")")))
  (is (= CALC (calc "=FILTER(A2:B5,B2:B5>100)")))
  (is (= "none" (calc "=FILTER(A2:B5,B2:B5>100,\"none\")")))
  (is (= [[1.0 3.0]] (calc "=FILTER({1,2,3},{TRUE,FALSE,TRUE})")))
  (is (= VALUE (calc "=FILTER(A2:B5,{TRUE,FALSE,TRUE})")))
  (is (= [["apple"] ["pear"] ["banana"] ["Cherry"]] (calc "=FILTER(A2:B5,{TRUE,FALSE})")))
  (is (= NA (calc "=FILTER({1;2},{TRUE;#N/A})")))
  (testing "stored spill"
    (is (= [["pear" 5.0] ["Cherry" 7.0]]
           (-> wb (c/set-cell "F1" "=FILTER(A2:B5,B2:B5>4)") (c/range-values "F1:G2"))))))

(deftest sort-sortby-unique
  (is (= [["Cherry" 7.0] ["pear" 5.0] ["apple" 3.0] ["banana" 1.0]] (calc "=SORT(A2:B5,2,-1)")))
  (is (= [["apple" 3.0] ["banana" 1.0] ["Cherry" 7.0] ["pear" 5.0]] (calc "=SORT(A2:B5)")))
  (is (= [[1.0 1.0] [1.0 2.0] [2.0 0.0]] (calc "=SORT({2,0;1,2;1,1},{1,2})")))
  (is (= [[1.0 2.0] [1.0 1.0] [2.0 0.0]] (calc "=SORT({2,0;1,2;1,1},{1,2},{1,-1})")))
  (is (= [[1.0 2.0 3.0]] (calc "=SORT({3,1,2},,,TRUE)")))
  (is (= VALUE (calc "=SORT(A2:B5,3)")))
  (is (= [["banana"] ["apple"] ["pear"] ["Cherry"]] (calc "=SORTBY(A2:A5,B2:B5)")))
  (is (= [["c"] ["b"] ["a"]] (calc "=SORTBY({\"a\";\"b\";\"c\"},{1;1;0},1,{1;2;3},-1)")))
  (is (= [["b" "a"]] (calc "=SORTBY({\"a\",\"b\"},{2,1})")))
  (is (= VALUE (calc "=SORTBY(A2:A5,{1;2})")))
  (is (= [[1.0] [2.0] [3.0]] (calc "=UNIQUE({1;2;1;3;2})")))
  (is (= [[3.0]] [[(calc "=UNIQUE({1;2;1;3;2},FALSE,TRUE)")]]))
  (is (= [["a"] ["b"]] (calc "=UNIQUE({\"a\";\"A\";\"b\"})")))
  (is (= [[1.0 2.0]] (calc "=UNIQUE({1,2,1,2},TRUE)")))
  (is (= [[1.0 2.0] [1.0 3.0]] (calc "=UNIQUE({1,2;1,3;1,2})")))
  (is (= CALC (calc "=UNIQUE({1;1},FALSE,TRUE)")))
  (is (= [["Cherry" 7.0] ["pear" 5.0]]
         (-> wb (c/set-cell "F1" "=SORT(A2:B5,2,-1)") (c/range-values "F1:G2")))))

(deftest sequence-randarray
  (is (= [[1.0 2.0 3.0] [4.0 5.0 6.0]] (calc "=SEQUENCE(2,3)")))
  (is (= [[10.0] [8.0] [6.0]] (calc "=SEQUENCE(3,1,10,-2)")))
  (is (= CALC (calc "=SEQUENCE(0)")))
  (is (= VALUE (calc "=SEQUENCE(-1)")))
  (let [a (calc "=RANDARRAY(3,4,5,10,TRUE)")]
    (is (= [3 4] [(count a) (count (first a))]))
    (is (every? #(and (<= 5.0 % 10.0) (== % (Math/floor %))) (flatten a))))
  (is (every? #(< 0.0 % 1.0) (flatten (calc "=RANDARRAY(2,2)"))))
  (is (= VALUE (calc "=RANDARRAY(2,2,5,1)"))))

(deftest stacking-and-slicing
  (is (= [[1.0 2.0] [3.0 NA] [4.0 5.0]] (calc "=VSTACK({1,2},3,{4,5})")))
  (is (= [[1.0 3.0 4.0] [2.0 NA 5.0]] (calc "=HSTACK({1;2},3,{4;5})")))
  (is (= [[1.0 2.0] [4.0 5.0]] (calc "=TAKE({1,2,3;4,5,6;7,8,9},2,2)")))
  (is (= [[9.0]] [[(calc "=TAKE({1,2,3;4,5,6;7,8,9},-1,-1)")]]))
  (is (= [[7.0 8.0 9.0]] (calc "=TAKE({1,2,3;4,5,6;7,8,9},-1)")))
  (is (= [[3.0] [6.0] [9.0]] (calc "=TAKE({1,2,3;4,5,6;7,8,9},,-1)")))
  (is (= CALC (calc "=TAKE({1,2},0)")))
  (is (= [[5.0 6.0] [8.0 9.0]] (calc "=DROP({1,2,3;4,5,6;7,8,9},1,1)")))
  (is (= [[1.0 2.0 3.0]] (calc "=DROP({1,2,3;4,5,6;7,8,9},-2)")))
  (is (= CALC (calc "=DROP({1,2},1)")))
  (is (= [[7.0 8.0 9.0] [1.0 2.0 3.0]] (calc "=CHOOSEROWS({1,2,3;4,5,6;7,8,9},3,1)")))
  (is (= [[7.0 8.0 9.0] [4.0 5.0 6.0]] (calc "=CHOOSEROWS({1,2,3;4,5,6;7,8,9},{-1,2})")))
  (is (= [[3.0 1.0] [6.0 4.0]] (calc "=CHOOSECOLS({1,2,3;4,5,6},-1,1)")))
  (is (= VALUE (calc "=CHOOSECOLS({1,2,3},4)")))
  (is (= VALUE (calc "=CHOOSEROWS({1,2,3},0)")))
  (is (= [[1.0] [2.0] [3.0] [4.0]] (calc "=TOCOL({1,2;3,4})")))
  (is (= [[1.0] [3.0] [2.0] [4.0]] (calc "=TOCOL({1,2;3,4},0,TRUE)")))
  (is (= [[1.0 2.0 4.0]] (calc "=TOROW({1,2;#N/A,4},2)")))
  (is (= [[1.0 2.0 3.0] [4.0 5.0 NA]] (calc "=WRAPROWS({1,2,3,4,5},3)")))
  (is (= [[1.0 3.0 5.0] [2.0 4.0 0.0]] (calc "=WRAPCOLS({1,2,3,4,5},2,0)")))
  (is (= v/ERR-NUM (calc "=WRAPROWS({1,2},0)")))
  (is (= [[1.0 2.0 "-"] [3.0 4.0 "-"] ["-" "-" "-"]] (calc "=EXPAND({1,2;3,4},3,3,\"-\")")))
  (is (= [[1.0 NA]] (calc "=EXPAND(1,,2)")))
  (is (= VALUE (calc "=EXPAND({1,2;3,4},1)"))))

(deftest trimrange
  (let [w (c/workbook {"S" [[nil nil nil] [nil 1 nil] [nil 2 3] [nil nil nil]]} {:functions fns})]
    (is (= [[1.0 0.0] [2.0 3.0]] (c/calculate w "=TRIMRANGE(A1:D5)")))
    (is (= 2.0 (c/calculate w "=ROWS(TRIMRANGE(A:C))")))
    (is (= [[0.0 0.0 0.0] [0.0 1.0 0.0] [0.0 2.0 3.0]] (c/calculate w "=TRIMRANGE(A1:D4,2,2)")))
    (is (= [3.0 3.0]
           (mapv #(c/calculate w (format "=%s(TRIMRANGE(A1:D4,1,1))" %)) ["ROWS" "COLUMNS"])))
    (is (= CALC (c/calculate w "=TRIMRANGE(F1:G2)")))
    (is (= 6.0 (c/calculate w "=SUM(TRIMRANGE(A:D))")))))

(deftest groupby-pivotby
  (let [w (c/workbook {"S" [["Region" "Item" "Sales"]
                            ["East" "a" 10]
                            ["West" "a" 5]
                            ["East" "b" 2]
                            ["West" "b" 1]
                            ["East" "a" 3]]}
                      {:functions fns})]
    (is (= [["East" 15.0] ["West" 6.0] ["Total" 21.0]]
           (c/calculate w "=GROUPBY(A2:A6,C2:C6,SUM)")))
    (is (= [["East" 15.0] ["West" 6.0]]
           (c/calculate w "=GROUPBY(A2:A6,C2:C6,LAMBDA(x,SUM(x)),0,0)")))
    (is (= [["Region" "Sales"] ["East" 15.0] ["West" 6.0] ["Total" 21.0]]
           (c/calculate w "=GROUPBY(A1:A6,C1:C6,SUM)")))
    (is (= [["West" 6.0] ["East" 15.0]]
           (c/calculate w "=GROUPBY(A2:A6,C2:C6,SUM,0,0,-1)")))
    (is (= [["East" "a" 13.0] ["East" "b" 2.0] ["West" "a" 5.0] ["West" "b" 1.0]]
           (c/calculate w "=GROUPBY(A2:B6,C2:C6,SUM,0,0)")))
    (is (= [[0.0 "a" "b" "Total"]
            ["East" 13.0 2.0 15.0]
            ["West" 5.0 1.0 6.0]
            ["Total" 18.0 3.0 21.0]]
           (c/calculate w "=PIVOTBY(A2:A6,B2:B6,C2:C6,SUM)")))
    (is (= [[0.0 "a"] ["East" 13.0] ["West" 5.0]]
           (c/calculate w "=PIVOTBY(A2:A6,B2:B6,C2:C6,SUM,0,0,1,0,1,B2:B6=\"a\")")))))
