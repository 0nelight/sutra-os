(ns sutra.calx.core-test
  (:require [clojure.test :refer [deftest is testing are]]
            [sutra.calx.core :as c]
            [sutra.calx.value :as v]
            [sutra.calx.parser :as p]
            [sutra.calx.workbook :as wb]))

(def E v/err)

(defn- wb1 [rows] (c/workbook {"Sheet1" rows}))

(deftest value-domain
  (are [x s] (= s (v/format-number x))
    1.0 "1" 0.1 "0.1" (+ 0.1 0.2) "0.3" -2.5 "-2.5" 1e21 "1E+21" 1.5e-10 "1.5E-10" 123456789012.0
    "123456789012")
  (is (= 0.12 (v/parse-number "12%")))
  (is (= 1234.5 (v/parse-number "1,234.5")))
  (is (= 45292.0 (v/parse-date-time "2024-01-01")))
  (is (= 60.0 (v/date->serial 1900 2 29)) "Excel's fake leap day")
  (is (= 61.0 (v/date->serial 1900 3 1)))
  (is (= 61.0 (v/date->serial 1900 2 30)))
  (is (= 60.0 (v/date->serial 1900 3 0)))
  (is (= 1.0 (v/date->serial 1900 1 1)))
  (is (= 45658.0 (v/date->serial 2024 13 1)) "month overflow")
  (is (= [1900 2 29] (v/serial->ymd 60)))
  (is (neg? (v/compare-values 5.0 "a")) "numbers sort before text")
  (is (neg? (v/compare-values "Z" true)) "text before booleans")
  (is (zero? (v/compare-values "abc" "ABC")) "case-insensitive"))

(deftest parser-roundtrip
  (doseq [f ["1+2*3" "-2^2" "(1+2)*3" "A1:B2" "$A$1+A$1+$A1" "Sheet2!A1" "'My Sheet'!A1:B3"
             "A:A" "1:3" "IF(A1,,\"x\"\"y\")" "{1,2;3,4}" "LAMBDA(x,x*2)(3)" "A1#" "10%" "1<>2"
             "A1:INDEX(B:B,3)" "#N/A" "\"a\"&\"b\"" "-(-1)" "2^3^2" "LET(x,1,x+1)"]]
    (is (= f (p/unparse (p/parse (str "=" f)))) f))
  (is (= "XLOOKUP(1,A:A,B:B)" (p/unparse (p/parse "=_xlfn.XLOOKUP(1,A:A,B:B)"))))
  (is (= {:error "Unexpected end of formula"} (p/try-parse "=SUM(1,")))
  (is (:error (p/try-parse "=1+)")))
  (testing "LOG10( is a function, TAX2024 a cell ref, ABCD1 a name"
    (is (= :call (:kind (p/parse "=LOG10(1)"))))
    (is (= :ref (:kind (p/parse "=TAX2024"))))
    (is (= :name (:kind (p/parse "=ABCD1"))))))

(deftest operator-semantics
  (are [f x] (= x (c/calculate (c/workbook) f))
    "=1+2*3" 7.0
    "=-2^2" 4.0
    "=2^3^2" 64.0
    "=10/4" 2.5
    "=1/0" (E :div0)
    "=\"3\"+4" 7.0
    "=\"a\"+1" (E :value)
    "=TRUE+TRUE" 2.0
    "=\"a\"&1&TRUE" "a1TRUE"
    "=\"abc\"=\"ABC\"" true
    "=1<\"a\"" true
    "=50%" 0.5
    "=0^0" (E :num)
    "=(-8)^(1/3)" (E :num)
    "=#N/A+1" (E :na)
    "=UNKNOWNFN(1)" (E :name)
    "=undefined_name" (E :name))
  (is (= [[2.0 4.0] [6.0 8.0]] (c/calculate (c/workbook) "={1,2;3,4}*2")))
  (is (= [[11.0 21.0] [12.0 22.0]] (c/calculate (c/workbook) "={1;2}+{10,20}"))
      "row/col broadcast"))

(deftest incremental-recalc
  (let [w (wb1 [[1 2 "=A1+B1"] ["=C1*2" nil nil]])]
    (is (= 3.0 (c/value w "C1")))
    (is (= 6.0 (c/value w "A2")))
    (let [w (c/set-cell w "A1" 10)]
      (is (= 12.0 (c/value w "C1")))
      (is (= 24.0 (c/value w "A2")))
      (is (= #{"Sheet1!A1" "Sheet1!C1" "Sheet1!A2"} (c/changes w))))))

(deftest blank-and-chain
  (let [w (wb1 [["=B1"]])]
    (is (= 0.0 (c/value w "A1")) "reference to blank is 0"))
  (let [n 3000
        w (c/set-cells (c/workbook)
                       (cons ["A1" 1]
                             (for [i (range 2 (inc n))] [(str "A" i) (str "=A" (dec i) "+1")])))]
    (is (= (double n) (c/value w (str "A" n))) "long chains don't blow the stack")
    (is (= (+ n 99.0) (c/value (c/set-cell w "A1" 100) (str "A" n))))))

(deftest cycles
  (let [w (c/set-cells (c/workbook) {"A1" "=B1+1" "B1" "=A1+1" "C1" "=A1*2" "D1" 5})]
    (is (= (E :cycle) (c/value w "A1")))
    (is (= (E :cycle) (c/value w "B1")))
    (is (= (E :cycle) (c/value w "C1")) "downstream of a cycle")
    (is (= 5.0 (c/value w "D1")))
    (let [w (c/set-cell w "B1" 1)]
      (is (= 2.0 (c/value w "A1")) "breaking the cycle recovers")
      (is (= 4.0 (c/value w "C1")))))
  (is (= (E :cycle) (c/value (c/set-cell (c/workbook) "A1" "=A1") "A1")) "self reference"))

(deftest whole-column-refs
  (let [w (-> (wb1 [[1] [2] [3]]) (c/set-cell "B1" "=IF(TRUE,A:A)"))]
    (is (= [[1.0] [2.0] [3.0]] (c/range-values w "B1:B3")))
    (is (= [[1.0] [2.0] [3.0] [4.0]] (c/range-values (c/set-cell w "A4" 4) "B1:B4"))
        "extends when column grows")))

(deftest spills
  (let [w (c/set-cell (wb1 [[2]]) "B1" "={1,2;3,4}*A1")]
    (is (= [[2.0 4.0] [6.0 8.0]] (c/range-values w "B1:C2")))
    (is (= [2 2] (c/spill-area w "B1")))
    (testing "dependents of spilled cells update"
      (let [w (-> w (c/set-cell "E1" "=C2+1") (c/set-cell "A1" 10))]
        (is (= 41.0 (c/value w "E1")))))
    (testing "spill reference"
      (is (= [[2.0 4.0] [6.0 8.0]] (c/calculate w "=B1#")))
      (is (= [[3.0 5.0] [7.0 9.0]] (c/calculate w "=B1#+1"))))
    (testing "blocking and unblocking"
      (let [blocked (c/set-cell w "C2" "x")]
        (is (= (E :spill) (c/value blocked "B1")))
        (is (nil? (c/value blocked "C1")))
        (is (= [[2.0 4.0] [6.0 8.0]] (c/range-values (c/set-cell blocked "C2" nil) "B1:C2")))))
    (testing "shrinking spill frees cells"
      (let [w (c/set-cell w "B1" "={1,2}")]
        (is (= [[1.0 2.0] [nil nil]] (c/range-values w "B1:C2")))))
    (testing "deleting the anchor clears the spill"
      (is (= [[nil nil] [nil nil]]
             (c/range-values (c/set-cell (c/set-cell w "Z9" 1) "B1" nil) "B1:C2"))))))

(deftest spill-into-spill
  (let [w (-> (c/workbook) (c/set-cell "A1" "={1;2;3}") (c/set-cell "B2" "={7,8}"))]
    (is (= 1.0 (c/value w "A1")))
    (is (= [[7.0 8.0]] (c/range-values w "B2:C2")))
    (let [w (c/set-cell w "A2" "={1,2}")]
      (is (= (E :spill) (c/value w "A1")) "A1 now blocked by content in A2"))))

(deftest lambdas
  (let [w (c/workbook)]
    (are [f x] (= x (c/calculate w f))
      "=LET(x,2,y,x*3,x+y)" 8.0
      "=LAMBDA(x,y,x*y)(3,4)" 12.0
      "=LET(sq,LAMBDA(n,n*n),sq(5))" 25.0
      "=REDUCE(0,{1,2,3},LAMBDA(a,b,a+b))" 6.0
      "=LAMBDA(x,IF(ISOMITTED(x),\"none\",x))()" "none"
      "=LAMBDA(x,x)" (E :calc))
    (is (= [[1.0 3.0 6.0]] (c/calculate w "=SCAN(0,{1,2,3},LAMBDA(a,b,a+b))")))
    (is (= [[2.0] [4.0]] (c/calculate w "=BYROW({1,1;2,2},LAMBDA(r,REDUCE(0,r,LAMBDA(a,b,a+b))))")))
    (is (= [[1.0 2.0] [2.0 4.0]] (c/calculate w "=MAKEARRAY(2,2,LAMBDA(r,c,r*c))"))))
  (testing "named lambda"
    (let [w (-> (c/workbook) (c/define-name "DOUBLE" "=LAMBDA(x,x*2)")
                (c/set-cell "A1" "=DOUBLE(21)"))]
      (is (= 42.0 (c/value w "A1"))))))

(deftest sheets
  (let [w (-> (c/workbook)
              (c/add-sheet "Data")
              (c/set-cell "Data!A1" 100)
              (c/set-cell "A1" "=Data!A1*2")
              (c/set-cell "A2" "=Later!A1"))]
    (is (= 200.0 (c/value w "A1")))
    (is (= (E :ref) (c/value w "A2")) "unknown sheet")
    (let [w (-> w (c/add-sheet "Later") (c/set-cell "Later!A1" 7))]
      (is (= 7.0 (c/value w "A2")) "binds when the sheet appears"))
    (let [w (c/rename-sheet w "Data" "My Data")]
      (is (= "='My Data'!A1*2" (c/formula w "A1")))
      (is (= 200.0 (c/value w "A1"))))
    (let [w (c/remove-sheet w "Data")]
      (is (= "=#REF!*2" (c/formula w "A1")))
      (is (= (E :ref) (c/value w "A1"))))
    (is (thrown? clojure.lang.ExceptionInfo (c/add-sheet w "data"))
        "names are case-insensitive unique")))

(deftest structural-edits
  (let [w (wb1 [[1 "=SUM2"] [2 nil] [3 nil] ["=A1+A3" "=$A$2*10"]])
        w (c/set-cell w "B1" "=IF(TRUE,A1:A3)")]
    (testing "insert rows shifts references, ranges expand"
      (let [w (c/insert-rows w "Sheet1" 2 2)]
        (is (= "=A1+A5" (c/formula w "A6")))
        (is (= "=$A$4*10" (c/formula w "B6")))
        (is (= "=IF(TRUE,A1:A5)" (c/formula w "B1")))
        (is (= 4.0 (c/value w "A6")))))
    (testing "delete rows: refs into the deleted area become #REF!, ranges shrink"
      (let [w (c/delete-rows w "Sheet1" 2)]
        (is (= "=A1+A2" (c/formula w "A3")))
        (is (= "=#REF!*10" (c/formula w "B3")))
        (is (= (E :ref) (c/value w "B3")))
        (is (= "=IF(TRUE,A1:A2)" (c/formula w "B1")))))
    (testing "columns"
      (let [w (c/insert-cols w "Sheet1" "A")]
        (is (= "=B1+B3" (c/formula w "B4")))
        (is (= 4.0 (c/value w "B4"))))
      (let [w (c/delete-cols w "Sheet1" "A")]
        (is (= "=#REF!*10" (c/formula w "A4")))))))

(deftest clipboard
  (let [w (wb1 [[1 2 "=A1+B1"] [10 20 nil]])]
    (testing "copy shifts relative refs"
      (let [w (c/copy-range w "C1" "C2")]
        (is (= "=A2+B2" (c/formula w "C2")))
        (is (= 30.0 (c/value w "C2")))))
    (testing "move: references to moved cells follow"
      (let [w (c/move-range w "A1:B1" "E5")]
        (is (= "=E5+F5" (c/formula w "C1")))
        (is (= 3.0 (c/value w "C1")))
        (is (nil? (c/value w "A1")))))
    (testing "move to another sheet"
      (let [w (-> w (c/add-sheet "S2") (c/move-range "C1" "S2!A1"))]
        (is (= "=Sheet1!A1+Sheet1!B1" (c/formula w "S2!A1")))
        (is (= 3.0 (c/value w "S2!A1")))))))

(deftest autofill-and-sort
  (let [w (-> (c/set-cells (c/workbook) {"A1" 1 "A2" 3 "B1" "=A1*2" "C1" "Item 1"})
              (c/fill "A1:A2" "A1:A5")
              (c/fill "B1:C1" "B1:C3"))]
    (is (= [[1.0] [3.0] [5.0] [7.0] [9.0]] (c/range-values w "A1:A5")))
    (is (= "=A3*2" (c/formula w "B3")))
    (is (= 10.0 (c/value w "B3")))
    (is (= "Item 3" (c/value w "C3"))))
  (let [w (-> (wb1 [["b" 2 "=B1*10"] ["c" 3 "=B2*10"] ["a" 1 "=B3*10"]])
              (c/sort-range "A1:C3"))]
    (is (= [["a" 1.0 10.0] ["b" 2.0 20.0] ["c" 3.0 30.0]] (c/range-values w "A1:C3")))
    (is (= "=B1*10" (c/formula w "C1")) "relative refs follow sorted rows")
    (is (= [["c" 3.0 30.0] ["b" 2.0 20.0] ["a" 1.0 10.0]]
           (c/range-values (c/sort-range w "A1:C3" [{:col "B" :ascending? false}]) "A1:C3")))))

(deftest named-expressions
  (let [w (-> (wb1 [[100]])
              (c/define-name "TaxRate" "=0.19")
              (c/define-name "Base" "=Sheet1!$A$1")
              (c/set-cell "B1" "=Base*TaxRate"))]
    (is (= 19.0 (c/value w "B1")))
    (is (= 38.0 (c/value (c/set-cell w "A1" 200) "B1")) "names track their refs")
    (is (= 9.5 (c/value (c/define-name w "TaxRate" "=0.095") "B1")) "redefining recalculates")
    (is (= "=Sheet1!$A$1" (get-in (c/names w) ["Base" :formula])))
    (let [w (c/insert-rows w "Sheet1" 1)]
      (is (= "=Sheet1!$A$2" (get-in (c/names w) ["Base" :formula])) "names follow structural edits")
      (is (= 19.0 (c/value w "B2"))))
    (is (= (E :name) (c/value (c/remove-name w "TaxRate") "B1")))
    (testing "sheet-scoped names shadow global ones"
      (let [w (-> w (c/add-sheet "S2") (c/define-name "TaxRate" "=0.5" "S2")
                  (c/set-cell "S2!A1" "=TaxRate"))]
        (is (= 0.5 (c/value w "S2!A1")))
        (is (= 19.0 (c/value w "B1")))))
    (is (thrown? clojure.lang.ExceptionInfo (c/define-name w "A1" "=1"))
        "cell-like names are rejected")))

(deftest custom-functions
  (let [w (-> (wb1 [["=TWICE(21)"]])
              (c/register-function "TWICE" (fn [x] (* 2 (v/to-number x)))))]
    (is (= 42.0 (c/value w "A1")) "registering resolves #NAME? formulas"))
  (let [w (-> (wb1 [[1 2 3]])
              (c/register-function "MYSUM"
                                   {:arity [1 nil] :args :refs
                                    :impl (fn [ctx & refs]
                                            (reduce +
                                                    (keep #(when (number? %) %)
                                                          (mapcat
                                                           #(apply concat
                                                                   (sutra.calx.eval/ref-values
                                                                    ctx %)) refs))))})
              (c/set-cell "D1" "=MYSUM(A1:C1)"))]
    (is (= 6.0 (c/value w "D1")))
    (is (= 15.0 (c/value (c/set-cell w "A1" 10) "D1"))
        "custom range functions take part in dependency tracking")))

(deftest undo-redo-and-log
  (let [w0 (wb1 [[1]])
        w1 (c/set-cell w0 "A2" "=A1*2")
        w2 (c/transact w1 [[:set-cell [1 0 0] 5] [:set-cell [1 2 0] "=A2+1"]])]
    (is (= 11.0 (c/value w2 "A3")))
    (let [u (c/undo w2)]
      (is (= 2.0 (c/value u "A2")))
      (is (nil? (c/value u "A3")) "a batch is one undo step")
      (is (= 11.0 (c/value (c/redo u) "A3")))
      (is (= 1.0 (c/value (c/undo u) "A1")))
      (is (not (c/can-undo? (c/undo u))))
      (is (= 2.0 (c/value (c/set-cell u "A1" 1) "A2")))
      (is (not (c/can-redo? (c/set-cell u "A1" 1))) "new edits clear redo"))
    (testing "replaying the log reproduces the state"
      (let [w3 (-> w2 c/undo c/redo (c/set-cell "B1" "=A3*2"))
            replayed (c/replay w0 (c/edit-log w3))]
        (is (= (c/sheet-values w3 "Sheet1") (c/sheet-values replayed "Sheet1")))))))

(deftest invalid-input
  (let [w (c/set-cell (c/workbook) "A1" "=SUM(1,")]
    (is (= (E :error) (c/value w "A1")))
    (is (= "=SUM(1," (c/formula w "A1"))))
  (let [w (c/set-cells (c/workbook)
                       {"A1" "'=not a formula" "A2" "'123" "A3" "12%" "A4" "true"
                        "A5" "2024-03-01"})]
    (is (= "=not a formula" (c/value w "A1")))
    (is (= "123" (c/value w "A2")))
    (is (= 0.12 (c/value w "A3")))
    (is (= true (c/value w "A4")))
    (is (= 45352.0 (c/value w "A5")))))

(deftest purity
  (testing "operations never mutate their input"
    (let [w (wb1 [[1 "=A1+1"]])
          snapshot (c/sheet-values w "Sheet1")]
      (-> w (c/set-cell "A1" 99) (c/insert-rows "Sheet1" 1) (c/add-sheet "X"))
      (is (= snapshot (c/sheet-values w "Sheet1"))))))

(deftest move-rows-and-cols
  (let [w (wb1 [["a" 1] ["b" 2] ["c" 3] [nil "=B1*10"]])]
    (let [m (c/move-rows w "Sheet1" 1 1 4)]
      (is (= [["b" 2.0] ["c" 3.0] ["a" 1.0]] (c/range-values m "A1:B3")))
      (is (= "=B3*10" (c/formula m "B4")) "reference follows the moved row")
      (is (= 10.0 (c/value m "B4"))))
    (let [m (c/move-rows w "Sheet1" 3 1 1)]
      (is (= [["c" 3.0] ["a" 1.0] ["b" 2.0]] (c/range-values m "A1:B3")))
      (is (= "=B2*10" (c/formula m "B4"))))
    (let [m (c/move-cols w "Sheet1" "B" 1 "A")]
      (is (= [[1.0 "a"]] (c/range-values m "A1:B1")))
      (is (= "=A1*10" (c/formula m "A4"))))))

(deftest text-format-keeps-what-is-typed
  (let [w (-> (c/workbook (array-map "T" []) {:config {:locale :de}})
              (c/set-style "A1" {:format "@"})
              (c/set-cells {"A1" "00123" "B1" "00123"}))]
    (is (= "00123" (c/value w "A1")) "a text cell keeps leading zeros")
    (is (= "00123" (c/input w "A1")) "and shows them as typed, without an apostrophe")
    (is (= 123.0 (c/value w "B1")) "a General cell reads a number")
    (is (= "=A1" (c/value (c/set-cell w "A1" "=A1") "A1")) "even a formula stays text")
    (is (nil? (c/value (c/set-cell w "A1" "") "A1")) "an empty input still clears")
    (is (= {:format "@"} (c/style (c/set-cell w "A1" "") "A1")) "the format stays")))
