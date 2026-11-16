(ns sutra.calx.integration-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.calx.core :as c]
            [sutra.calx.io.xlsx :as xlsx]
            [sutra.calx.io.edn :as edn-io]
            [sutra.calx.test-helpers :refer [approx=]]))

(defn- invoices []
  (c/workbook
   {"Orders" [["Date" "Customer" "Product" "Qty" "Price" "Net"]
              ["2024-01-15" "Acme" "Widget" 10 2.5 "=D2*E2"]
              ["2024-02-03" "Globex" "Gadget" 3 12 "=D3*E3"]
              ["2024-02-20" "Acme" "Gadget" 1 12 "=D4*E4"]
              ["2024-03-11" "Initech" "Widget" 7 2.5 "=D5*E5"]]
    "Report" [["Customer" "Revenue" "Orders" "Gross"]
              ["=SORT(UNIQUE(Orders!B2:B5))" "=SUMIFS(Orders!F:F,Orders!B:B,A2#)"
               "=COUNTIF(Orders!B:B,A2#)" "=ROUND(B2#*(1+VAT),2)"]]}
   {:names {"VAT" "=0.19"}}))

(deftest report-with-dynamic-arrays
  (let [w (invoices)]
    (is (= [["Acme" 37.0 2.0 44.03] ["Globex" 36.0 1.0 42.84] ["Initech" 17.5 1.0 20.83]]
           (c/range-values w "Report!A2:D4")))
    (testing "a new order inserted inside the range flows through UNIQUE/SORT/SUMIFS spills"
      (let [w (-> w
                  (c/insert-rows "Orders" 5)
                  (c/set-range "Orders!A5" [["2024-03-30" "Bluth" "Widget" 4 2.5 "=D5*E5"]]))]
        (is (= "=SORT(UNIQUE(Orders!B2:B6))" (c/formula w "Report!A2")))
        (is (= ["Acme" "Bluth" "Globex" "Initech"] (map first (c/range-values w "Report!A2:A5"))))
        (is (= 10.0 (c/value w "Report!B3")))))
    (testing "VAT change recalculates gross"
      (is (= 40.7 (c/value (c/define-name w "VAT" "=0.1") "Report!D2"))))))

(deftest cross-library-formulas
  (let [w (invoices)
        calc #(c/calculate w % "Orders")]
    (is (= "Q1 2024" (calc "=\"Q\"&ROUNDUP(MONTH(A2)/3,0)&\" \"&YEAR(A2)")))
    (is (= "Gadget" (calc "=XLOOKUP(MAX(F2:F5),F2:F5,C2:C5)")) "max net is Globex's gadget order")
    (is (= "15.01.2024" (calc "=TEXT(A2,\"dd.mm.yyyy\")")))
    (is (= 22.625 (calc "=AVERAGE(F2:F5)")))
    (is (= "Acme, Globex, Initech" (calc "=TEXTJOIN(\", \",TRUE,SORT(UNIQUE(B2:B5)))")))
    (is (= [["Acme" 2.0] ["Globex" 1.0] ["Initech" 1.0]]
           (calc "=LET(u,SORT(UNIQUE(B2:B5)),HSTACK(u,COUNTIF(B2:B5,u)))")))
    (is (approx= -95.45896 (calc "=PMT(5%/12,10*12,9000)") 1e-6))
    (is (= 41.0 (calc "=NETWORKDAYS(A2,A5)")))
    (is (= [["ACME"] ["GLOBEX"] ["INITECH"]]
           (calc "=MAP(FILTER(B2:B5,D2:D5>2),LAMBDA(x,UPPER(x)))")))
    (is (= "1E+03" (calc "=TEXT(1000,\"0E+00\")")))
    (is (approx= 0.9750021 (calc "=NORM.S.DIST(1.96,TRUE)") 1e-6))))

(deftest roundtrips-keep-everything-working
  (let [w (invoices)]
    (doseq [w2 [(xlsx/read-xlsx (xlsx/xlsx-bytes w))
                (edn-io/read-string* (edn-io/write-string w))]]
      (is (= (c/range-values w "Report!A2:D4") (c/range-values w2 "Report!A2:D4")))
      (is (= (c/formula w "Report!B2") (c/formula w2 "Report!B2"))))))

(deftest display-with-formats
  (let [w (-> (invoices)
              (c/set-style "Orders!A2" {:format "dddd, d. mmmm yyyy"})
              (c/set-style "Orders!F2" {:format "#,##0.00 \"EUR\""}))]
    (is (= "Monday, 15. January 2024" (c/display w "Orders!A2")))
    (is (= "25.00 EUR" (c/display w "Orders!F2")))
    (is (= "Acme" (c/display w "Orders!B2")))))
