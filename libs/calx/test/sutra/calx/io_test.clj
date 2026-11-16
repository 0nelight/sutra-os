(ns sutra.calx.io-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [sutra.calx.core :as c]
            [sutra.calx.value :as v]
            [sutra.calx.io.edn :as edn-io]
            [sutra.calx.io.csv :as csv]
            [sutra.calx.io.xlsx :as xlsx])
  (:import (java.io ByteArrayOutputStream)
           (java.util.zip ZipOutputStream ZipEntry)))

(defn- sample []
  (-> (c/workbook {"Sheet1" [[1 2 "=A1+B1"] ["text" true "=1/0"] ["=not" nil "={1,2}*C1"]]
                   "My Data" [[10 "=Sheet1!C1*2"]]})
      (c/set-cell "A3" "'=not")
      (c/set-cell "E2" "#DIV/0!")
      (c/set-style "A1" {:format "0.00"})
      (c/define-name "Rate" "=0.5")
      (c/define-name "Local" "='My Data'!$A$1" "My Data")
      (c/set-cell "D1" "=C1*Rate")))

(deftest edn-roundtrip
  (let [w (sample)
        s (edn-io/write-string w)
        w2 (edn-io/read-string* s)]
    (is (str/includes? s "{:formula \"=A1+B1\"}"))
    (is (str/includes? s "#calx/error :div0") "literal errors survive")
    (is (= (c/sheet-values w "Sheet1") (c/sheet-values w2 "Sheet1")))
    (is (= (c/sheet-values w "My Data") (c/sheet-values w2 "My Data")))
    (is (= "=not" (c/value w2 "A3")) "text starting with = stays text")
    (is (= {:format "0.00"} (c/style w2 "A1")))
    (is (= (c/names w) (c/names w2)))
    (is (= (edn-io/workbook->doc w) (edn-io/workbook->doc w2)) "doc is a fixpoint")))

(deftest csv-io
  (is (= [["a" "b,c" "d\"e"] ["1" "" "x\ny"]] (csv/parse "a,\"b,c\",\"d\"\"e\"\r\n1,,\"x\ny\"")))
  (is (= "a,\"b,c\",\"d\"\"e\"" (csv/write [["a" "b,c" "d\"e"]])))
  (let [w (csv/import-csv (c/workbook) "Sheet1" "name,qty\napple,3\npear,5\ntotal,=B2+B3")]
    (is (= 3.0 (c/value w "B2")))
    (is (= 8.0 (c/value w "B4")))
    (is (= "name,qty\r\napple,3\r\npear,5\r\ntotal,8" (csv/export-csv w "Sheet1")))
    (is (str/ends-with? (csv/export-csv w "Sheet1" {:mode :inputs}) "total,=B2+B3"))))

(deftest xlsx-roundtrip
  (let [w (sample)
        bytes (xlsx/xlsx-bytes w)
        w2 (xlsx/read-xlsx bytes)]
    (is (= ["Sheet1" "My Data"] (c/sheet-names w2)))
    (is (= (c/sheet-values w "Sheet1") (c/sheet-values w2 "Sheet1")))
    (is (= (c/sheet-values w "My Data") (c/sheet-values w2 "My Data")))
    (is (= "=Sheet1!C1*2" (c/formula w2 "'My Data'!B1")))
    (is (= "=C1*Rate" (c/formula w2 "D1")))
    (is (= {:format "0.00"} (c/style w2 "A1")))
    (is (= (c/names w) (c/names w2)))
    (is (= [[3.0 6.0]] (c/range-values w2 "C3:D3")) "dynamic array re-spills")))

(deftest xlsx-future-functions
  (let [w (-> (c/workbook)
              (c/set-cell "A1" "=LET(x,2,LAMBDA(y,x*y)(3))"))
        w2 (xlsx/read-xlsx (xlsx/xlsx-bytes w))]
    (is (= 6.0 (c/value w2 "A1")))
    (is (= "=LET(x,2,LAMBDA(y,x*y)(3))" (c/formula w2 "A1"))
        "_xlfn./_xlpm. prefixes are written and stripped")))

(defn- zip-bytes [entries]
  (let [bos (ByteArrayOutputStream.)]
    (with-open [z (ZipOutputStream. bos)]
      (doseq [[path content] entries]
        (.putNextEntry z (ZipEntry. ^String path))
        (.write z (.getBytes ^String content "UTF-8"))
        (.closeEntry z)))
    (.toByteArray bos)))

(deftest xlsx-excel-style-file
  (testing "shared strings, shared formulas, booleans, errors, defined names"
    (let [ns-main "xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\""
          bytes (zip-bytes
                 [["_rels/.rels"
                   "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"x\" Target=\"xl/workbook.xml\"/></Relationships>"]
                  ["xl/workbook.xml"
                   (str "<workbook " ns-main
                        "><sheets><sheet name=\"Data\" sheetId=\"1\" r:id=\"rId1\"/></sheets>"
                        "<definedNames><definedName "
                        "name=\"Total\">Data!$C$4</definedName><definedName "
                        "name=\"_xlnm.Print_Area\" "
                        "localSheetId=\"0\">Data!$A$1:$C$4</definedName></definedNames></workbook>")]
                  ["xl/_rels/workbook.xml.rels"
                   "<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\"><Relationship Id=\"rId1\" Type=\"ws\" Target=\"worksheets/sheet1.xml\"/><Relationship Id=\"rId2\" Type=\"ss\" Target=\"sharedStrings.xml\"/></Relationships>"]
                  ["xl/sharedStrings.xml"
                   "<sst xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\"><si><t>qty</t></si><si><r><t>pr</t></r><r><t>ice</t></r></si></sst>"]
                  ["xl/worksheets/sheet1.xml"
                   (str "<worksheet " ns-main "><sheetData>"
                        "<row r=\"1\"><c r=\"A1\" t=\"s\"><v>0</v></c><c r=\"B1\" "
                        "t=\"s\"><v>1</v></c><c r=\"D1\" t=\"b\"><v>1</v></c><c r=\"E1\" "
                        "t=\"e\"><v>#N/A</v></c></row>"
                        "<row r=\"2\"><c r=\"A2\"><v>2</v></c><c r=\"B2\"><v>1.5</v></c><c "
                        "r=\"C2\"><f t=\"shared\" ref=\"C2:C3\" "
                        "si=\"0\">A2*B2</f><v>3</v></c></row>"
                        "<row r=\"3\"><c r=\"A3\"><v>4</v></c><c r=\"B3\"><v>2</v></c><c "
                        "r=\"C3\"><f t=\"shared\" si=\"0\"/><v>8</v></c></row>"
                        "<row r=\"4\"><c r=\"C4\"><f>_xlfn.SINGLE(C2)+C3</f><v>11</v></c><c "
                        "r=\"D4\"><f>Total*2</f></c></row>"
                        "</sheetData></worksheet>")]])
          w (xlsx/read-xlsx bytes)]
      (is (= "qty" (c/value w "A1")))
      (is (= "price" (c/value w "B1")) "rich text runs are concatenated")
      (is (= true (c/value w "D1")))
      (is (= (v/err :na) (c/value w "E1")))
      (is (= "=A3*B3" (c/formula w "C3")) "shared formulas are expanded")
      (is (= 8.0 (c/value w "C3")))
      (is (= {"Total" {:scope nil :formula "=Data!$C$4"}} (c/names w))
          "built-in _xlnm names are skipped"))))
