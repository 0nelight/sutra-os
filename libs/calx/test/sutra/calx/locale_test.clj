(ns sutra.calx.locale-test
  (:require [clojure.test :refer [deftest is testing]]
            [sutra.calx.core :as c]
            [sutra.calx.format :as fmt]
            [sutra.calx.i18n :as i18n]
            [sutra.calx.io.csv :as csv]
            [sutra.calx.io.edn :as edn]
            [sutra.calx.value :as v]))

(defn- de
  ([] (de [[]]))
  ([rows] (c/workbook {"Tabelle1" rows} {:config {:locale :de :now 46328.5}})))

(def ^:private typed
  [["1,5"        1.5           "1,5"]
   ["1.234,5"    1234.5        "1234,5"]
   ["-0,25"      -0.25         "-0,25"]
   ["12%"        0.12          "0,12"]
   ["1.5"        "1.5"         "1.5"]
   ["WAHR"       true          "WAHR"]
   ["falsch"     false         "FALSCH"]
   ["#NV"        (v/err :na)   "#NV"]
   ["'1,5"       "1,5"         "1,5"]
   ["02.11.2026" 46328.0       "02.11.2026"]
   ["2.11.26"    46328.0       "02.11.2026"]
   ["2/11/2026"  46328.0       "02.11.2026"]
   ["2026-11-02" 46328.0       "02.11.2026"]
   ["13:30"      (/ 13.5 24)   "13:30"]
   ["Hallo"      "Hallo"       "Hallo"]])

(deftest typed-input
  (doseq [[in value shown] typed
          :let [w (c/set-cell (de) "A1" in)]]
    (testing in
      (is (= value (c/value w "A1")))
      (is (= shown (c/display w "A1"))))))

(deftest edits-are-canonical
  (let [w   (reduce (fn [w [a x]] (c/set-cell w a x)) (de)
                    [["A1" "1,5"] ["A2" "02.11.2026"] ["A3" "1.5"] ["A4" "WAHR"]
                     ["A5" "=SUMME(A1;2,5)"]])
        log (mapv (fn [[[_ _ x]]] x) (c/edit-log w))]
    (is (= [1.5 "2026-11-02" "'1.5" true "=SUM(A1,2.5)"] log))
    (testing "the log replays alike without a locale"
      (let [w' (c/replay (c/workbook {"Tabelle1" [[]]}) (c/edit-log w))]
        (is (= (map #(c/value w %) ["A1" "A2" "A3" "A4" "A5"])
               (map #(c/value w' %) ["A1" "A2" "A3" "A4" "A5"])))
        (is (= "11/2/2026" (c/display w' "A2")) "a system format shows as its locale writes it")))))

(deftest edit-text
  (let [w (-> (de) (c/set-cell "A1" "1,5") (c/set-cell "A2" "02.11.2026")
              (c/set-cell "A3" "1.5") (c/set-cell "A4" "=WENN(A1>1;WAHR;#NV)"))]
    (is (= ["1,5" "02.11.2026" "1.5" "=WENN(A1>1;WAHR;#NV)"]
           (map #(c/input w %) ["A1" "A2" "A3" "A4"])))
    (is (= "=IF(A1>1,TRUE,#N/A)" (get-in (edn/workbook->doc w) [:sheets 0 :cells "A4" :formula])))))

(def ^:private formulas
  [["=\"x\"&1,5"                      "x1,5"]
   ["=\"1,5\"*2"                      3.0]
   ["=LÄNGE(1,5)"                     3.0]
   ["=VERKETTEN(WAHR;\"-\";2,5)"      "WAHR-2,5"]
   ["=TEXTVERKETTEN(\";\";WAHR;1,5;2)" "1,5;2"]
   ["=TEXT(1234,5;\"#.##0,00\")"      "1.234,50"]
   ["=TEXT(46328;\"TTTT, T. MMMM JJJJ\")" "Montag, 2. November 2026"]
   ["=TEXT(46328;\"TTT MMM\")"        "Mo Nov"]
   ["=WERT(\"1.234,5\")"              1234.5]
   ["=WERT(\"12,50 €\")"              12.5]
   ["=ZAHLENWERT(\"1,5\")"            1.5]
   ["=FEST(1234,567;2)"               "1.234,57"]
   ["=DM(-1234,567)"                  "-1.234,57 €"]
   ["=DATWERT(\"15. März 2026\")"     46096.0]
   ["=DATWERT(\"15.03.2026\")"        46096.0]
   ["=WENNFEHLER(#NV;0)"              0.0]
   ["=FEHLER.TYP(#WERT!)"             3.0]

   ["=MATRIXZUTEXT({1,5.2;3.4};1)"    "{1,5.2;3.4}"]])

(deftest formulas-in-the-locale
  (doseq [[f want] formulas]
    (is (= want (c/calculate (de) f)) f)))

(deftest formula-text-and-names
  (let [w (-> (de [[1 2]])
              (c/define-name "Satz" "=0,19")
              (c/set-cell "C1" "=SUMME(A1:B1)*Satz")
              (c/set-cell "D1" "=FORMELTEXT(C1)"))]
    (is (< (Math/abs (- 0.57 (c/value w "C1"))) 1e-12))
    (is (= 2.0 (c/calculate w "=ZÄHLENWENN(A1:B1;\">0,5\")")))
    (is (= "=SUMME(A1:B1)*Satz" (c/value w "D1")))
    (is (= "=0,19" (get-in (c/names w) ["Satz" :formula])))
    (is (some #{"SUMME"} (c/function-names w)))))

(def ^:private codes
  [["#.##0,00"            "#,##0.00"]
   ["0,0%"                "0.0%"]
   ["TT.MM.JJJJ hh:mm"    "dd.mm.yyyy hh:mm"]
   ["MMM JJ"              "mmm yy"]
   ["Standard"            "General"]
   ["#.##0 \"Stück\""     "#,##0 \"Stück\""]
   ["#.##0,00;[Rot]-#.##0,00" "#,##0.00;[Red]-#,##0.00"]])

(deftest format-codes
  (doseq [[local canonical] codes]
    (is (= canonical (i18n/delocalize-format local :de)) local)
    (is (= local (i18n/localize-format canonical :de)) canonical))
  (let [w (-> (de) (c/set-cell "A1" "1234,5") (c/set-style "A1" {:format "#.##0,00 \"€\""}))]
    (is (= "#,##0.00 \"€\"" (get-in (edn/workbook->doc w) [:sheets 0 :styles "A1" :format])))
    (is (= "#.##0,00 \"€\"" (:format (c/style w "A1"))))
    (is (= "1.234,50 €" (c/display w "A1")))))

(deftest csv-in-the-locale
  (let [w (csv/import-csv (de) "Tabelle1" "1,5;WAHR;02.11.2026\n\"=SUMME(A1;1)\";x;")]
    (is (= [1.5 true 46328.0 2.5 "x"] (map #(c/value w %) ["A1" "B1" "C1" "A2" "B2"])))
    (is (= "1,5;WAHR;46328\r\n2,5;x;" (csv/export-csv w "Tabelle1")))
    (is (= "1,5;WAHR;02.11.2026\r\n\"=SUMME(A1;1)\";x;"
           (csv/export-csv w "Tabelle1" {:mode :inputs})))
    (testing "RFC 4180's comma when asked for"
      (is (= "\"1,5\",WAHR,46328\r\n\"2,5\",x,"
             (csv/export-csv w "Tabelle1" {:sep :rfc4180}))))))

(deftest canonical-without-a-locale
  (let [w (-> (c/workbook {"Sheet1" [[]]}) (c/set-cell "A1" "1,5") (c/set-cell "A2" "1,234.5"))]
    (is (= "1,5" (c/value w "A1")))
    (is (= 1234.5 (c/value w "A2")))
    (is (= "TRUE" (v/display true)))
    (is (nil? (i18n/locale :xx)))))

(deftest colors
  (testing "a section's colour, named or by palette index"
    (is (= {:text "-5" :color :red} (fmt/render -5 "#,##0;[Red]-#,##0")))
    (is (= {:text "5"} (fmt/render 5 "#,##0;[Red]-#,##0")))
    (is (= {:text "7" :color :red} (fmt/render 7 "[Color3]0")))
    (is (= {:text "7" :color 20} (fmt/render 7 "[Color20]0")))
    (is (= {:text "x" :color :blue} (fmt/render "x" "0;0;0;[Blue]@"))))
  (testing "colour names translate with the code"
    (is (= "[Red]#,##0.00;[Blue]-#,##0.00"
           (i18n/delocalize-format "[Rot]#.##0,00;[Blau]-#.##0,00" :de)))
    (is (= "[Rot]#.##0,00;[Blau]-#.##0,00"
           (i18n/localize-format "[Red]#,##0.00;[Blue]-#,##0.00" :de)))
    (is (= "[Farbe12]0" (i18n/localize-format "[Color12]0" :de)))
    (is (= "[$€-407] #.##0" (i18n/localize-format "[$€-407] #,##0" :de)))
    (is (= "[>=1000]#.##0" (i18n/localize-format "[>=1000]#,##0" :de))))
  (let [w (-> (de) (c/set-cell "A1" "-1,5") (c/set-style "A1" {:format "0,0;[Rot]-0,0"}))]
    (is (= {:text "-1,5" :color :red} (c/rendered w "A1")))))
