(ns sutra.calx.fn.text-test
  (:require [clojure.test :refer [deftest is are testing]]
            [sutra.calx.core :as c]
            [sutra.calx.value :as v]
            [sutra.calx.fn.text :as text]
            [sutra.calx.fn.info :as info]))

(def wb (c/workbook {"Sheet1" [[1 "abc" true nil "x"]
                               ["a,b;c,d" 2.5 45000 "=1/0"]]}
                    {:functions (merge text/functions info/functions)}))

(defn calc [f] (c/calculate wb f))

(def VALUE (v/err :value))
(def NA (v/err :na))

(deftest concatenation
  (are [f r] (= r (calc f))
    "=CONCATENATE(\"a\",1,TRUE)" "a1TRUE"
    "=CONCATENATE(A1,D1,B1)" "1abc"
    "=CONCAT(A1:C1)" "1abcTRUE"
    "=CONCAT(A1:E1,\"!\")" "1abcTRUEx!"
    "=CONCAT(A1:D2)" (v/err :div0)
    "=TEXTJOIN(\", \",TRUE,A1:E1)" "1, abc, TRUE, x"
    "=TEXTJOIN(\"-\",FALSE,A1:E1)" "1-abc-TRUE--x"
    "=TEXTJOIN({\"-\",\"+\"},TRUE,\"a\",\"b\",\"c\",\"d\")" "a-b+c-d"
    "=TEXTJOIN(\",\",TRUE,\"\",\"\")" "")
  (is (= [["ax" "bx"]] (calc "=CONCATENATE({\"a\",\"b\"},\"x\")"))))

(deftest slicing
  (are [f r] (= r (calc f))
    "=LEFT(\"hello\")" "h"
    "=LEFT(\"hello\",2)" "he"
    "=LEFT(\"hello\",10)" "hello"
    "=LEFT(\"hello\",-1)" VALUE
    "=LEFTB(\"hello\",3)" "hel"
    "=RIGHT(\"hello\",3)" "llo"
    "=RIGHT(\"hello\")" "o"
    "=RIGHTB(\"hello\",9)" "hello"
    "=MID(\"hello\",2,3)" "ell"
    "=MID(\"hello\",10,3)" ""
    "=MID(\"hello\",0,3)" VALUE
    "=MIDB(\"hello\",4,9)" "lo"
    "=LEN(\"hello\")" 5.0
    "=LEN(D1)" 0.0
    "=LENB(12.5)" 4.0
    "=REPLACE(\"abcdef\",2,3,\"X\")" "aXef"
    "=REPLACE(\"abc\",5,1,\"X\")" "abcX"
    "=REPLACEB(\"abc\",1,1,\"\")" "bc"
    "=SUBSTITUTE(\"a-b-c\",\"-\",\"+\")" "a+b+c"
    "=SUBSTITUTE(\"a-b-c\",\"-\",\"+\",2)" "a-b+c"
    "=SUBSTITUTE(\"a-b-c\",\"-\",\"+\",3)" "a-b-c"
    "=SUBSTITUTE(\"a-b-c\",\"\",\"+\")" "a-b-c"
    "=SUBSTITUTE(\"a-b\",\"-\",\"+\",0)" VALUE)
  (is (= [["h" "w"]] (calc "=LEFT({\"hi\",\"world\"})"))))

(deftest searching
  (are [f r] (= r (calc f))
    "=FIND(\"l\",\"hello\")" 3.0
    "=FIND(\"l\",\"hello\",4)" 4.0
    "=FIND(\"L\",\"hello\")" VALUE
    "=FIND(\"\",\"hello\",2)" 2.0
    "=FIND(\"x\",\"hello\",9)" VALUE
    "=FINDB(\"o\",\"hello\")" 5.0
    "=SEARCH(\"L\",\"hello\")" 3.0
    "=SEARCH(\"l?o\",\"hello\")" 3.0
    "=SEARCH(\"h*o\",\"say hello\")" 5.0
    "=SEARCH(\"~*\",\"a*b\")" 2.0
    "=SEARCH(\"z\",\"hello\")" VALUE
    "=SEARCHB(\"E\",\"hello\")" 2.0))

(deftest case-and-cleanup
  (are [f r] (= r (calc f))
    "=LOWER(\"HeLLo\")" "hello"
    "=UPPER(\"HeLLo\")" "HELLO"
    "=PROPER(\"this is a TITLE\")" "This Is A Title"
    "=PROPER(\"2-way street's\")" "2-Way Street'S"
    "=TRIM(\"  a   b  c \")" "a b c"
    "=CLEAN(CHAR(9)&\"a\"&CHAR(10)&\"b\")" "ab"
    "=REPT(\"ab\",3)" "ababab"
    "=REPT(\"ab\",0)" ""
    "=REPT(\"ab\",-1)" VALUE
    "=REPT(\"ab\",20000)" VALUE
    "=EXACT(\"a\",\"a\")" true
    "=EXACT(\"a\",\"A\")" false
    "=EXACT(1,\"1\")" true
    "=T(\"a\")" "a"
    "=T(1)" ""
    "=T(D2)" (v/err :div0)
    "=ASC(\"ＡＢＣ　１\")" "ABC 1"
    "=DBCS(\"AB 1\")" "ＡＢ　１"))

(deftest char-codes
  (are [f r] (= r (calc f))
    "=CHAR(65)" "A"
    "=CHAR(128)" "€"
    "=CHAR(0)" VALUE
    "=CHAR(256)" VALUE
    "=CODE(\"A\")" 65.0
    "=CODE(\"€\")" 128.0
    "=CODE(\"\")" VALUE
    "=UNICHAR(9731)" "☃"
    "=UNICHAR(128512)" "😀"
    "=UNICHAR(0)" VALUE
    "=UNICHAR(55296)" NA
    "=UNICODE(\"😀\")" 128512.0
    "=UNICODE(\"a\")" 97.0))

(deftest conversion
  (are [f r] (= r (calc f))
    "=TEXT(1234.567,\"#,##0.00\")" "1,234.57"
    "=TEXT(0.25,\"0%\")" "25%"
    "=TEXT(C2,\"yyyy-mm-dd\")" "2023-03-15"
    "=TEXT(\"12\",\"0.00\")" "12.00"
    "=TEXT(\"abc\",\"0.00\")" "abc"
    "=TEXT(D1,\"0\")" "0"
    "=TEXT(-1,\"yyyy\")" VALUE
    "=TEXT(TRUE,\"0\")" "TRUE"
    "=VALUE(\"123.5\")" 123.5
    "=VALUE(\"1,234\")" 1234.0
    "=VALUE(\"50%\")" 0.5
    "=VALUE(\"$1,000\")" 1000.0
    "=VALUE(\"(12)\")" -12.0
    "=VALUE(\"12:00\")" 0.5
    "=VALUE(\"abc\")" VALUE
    "=VALUE(TRUE)" VALUE
    "=NUMBERVALUE(\"2.500,27\",\",\",\".\")" 2500.27
    "=NUMBERVALUE(\"3.5%\")" 0.035
    "=NUMBERVALUE(\" 1 000 \")" 1000.0
    "=NUMBERVALUE(\"\")" 0.0
    "=NUMBERVALUE(\"1,5\",\",\",\",\")" VALUE
    "=NUMBERVALUE(\"1.5.2\")" VALUE
    "=FIXED(1234.567)" "1,234.57"
    "=FIXED(1234.567,1,TRUE)" "1234.6"
    "=FIXED(1234.567,-1)" "1,230"
    "=FIXED(-1234.567,0)" "-1,235"
    "=DOLLAR(1234.567)" "$1,234.57"
    "=DOLLAR(-1234.567,-2)" "($1,200)"
    "=DOLLAR(0.5,0)" "$1"))

(deftest text-before-after
  (are [f r] (= r (calc f))
    "=TEXTBEFORE(\"a-b-c\",\"-\")" "a"
    "=TEXTBEFORE(\"a-b-c\",\"-\",2)" "a-b"
    "=TEXTBEFORE(\"a-b-c\",\"-\",-1)" "a-b"
    "=TEXTAFTER(\"a-b-c\",\"-\")" "b-c"
    "=TEXTAFTER(\"a-b-c\",\"-\",-1)" "c"
    "=TEXTAFTER(\"a-b-c\",\"-\",-2)" "b-c"
    "=TEXTBEFORE(\"a-b-c\",\"-\",3)" NA
    "=TEXTBEFORE(\"a-b-c\",\"-\",3,,,\"none\")" "none"
    "=TEXTBEFORE(\"a-b-c\",\"-\",3,,1)" "a-b-c"
    "=TEXTAFTER(\"aXbxc\",\"x\")" "c"
    "=TEXTAFTER(\"aXbxc\",\"x\",1,1)" "bxc"
    "=TEXTBEFORE(\"a,b;c\",{\";\",\",\"},2)" "a,b"
    "=TEXTBEFORE(\"abc\",\"\")" ""
    "=TEXTAFTER(\"abc\",\"\")" "abc"
    "=TEXTBEFORE(\"abc\",\"b\",0)" VALUE
    "=TEXTBEFORE(\"abc\",\"b\",9)" VALUE)
  (is (= [["a"] ["c"]] (calc "=TEXTBEFORE({\"a-b\";\"c-d\"},\"-\")"))))

(deftest text-split
  (are [f r] (= r (calc f))
    "=TEXTSPLIT(\"a,b,c\",\",\")" [["a" "b" "c"]]
    "=TEXTSPLIT(\"a,b,c\",,\",\")" [["a"] ["b"] ["c"]]
    "=TEXTSPLIT(A2,\",\",\";\")" [["a" "b"] ["c" "d"]]
    "=TEXTSPLIT(\"a,b;c\",\",\",\";\")" [["a" "b"] ["c" NA]]
    "=TEXTSPLIT(\"a,b;c\",\",\",\";\",,,\"-\")" [["a" "b"] ["c" "-"]]
    "=TEXTSPLIT(\"a,,b\",\",\")" [["a" "" "b"]]
    "=TEXTSPLIT(\"a,,b\",\",\",,TRUE)" [["a" "b"]]
    "=TEXTSPLIT(\"a1b2c\",{\"1\",\"2\"})" [["a" "b" "c"]]
    "=TEXTSPLIT(\"aXbxc\",\"x\",,,1)" [["a" "b" "c"]]
    "=TEXTSPLIT(\"abc\",\"\")" VALUE)
  (is (= [["a"] ["c"]] (calc "=TEXTSPLIT({\"a,b\";\"c,d\"},\",\")"))))

(deftest regex
  (are [f r] (= r (calc f))
    "=REGEXTEST(\"abc123\",\"[0-9]+\")" true
    "=REGEXTEST(\"ABC\",\"abc\")" false
    "=REGEXTEST(\"ABC\",\"abc\",1)" true
    "=REGEXTEST(\"abc\",\"[\")" VALUE
    "=REGEXEXTRACT(\"abc123def456\",\"[0-9]+\")" "123"
    "=REGEXEXTRACT(\"abc123def456\",\"[0-9]+\",1)" [["123"] ["456"]]
    "=REGEXEXTRACT(\"john@example.com\",\"(\\w+)@(\\w+)\",2)" [["john" "example"]]
    "=REGEXEXTRACT(\"abc\",\"[0-9]\")" NA
    "=REGEXREPLACE(\"a1b2c3\",\"[0-9]\",\"#\")" "a#b#c#"
    "=REGEXREPLACE(\"a1b2c3\",\"[0-9]\",\"#\",2)" "a1b#c3"
    "=REGEXREPLACE(\"a1b2c3\",\"[0-9]\",\"#\",-1)" "a1b2c#"
    "=REGEXREPLACE(\"a1b2c3\",\"[0-9]\",\"#\",9)" "a1b2c3"
    "=REGEXREPLACE(\"John Smith\",\"(\\w+) (\\w+)\",\"$2, $1\")" "Smith, John"
    "=REGEXREPLACE(\"ABC\",\"b\",\"x\",0,1)" "AxC"))

(deftest array-and-value-to-text
  (are [f r] (= r (calc f))
    "=ARRAYTOTEXT({1,\"a\";TRUE,2.5})" "1, a, TRUE, 2.5"
    "=ARRAYTOTEXT({1,\"a\";TRUE,2.5},1)" "{1,\"a\";TRUE,2.5}"
    "=ARRAYTOTEXT(A1:D1)" "1, abc, TRUE, "
    "=ARRAYTOTEXT(D2)" "#DIV/0!"
    "=ARRAYTOTEXT(A1,2)" VALUE
    "=VALUETOTEXT(\"a\")" "a"
    "=VALUETOTEXT(\"a\",1)" "\"a\""
    "=VALUETOTEXT(1.5)" "1.5"
    "=VALUETOTEXT(NA())" "#N/A"
    "=HYPERLINK(\"http://x.com\")" "http://x.com"
    "=HYPERLINK(\"http://x.com\",\"X\")" "X"
    "=HYPERLINK(\"http://x.com\",2)" 2.0))
