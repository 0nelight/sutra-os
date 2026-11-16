(ns ^:unit platform.csv-rfc4180-test
  (:require [clojure.data.csv :as csv]
            [clojure.test :refer [deftest is testing]]
            [sutra.builds.interchange :as ix]
            [sutra.capps.object-display :as od]))

(def ^:private tricky ["plain" "a,b" "say \"hi\"" "cr\rhere" "lf\nhere" ""])

(deftest the-dataset-export-is-rfc-4180
  (let [text (ix/csv ["x"] (map vector tricky))]
    (is (= (str "x\r\nplain\r\n\"a,b\"\r\n\"say \"\"hi\"\"\"\r\n\"cr\rhere\"\r\n"
                "\"lf\nhere\"\r\n\r\n")
           text))
    (testing "a reader of RFC 4180 gets every field back unchanged"
      (is (= (cons ["x"] (map vector tricky)) (csv/read-csv text))))
    (testing "another separator only on request"
      (is (= "a;b\r\n\"1;2\";3\r\n" (ix/csv ["a" "b"] [["1;2" "3"]] {:separator \;}))))))

(deftest the-app-export-quotes-cr
  (is (= "x\r\n\"cr\rhere\"" (od/csv-text {:header ["x"] :rows [["cr\rhere"]]}))))

(deftest a-file-row-is-typed-by-the-columns
  (let [cols [{:attribute-name :n :attribute-type :long}
              {:attribute-name :amt :attribute-type [:decimal 12 2]}
              {:attribute-name :d :attribute-type :date}
              {:attribute-name :ts :attribute-type :timestamp}
              {:attribute-name :s :attribute-type :string}]
        [{v :values}] (ix/file-rows cols [{"n" "7" "amt" "12.50" "d" "2026-11-11"
                                           "ts" "2026-11-11T10:00:00Z" "s" ""
                                           "extra" "x"}])]
    (is (= 7 (:n v)))
    (is (= 12.50M (:amt v)))
    (is (= (java.time.LocalDate/parse "2026-11-11") (:d v)))
    (is (= (java.time.Instant/parse "2026-11-11T10:00:00Z") (:ts v)))
    (is (= "" (:s v)) "an empty string column stays a string")
    (is (= "x" (:extra v)) "an undeclared column stays, so the write names it")
    (testing "text that does not parse stays text"
      (is (= "seven" (:n (:values (first (ix/file-rows cols [{"n" "seven"}])))))))))

(deftest a-stored-value-is-plain-json
  (is (= "12.50" (ix/plain 12.50M)))
  (is (= "2026-11-11T10:00:00Z" (ix/plain (java.time.Instant/parse "2026-11-11T10:00:00Z"))))
  (is (= {"k" ["v" "1.0"]} (ix/plain {:k [:v 1.0M]}))))
