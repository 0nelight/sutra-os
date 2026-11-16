(ns ^:unit platform.python-wire-exactness-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [sutra.compute.external :as ext]
            [sutra.compute.sandbox :as sandbox]))

(def ^:private echo-code
  "from decimal import Decimal
from sutra_transforms import transform, Input, Output

@transform(inputs={'src': Input(kind='object-type')},
           output=Output(kind='object-type', object_type='beleg',
                         primary_key='id'))
def echo(src):
    out = []
    for r in src:
        out.append({
            'id': r['id'],
            'betrag': r['betrag'],
            'betrag_type': type(r['betrag']).__name__,
            'betrag_plus_cent': r['betrag'] + Decimal('0.01'),
            'n': r['n'],
            'n_type': type(r['n']).__name__,
            'gross': r['gross'],
            'gross_type': type(r['gross']).__name__,
            'auftrag': r['auftrag'],
            'auftrag_type': type(r['auftrag']).__name__,
            'quote': r['quote'],
            'quote_type': type(r['quote']).__name__,
            'pfad': r['pfad'],
            'preis': r['preis'],
        })
    return out
")

(def ^:private in-rows
  [{:id "1"
    :betrag (bigdec "12345678901234567.89")
    :n 9007199254740993
    :gross (biginteger 99999999999999999999N)
    :auftrag "58319870951433"
    :quote 3.14159265
    :pfad "~/inbox/beleg.pdf"
    :preis (BigDecimal. "10.00")}])

(defn- run-echo
  []
  (let [path (ext/export-rows-json! in-rows)]
    (first (:rows (ext/run-transform-local!
                   {:transform-name :wire-echo :code echo-code}
                   {:input-paths [path]
                    :inputs-manifest {:src {:kind :object-type :path path}}})))))

(deftest the-exported-json-carries-the-type-not-the-magnitude
  (let [raw (json/read-str (slurp (ext/export-rows-json! in-rows)))
        row (first raw)]
    (testing "a decimal has no exact JSON form, so it rides tagged with its digits"
      (is (= "~f12345678901234567.89" (get row "betrag")))
      (testing "and the declared scale survives — a price keeps its cents column"
        (is (= "~f10.00" (get row "preis"))))
      (testing "no exponent: the digits are sent, not a form to re-parse"
        (is (= "~f100000000000000000000"
               (get (first (json/read-str
                            (slurp (ext/export-rows-json! [{:v (bigdec 1e20M)}]))))
                    "v")))))
    (testing "an integer is exact in JSON on both sides — it stays a bare number"
      (is (= 9007199254740993 (get row "n")))
      (is (number? (get row "n")))
      (testing "including a BigInteger no Long could hold"
        (is (number? (get row "gross")))
        (is (== 99999999999999999999N (get row "gross")))))
    (testing "a double is inexact by declaration — nothing to protect"
      (is (number? (get row "quote"))))
    (testing "a string keeping a leading ~ is escaped, so one ~ always means a tag"
      (is (= "~~/inbox/beleg.pdf" (get row "pfad"))))
    (testing "a numeric-LOOKING string is not touched — the content decides nothing"
      (is (= "58319870951433" (get row "auftrag"))))))

(deftest a-betrag-survives-jvm-python-jvm-unchanged
  (let [row (run-echo)]
    (testing "the body received a decimal.Decimal, not a float"
      (is (= "Decimal" (:betrag_type row))))
    (testing "every digit came back — this is the value the build would store"
      (is (= "12345678901234567.89" (.toPlainString ^BigDecimal (:betrag row))))
      (is (instance? BigDecimal (:betrag row))))
    (testing "python computed on it EXACTLY: one cent added, no digit lost"
      (is (= "12345678901234567.90"
             (.toPlainString ^BigDecimal (:betrag_plus_cent row)))))
    (testing "the declared scale survives the return trip too"
      (is (= "10.00" (.toPlainString ^BigDecimal (:preis row)))))))

(deftest a-string-that-looks-like-a-number-stays-a-string
  (let [row (run-echo)]
    (testing "the counter-case: nothing lifted it, because nothing guessed"
      (is (= "str" (:auftrag_type row)))
      (is (= "58319870951433" (:auftrag row)))
      (is (string? (:auftrag row))))
    (testing "and a string carrying its own ~ is handed back whole"
      (is (= "~/inbox/beleg.pdf" (:pfad row))))))

(deftest an-exact-integer-and-an-inexact-double-keep-their-types
  (let [row (run-echo)]
    (testing "9007199254740993 — past 2^53−1, exact as an int at both ends"
      (is (= "int" (:n_type row)))
      (is (= 9007199254740993 (:n row))))
    (testing "and a BigInteger, which python holds as an int just as exactly"
      (is (= "int" (:gross_type row)))
      (is (== 99999999999999999999N (:gross row))))
    (testing "a double stays a float and a number; it had nothing left to lose"
      (is (= "float" (:quote_type row)))
      (is (= 3.14159265 (:quote row))))))

(deftest previous-output-is-lifted-too-though-it-carries-no-manifest-entry
  (testing "$SUTRA_PREVIOUS_OUTPUT is a bare path — the tag is the only type it has"
    (let [prev (ext/export-rows-json! [{:id "1" :betrag (bigdec "12345678901234567.89")}])
          code "from sutra_transforms import transform, Input, Output, previous_output

@transform(inputs={'src': Input(kind='object-type')},
           output=Output(kind='object-type', object_type='beleg',
                         primary_key='id'),
           incremental=True)
def echo_prev(src):
    p = previous_output()[0]
    return [{'id': 'p', 'betrag': p['betrag'],
             'betrag_type': type(p['betrag']).__name__}]
"
          src  (ext/export-rows-json! [{:id "x"}])
          row  (first (:rows (ext/run-transform-local!
                              {:transform-name :wire-prev :code code}
                              {:input-paths [src]
                               :inputs-manifest {:src {:kind :object-type :path src}}
                               :previous-path prev})))]
      (is (= "Decimal" (:betrag_type row)))
      (is (= "12345678901234567.89" (.toPlainString ^BigDecimal (:betrag row)))))))

(deftest the-sandbox-is-present-so-a-green-run-means-the-wire-was-crossed
  (testing "no bwrap would mean these tests proved nothing at all"
    (is (sandbox/available?))
    (is (io/resource "python/sutra_transforms.py"))))
